/*
 * elf.c - ELF file format handlers
 *
 * Copyright 2010 Rui Ueyama <rui314@gmail.com>.  All rights reserved.
 * This code is available under the simplified BSD license.  See LICENSE for details.
 */

#include "8cc.h"

/*
 * Functions to create an ELF format object file that you can link to create an
 * executable.
 *
 * ELF object file consists with the file header and one or more "sections".
 * Some sections contains data used by executable itself, such as data section
 * (contains initialized data), text section (code), or bss (uninitialized
 * data).  Other sections are used by linkers and loaders.  These includes
 * relocation information and symbol tables.
 *
 * In order to understand what the functions in this file actually do, you may
 * want to read documents regarding ELF format first.  Here is list of documents
 * I found useful for the purpose.
 *
 *   Linkers and Loaders by John R. Levine, published by Morgan-Kauffman in
 *   October 1999, ISBN 1-55860-496-0.
 *   http://linker.iecc.com/
 *
 *   Tool Interface Standard (TIS) Executable and Linking Format (ELF)
 *   Specification - Version 1.2 (May 1995)
 *   http://refspecs.freestandards.org/elf/elf.pdf
 *
 *   ELF-64 Object File Format - Version 1.5 Draft 2 (May 27, 1998)
 *   http://downloads.openwatcom.org/ftp/devel/docs/elf-64-gen.pdf
 *
 *   Ulrich Drepper (August 20, 2006). How To Write Shared Libraries. 4.0.
 *   http://people.redhat.com/drepper/dsohowto.pdf
 */

/* First 16 bytes of ELF file on x86-64. */
static u8 elf_ident[] = {0x7f, 0x45, 0x4c, 0x46, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0};

static Elf *make_elf(void) {
    Elf *elf = malloc(sizeof(Elf));
    elf->sections = make_list();
    elf->shnum = 0;
    elf->symtabnum = 0;
    elf->syms = make_string_dict();
    return elf;
}

static Section *make_section(char *name, int type) {
    Section *sect = malloc(sizeof(Section));
    sect->body = make_string();
    sect->name = malloc(strlen(name) + 1);
    strcpy(sect->name, name);
    sect->shstrtab_off = 0;
    sect->type = type;
    sect->flags = 0;
    sect->align = 1;
    sect->rels = make_list();
    sect->link = 0;
    sect->info = 0;
    sect->entsize = 0;
    sect->symindex = 0;
    return sect;
}

static void add_section(Elf *elf, Section *sect) {
    list_push(elf->sections, sect);
    sect->shndx = LIST_LEN(elf->sections);
}

Elf *new_elf(void) {
    Elf *elf = make_elf();

    Section *data = make_section(".data", SHT_PROGBITS);
    data->flags = SHF_ALLOC | SHF_WRITE;
    data->align = 4;
    add_section(elf, data);

    Section *text = make_section(".text", SHT_PROGBITS);
    text->flags = SHF_ALLOC | SHF_EXECINSTR;
    text->align = 16;
    add_section(elf, text);
    return elf;
}

/*============================================================
 * Symbol table
 */

static void write_one_symbol(Symbol *sym, int *index, String *symtab, String *strtab) {
    if (sym->name) {
        o4(symtab, STRING_LEN(strtab)); // st_name
        ostr(strtab, STRING_BODY(sym->name));
    } else {
        o4(symtab, 0); // st_name
    }
    o1(symtab, ELF64_ST_INFO(sym->bind, sym->type)); // st_info;
    o1(symtab, 0); // st_other;
    if (sym->defined) {
        o2(symtab, sym->section->shndx); // st_shndx
    } else {
        o2(symtab, 0); // st_shndx
    }
    o8(symtab, sym->value); // st_value
    o8(symtab, 0); // st_size
    sym->index = (*index)++;
}

/*
 * Symbols whose attribute is LOCAL must come before other symbols in symbol
 * table.  This function is called twice.  In the first pass, localonly is false
 * so this outputs local symbols.  In the second pass, this outputs non-local
 * ones.
 */
static void write_sym_to_buf(Elf *elf, int *index, String *symtab, String *strtab, bool localonly) {
    DictIter *iter = make_dict_iter(elf->syms);
    void **p;
    for (p = dict_iter_next(iter); p; p = dict_iter_next(iter)) {
        Symbol *sym = p[1];
        if (localonly && sym->bind != STB_LOCAL)
            continue;
        if (!localonly && sym->bind == STB_LOCAL)
            continue;
        write_one_symbol(sym, index, symtab, strtab);
    }
}

static void write_section_sym(Elf *elf, int *index, String *symtab, String *strtab) {
    for (int i = 0; i < LIST_LEN(elf->sections); i++) {
        Section *sect = LIST_REF(elf->sections, i);
        Symbol *sym = make_symbol(NULL, sect, 0, STB_LOCAL, STT_SECTION, 1);
        write_one_symbol(sym, index, symtab, strtab);
        sect->symindex = sym->index;
    }
}

static void add_symtab(Elf *elf) {
    String *symtabb = make_string();
    String *strtabb = make_string();
    o1(strtabb, 0);
    // Null symbol
    for (int i = 0; i < 24; i++) o1(symtabb, 0);
    // File symbol
    o4(symtabb, STRING_LEN(strtabb)); // st_name
    ostr(strtabb, "noname");
    o1(symtabb, ELF64_ST_INFO(STB_LOCAL, STT_FILE)); // st_info
    o1(symtabb, 0); // other
    o2(symtabb, SHN_ABS); // st_shndx
    o8(symtabb, 0); // st_value
    o8(symtabb, 0); // st_size

    int index = 2;
    write_sym_to_buf(elf, &index, symtabb, strtabb, true);
    int localidx = index;
    write_section_sym(elf, &index, symtabb, strtabb);
    write_sym_to_buf(elf, &index, symtabb, strtabb, false);
    elf->symtabnum = LIST_LEN(elf->sections) + 1;

    Section *symtab = make_section(".symtab", SHT_SYMTAB);
    symtab->body = symtabb;
    symtab->link = LIST_LEN(elf->sections) + 2;
    symtab->info = localidx + 2;
    symtab->entsize = 24;
    symtab->align = 4;
    add_section(elf, symtab);

    Section *strtab = make_section(".strtab", SHT_STRTAB);
    strtab->body = strtabb;
    add_section(elf, strtab);
}

/*============================================================
 * Relocations
 */

static Symbol *find_symbol(Elf *elf, char *name) {
    Symbol *sym = dict_get(elf->syms, to_string(name));
    if (!sym)
        error("cannot find symbol '%s'", name);
    return sym;
}

static void add_reloc(Elf *elf) {
    char name[100];
    for (int i = 0; i < LIST_LEN(elf->sections); i++) {
        Section *sect = LIST_REF(elf->sections, i);
        if (LIST_LEN(sect->rels) == 0)
            continue;
        String *b = make_string();
        for (int j = 0; j < LIST_LEN(sect->rels); j++) {
            Reloc *rel = LIST_REF(sect->rels, j);
            o8(b, rel->off);
            if (rel->sym) {
                o8(b, ELF64_R_INFO(find_symbol(elf, rel->sym)->index, rel->type));
            } else {
                o8(b, ELF64_R_INFO(find_section(elf, rel->section)->symindex, rel->type));
            }
            o8(b, rel->addend);
        }

        strcpy(name, ".rela");
        strcpy(name + 5, sect->name);
        Section *relsec = make_section(name, SHT_RELA);
        relsec->link = elf->symtabnum;
        relsec->info = i + 1;
        relsec->body = b;
        relsec->entsize = 24;
        relsec->align = 4;
        add_section(elf, relsec);
    }
}

/*============================================================
 * ".shstrtab" section
 */

static void add_shstrtab(Elf *elf) {
    Section *shstr = make_section(".shstrtab", SHT_STRTAB);
    elf->shnum = LIST_LEN(elf->sections) + 1;
    add_section(elf, shstr);
    String *b = make_string();
    o1(b, 0);
    for (int i = 0; i < LIST_LEN(elf->sections); i++) {
        Section *sect = LIST_REF(elf->sections, i);
        sect->shstrtab_off = STRING_LEN(b);
        ostr(b, sect->name);
    }
    shstr->body = b;
}

/*============================================================
 * Outputs ELF file
 */

static void write_section(String *header, String *content, Section *sect, int offset) {
    o4(header, sect->shstrtab_off); // sh_name
    o4(header, sect->type); // sh_type
    o8(header, sect->flags); // sh_flags
    o8(header, 0); // sh_addr
    o8(header, STRING_LEN(content) + offset); // sh_offset
    o8(header, STRING_LEN(sect->body)); // sh_size
    o4(header, sect->link); // sh_link = SHN_UNDEF
    o4(header, sect->info); // sh_info
    o8(header, sect->align); // sh_addralign
    o8(header, sect->entsize); // sh_entsize
    out(content, STRING_BODY(sect->body), STRING_LEN(sect->body));
    align(content, 16);
}

void write_elf(FILE *outfile, Elf *elf) {
    add_symtab(elf);
    add_reloc(elf);
    add_shstrtab(elf);

    // Section header
    String *sh = make_string();
    for (int i = 0; i < 64; i++)
        o1(sh, 0); // NULL section header

    // Body
    String *content = make_string();
    for (int i = 0; i < LIST_LEN(elf->sections); i++) {
        write_section(sh, content, LIST_REF(elf->sections, i), 64);
    }
    align(content, 16);

    // ELF header
    String *eh = make_string();
    int numsect = LIST_LEN(elf->sections) + 1;
    out(eh, elf_ident, sizeof(elf_ident));
    o2(eh, 1);  // e_type = ET_REL
    o2(eh, 62); // e_machine = EM_X86_64
    o4(eh, 1);  // e_version = EV_CURRENT
    o8(eh, 0);  // e_entry
    o8(eh, 0);  // e_phoff
    o8(eh, STRING_LEN(content) + 64);  // e_shoff;
    o4(eh, 0);  // e_flags
    o2(eh, 64); // e_ehsize
    o2(eh, 0);  // e_phentsize
    o2(eh, 0);  // e_phnum
    o2(eh, 64); // e_shentsize
    o2(eh, numsect);  // e_shnum
    o2(eh, elf->shnum);  // e_shstrndx

    fwrite(STRING_BODY(eh), STRING_LEN(eh), 1, outfile);
    fwrite(STRING_BODY(content), STRING_LEN(content), 1, outfile);
    fwrite(STRING_BODY(sh), STRING_LEN(sh), 1, outfile);
    fclose(outfile);
}

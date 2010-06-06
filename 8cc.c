/* -*- c-basic-offset: 4 -*- */

#include "8cc.h"

static unsigned char elf_ident[] = {0x7f, 0x45, 0x4c, 0x46, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0};

static char hello_world_insn[] = {
    0x55,                       // push %rbp
    0x48, 0x89, 0xe5,           // mov %rsp, %rbp
    0x48, 0xbf, 0, 0, 0, 0, 0,  // mov $0x0, %rdi
    0, 0, 0,
    0x31, 0xc0,                 // xor %eax, %eax
    0xe8, 0, 0, 0, 0,           // callq 15 <main+0x15>
    0x31, 0xc0,                 // xor %eax, %eax
    0xc9,                       // leaveq
    0xc3 };                     // retq

static char hello_world_str[] = "Hello, world!\n";

static Elf *new_elf(void) {
    Elf *elf = malloc(sizeof(Elf));
    elf->size = 0;
    elf->shnum = 0;
    elf->symtabnum = 0;
    elf->syms = make_list();
    return elf;
}

static Section *new_section(char *name, int type) {
    Section *sect = malloc(sizeof(Section));
    sect->off = 0;
    sect->data = malloc(1024*10);
    memset(sect->data, 0, 1024*10);
    sect->size = 0;
    sect->name = malloc(strlen(name) + 1);
    sect->shstrtab_off = 0;
    sect->type = type;
    sect->flags = 0;
    sect->align = 1;
    sect->syms = make_list();
    sect->rels = make_list();
    sect->link = 0;
    sect->info = 0;
    strcpy(sect->name, name);
    sect->entsize = 0;
    return sect;
}

static void write_sym_to_buf(Elf *elf, StringBuilder *b, StringBuilder *b2, bool localonly) {
    for (int i = 0; i < elf->size; i++) {
        Section *sect = elf->sections[i];
        for (int j = 0; j < LIST_LEN(sect->syms); j++) {
            Symbol *sym = LIST_ELEM(Symbol, sect->syms, j);
            if (localonly && sym->bind != STB_LOCAL)
                continue;
            if (!localonly && sym->bind == STB_LOCAL)
                continue;
            if (sym->name) {
                o4(b, SBUILDER_LEN(b2)); // st_name
                ostr(b2, sym->name);
            } else {
                o4(b, 0); // st_name
            }
            o1(b, ELF64_ST_INFO(sym->bind, sym->type)); // st_info;
            o1(b, 0); // st_other;
            if (sym->defined) {
                sym->sectidx = i + 1;
                o2(b, sym->sectidx); // st_shndx
            } else {
                o2(b, 0); // st_shndx
            }
            o8(b, sym->value); // st_value
            o8(b, 0); // st_size
            list_push(elf->syms, sym);
        }
    }
}

static void add_symtab(Elf *elf) {
    StringBuilder *b = make_sbuilder();
    StringBuilder *b2 = make_sbuilder();
    o1(b2, 0);
    // Null symbol
    for (int i = 0; i < 24; i++) o1(b, 0);
    // File symbol
    o4(b, SBUILDER_LEN(b2)); // st_name
    ostr(b2, "hello.asm");
    o1(b, ELF64_ST_INFO(STB_LOCAL, STT_FILE)); // st_info
    o1(b, 0); // other
    o2(b, SHN_ABS); // st_shndx
    o8(b, 0); // st_value
    o8(b, 0); // st_size

    write_sym_to_buf(elf, b, b2, true);
    int localidx = LIST_LEN(elf->syms);
    write_sym_to_buf(elf, b, b2, false);
    elf->symtabnum = elf->size + 1;

    Section *symtab = new_section(".symtab", SHT_SYMTAB);
    symtab->data = SBUILDER_BODY(b);
    symtab->off = symtab->size = SBUILDER_LEN(b);
    symtab->link = elf->size + 2;
    symtab->info = localidx + 2;
    symtab->entsize = 24;
    symtab->align = 4;
    elf->sections[elf->size++] = symtab;

    Section *strtab = new_section(".strtab", SHT_STRTAB);
    strtab->data = SBUILDER_BODY(b2);
    strtab->off = strtab->size = SBUILDER_LEN(b2);
    elf->sections[elf->size++] = strtab;
}

static int find_symbol(Elf *elf, char *sym) {
    for (int i = 0; i < LIST_LEN(elf->syms); i++) {
        char *name = LIST_ELEM(Symbol, elf->syms, i)->name;
        if (name && strcmp(name, sym) == 0)
            return i + 2;
    }
    fprintf(stderr, "8cc: cannot find symbol '%s'\n", sym);
    exit(-1);
}

static int find_section(Elf *elf, char *name) {
    for (int i = 0; i < elf->size; i++) {
        if (strcmp(elf->sections[i]->name, name) == 0)
            return i + 1;
    }
    fprintf(stderr, "8cc: cannot find section '%s'\n", name);
    exit(-1);
}


static int find_symbol_section(Elf *elf, char *name) {
    int sectidx = find_section(elf, name);
    for (int i = 0; i < LIST_LEN(elf->syms); i++) {
        if (LIST_ELEM(Symbol, elf->syms, i)->sectidx == sectidx
            && !LIST_ELEM(Symbol, elf->syms, i)->name) {
            return i + 2;
        }
    }
    fprintf(stderr, "8cc: cannot find symbol section '%s'\n", name);
    exit(-1);
}

static void add_reloc(Elf *elf) {
    char name[100];
    for (int i = 0; i < elf->size; i++) {
        Section *sect = elf->sections[i];
        if (LIST_LEN(sect->rels) == 0)
            continue;
        StringBuilder *b = make_sbuilder();
        for (int j = 0; j < LIST_LEN(sect->rels); j++) {
            Reloc *rel = LIST_ELEM(Reloc, sect->rels, j);
            o8(b, rel->off);
            if (rel->sym) {
                o8(b, ELF64_R_INFO(find_symbol(elf, rel->sym), rel->type));
            } else {
                o8(b, ELF64_R_INFO(find_symbol_section(elf, rel->section), rel->type));
            }
            o8(b, rel->addend);
        }

        strcpy(name, ".rela");
        strcpy(name + 5, sect->name);
        Section *relsec = new_section(name, SHT_RELA);
        relsec->link = elf->symtabnum;
        relsec->info = i + 1;
        relsec->data = SBUILDER_BODY(b);
        relsec->off = relsec->size = SBUILDER_LEN(b);
        relsec->entsize = 24;
        relsec->align = 4;
        elf->sections[elf->size++] = relsec;
    }
}

static void add_shstrtab(Elf *elf) {
    Section *shstr = new_section(".shstrtab", SHT_STRTAB);
    elf->shnum = elf->size + 1;
    elf->sections[elf->size++] = shstr;
    StringBuilder *b = make_sbuilder();
    o1(b, 0);
    for (int i = 0; i < elf->size; i++) {
        elf->sections[i]->shstrtab_off = SBUILDER_LEN(b);
        char *name = elf->sections[i]->name;
        ostr(b, name);
    }
    shstr->data = SBUILDER_BODY(b);
    shstr->off = shstr->size = SBUILDER_LEN(b);
}

static void write_section(StringBuilder *b, StringBuilder *b2, Section *sect, int offset) {
    o4(b, sect->shstrtab_off); // sh_name
    o4(b, sect->type); // sh_type
    o8(b, sect->flags); // sh_flags
    o8(b, 0); // sh_addr
    o8(b, SBUILDER_LEN(b2) + offset); // sh_offset
    o8(b, sect->size); // sh_size
    o4(b, sect->link); // sh_link = SHN_UNDEF
    o4(b, sect->info); // sh_info
    o8(b, sect->align); // sh_addralign
    o8(b, sect->entsize); // sh_entsize
    out(b2, sect->data, sect->size);
    align(b2, 16);
}

static int num_sections(Elf *elf) {
    return elf->size + 1;
}

static Symbol *new_symbol(char *name, long value, int bind, int type, int defined) {
    Symbol *sym = malloc(sizeof(Symbol));
    sym->name = name;
    sym->value = value;
    sym->bind = bind;
    sym->type = type;
    sym->defined = defined;
    return sym;
}

static Reloc *new_reloc(long off, char *sym, char *section, int type, uint64 addend) {
    Reloc *rel = malloc(sizeof(Reloc));
    rel->off = off;
    rel->sym = sym;
    rel->section = section;
    rel->type = type;
    rel->addend = addend;
    return rel;
}

static void write_elf(FILE *outfile, Elf *elf) {
    add_symtab(elf);
    add_reloc(elf);
    add_shstrtab(elf);

    StringBuilder *b = make_sbuilder();
    int numsect = num_sections(elf);
    out(b, elf_ident, sizeof(elf_ident));
    o2(b, 2);  // e_type = ET_EXEC
    o2(b, 62); // e_machine = EM_X86_64
    o4(b, 1);  // e_version = EV_CURRENT
    o8(b, 0);  // e_entry
    o8(b, 0);  // e_phoff
    o8(b, 64); // e_shoff
    o4(b, 0);  // e_flags
    o2(b, 64); // e_ehsize
    o2(b, 0);  // e_phentsize
    o2(b, 0);  // e_phnum
    o2(b, 64); // e_shentsize
    o2(b, numsect);  // e_shnum
    o2(b, elf->shnum);  // e_shstrndx

    // null section
    for (int i = 0; i < 64; i++)
        o1(b, 0);

    StringBuilder *b2 = make_sbuilder();
    for (int i = 0; i < elf->size; i++) {
        write_section(b, b2, elf->sections[i], (numsect + 1) * 64);
    }

    fwrite(SBUILDER_BODY(b), SBUILDER_LEN(b), 1, outfile);
    fwrite(SBUILDER_BODY(b2), SBUILDER_LEN(b2), 1, outfile);
    fclose(outfile);
}

int main(int argc, char **argv) {
    FILE *outfile = fopen(argv[1], "w");
    if (outfile == NULL) {
        fprintf(stderr, "Usage: 8cc <outfile>\n");
        exit(-1);
    }

    Elf *elf = new_elf();

    Section *text = new_section(".text", SHT_PROGBITS);
    text->flags = SHF_ALLOC | SHF_EXECINSTR;
    text->data = hello_world_insn;
    text->size = sizeof(hello_world_insn);
    text->align = 16;
    list_push(text->syms, new_symbol("main", 0, STB_GLOBAL, STT_NOTYPE, 1));
    list_push(text->syms, new_symbol("printf", 0, STB_GLOBAL, STT_NOTYPE, 0));
    list_push(text->syms, new_symbol(NULL, 0, STB_LOCAL, STT_SECTION, 1));
    list_push(text->rels, new_reloc(6, NULL, ".data", R_X86_64_64, 0));
    list_push(text->rels, new_reloc(0x11, "printf", NULL, R_X86_64_PC32, 0xfffffffffffffffc));
    elf->sections[elf->size++] = text;

    Section *data = new_section(".data", SHT_PROGBITS);
    data->flags = SHF_ALLOC | SHF_WRITE;
    data->data = hello_world_str;
    data->size = sizeof(hello_world_str);
    data->align = 4;
    list_push(data->syms, new_symbol("message", 0, STB_LOCAL, STT_NOTYPE, 1));
    list_push(data->syms, new_symbol(NULL, 0, STB_LOCAL, STT_SECTION, 1));
    elf->sections[elf->size++] = data;

    write_elf(outfile, elf);
}

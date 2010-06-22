#include "8cc.h"

static u8 elf_ident[] = {0x7f, 0x45, 0x4c, 0x46, 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0};

static Elf *new_elf(void) {
    Elf *elf = malloc(sizeof(Elf));
    elf->size = 0;
    elf->shnum = 0;
    elf->symtabnum = 0;
    elf->syms = make_list();
    return elf;
}

static void write_sym_to_buf(Elf *elf, String *symtab, String *strtab, bool localonly) {
    for (int i = 0; i < elf->size; i++) {
        Section *sect = elf->sections[i];
        for (int j = 0; j < LIST_LEN(sect->syms); j++) {
            Symbol *sym = LIST_ELEM(Symbol, sect->syms, j);
            if (localonly && sym->bind != STB_LOCAL)
                continue;
            if (!localonly && sym->bind == STB_LOCAL)
                continue;
            if (sym->name) {
                o4(symtab, STRING_LEN(strtab)); // st_name
                ostr(strtab, sym->name);
            } else {
                o4(symtab, 0); // st_name
            }
            o1(symtab, ELF64_ST_INFO(sym->bind, sym->type)); // st_info;
            o1(symtab, 0); // st_other;
            if (sym->defined) {
                sym->sectidx = i + 1;
                o2(symtab, sym->sectidx); // st_shndx
            } else {
                o2(symtab, 0); // st_shndx
            }
            o8(symtab, sym->value); // st_value
            o8(symtab, 0); // st_size
            list_push(elf->syms, sym);
        }
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
    ostr(strtabb, "hello.asm");
    o1(symtabb, ELF64_ST_INFO(STB_LOCAL, STT_FILE)); // st_info
    o1(symtabb, 0); // other
    o2(symtabb, SHN_ABS); // st_shndx
    o8(symtabb, 0); // st_value
    o8(symtabb, 0); // st_size

    write_sym_to_buf(elf, symtabb, strtabb, true);
    int localidx = LIST_LEN(elf->syms);
    write_sym_to_buf(elf, symtabb, strtabb, false);
    elf->symtabnum = elf->size + 1;

    Section *symtab = make_section(".symtab", SHT_SYMTAB);
    symtab->body = symtabb;
    symtab->link = elf->size + 2;
    symtab->info = localidx + 2;
    symtab->entsize = 24;
    symtab->align = 4;
    elf->sections[elf->size++] = symtab;

    Section *strtab = make_section(".strtab", SHT_STRTAB);
    strtab->body = strtabb;
    elf->sections[elf->size++] = strtab;
}

static int find_symbol(Elf *elf, char *sym) {
    for (int i = 0; i < LIST_LEN(elf->syms); i++) {
        char *name = LIST_ELEM(Symbol, elf->syms, i)->name;
        if (name && strcmp(name, sym) == 0)
            return i + 2;
    }
    fprintf(stderr, "cannot find symbol '%s'", sym);
    exit(-1);
}

static int find_section(Elf *elf, char *name) {
    for (int i = 0; i < elf->size; i++) {
        if (strcmp(elf->sections[i]->name, name) == 0)
            return i + 1;
    }
    fprintf(stderr, "cannot find section '%s'", name);
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
    fprintf(stderr, "cannot find symbol section '%s'", name);
    exit(-1);
}

static void add_reloc(Elf *elf) {
    char name[100];
    for (int i = 0; i < elf->size; i++) {
        Section *sect = elf->sections[i];
        if (LIST_LEN(sect->rels) == 0)
            continue;
        String *b = make_string();
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
        Section *relsec = make_section(name, SHT_RELA);
        relsec->link = elf->symtabnum;
        relsec->info = i + 1;
	relsec->body = b;
        relsec->entsize = 24;
        relsec->align = 4;
        elf->sections[elf->size++] = relsec;
    }
}

static void add_shstrtab(Elf *elf) {
    Section *shstr = make_section(".shstrtab", SHT_STRTAB);
    elf->shnum = elf->size + 1;
    elf->sections[elf->size++] = shstr;
    String *b = make_string();
    o1(b, 0);
    for (int i = 0; i < elf->size; i++) {
        elf->sections[i]->shstrtab_off = STRING_LEN(b);
        char *name = elf->sections[i]->name;
        ostr(b, name);
    }
    shstr->body = b;
}

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

static int num_sections(Elf *elf) {
    return elf->size + 1;
}

static void write_elf(FILE *outfile, Elf *elf) {
    add_symtab(elf);
    add_reloc(elf);
    add_shstrtab(elf);

    String *header = make_string();
    int numsect = num_sections(elf);
    out(header, elf_ident, sizeof(elf_ident));
    o2(header, 2);  // e_type = ET_EXEC
    o2(header, 62); // e_machine = EM_X86_64
    o4(header, 1);  // e_version = EV_CURRENT
    o8(header, 0);  // e_entry
    o8(header, 0);  // e_phoff
    o8(header, 64); // e_shoff
    o4(header, 0);  // e_flags
    o2(header, 64); // e_ehsize
    o2(header, 0);  // e_phentsize
    o2(header, 0);  // e_phnum
    o2(header, 64); // e_shentsize
    o2(header, numsect);  // e_shnum
    o2(header, elf->shnum);  // e_shstrndx

    // null section
    for (int i = 0; i < 64; i++)
        o1(header, 0);

    String *content = make_string();
    for (int i = 0; i < elf->size; i++) {
        write_section(header, content, elf->sections[i], (numsect + 1) * 64);
    }

    fwrite(STRING_BODY(header), STRING_LEN(header), 1, outfile);
    fwrite(STRING_BODY(content), STRING_LEN(content), 1, outfile);
    fclose(outfile);
}

int main(int argc, char **argv) {
    if (argc != 3) {
        fprintf(stderr, "Usage: 8cc <infile> <outfile>\n");
        exit(-1);
    }

    FILE *infile = strcmp(argv[1], "-") ? fopen(argv[1], "r") : stdin;
    FILE *outfile = fopen(argv[2], "w");
    Elf *elf = new_elf();

    Section *data = make_section(".data", SHT_PROGBITS);
    data->flags = SHF_ALLOC | SHF_WRITE;
    data->align = 4;
    list_push(data->syms, make_symbol(NULL, 0, STB_LOCAL, STT_SECTION, 1));
    elf->sections[elf->size++] = data;

    Section *text = make_section(".text", SHT_PROGBITS);
    text->flags = SHF_ALLOC | SHF_EXECINSTR;
    List *insts = parse(infile, text, data);
    assemble(text, insts);
    text->align = 16;
    list_push(text->syms, make_symbol("main", 0, STB_GLOBAL, STT_NOTYPE, 1));
    list_push(text->syms, make_symbol(NULL, 0, STB_LOCAL, STT_SECTION, 1));
    elf->sections[elf->size++] = text;

    write_elf(outfile, elf);
}

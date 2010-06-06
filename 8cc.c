/* -*- c-basic-offset: 4 -*- */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

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

static WriteCtx *new_write_ctx(void) {
    WriteCtx *c = malloc(sizeof(WriteCtx));
    c->data = malloc(1024*100);
    memset(c->data, 0, 1024*100);
    c->off = c->size = 0;
    return c;
}

static void o1(WriteCtx *c, int byte) {
    c->data[c->off++] = (char) byte;
    if (c->size < c->off)
        c->size = c->off;
}

static void out(WriteCtx *c, void *data, size_t siz) {
    for (int i = 0; i < siz; i++)
        o1(c, ((char*)data)[i]);
}

static void ostr(WriteCtx *c, char *str) {
    out(c, str, strlen(str) + 1);
}

static void o2(WriteCtx *c, uint16_t data) {
    out(c, &data, 2);
}

static void o4(WriteCtx *c, uint32_t data) {
    out(c, &data, 4);
}

static void o8(WriteCtx *c, uint64_t data) {
    out(c, &data, 8);
}

static void align(WriteCtx *c, int n) {
    c->off = c->size = c->off + n - c->off % n;
}
static Elf *new_elf(void) {
    Elf *elf = malloc(sizeof(Elf));
    elf->size = 0;
    elf->size = 0;
    elf->shnum = 0;
    elf->symtabnum = 0;
    elf->nsym = 0;
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
    sect->nsym = 0;
    sect->nrel = 0;
    sect->link = 0;
    sect->info = 0;
    strcpy(sect->name, name);
    sect->entsize = 0;
    return sect;
}

static void write_sym_to_buf(Elf *elf, WriteCtx *c, WriteCtx *c2, bool localonly) {
    for (int i = 0; i < elf->size; i++) {
        Section *sect = elf->sections[i];
        for (int j = 0; j < sect->nsym; j++) {
            Symbol *sym = sect->syms[j];
            if (localonly && sym->bind != STB_LOCAL)
                continue;
            if (!localonly && sym->bind == STB_LOCAL)
                continue;
            if (sym->name) {
                o4(c, c2->off); // st_name
                ostr(c2, sym->name);
            } else {
                o4(c, 0); // st_name
            }
            o1(c, ELF64_ST_INFO(sym->bind, sym->type)); // st_info;
            o1(c, 0); // st_other;
            if (sym->defined) {
                sym->sectidx = i + 1;
                o2(c, sym->sectidx); // st_shndx
            } else {
                o2(c, 0); // st_shndx
            }
            o8(c, sym->value); // st_value
            o8(c, 0); // st_size
            elf->syms[elf->nsym++] = sym;
        }
    }
}

static void add_symtab(Elf *elf) {
    WriteCtx *c = new_write_ctx();
    WriteCtx *c2 = new_write_ctx();
    o1(c2, 0);
    // Null symbol
    for (int i = 0; i < 24; i++) o1(c, 0);
    // File symbol
    o4(c, c2->off); // st_name
    ostr(c2, "hello.asm");
    o1(c, ELF64_ST_INFO(STB_LOCAL, STT_FILE)); // st_info
    o1(c, 0); // other
    o2(c, SHN_ABS); // st_shndx
    o8(c, 0); // st_value
    o8(c, 0); // st_size

    write_sym_to_buf(elf, c, c2, true);
    int localidx = elf->nsym;
    write_sym_to_buf(elf, c, c2, false);
    elf->symtabnum = elf->size + 1;

    Section *symtab = new_section(".symtab", SHT_SYMTAB);
    symtab->data = c->data;
    symtab->off = symtab->size = c->size;
    symtab->link = elf->size + 2;
    symtab->info = localidx + 2;
    symtab->entsize = 24;
    symtab->align = 4;
    elf->sections[elf->size++] = symtab;

    Section *strtab = new_section(".strtab", SHT_STRTAB);
    strtab->data = c2->data;
    strtab->off = strtab->size = c2->size;
    elf->sections[elf->size++] = strtab;
}

static int find_symbol(Elf *elf, char *sym) {
    for (int i = 0; i < elf->nsym; i++) {
        if (elf->syms[i]->name && strcmp(elf->syms[i]->name, sym) == 0)
            return i + 2;
    }
    fprintf(stderr, "cannot find symbol '%s'\n", sym);
    exit(-1);
}

static int find_section(Elf *elf, char *name) {
    for (int i = 0; i < elf->size; i++) {
        if (strcmp(elf->sections[i]->name, name) == 0)
            return i + 1;
    }
    fprintf(stderr, "cannot find section '%s'\n", name);
    exit(-1);
}


static int find_symbol_section(Elf *elf, char *name) {
    int sectidx = find_section(elf, name);
    for (int i = 0; i < elf->nsym; i++) {
        if (elf->syms[i]->sectidx == sectidx && !elf->syms[i]->name)
            return i + 2;
    }
    fprintf(stderr, "cannot find symbol section '%s'\n", name);
    exit(-1);
}

static void add_reloc(Elf *elf) {
    char name[100];
    for (int i = 0; i < elf->size; i++) {
        Section *sect = elf->sections[i];
        if (sect->nrel == 0)
            continue;
        WriteCtx *c = new_write_ctx();
        for (int j = 0; j < sect->nrel; j++) {
            Reloc *rel = sect->rels[j];
            o8(c, rel->off);
            if (rel->sym) {
                o8(c, ELF64_R_INFO(find_symbol(elf, rel->sym), rel->type));
            } else {
                o8(c, ELF64_R_INFO(find_symbol_section(elf, rel->section), rel->type));
            }
            o8(c, rel->addend);
        }

        strcpy(name, ".rela");
        strcpy(name + 5, sect->name);
        Section *relsec = new_section(name, SHT_RELA);
        relsec->link = elf->symtabnum;
        relsec->info = i + 1;
        relsec->data = c->data;
        relsec->off = relsec->size = c->size;
        relsec->entsize = 24;
        relsec->align = 4;
        elf->sections[elf->size++] = relsec;
    }
}

static void add_shstrtab(Elf *elf) {
    Section *shstr = new_section(".shstrtab", SHT_STRTAB);
    elf->shnum = elf->size + 1;
    elf->sections[elf->size++] = shstr;
    WriteCtx *c = new_write_ctx();
    o1(c, 0);
    for (int i = 0; i < elf->size; i++) {
        elf->sections[i]->shstrtab_off = c->off;
        char *name = elf->sections[i]->name;
        ostr(c, name);
    }
    shstr->data = c->data;
    shstr->off = shstr->size = c->size;
}

static void write_section(WriteCtx *c, WriteCtx *c2, Section *sect, int offset) {
    o4(c, sect->shstrtab_off); // sh_name
    o4(c, sect->type); // sh_type
    o8(c, sect->flags); // sh_flags
    o8(c, 0); // sh_addr
    o8(c, c2->off + offset); // sh_offset
    o8(c, sect->size); // sh_size
    o4(c, sect->link); // sh_link = SHN_UNDEF
    o4(c, sect->info); // sh_info
    o8(c, sect->align); // sh_addralign
    o8(c, sect->entsize); // sh_entsize
    out(c2, sect->data, sect->size);
    align(c2, 16);
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

static Reloc *new_reloc(long off, char *sym, char *section, int type, uint64_t addend) {
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

    WriteCtx *c = new_write_ctx();
    int numsect = num_sections(elf);
    out(c, elf_ident, sizeof(elf_ident));
    o2(c, 2);  // e_type = ET_EXEC
    o2(c, 62); // e_machine = EM_X86_64
    o4(c, 1);  // e_version = EV_CURRENT
    o8(c, 0);  // e_entry
    o8(c, 0);  // e_phoff
    o8(c, 64); // e_shoff
    o4(c, 0);  // e_flags
    o2(c, 64); // e_ehsize
    o2(c, 0);  // e_phentsize
    o2(c, 0);  // e_phnum
    o2(c, 64); // e_shentsize
    o2(c, numsect);  // e_shnum
    o2(c, elf->shnum);  // e_shstrndx

    // null section
    for (int i = 0; i < 64; i++)
        o1(c, 0);

    WriteCtx *c2 = new_write_ctx();
    for (int i = 0; i < elf->size; i++) {
        write_section(c, c2, elf->sections[i], (numsect + 1) * 64);
    }

    fwrite(c->data, c->size, 1, outfile);
    fwrite(c2->data, c2->size, 1, outfile);
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
    text->syms[text->nsym++] = new_symbol("main", 0, STB_GLOBAL, STT_NOTYPE, 1);
    text->syms[text->nsym++] = new_symbol("printf", 0, STB_GLOBAL, STT_NOTYPE, 0);
    text->syms[text->nsym++] = new_symbol(NULL, 0, STB_LOCAL, STT_SECTION, 1);
    text->rels[text->nrel++] = new_reloc(6, NULL, ".data", R_X86_64_64, 0);
    text->rels[text->nrel++] = new_reloc(0x11, "printf", NULL, R_X86_64_PC32, 0xfffffffffffffffc);
    elf->sections[elf->size++] = text;

    Section *data = new_section(".data", SHT_PROGBITS);
    data->flags = SHF_ALLOC | SHF_WRITE;
    data->data = hello_world_str;
    data->size = sizeof(hello_world_str);
    data->align = 4;
    data->syms[data->nsym++] = new_symbol("message", 0, STB_LOCAL, STT_NOTYPE, 1);
    data->syms[data->nsym++] = new_symbol(NULL, 0, STB_LOCAL, STT_SECTION, 1);
    elf->sections[elf->size++] = data;

    write_elf(outfile, elf);
}

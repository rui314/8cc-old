/*
 * run.c - compile and execute directly
 *
 * Copyright 2010 Rui Ueyama <rui314@gmail.com>.  All rights reserved.
 * This code is available under the simplified BSD license.  See LICENSE for details.
 */

#define _GNU_SOURCE 1
#include "8cc.h"
#include <sys/mman.h>
#include <dlfcn.h>

typedef struct JumpTable {
    void *ptr;
    int off;
} JumpTable;

static char **list_to_argv(List *args) {
    char **r = malloc(sizeof(char *) * (LIST_LEN(args) + 1));
    for (int i = 0; i < LIST_LEN(args); i++) {
        String *s = LIST_REF(args, i);
        r[i] = STRING_BODY(s);
    }
    r[LIST_LEN(args)] = NULL;
    return r;
}

static void *allocate_memory(void *ptr, int size, bool exec) {
    int prot = PROT_READ | PROT_WRITE;
    prot |= exec ? PROT_EXEC : 0;
    int flags = MAP_ANONYMOUS | MAP_PRIVATE | MAP_32BIT;
    void *r = mmap(NULL, size, prot, flags, 0, 0);
    if (r == MAP_FAILED) {
        perror("mmap failed");
        exit(-1);
    }
    if (ptr)
        memcpy(r, ptr, size);
    return r;
}

static void allocate(Section *sect) {
    if (!(sect->flags & SHF_ALLOC))
        return;
    void *ptr = STRING_BODY(sect->body);
    int size = STRING_LEN(sect->body);
    if (!size)
        return;
    bool exec = sect->flags & SHF_EXECINSTR;
    sect->memory_pos = allocate_memory(ptr, size, exec);
}

static JumpTable *allocate_jump_table(void) {
    JumpTable *r = malloc(sizeof(JumpTable));
    r->ptr = allocate_memory(NULL, 4096, true);
    r->off = 0;
    return r;
}

static intptr add_jump(JumpTable *tab, u64 off) {
    intptr r = (intptr)tab->ptr + tab->off;

    // mov r11, off
    *((u8 *)tab->ptr + tab->off++) = 0x49;
    *((u8 *)tab->ptr + tab->off++) = 0xbb;
    for (int i = 0; i < 8; i++)
        *((u8 *)tab->ptr + tab->off++) = (off >> (i * 8)) & 0xff;

    // jmp r11
    *((u8 *)tab->ptr + tab->off++) = 0x41;
    *((u8 *)tab->ptr + tab->off++) = 0xff;
    *((u8 *)tab->ptr + tab->off++) = 0xe3;
    return r;
}

static void relocate(Elf *elf, Section *sect, JumpTable *tab) {
    if (!(sect->flags & SHF_ALLOC))
        return;
    for (int i = 0; i < LIST_LEN(sect->rels); i++) {
        Reloc *rel = LIST_REF(sect->rels, i);
        if (rel->sym) {
            assert(rel->type == R_X86_64_PC32);
            intptr loc = (intptr)dlsym(RTLD_DEFAULT, rel->sym);
            if (!loc)
                error("cannot resolve symbol '%s'", rel->sym);
            intptr loc1 = add_jump(tab, loc);
            assert(loc1 < 0xffffffffUL);
            Symbol *sym = find_symbol(elf, rel->sym);
            u32 *ptr = (void *)((intptr)(sym->section->memory_pos) + rel->off);
            *ptr = loc1 - (intptr)ptr - 4;
            continue;
        }
        assert(rel->type == R_X86_64_64);
        intptr loc = (intptr)(rel->section->memory_pos) + rel->addend;
        u64 *ptr = (void *)((intptr)(sect->memory_pos) + rel->off);
        *ptr = loc;
    }
}

static void release(Section *sect) {
    if (sect->flags & SHF_ALLOC)
        munmap(sect->memory_pos, STRING_LEN(sect->body));
}

typedef int (*main_fn_type)(int argc, char **argv);

static int call_main(main_fn_type main_fn, int argc, char **argv) {
    return main_fn(argc, argv);
}

extern int run_main(Elf *elf, List *args) {
    for (int i = 0; i < LIST_LEN(elf->sections); i++)
        allocate((Section *)LIST_REF(elf->sections, i));
    JumpTable *tab = allocate_jump_table();
    for (int i = 0; i < LIST_LEN(elf->sections); i++)
        relocate(elf, (Section *)LIST_REF(elf->sections, i), tab);

    Symbol *sym = find_symbol(elf, "main");
    main_fn_type main_fn = (void *)(((intptr)sym->section->memory_pos) + sym->value);
    int r = call_main(main_fn, LIST_LEN(args), list_to_argv(args));

    for (int i = 0; i < LIST_LEN(elf->sections); i++)
        release(LIST_REF(elf->sections, i));
    return r;
}

int run_string(char *code) {
    File *file = make_string_file(to_string(code));
    Elf *elf = new_elf();
    List *fns = parse(file, elf);
    assemble(elf, fns);
    return run_main(elf, make_list());
}

/* -*- c-basic-offset: 4 -*- */

#include "8cc.h"

Section *make_section(char *name, int type) {
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

Symbol *make_symbol(char *name, long value, int bind, int type, int defined) {
    Symbol *sym = malloc(sizeof(Symbol));
    sym->name = name;
    sym->value = value;
    sym->bind = bind;
    sym->type = type;
    sym->defined = defined;
    return sym;
}

Reloc *make_reloc(long off, char *sym, char *section, int type, uint64 addend) {
    Reloc *rel = malloc(sizeof(Reloc));
    rel->off = off;
    rel->sym = sym;
    rel->section = section;
    rel->type = type;
    rel->addend = addend;
    return rel;
}

static Symbol *find_symbol(Section *sect, char *name) {
    for (int i = 0; i < LIST_LEN(sect->syms); i++) {
        Symbol *sym = LIST_ELEM(Symbol, sect->syms, i);
        if (sym->name && strcmp(sym->name, name) == 0)
            return sym;
    }
    return NULL;
}

static long find_data(Section *sect, char *name) {
    Symbol *sym = find_symbol(sect, name);
    if (!sym)
        error("8cc: cannot find data reloc for '%s'\n", name);
    return sym->value;
}

static Insn *make_insn(int op, Operand *dst, Operand *src) {
    Insn *insn = malloc(sizeof(Insn));
    insn->op = op;
    insn->dst = dst;
    insn->src = src;
    return insn;
}

static Operand *regop(int reg) {
    Operand *op = malloc(sizeof(Operand));
    op->type = TYPE_REG;
    OPERAND_REG(op) = reg;
    return op;
}

// static Operand *immop(uint64 imm) {
//     Operand *op = malloc(sizeof(Operand));
//     op->type = TYPE_IMM;
//     OPERAND_IMM(op) = imm;
//     return op;
// }

static Operand *symop(char *name) {
    Operand *op = malloc(sizeof(Operand));
    op->type = TYPE_SYM;
    OPERAND_SYM(op) = name;
    return op;
}

List *create_insn_list(void) {
    List *list = make_list();
    list_push(list, make_insn(OP_PUSH,  regop(RBP), NULL));
    list_push(list, make_insn(OP_MOV,   regop(RBP),  regop(RSP)));
    list_push(list, make_insn(OP_MOV,   regop(RDI),  symop("hello")));
    list_push(list, make_insn(OP_XOR,   regop(RAX),  regop(RAX)));
    list_push(list, make_insn(OP_CALL,  symop("printf"), NULL));
    list_push(list, make_insn(OP_MOV,   regop(RDI),  symop("sekai")));
    list_push(list, make_insn(OP_XOR,   regop(RAX),  regop(RAX)));
    list_push(list, make_insn(OP_CALL,  symop("printf"), NULL));
    list_push(list, make_insn(OP_LEAVE, NULL, NULL));
    list_push(list, make_insn(OP_RET,   NULL, NULL));
    return list;
}

StringBuilder *assemble(Section *text, Section *data, List *insns) {
    StringBuilder *b = make_sbuilder();
    for (int i = 0; i < LIST_LEN(insns); i++) {
        Insn *insn = LIST_ELEM(Insn, insns, i);
        switch (insn->op) {
            case OP_MOV:
                if (IS_REG(insn->dst) && IS_REG(insn->src)) {
                    o1(b, 0x48 | (IS_REX(insn->src) << 2) | IS_REX(insn->dst));
                    o1(b, 0x89);
                    o1(b, PACK_REG(3, insn->src, insn->dst));
                } else if (IS_REG(insn->dst) && IS_IMM(insn->src)) {
                    o1(b, 0x48 | IS_REX(insn->dst));
                    o1(b, 0xb8 | MASK_REG(insn->dst));
                    o8(b, OPERAND_IMM(insn->src));
                } else if (IS_REG(insn->dst) && IS_SYM(insn->src)) {
                    o1(b, 0x48 | IS_REX(insn->dst));
                    o1(b, 0xb8 | MASK_REG(insn->dst));
                    Reloc *rel = make_reloc(SBUILDER_LEN(b), NULL, ".data", R_X86_64_64,
                                            find_data(data, OPERAND_SYM(insn->src)));
                    list_push(text->rels, rel);
                 o8(b, 0);
                } else {
                    error("8cc: unsupported MOV\n");
                }
                break;
            case OP_PUSH:
                o1(b, 0x50 | MASK_REG(insn->dst));
                break;
            case OP_XOR:
                if (IS_REG(insn->dst) && IS_REG(insn->src) && !IS_REX(insn->dst) && !IS_REX(insn->src)) {
                    o1(b, 0x31);
                    o1(b, PACK_REG(3, insn->src, insn->dst));
                } else {
                    error("8cc: unsupported XOR\n");
                }
                break;
            case OP_CALL:
                o1(b, 0xE8);
                Symbol *sym = find_symbol(text, OPERAND_SYM(insn->dst));
                if (!sym) {
                    sym = make_symbol(OPERAND_SYM(insn->dst), 0, STB_GLOBAL, STT_NOTYPE, 0);
                    list_push(text->syms, sym);
                }
                Reloc *rel = make_reloc(SBUILDER_LEN(b), OPERAND_SYM(insn->dst), NULL, R_X86_64_PC32, 0xfffffffffffffffc);
                list_push(text->rels, rel);
                o4(b, 0);
                break;
            case OP_LEAVE:
                o1(b, 0xc9);
                break;
            case OP_RET:
                o1(b, 0xc3);
                break;
            default:
                error("8cc: unknown op\n");
        }
    }
    return b;
}

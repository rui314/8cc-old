#include "8cc.h"

Section *make_section(char *name, int type) {
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

Symbol *make_symbol(char *name, Section *sect, long value, int bind, int type, int defined) {
    Symbol *sym = malloc(sizeof(Symbol));
    sym->name = name;
    sym->section = sect;
    sym->value = value;
    sym->bind = bind;
    sym->type = type;
    sym->defined = defined;
    sym->index = -1;
    return sym;
}

Reloc *make_reloc(long off, char *sym, char *section, int type, u64 addend) {
    Reloc *rel = malloc(sizeof(Reloc));
    rel->off = off;
    rel->sym = sym;
    rel->section = section;
    rel->type = type;
    rel->addend = addend;
    return rel;
}

static Var *make_var(int stype) {
    Var *r = malloc(sizeof(Var));
    r->stype = stype;
    r->ctype = CTYPE_INT;
    r->val.i = 0;
    r->name = NULL;
    r->sym = NULL;
    return r;
}

Var *make_imm(u64 val) {
    Var *r = make_var(VAR_IMM);
    r->val.i = val;
    return r;
}

Var *make_global(char *name, u64 val) {
    Var *r = make_var(VAR_GLOBAL);
    r->name = name;
    r->val.i = val;
    return r;
}

Var *make_extern(char *name) {
    Var *r = make_var(VAR_EXTERN);
    r->name = name;
    return r;
}

static Inst *make_inst(char op) {
    Inst *r = malloc(sizeof(Inst));
    r->op = op;
    r->arg0 = NULL;
    r->arg1 = NULL;
    r->args = NULL;
    return r;
}

Inst *make_func_call(Var *fn, List *args) {
    Inst * r = make_inst('$');
    r->arg0 = fn;
    r->args = args;
    return r;
}

struct Var **make_list1(Var *arg0) {
    Var **r = malloc(sizeof(Var *) * 2);
    r[0] = arg0;
    r[1] = NULL;
    return r;
}

struct Var **make_list2(Var *arg0, Var *arg1) {
    Var **r = malloc(sizeof(Var *) * 3);
    r[0] = arg0;
    r[1] = arg1;
    r[2] = NULL;
    return r;
}

int add_string(Section *data, char *str) {
    int r = STRING_LEN(data->body);
    out(data->body, str, strlen(str));
    return r;
}

static void add_reloc(Section *text, long off, char *sym, char *section, int type, u64 addend) {
    Reloc *rel = make_reloc(off, sym, section, type, addend);
    list_push(text->rels, rel);
}

// rdi, rsi, rdx, rcx, r8, r9
// static u32 PUSH_STACK[] = { 0x7d8b48, 0x758b48, 0x558b48, 0x4d8b48, 0x458b4c, 0x4d8b4c };
static u16 PUSH_ABS[] = { 0xbf48, 0xbe48, 0xba48, 0xb948, 0xb849, 0xb949 };
    
static void gen_call(String *b, Elf *elf, Section *text, Var *fn, List *args) {
    for (int i = 0; i < LIST_LEN(args); i++) {
        switch (LIST_ELEM(Var, args, i)->stype) {
        case VAR_GLOBAL:
            o2(b, PUSH_ABS[i]);
            add_reloc(text, STRING_LEN(b), NULL, ".data", R_X86_64_64, LIST_ELEM(Var, args, i)->val.i);
            o8(b, 0);
            break;
        case VAR_IMM:
            o2(b, PUSH_ABS[i]);
            o8(b, LIST_ELEM(Var, args, i)->val.i);
            break;
        default:
            error("8cc: unsupported var type: %c\n", LIST_ELEM(Var, args, i)->stype);
        }
    }
    if (!fn->sym) {
        fn->sym = make_symbol(fn->name, text, 0, STB_GLOBAL, STT_NOTYPE, 0);
        list_push(elf->syms, fn->sym);
    }
    o2(b, 0xc031); // XOR eax, eax
    o1(b, 0xe8); // CALL
    add_reloc(text, STRING_LEN(b), fn->name, NULL, R_X86_64_PC32, 0xfffffffffffffffc);
    o4(b, 0);
}

void assemble(Elf *elf, Section *text, List *insts) {
    String *b = text->body;
    o1(b, 0x55); // PUSH rbp
    o1(b, 0x48); // MOV rbp, rsp
    o1(b, 0x89);
    o1(b, 0xe5);
    for (int i = 0; i < LIST_LEN(insts); i++) {
        Inst *inst = LIST_ELEM(Inst, insts, i);
        switch (inst->op) {
        case '$': {
            Var *fn = inst->arg0;
            List *args = inst->args;
            gen_call(b, elf, text, fn, args);
            break;
        }
        default:
            error("8cc: unknown op\n");
        }
    }
    o2(b, 0xc031); // XOR eax, eax
    o1(b, 0xc9); // LEAVE
    o1(b, 0xc3); // RET
}

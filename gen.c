#include "8cc.h"

Section *make_section(char *name, int type) {
    Section *sect = malloc(sizeof(Section));
    sect->body = make_string();
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

Reloc *make_reloc(long off, char *sym, char *section, int type, u64 addend) {
    Reloc *rel = malloc(sizeof(Reloc));
    rel->off = off;
    rel->sym = sym;
    rel->section = section;
    rel->type = type;
    rel->addend = addend;
    return rel;
}

static Var *make_var(int type) {
    Var *r = malloc(sizeof(Var));
    r->type = type;
    r->val = 0;
    r->name = NULL;
    return r;
}

Var *make_imm(u64 val) {
    Var *r = make_var(VAR_IMM);
    r->val = val;
    return r;
}

Var *make_global(char *name, u64 val) {
    Var *r = make_var(VAR_GLOBAL);
    r->name = name;
    r->val = val;
    return r;
}

Var *make_extern(char *name, Section *text) {
    Var *r = make_var(VAR_EXTERN);
    r->name = name;
    Symbol *sym = make_symbol(name, 0, STB_GLOBAL, STT_NOTYPE, 0);
    list_push(text->syms, sym);
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

Inst *make_func_call(Var *fn, Var **args) {
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
static u32 PUSH_STACK[] = { 0x7d8b48, 0x758b48, 0x558b48, 0x4d8b48, 0x458b4c, 0x4d8b4c };
static u16 PUSH_ABS[] = { 0xbf48, 0xbe48, 0xba48, 0xb948, 0xb849, 0xb949 };
    
void assemble(Section *text, List *insts) {
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
	    Var **args = inst->args;
	    for (int j = 0; args[j]; j++) {
		switch (args[j]->type) {
		case VAR_GLOBAL:
		    o2(b, PUSH_ABS[j]);
		    add_reloc(text, STRING_LEN(b), NULL, ".data", R_X86_64_64, args[j]->val);
		    o8(b, 0);
		    break;
		case VAR_IMM:
		    o2(b, PUSH_ABS[j]);
		    o8(b, args[j]->val);
		    break;
		default:
		    error("8cc: unsupported var type: %c\n", args[j]->type);
		}
	    }
	    o2(b, 0xc031); // XOR eax, eax
	    o1(b, 0xe8); // CALL
	    add_reloc(text, STRING_LEN(b), fn->name, NULL, R_X86_64_PC32, 0xfffffffffffffffc);
	    o4(b, 0);
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

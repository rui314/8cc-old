/*
 * gen.c - code generator
 *
 *   Copyright 2010 Rui Ueyama <rui314@gmail.com>.  All rights reserved.
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions are met:
 *
 *      1. Redistributions of source code must retain the above copyright
 *         notice, this list of conditions and the following disclaimer.
 *
 *      2. Redistributions in binary form must reproduce the above copyright
 *         notice, this list of conditions and the following disclaimer in the
 *         documentation and/or other materials provided with the distribution.
 *
 *   THIS SOFTWARE IS PROVIDED BY COPYRIGHT HOLDER ``AS IS'' AND ANY EXPRESS OR
 *   IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 *   OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
 *   NO EVENT SHALL COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 *   INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 *   (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 *   SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 *   CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *   LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 *   OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 *   DAMAGE.
 */

#include "8cc.h"

/*
 * Code generator for x86-64.
 */

typedef struct CompiledVar {
    int sp;
} CompiledVar;

CompiledVar *make_compiled_var(int sp) {
    CompiledVar *r = malloc(sizeof(CompiledVar));
    r->sp = sp;
    return r;
}

Ctype *make_ctype(int type) {
    Ctype *r = malloc(sizeof(Ctype));
    r->type = type;
    r->ptr = NULL;
    return r;
}

Ctype *make_ctype_ptr(Ctype *type) {
    Ctype *r = malloc(sizeof(Ctype));
    r->type = CTYPE_PTR;
    r->ptr = type;
    return r;
}

Section *find_section(Elf *elf, char *name) {
    for (int i = 0; i < LIST_LEN(elf->sections); i++) {
        Section *sect = LIST_ELEM(elf->sections, i);
        if (!strcmp(sect->name, name))
            return sect;
    }
    error("cannot find section '%s'", name);
    return NULL;
}

Symbol *make_symbol(String *name, Section *sect, long value, int bind, int type, int defined) {
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

Var *make_var(int ctype, String *name) {
    Var *r = malloc(sizeof(Var));
    r->stype = VAR_GLOBAL;
    r->ctype = make_ctype(ctype);
    r->val.i = 0;
    r->name = name;
    r->is_lvalue = false;
    return r;
}

Var *make_imm(int ctype, Cvalue val) {
    Var *r = make_var(ctype, NULL);
    r->stype = VAR_IMM;
    r->val = val;
    return r;
}

Var *make_global_var(String *name, u64 val) {
    Var *r = make_var(CTYPE_INT, name);
    r->val.i = val;
    return r;
}

Var *make_extern(String *name) {
    Var *r = make_var(CTYPE_INT, name);
    r->stype = VAR_EXTERN;
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

Inst *make_var_set(Var *var, Var *val) {
    Inst *r = make_inst('=');
    r->arg0 = var;
    r->arg1 = val;
    return r;
}

Inst *make_func_call(Var *fn, Var *rval, List *args) {
    Inst *r = make_inst('$');
    r->arg0 = fn;
    r->arg1 = rval;
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

int add_string(Section *data, String *str) {
    int r = STRING_LEN(data->body);
    out(data->body, STRING_BODY(str), STRING_LEN(str));
    return r;
}

static void add_reloc(Section *text, long off, char *sym, char *section, int type, u64 addend) {
    Reloc *rel = make_reloc(off, sym, section, type, addend);
    list_push(text->rels, rel);
}

// MOV to rdi, rsi, rdx, rcx, r8, or r9
// static u32 PUSH_STACK[] = { 0x7d8b48, 0x758b48, 0x558b48, 0x4d8b48, 0x458b4c, 0x4d8b4c };
static u16 PUSH_ABS[] = { 0xbf48, 0xbe48, 0xba48, 0xb948, 0xb849, 0xb949 };

// MOVSD to xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, or xmm7
static u32 PUSH_XMM_ABS[] = { 0x05100ff2, 0x0d100ff2, 0x15100ff2, 0x1d100ff2,
                              0x25100ff2, 0x2d100ff2, 0x35100ff2, 0x3d100ff2 };

// MOV rdi/rsi/rdx/rcx/r8/r9, [rbp+x]
static u32 MOV_STACK[] = { 0x7d8b48, 0x758b48, 0x558b48, 0x4d8b48, 0x458b4c, 0x4d8b4c };

static void gen_call(String *b, Elf *elf, Var *fn, int off, List *args, Dict *scope) {
    Section *text = find_section(elf, ".text");
    Section *data = find_section(elf, ".data");
    int gpr = 0;
    int xmm = 0;
    for (int i = 0; i < LIST_LEN(args); i++) {
        Var *var = LIST_ELEM(args, i);
        switch (var->stype) {
        case VAR_GLOBAL:
            if (var->ctype->type == CTYPE_INT) {
                CompiledVar *cvar = dict_get(scope, var);
                if (cvar == NULL)
                    error("undefined variable: '%s'", var->name);
                o4(b, MOV_STACK[gpr++] | (cvar->sp * 8) << 24);
                break;
            }

            error("unsupported type: %d", var->ctype->type);
        case VAR_IMM:
            switch (var->ctype->type) {
            case CTYPE_PTR:
                o2(b, PUSH_ABS[gpr++]);
                add_reloc(text, STRING_LEN(b), NULL, ".data", R_X86_64_64, var->val.i);
                o8(b, 0);
                break;
            case CTYPE_INT:
                o2(b, PUSH_ABS[gpr++]);
                o8(b, var->val.i);
                break;
            case CTYPE_FLOAT:
                align(data->body, 8);
                o4(b, PUSH_XMM_ABS[xmm++]);
                add_reloc(text, STRING_LEN(b), NULL, ".data", R_X86_64_PC32, STRING_LEN(data->body) - 4);
                o4(b, 0);
                double tmp = var->val.f;
                o8(data->body, *(u64 *)&tmp);
                break;
            }
            break;
        default:
            error("unsupported var type: %c\n", var->stype);
        }
    }
    if (!dict_get(elf->syms, fn->name)) {
        Symbol *sym = make_symbol(fn->name, text, 0, STB_GLOBAL, STT_NOTYPE, 0);
        dict_put(elf->syms, fn->name, sym);
    }
    o1(b, 0xb8); // MOV eax
    o4(b, xmm);
    o1(b, 0xe8); // CALL
    add_reloc(text, STRING_LEN(b), STRING_BODY(fn->name), NULL, R_X86_64_PC32, -4);
    o4(b, 0);

    // Save function return value to the stack;
    o4(b, 0x458948 | ((off * 8) << 24)); // MOV [rbp+off], rax
}

int var_off(Dict *dict, Var *var, int *offset) {
    CompiledVar *cvar = dict_get(dict, var);
    if (cvar == NULL) {
        cvar = make_compiled_var((*offset)--);
        dict_put(dict, var, cvar);
    }
    return cvar->sp;
}

void assemble(Elf *elf, List *insts) {
    String *b = find_section(elf, ".text")->body;
    Dict *dict = make_address_dict();
    int offset = -1;
    o1(b, 0x55); // PUSH rbp
    o1(b, 0x48); o1(b, 0x89); o1(b, 0xe5); // MOV rbp, rsp
    o1(b, 0x48); o1(b, 0x81); o1(b, 0xec); o4(b, 80); // SUB rsp, 80
    for (int i = 0; i < LIST_LEN(insts); i++) {
        Inst *inst = LIST_ELEM(insts, i);
        switch (inst->op) {
        case '$': {
            Var *fn = inst->arg0;
            int off = var_off(dict, inst->arg1, &offset);
            List *args = inst->args;
            gen_call(b, elf, fn, off, args, dict);
            break;
        }
        case '=': {
            Var *var = inst->arg0;
            Var *val = inst->arg1;
            int off = var_off(dict, var, &offset);
            if (val->stype == VAR_IMM) {
                // MOV [rbp-off*8], inst->val.i
                o1(b, 0x48);
                o1(b, 0xc7);
                o1(b, 0x45);
                o1(b, off * 8);
                o4(b, val->val.i);
            } else if (val->stype == VAR_GLOBAL) {
                int off1 = var_off(dict, val, &offset);
                o4(b, 0x458b48 | ((off1 * 8) << 24)); // MOV rax, [rbp-val]
                o4(b, 0x458948 | ((off * 8) << 24));  // MOV [rbp-var], rax
            } else {
                error("not supported");
            }
            break;
        }
        default:
            error("unknown op\n");
        }
    }
    o2(b, 0xc031); // XOR eax, eax
    o1(b, 0xc9); // LEAVE
    o1(b, 0xc3); // RET
}

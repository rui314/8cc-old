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
 *
 * Reference documents for x86-64 instruction sets:
 *
 *   Intel 64 and IA-32 Architecture, Volume 1: Basic Architectures (March 2010)
 *   http://www.intel.com/Assets/PDF/manual/253665.pdf
 *
 *   Intel 64 and IA-32 Architecture, Volume 2A:Instruction Set Reference, A-M
 *   http://www.intel.com/Assets/PDF/manual/253666.pdf
 *
 *   Intel 64 and IA-32 Architecture, Volume 2B: Instruction Set Reference, N-Z
 *   http://www.intel.com/Assets/PDF/manual/253667.pdf
 */

typedef struct CompiledVar {
    int sp;
} CompiledVar;

CompiledVar *make_compiled_var(int sp) {
    CompiledVar *r = malloc(sizeof(CompiledVar));
    r->sp = sp;
    return r;
}

typedef struct Context {
    Elf *elf;
    String *text;
    Dict *stack;
    Dict *tmpvar;
} Context;

Context *make_context(Elf *elf) {
    Context *r = malloc(sizeof(Context));
    r->elf = elf;
    r->text = find_section(elf, ".text")->body;
    r->stack = make_address_dict();
    r->tmpvar = make_address_dict();
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

/*
 * Instructions for intermediate code
 */
static Inst *make_inst(int op, int narg) {
    Inst *r = malloc(sizeof(Inst));
    r->op = op;
    r->args = (0 < narg) ? make_list() : NULL;
    return r;
}

Inst *make_inst0(int op) {
    return make_inst(op, 0);
}

Inst *make_inst1(int op, void *v0) {
    Inst *r = make_inst(op, 1);
    LIST_ELEM(r->args, 0) = v0;
    return r;
}

Inst *make_inst2(int op, void *v0, void *v1) {
    Inst *r = make_inst(op, 2);
    LIST_ELEM(r->args, 0) = v0;
    LIST_ELEM(r->args, 1) = v1;
    return r;
}

Inst *make_inst3(int op, void *v0, void *v1, void *v2) {
    Inst *r = make_inst(op, 3);
    LIST_ELEM(r->args, 0) = v0;
    LIST_ELEM(r->args, 1) = v1;
    LIST_ELEM(r->args, 2) = v2;
    return r;
}

Inst *make_instn(int op, List *args) {
    Inst *r = make_inst(op, LIST_LEN(args));
    r->args = args;
    return r;
}

int add_string(Section *data, String *str) {
    int r = STRING_LEN(data->body);
    out(data->body, STRING_BODY(str), STRING_LEN(str));
    return r;
}

/*
 * Code generator
 */

static int var_stack_pos(Context *ctx, Var *var) {
    CompiledVar *cvar = dict_get(ctx->stack, var);
    if (cvar == NULL) {
        cvar = make_compiled_var((-ctx->stack->nelem - 1) * 8);
        dict_put(ctx->stack, var, cvar);
    }
    return cvar->sp;
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


static void emit_load(Context *ctx, Var *var) {
    if (var->stype == VAR_IMM) {
        // MOV rax, imm
        o1(ctx->text, 0x48);
        o1(ctx->text, 0xb8);
        o8(ctx->text, var->val.i);
    } else {
        // MOV rax, [rbp-off]
        int off = var_stack_pos(ctx, var);
        o3(ctx->text, 0x458b48);
        o1(ctx->text, off);
    }
}

static void handle_func_call(Context *ctx, Inst *inst) {
    Var *fn = LIST_ELEM(inst->args, 0);
    int rval_off = var_stack_pos(ctx, LIST_ELEM(inst->args, 1));
    Section *text = find_section(ctx->elf, ".text");
    Section *data = find_section(ctx->elf, ".data");
    int gpr = 0;
    int xmm = 0;

    for (int i = 2; i < LIST_LEN(inst->args); i++) {
        Var *var = LIST_ELEM(inst->args, i);
        switch (var->stype) {
        case VAR_GLOBAL:
            if (var->ctype->type == CTYPE_INT) {
                int off = var_stack_pos(ctx, var);
                o4(ctx->text, MOV_STACK[gpr++] | off << 24);
                break;
            }

            error("unsupported type: %d", var->ctype->type);
        case VAR_IMM:
            switch (var->ctype->type) {
            case CTYPE_PTR:
                o2(ctx->text, PUSH_ABS[gpr++]);
                add_reloc(text, STRING_LEN(ctx->text), NULL, ".data", R_X86_64_64, var->val.i);
                o8(ctx->text, 0);
                break;
            case CTYPE_INT:
            case CTYPE_CHAR:
                o2(ctx->text, PUSH_ABS[gpr++]);
                o8(ctx->text, var->val.i);
                break;
            case CTYPE_FLOAT:
                align(data->body, 8);
                o4(ctx->text, PUSH_XMM_ABS[xmm++]);
                add_reloc(text, STRING_LEN(ctx->text), NULL, ".data", R_X86_64_PC32, STRING_LEN(data->body) - 4);
                o4(ctx->text, 0);
                double tmp = var->val.f;
                o8(data->body, *(u64 *)&tmp);
                break;
            }
            break;
        default:
            error("unsupported var type: %c\n", var->stype);
        }
    }
    if (!dict_get(ctx->elf->syms, fn->name)) {
        Symbol *sym = make_symbol(fn->name, text, 0, STB_GLOBAL, STT_NOTYPE, 0);
        dict_put(ctx->elf->syms, fn->name, sym);
    }
    o1(ctx->text, 0xb8); // MOV eax
    o4(ctx->text, xmm);
    o1(ctx->text, 0xe8); // CALL
    add_reloc(text, STRING_LEN(ctx->text), STRING_BODY(fn->name), NULL, R_X86_64_PC32, -4);
    o4(ctx->text, 0);

    // Save function return value to the stack;
    o4(ctx->text, 0x458948 | (rval_off << 24)); // MOV [rbp+off], rax
}

static void store_rax(Context *ctx, Var *dst) {
    int off = var_stack_pos(ctx, dst);
    // MOV [rbp+off], rax
    o3(ctx->text, 0x458948);
    o1(ctx->text, off);
}

static void handle_add_or_sub(Context *ctx, Inst *inst, bool add) {
    Var *v0 = LIST_ELEM(inst->args, 1);
    emit_load(ctx, v0);
    Var *v1 = LIST_ELEM(inst->args, 2);
    if (v1->stype == VAR_IMM) {
        // ADD rax, imm
        o1(ctx->text, add ? 0x4805 : 0x482d);
        o4(ctx->text, v1->val.i);
    } else {
        int off = var_stack_pos(ctx, v1);
        // ADD rax, [rbp-v1]
        o3(ctx->text, add ? 0x450348 : 0x452b48);
        o1(ctx->text, off);
    }
    store_rax(ctx, LIST_ELEM(inst->args, 0));
}

static void handle_imul(Context *ctx, Inst *inst) {
    Var *src0 = LIST_ELEM(inst->args, 1);
    Var *src1 = LIST_ELEM(inst->args, 2);
    emit_load(ctx, src0);
    if (src1->stype == VAR_IMM) {
        // IMUL rax, imm, rax
        o3(ctx->text, 0xc06948);
        o4(ctx->text, src1->val.i);
    } else {
        int off = var_stack_pos(ctx, src1);
        // IMUL rax, [rbp+off]
        o4(ctx->text, 0x45af0f48);
        o1(ctx->text, off);
    }
    store_rax(ctx, LIST_ELEM(inst->args, 0));
}

static void handle_idiv(Context *ctx, Inst *inst) {
    Var *src0 = LIST_ELEM(inst->args, 1);
    Var *src1 = LIST_ELEM(inst->args, 2);
    emit_load(ctx, src0);
    // XOR edx, edx
    o2(ctx->text, 0xd231);
    if (src1->stype == VAR_IMM) {
        // MOV r11, imm
        o2(ctx->text, 0xbb49);
        o8(ctx->text, src1->val.i);
        // IDIV r11
        o3(ctx->text, 0xfbf749);
    } else {
        int off = var_stack_pos(ctx, src1);
        // IDIV [rbp+off]
        o3(ctx->text, 0x7df748);
        o1(ctx->text, off);
    }
    store_rax(ctx, LIST_ELEM(inst->args, 0));
}

static void handle_assign(Context *ctx, Inst *inst) {
    Var *var = LIST_ELEM(inst->args, 0);
    Var *val = LIST_ELEM(inst->args, 1);
    int off = var_stack_pos(ctx, var);
    if (val->stype == VAR_IMM) {
        // MOV [rbp-off*8], inst->val.i
        o3(ctx->text, 0x45c748);
        o1(ctx->text, off);
        o4(ctx->text, val->val.i);
    } else if (val->stype == VAR_GLOBAL) {
        // MOV rax, [rbp-val]
        // MOV [rbp-var], rax
        int off1 = var_stack_pos(ctx, val);
        o4(ctx->text, 0x458b48 | (off1 << 24));
        o4(ctx->text, 0x458948 | (off << 24));
    } else {
        error("not supported");
    }
}

void assemble(Elf *elf, List *insts) {
    Context *ctx = make_context(elf);
    o1(ctx->text, 0x55); // PUSH rbp
    o3(ctx->text, 0xe58948); // MOV rbp, rsp
    o3(ctx->text, 0xec8148); o4(ctx->text, 160); // SUB rsp, 160

    for (int i = 0; i < LIST_LEN(insts); i++) {
        Inst *inst = LIST_ELEM(insts, i);
        switch (inst->op) {
        case '+': case '-':
            handle_add_or_sub(ctx, inst, inst->op == '+');
            break;
        case '*':
            handle_imul(ctx, inst);
            break;
        case '/':
            handle_idiv(ctx, inst);
            break;
        case '$':
            handle_func_call(ctx, inst);
            break;
        case '=':
            handle_assign(ctx, inst);
            break;
        default:
            error("unknown op\n");
        }
    }
    o2(ctx->text, 0xc031); // XOR eax, eax
    o1(ctx->text, 0xc9); // LEAVE
    o1(ctx->text, 0xc3); // RET
}

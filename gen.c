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
    List *func_tbf;
    int sp;
} Context;

Context *make_context(Elf *elf) {
    Context *r = malloc(sizeof(Context));
    r->elf = elf;
    r->text = find_section(elf, ".text")->body;
    r->stack = make_address_dict();
    r->tmpvar = make_address_dict();
    r->func_tbf = NULL;
    r->sp = 0;
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

/*
 * Instructions for intermediate code
 */

static void handle_block(Context *ctx, Block *block);

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

Inst *make_inst4(int op, void *v0, void *v1, void *v2, void *v3) {
    Inst *r = make_inst(op, 4);
    LIST_ELEM(r->args, 0) = v0;
    LIST_ELEM(r->args, 1) = v1;
    LIST_ELEM(r->args, 2) = v2;
    LIST_ELEM(r->args, 3) = v3;
    return r;
}

Inst *make_instn(int op, List *args) {
    Inst *r = make_inst(op, LIST_LEN(args));
    r->args = args;
    return r;
}

/*
 * Code generator
 */

static int var_size(Ctype *ctype) {
    if (ctype->type == CTYPE_ARRAY) {
        return ctype->size * var_size(ctype->ptr);
    }
    return 1;
}

static int var_stack_pos(Context *ctx, Var *var) {
    CompiledVar *cvar = dict_get(ctx->stack, var);
    if (cvar == NULL) {
        ctx->sp += var_size(var->ctype);
        cvar = make_compiled_var(ctx->sp * -8);
        dict_put(ctx->stack, var, cvar);
    }
    return cvar->sp;
}

static void add_reloc(Section *text, long off, char *sym, char *section, int type, u64 addend) {
    Reloc *rel = make_reloc(off, sym, section, type, addend);
    list_push(text->rels, rel);
}

// MOV rdi/rsi/rdx/rcx/r8/r9, imm
static u16 push_arg_imm[] = { 0xbf48, 0xbe48, 0xba48, 0xb948, 0xb849, 0xb949 };

// MOVSD xmm0/xmm1/xmm2/xmm3/xmm4/xmm5/xmm6/xmm7, imm
static u32 push_arg_xmm_imm[] = { 0x05100ff2, 0x0d100ff2, 0x15100ff2, 0x1d100ff2,
                                  0x25100ff2, 0x2d100ff2, 0x35100ff2, 0x3d100ff2 };

// MOV rdi/rsi/rdx/rcx/r8/r9, [rbp+off]
static u32 push_arg[] = { 0xbd8b48, 0xb58b48, 0x958b48, 0x8d8b48, 0x858b4c, 0x8d8b4c };

// MOV [rbp+off], rdi/rsi/rdx/rcx/r8/r9
static u32 pop_arg[] = { 0xbd8948, 0xb58948, 0x958948, 0x8d8948, 0x85894c, 0x8d894c };

static void load_rax(Context *ctx, Var *var) {
    if (var->stype == VAR_IMM) {
        // MOV rax, imm
        o2(ctx->text, 0xb848);
        o8(ctx->text, var->val.i);
    } else {
        // MOV rax, [rbp+off]
        int off = var_stack_pos(ctx, var);
        o3(ctx->text, 0x858b48);
        o4(ctx->text, off);
    }
}

static void load_r11(Context *ctx, Var *var) {
    if (var->stype == VAR_IMM) {
        // MOV r11, imm
        o2(ctx->text, 0xbb49);
        o8(ctx->text, var->val.i);
    } else {
        // MOV r11, [rbp+off]
        int off = var_stack_pos(ctx, var);
        o3(ctx->text, 0x9d8b4c);
        o4(ctx->text, off);
    }
}

static void save_rax(Context *ctx, Var *dst) {
    int off = var_stack_pos(ctx, dst);
    // MOV [rbp+off], rax
    o3(ctx->text, 0x858948);
    o4(ctx->text, off);
}

static void handle_func_call(Context *ctx, Inst *inst) {
    Var *fn = LIST_ELEM(inst->args, 0);
    Section *text = find_section(ctx->elf, ".text");
    Section *data = find_section(ctx->elf, ".data");
    int gpr = 0;
    int xmm = 0;

    for (int i = 2; i < LIST_LEN(inst->args); i++) {
        Var *var = LIST_ELEM(inst->args, i);
        switch (var->stype) {
        case VAR_GLOBAL:
            if (var->ctype->type == CTYPE_INT || var->ctype->type == CTYPE_PTR) {
                int off = var_stack_pos(ctx, var);
                o3(ctx->text, push_arg[gpr++]);
                o4(ctx->text, off);
                break;
            }
            error("unsupported type: %d", var->ctype->type);
        case VAR_IMM:
            switch (var->ctype->type) {
            case CTYPE_PTR:
            case CTYPE_ARRAY:
                o2(ctx->text, push_arg_imm[gpr++]);
                add_reloc(text, STRING_LEN(ctx->text), NULL, ".data", R_X86_64_64, var->val.i);
                o8(ctx->text, 0);
                break;
            case CTYPE_INT:
            case CTYPE_CHAR:
                o2(ctx->text, push_arg_imm[gpr++]);
                o8(ctx->text, var->val.i);
                break;
            case CTYPE_FLOAT:
                align(data->body, 8);
                o4(ctx->text, push_arg_xmm_imm[xmm++]);
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
    list_push(ctx->func_tbf, fn->name);
    list_push(ctx->func_tbf, (void *)(intptr)STRING_LEN(ctx->text));
    o4(ctx->text, 0);

    // Save function return value to the stack;
    Var *rval = LIST_ELEM(inst->args, 1);
    save_rax(ctx, rval);
}

static void finish_func_call(Elf *elf, Dict *func, List *tbf) {
    Section *text = find_section(elf, ".text");
    for (int i = 0; i < LIST_LEN(tbf); i = i + 2) {
        String *fname = LIST_ELEM(tbf, i);
        u32 pos = (intptr)LIST_ELEM(tbf, i + 1);
        if (dict_has(func, fname)) {
            string_seek(text->body, pos);
            o4(text->body, (u32)(intptr)dict_get(func, fname) - pos - 4);
            string_seek(text->body, STRING_LEN(text->body));
        } else {
            add_reloc(text, pos, STRING_BODY(fname), NULL, R_X86_64_PC32, -4);
        }
    }
}

static void handle_add_or_sub(Context *ctx, Inst *inst, bool add) {
    Var *v0 = LIST_ELEM(inst->args, 1);
    load_rax(ctx, v0);
    Var *v1 = LIST_ELEM(inst->args, 2);
    if (v1->stype == VAR_IMM) {
        // ADD/SUB rax, imm
        o2(ctx->text, add ? 0x0548 : 0x2d48);
        o4(ctx->text, v1->val.i);
    } else {
        int off = var_stack_pos(ctx, v1);
        // ADD/SUB rax, [rbp-v1]
        o3(ctx->text, add ? 0x850348 : 0x852b48);
        o4(ctx->text, off);
    }
    save_rax(ctx, LIST_ELEM(inst->args, 0));
}

static void handle_imul(Context *ctx, Inst *inst) {
    Var *src0 = LIST_ELEM(inst->args, 1);
    Var *src1 = LIST_ELEM(inst->args, 2);
    load_rax(ctx, src0);
    if (src1->stype == VAR_IMM) {
        // IMUL rax, imm, rax
        o3(ctx->text, 0xc06948);
        o4(ctx->text, src1->val.i);
    } else {
        int off = var_stack_pos(ctx, src1);
        // IMUL rax, [rbp+off]
        o4(ctx->text, 0x85af0f48);
        o4(ctx->text, off);
    }
    save_rax(ctx, LIST_ELEM(inst->args, 0));
}

static void handle_idiv(Context *ctx, Inst *inst) {
    Var *src0 = LIST_ELEM(inst->args, 1);
    Var *src1 = LIST_ELEM(inst->args, 2);
    load_rax(ctx, src0);
    load_r11(ctx, src1);
    // XOR edx, edx
    o2(ctx->text, 0xd231);
    // IDIV r11
    o3(ctx->text, 0xfbf749);
    save_rax(ctx, LIST_ELEM(inst->args, 0));
}

static void handle_not(Context *ctx, Inst *inst) {
    Var *dst = LIST_ELEM(inst->args, 0);
    Var *src = LIST_ELEM(inst->args, 1);
    load_rax(ctx, src);
    // TEST eax, eax
    o2(ctx->text, 0xc085);
    // SETE al
    o3(ctx->text, 0xc0940f);
    // MOVZBL eax, al
    o3(ctx->text, 0xc0b60f);
    save_rax(ctx, dst);
}

static void emit_cmp(Context *ctx, Inst *inst) {
    Var *src0 = LIST_ELEM(inst->args, 1);
    Var *src1 = LIST_ELEM(inst->args, 2);
    load_rax(ctx, src0);
    if (src1->stype == VAR_IMM) {
        // CMP rax, imm
        o2(ctx->text, 0x3d48);
        o4(ctx->text, src1->val.i);
    } else if (src1->stype == VAR_GLOBAL) {
        int off = var_stack_pos(ctx, src1);
        // CMP rax, [rbp+off]
        o3(ctx->text, 0x453b48);
        o1(ctx->text, off);
    } else {
        error("not supported");
    }
}

static void handle_less(Context *ctx, Inst *inst) {
    Var *dst = LIST_ELEM(inst->args, 0);
    emit_cmp(ctx, inst);
    // SETL al
    o3(ctx->text, 0xc09c0f);
    // MOVZX eax, al
    o3(ctx->text, 0xc0b60f);
    save_rax(ctx, dst);
}

static void handle_less_equal(Context *ctx, Inst *inst) {
    Var *dst = LIST_ELEM(inst->args, 0);
    emit_cmp(ctx, inst);
    // SETLE al
    o3(ctx->text, 0xc09e0f);
    // MOVZX eax, al
    o3(ctx->text, 0xc0b60f);
    save_rax(ctx, dst);
}

static void handle_neg(Context *ctx, Inst *inst) {
    Var *dst = LIST_ELEM(inst->args, 0);
    Var *src = LIST_ELEM(inst->args, 1);
    load_rax(ctx, src);
    // NOT eax
    o2(ctx->text, 0xd0f7);
    save_rax(ctx, dst);
}

static void handle_inst3(Context *ctx, Inst *inst, u64 op) {
    Var *dst = LIST_ELEM(inst->args, 0);
    Var *src0 = LIST_ELEM(inst->args, 1);
    Var *src1 = LIST_ELEM(inst->args, 2);
    load_rax(ctx, src0);
    load_r11(ctx, src1);
    o3(ctx->text, op);
    save_rax(ctx, dst);
}

static void handle_and(Context *ctx, Inst *inst) {
    // AND rax, r11
    handle_inst3(ctx, inst, 0xd8214c);
}

static void handle_or(Context *ctx, Inst *inst) {
    // OR rax, r11
    handle_inst3(ctx, inst, 0xd8094c);
}

static void handle_xor(Context *ctx, Inst *inst) {
    // XOR rax, r11
    handle_inst3(ctx, inst, 0xd8314c);
}

static void handle_assign(Context *ctx, Inst *inst) {
    Var *var = LIST_ELEM(inst->args, 0);
    Var *val = LIST_ELEM(inst->args, 1);
    if (!val) {
        var_stack_pos(ctx, var);
        return;
    }
    int off = var_stack_pos(ctx, var);
    if (val->stype == VAR_IMM) {
        // MOV [rbp-off*8], inst->val.i
        o3(ctx->text, 0x85c748);
        o4(ctx->text, off);
        o4(ctx->text, val->val.i);
    } else if (val->stype == VAR_GLOBAL) {
        // MOV rax, [rbp-val]
        // MOV [rbp-var], rax
        int off1 = var_stack_pos(ctx, val);
        o3(ctx->text, 0x858b48);
        o4(ctx->text, off1);
        o3(ctx->text, 0x858948);
        o4(ctx->text, off);
    } else {
        error("not supported");
    }
}

static void handle_equal(Context *ctx, Inst *inst, bool eq) {
    Var *dst = LIST_ELEM(inst->args, 0);
    emit_cmp(ctx, inst);
    // SETE al or SETNE al
    o3(ctx->text, eq ? 0xc0940f : 0xc0950f);
    // MOVZX eax, al
    o3(ctx->text, 0xc0b60f);
    save_rax(ctx, dst);
}

static void handle_address(Context *ctx, Inst *inst) {
    Var *p = LIST_ELEM(inst->args, 0);
    Var *v = LIST_ELEM(inst->args, 1);
    int off = var_stack_pos(ctx, v);
    // LEA rax, [ebp+off]
    o3(ctx->text, 0x858d48);
    o4(ctx->text, off);
    save_rax(ctx, p);
}

static void handle_deref(Context *ctx, Inst *inst) {
    Var *v = LIST_ELEM(inst->args, 0);
    Var *p = LIST_ELEM(inst->args, 1);
    load_rax(ctx, p);
    // MOV rax, [rax]
    o3(ctx->text, 0x008b48);
    save_rax(ctx, v);
}

static void handle_assign_deref(Context *ctx, Inst *inst) {
    Var *loc = LIST_ELEM(inst->args, 0);
    Var *v = LIST_ELEM(inst->args, 1);
    load_rax(ctx, v);
    load_r11(ctx, loc);
    // MOV [r11], rax
    o3(ctx->text, 0x038949);
}

static void handle_if(Context *ctx, Inst *inst) {
    Var *cond = LIST_ELEM(inst->args, 0);
    Block *then = LIST_ELEM(inst->args, 1);
    Block *els = LIST_ELEM(inst->args, 2);
    Block *cont = LIST_ELEM(inst->args, 3);
    load_rax(ctx, cond);

    // TEST rax, rax
    o2(ctx->text, 0xc085);

    // JE offset
    o2(ctx->text, 0x840f);
    o4(ctx->text, 0); // filled later
    int pos0 = STRING_LEN(ctx->text);
    handle_block(ctx, then);

    int pos1;
    if (els) {
        // JMP offset
        o1(ctx->text, 0xe9);
        o4(ctx->text, 0); // filled later
        pos1 = STRING_LEN(ctx->text);
        handle_block(ctx, els);
    }

    // Backfill
    int save = STRING_LEN(ctx->text);
    string_seek(ctx->text, pos0 - 4);
    if (els) {
        o4(ctx->text, pos1 - pos0);
        string_seek(ctx->text, pos1 - 4);
        o4(ctx->text, save - pos1);
    } else {
        o4(ctx->text, save - pos0);
    }
    string_seek(ctx->text, save);

    handle_block(ctx, cont);
}

static void jump_to(Context *ctx, int pos) {
    // JMP offset
    o1(ctx->text, 0xe9);
    o4(ctx->text, pos - STRING_LEN(ctx->text) - 4);
}

static void handle_jmp(Context *ctx, Inst *inst) {
    Block *dst = LIST_ELEM(inst->args, 0);
    if (dst->pos < 0) {
        handle_block(ctx, dst);
    } else {
        jump_to(ctx, dst->pos);
    }
}

void handle_return(Context *ctx, Inst *inst) {
    Var *retval = LIST_ELEM(inst->args, 0);
    load_rax(ctx, retval);
    o1(ctx->text, 0xc9); // LEAVE
    o1(ctx->text, 0xc3); // RET
}

static void handle_block(Context *ctx, Block *block) {
    if (block->pos >= 0) {
        jump_to(ctx, block->pos);
        return;
    }
    block->pos = STRING_LEN(ctx->text);
    for (int i = 0; i < LIST_LEN(block->code); i++) {
        Inst *inst = LIST_ELEM(block->code, i);
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
        case '!':
            handle_not(ctx, inst);
            break;
        case '|':
            handle_or(ctx, inst);
            break;
        case '^':
            handle_xor(ctx, inst);
            break;
        case '<':
            handle_less(ctx, inst);
            break;
        case '~':
            handle_neg(ctx, inst);
            break;
        case '&':
            handle_and(ctx, inst);
            break;
        case OP_LE:
            handle_less_equal(ctx, inst);
            break;
        case OP_FUNC_CALL:
            handle_func_call(ctx, inst);
            break;
        case OP_EQ:
            handle_equal(ctx, inst, true);
            break;
        case OP_NE:
            handle_equal(ctx, inst, false);
            break;
        case OP_ADDRESS:
            handle_address(ctx, inst);
            break;
        case OP_DEREF:
            handle_deref(ctx, inst);
            break;
        case OP_ASSIGN:
            handle_assign(ctx, inst);
            break;
        case OP_ASSIGN_DEREF:
            handle_assign_deref(ctx, inst);
            break;
        case OP_IF:
            handle_if(ctx, inst);
            return; // return here
        case OP_JMP:
            handle_jmp(ctx, inst);
            return; // return here
        case OP_RETURN:
            handle_return(ctx, inst);
            return; // return here
        default:
            error("unknown op\n");
        }
        // NOP
        o1(ctx->text, 0x90);
    }
}

void save_params(Context *ctx, Function *func) {
    for (int i = 0; i < LIST_LEN(func->params); i++) {
        Var *param = LIST_ELEM(func->params, i);
        int off = var_stack_pos(ctx, param);
        o3(ctx->text, pop_arg[i]);
        o4(ctx->text, off);
    }
}

void assemble1(Context *ctx, Function *func) {
    o1(ctx->text, 0x55); // PUSH rbp
    o3(ctx->text, 0xe58948); // MOV rbp, rsp
    save_params(ctx, func);

    // SUB rsp, 0
    o3(ctx->text, 0xec8148);
    int pos = STRING_LEN(ctx->text);
    o4(ctx->text, 0); // filled later
    handle_block(ctx, func->entry);

    // Backfill SUB rsp, 0
    string_seek(ctx->text, pos);
    o4(ctx->text, (ctx->sp + 1) * 8);
    string_seek(ctx->text, STRING_LEN(ctx->text));
}

void assemble(Elf *elf, List *fns) {
    Section *text = find_section(elf, ".text");
    Dict *dict = make_string_dict();
    List *tbf = make_list();
    for (int i = 0; i < LIST_LEN(fns); i++) {
        Context *ctx = make_context(elf);
        ctx->func_tbf = tbf;
        Function *func = LIST_ELEM(fns, i);

        Symbol *fsym = make_symbol(func->name, text, STRING_LEN(ctx->text), STB_GLOBAL, STT_NOTYPE, 1);
        dict_put(ctx->elf->syms, func->name, fsym);

        dict_put(dict, func->name, (void *)(intptr)STRING_LEN(ctx->text));
        assemble1(ctx, func);
    }
    finish_func_call(elf, dict, tbf);
}

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
    int off;
} CompiledVar;

static CompiledVar *make_compiled_var(int off) {
    CompiledVar *r = malloc(sizeof(CompiledVar));
    r->off = off;
    return r;
}

typedef struct Context {
    Elf *elf;
    String *text;
    Dict *stack;
    Dict *global;
    List *func_tbf;
    int sp;
} Context;

static Context *make_context(Elf *elf) {
    Context *r = malloc(sizeof(Context));
    r->elf = elf;
    r->text = find_section(elf, ".text")->body;
    r->stack = make_address_dict();
    r->global = make_address_dict();
    r->func_tbf = NULL;
    r->sp = 0;
    return r;
}

Section *find_section(Elf *elf, char *name) {
    for (int i = 0; i < LIST_LEN(elf->sections); i++) {
        Section *sect = LIST_REF(elf->sections, i);
        if (!strcmp(sect->name, name))
            return sect;
    }
    panic("cannot find section '%s'", name);
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

static Reloc *make_reloc(long off, char *sym, char *section, int type, u64 addend) {
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

Inst *make_inst1(int op, void *v0) {
    Inst *r = make_inst(op, 1);
    LIST_REF(r->args, 0) = v0;
    return r;
}

Inst *make_inst2(int op, void *v0, void *v1) {
    Inst *r = make_inst(op, 2);
    LIST_REF(r->args, 0) = v0;
    LIST_REF(r->args, 1) = v1;
    return r;
}

Inst *make_inst3(int op, void *v0, void *v1, void *v2) {
    Inst *r = make_inst(op, 3);
    LIST_REF(r->args, 0) = v0;
    LIST_REF(r->args, 1) = v1;
    LIST_REF(r->args, 2) = v2;
    return r;
}

Inst *make_inst4(int op, void *v0, void *v1, void *v2, void *v3) {
    Inst *r = make_inst(op, 4);
    LIST_REF(r->args, 0) = v0;
    LIST_REF(r->args, 1) = v1;
    LIST_REF(r->args, 2) = v2;
    LIST_REF(r->args, 3) = v3;
    return r;
}

Inst *make_instn(int op, List *args) {
    Inst *r = make_inst(op, LIST_LEN(args));
    r->args = args;
    return r;
}

bool is_flonum(Ctype *ctype) {
    return ctype->type == CTYPE_FLOAT || ctype->type == CTYPE_DOUBLE;
}

/*
 * Code generator
 */

static void emit1(Context *ctx, u8 b)  { o1(ctx->text, b); }
static void emit2(Context *ctx, u16 w) { o2(ctx->text, w); }
static void emit3(Context *ctx, u32 d) { o3(ctx->text, d); }
static void emit4(Context *ctx, u32 d) { o4(ctx->text, d); }
static void emit8(Context *ctx, u64 q) { o8(ctx->text, q); }

static int var_abs_pos(Context *ctx, Var *var) {
    assert(var->stype == VAR_IMM);
    assert(var->ctype->type == CTYPE_ARRAY);
    CompiledVar *cvar = dict_get(ctx->global, var);
    if (!cvar) {
        Section *data = find_section(ctx->elf, ".data");
        cvar = make_compiled_var(STRING_LEN(data->body));
        out(data->body, STRING_BODY(var->val.s), STRING_LEN(var->val.s));
        dict_put(ctx->global, var, cvar);
    }
    return cvar->off;
}

static int var_stack_pos(Context *ctx, Var *var) {
    CompiledVar *cvar = dict_get(ctx->stack, var);
    if (cvar == NULL) {
        ctx->sp += ctype_sizeof(var->ctype);
        cvar = make_compiled_var(ctx->sp * -8);
        dict_put(ctx->stack, var, cvar);
    }
    return cvar->off;
}

static void add_reloc(Section *text, long off, char *sym, char *section, int type, u64 addend) {
    Reloc *rel = make_reloc(off, sym, section, type, addend);
    list_push(text->rels, rel);
}


// MOV rdi/rsi/rdx/rcx/r8/r9, rax
static u32 push_arg[] = { 0xc78948, 0xc68948, 0xc28948, 0xc18948, 0xc08949, 0xc18949 };

// MOV rax, rdi/rsi/rdx/rcx/r8/r9
static u32 pop_arg[] = { 0xf88948, 0xf08948, 0xd08948, 0xc88948, 0xc0894c, 0xc8894c };

// MOVSD xmm[0-7], xmm7
static u32 push_xmm_arg[] = {
    0xc7100ff2, 0xcf100ff2, 0xd7100ff2, 0xdf100ff2,
    0xe7100ff2, 0xef100ff2, 0xf7100ff2, 0xff100ff2,
};

static void load_imm(Context *ctx, Var *var, u16 op) {
    if (var->ctype->type == CTYPE_INT) {
        // MOV rax/r11, imm
        emit2(ctx, op);
        emit8(ctx, var->val.i);
        return;
    }
    if (var->ctype->type == CTYPE_ARRAY) {
        Section *text = find_section(ctx->elf, ".text");
        // MOV rax/r11, imm
        emit2(ctx, op);
        int off = var_abs_pos(ctx, var);
        add_reloc(text, STRING_LEN(ctx->text), NULL, ".data", R_X86_64_64, off);
        emit8(ctx, 0);
        return;
    }
    panic("unsupported IMM ctype: %d", var->ctype->type);
}

static void load_rax(Context *ctx, Var *var) {
    if (var->stype == VAR_IMM) {
        load_imm(ctx, var, 0xb848);
        return;
    }
    int off = var_stack_pos(ctx, var);
    int bits = ctype_sizeof(var->ctype) * 8;
    // MOV rax/eax, [rbp+off]
    if (bits == 64)
        emit1(ctx, 0x48);
    emit2(ctx, 0x858b);
    emit4(ctx, off);

    if (bits == 8) {
        // MOVSX/MOVZX eax, al
        emit4(ctx, var->ctype->signedp ? 0xc0be0f48 : 0xc0b60f48);
    } else if (bits == 16) {
        // MOVSX/MOVZX eax, ax
        emit4(ctx, var->ctype->signedp ? 0xc0bf0f48 : 0xc0b70f48);
    } else if (bits == 32) {
        // MOVSX rax, eax
        if (var->ctype->signedp)
            emit3(ctx, 0xc06348);
    }
}

static void load_r11(Context *ctx, Var *var) {
    if (var->stype == VAR_IMM) {
        load_imm(ctx, var, 0xbb49);
        return;
    }
    int off = var_stack_pos(ctx, var);
    int bits = ctype_sizeof(var->ctype) * 8;
    // MOV r11d, [rbp+off]
    emit1(ctx, (bits == 64) ? 0x4c : 0x44);
    emit2(ctx, 0x9d8b);
    emit4(ctx, off);

    if (bits == 8) {
        // MOVSX/MOVZX r11, r11b
        emit4(ctx, var->ctype->signedp ? 0xdbbe0f4d : 0xdbb60f4d);
    } else if (bits == 16) {
        // MOVSX/MOVZX r11, r11w
        emit4(ctx, var->ctype->signedp ? 0xdbbf0f4d : 0xdbb70f4d);
    } else if (bits == 32) {
        // MOVSX r11, r11d
        if (var->ctype->signedp)
            emit3(ctx, 0xdb634d);
    }
}

static void save_rax(Context *ctx, Var *dst) {
    int off = var_stack_pos(ctx, dst);
    int bits = ctype_sizeof(dst->ctype) * 8;
    // MOV [rbp+off], rax/eax/ax/al
    if (bits >= 64)
        emit3(ctx, 0x858948);
    else if (bits == 32)
        emit2(ctx, 0x8589);
    else if (bits == 16)
        emit3(ctx, 0x858966);
    else if (bits == 8)
        emit2(ctx, 0x8588);
    else panic("unknown variable size: %d", bits);
    emit4(ctx, off);
}

static u64 flonum_to_u64(double d) {
    u64 *p = (u64 *)&d;
    return *p;
}

static void load_xmm_imm(Context *ctx, Var *var, u16 movss) {
    assert(is_flonum(var->ctype));
    // SUB rsp, 8
    emit3(ctx, 0xec8148);
    emit4(ctx, 8);
    // MOV rax, imm
    emit2(ctx, 0xb848);
    emit8(ctx, flonum_to_u64(var->val.f));
    // MOV [rsp], rax
    emit4(ctx, 0x24048948);
    // MOVSD xmm0, [rsp]
    emit3(ctx, 0x100ff2);
    emit2(ctx, movss);
    // ADD rsp, 8
    emit3(ctx, 0xc48148);
    emit4(ctx, 8);
}

static void load_xmm0(Context *ctx, Var *var) {
    if (var->stype == VAR_IMM) {
        load_xmm_imm(ctx, var, 0x2404);
        return;
    }
    int off = var_stack_pos(ctx, var);
    if (var->ctype->type == CTYPE_FLOAT) {
        // MOVSS xmm0, [rbp+off]
        emit4(ctx, 0x85100ff2);
        emit4(ctx, off);
        // CVTPS2PD xmm0, xmm0
        emit3(ctx, 0xc05a0f);
    } else {
        // MOVSD off, xmm0
        emit4(ctx, 0x85100ff2);
        emit4(ctx, off);
    }
}

static void load_xmm7(Context *ctx, Var *var) {
    if (var->stype == VAR_IMM) {
        load_xmm_imm(ctx, var, 0x243c);
        return;
    }
    int off = var_stack_pos(ctx, var);
    if (var->ctype->type == CTYPE_FLOAT) {
        // MOVSS xmm7, [rbp+off]
        emit4(ctx, 0xbd100ff3);
        emit4(ctx, off);
        // CVTPS2PD xmm7, xmm7
        emit3(ctx, 0xff5a0f);
    } else {
        // MOVSD off, xmm7
        emit4(ctx, 0xbd100ff2);
        emit4(ctx, off);
    }
}

static void save_xmm0(Context *ctx, Var *var) {
    assert(is_flonum(var->ctype));
    if (var->ctype->type == CTYPE_FLOAT) {
        // CVTPD2PS xmm0, xmm0
        emit4(ctx, 0xc05a0f66);
        // MOVSS off, xmm0
        emit4(ctx, 0x85110ff3);
    } else {
        // MOVSD off, xmm0
        emit4(ctx, 0x85110ff2);
    }
    int off = var_stack_pos(ctx, var);
    emit4(ctx, off);
}

static void handle_int_to_float(Context *ctx, Inst *inst) {
    Var *dst = LIST_REF(inst->args, 0);
    Var *src = LIST_REF(inst->args, 1);
    assert(is_flonum(dst->ctype));
    assert(src->ctype->type == CTYPE_INT);
    load_rax(ctx, src);
    // cvtsi2sd xmm0, rax
    emit4(ctx, 0x2a0f48f2);
    emit1(ctx, 0xc0);
    save_xmm0(ctx, dst);
}

static void handle_float_to_int(Context *ctx, Inst *inst) {
    Var *dst = LIST_REF(inst->args, 0);
    Var *src = LIST_REF(inst->args, 1);
    assert(dst->ctype->type == CTYPE_INT);
    assert(is_flonum(src->ctype));
    load_xmm0(ctx, src);
    // CVTTSD2SI eax, xmm0
    emit4(ctx, 0xc02c0ff2);
    save_rax(ctx, dst);
}

static void handle_func_call(Context *ctx, Inst *inst) {
    Var *fn = LIST_REF(inst->args, 0);
    Section *text = find_section(ctx->elf, ".text");
    int gpr = 0;
    int xmm = 0;

    for (int i = 2; i < LIST_LEN(inst->args); i++) {
        Var *var = LIST_REF(inst->args, i);
        if (is_flonum(var->ctype)) {
            load_xmm7(ctx, var);
            emit4(ctx, push_xmm_arg[xmm++]);
        } else {
            load_rax(ctx, var);
            emit3(ctx, push_arg[gpr++]);
        }
    }
    if (!dict_get(ctx->elf->syms, fn->name)) {
        Symbol *sym = make_symbol(fn->name, text, 0, STB_GLOBAL, STT_NOTYPE, 0);
        dict_put(ctx->elf->syms, fn->name, sym);
    }
    emit1(ctx, 0xb8); // MOV eax
    emit4(ctx, xmm);
    emit1(ctx, 0xe8); // CALL
    list_push(ctx->func_tbf, fn->name);
    list_push(ctx->func_tbf, (void *)(intptr)STRING_LEN(ctx->text));
    emit4(ctx, 0);

    // Save function return value to the stack;
    Var *rval = LIST_REF(inst->args, 1);
    save_rax(ctx, rval);
}

static void finish_func_call(Elf *elf, Dict *func, List *tbf) {
    Section *text = find_section(elf, ".text");
    for (int i = 0; i < LIST_LEN(tbf); i = i + 2) {
        String *fname = LIST_REF(tbf, i);
        u32 pos = (intptr)LIST_REF(tbf, i + 1);
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
    Var *dst = LIST_REF(inst->args, 0);
    Var *src0 = LIST_REF(inst->args, 1);
    Var *src1 = LIST_REF(inst->args, 2);
    if (is_flonum(src0->ctype)) {
        load_xmm0(ctx, src0);
        load_xmm7(ctx, src1);
        // ADDSD/SUBSD xmm0, xmm7
        emit4(ctx, add ? 0xc7580ff2 : 0xc75c0ff2);
        save_xmm0(ctx, dst);
        return;
    }
    load_rax(ctx, src0);
    load_r11(ctx, src1);
    // ADD/SUB rax, r11
    emit3(ctx, add ? 0xd8014c : 0xd8294c);
    save_rax(ctx, dst);
}

static void handle_imul(Context *ctx, Inst *inst) {
    Var *dst = LIST_REF(inst->args, 0);
    Var *src0 = LIST_REF(inst->args, 1);
    Var *src1 = LIST_REF(inst->args, 2);
    if (is_flonum(src0->ctype)) {
        load_xmm0(ctx, src0);
        load_xmm7(ctx, src1);
        // MULSD xmm0 xmm7
        emit4(ctx, 0xc7590ff2);
        save_xmm0(ctx, dst);
        return;
    }
    load_rax(ctx, src0);
    load_r11(ctx, src1);
    // IMUL rax, r11
    emit4(ctx, 0xc3af0f49);
    save_rax(ctx, dst);
}

static void handle_idiv(Context *ctx, Inst *inst) {
    Var *dst = LIST_REF(inst->args, 0);
    Var *src0 = LIST_REF(inst->args, 1);
    Var *src1 = LIST_REF(inst->args, 2);
    if (is_flonum(src0->ctype)) {
        load_xmm0(ctx, src0);
        load_xmm7(ctx, src1);
        // DIVSD xmm0, xmm7
        emit4(ctx, 0xc75e0ff2);
        save_xmm0(ctx, dst);
        return;
    }
    load_rax(ctx, src0);
    load_r11(ctx, src1);
    // XOR edx, edx
    emit2(ctx, 0xd231);
    // IDIV r11
    emit3(ctx, 0xfbf749);
    save_rax(ctx, LIST_REF(inst->args, 0));
}

static void handle_not(Context *ctx, Inst *inst) {
    Var *dst = LIST_REF(inst->args, 0);
    Var *src = LIST_REF(inst->args, 1);
    load_rax(ctx, src);
    // TEST eax, eax
    emit2(ctx, 0xc085);
    // SETE al
    emit3(ctx, 0xc0940f);
    // MOVZBL eax, al
    emit3(ctx, 0xc0b60f);
    save_rax(ctx, dst);
}

static void emit_cmp(Context *ctx, Inst *inst, u32 op) {
    Var *dst = LIST_REF(inst->args, 0);
    Var *src0 = LIST_REF(inst->args, 1);
    Var *src1 = LIST_REF(inst->args, 2);
    load_rax(ctx, src0);
    load_r11(ctx, src1);
    // CMP rax, r11
    emit3(ctx, 0xd8394c);
    emit3(ctx, op);
    // MOVZX eax, al
    emit3(ctx, 0xc0b60f);
    save_rax(ctx, dst);
}

static void emit_fcmp(Context *ctx, Inst *inst, u32 op) {
    Var *dst = LIST_REF(inst->args, 0);
    Var *src0 = LIST_REF(inst->args, 1);
    Var *src1 = LIST_REF(inst->args, 2);
    emit1(ctx, 0x90);
    emit1(ctx, 0x90);
    load_xmm0(ctx, src0);
    load_xmm7(ctx, src1);
    // UCOMISD xmm0, xmm7
    emit4(ctx, 0xc72e0f66);
    emit3(ctx, op);
    // MOVZX eax, al
    emit3(ctx, 0xc0b60f);
    save_rax(ctx, dst);
}

static void handle_less(Context *ctx, Inst *inst) {
    if (is_flonum(((Var *)LIST_REF(inst->args, 1))->ctype)) {
        // SETL al
        emit_cmp(ctx, inst, 0xc09c0f);
    } else {
        // SETL al
        emit_cmp(ctx, inst, 0xc09c0f);
    }
}

static void handle_less_equal(Context *ctx, Inst *inst) {
    if (is_flonum(((Var *)LIST_REF(inst->args, 1))->ctype)) {
        // SETA al
        // emit_fcmp(ctx, inst, 0xc0970f);
        // SETLE al
        // emit_fcmp(ctx, inst, 0xc09e0f);
        // SETBE al
        emit_fcmp(ctx, inst, 0xc0960f);
    } else {
        // SETLE al
        emit_cmp(ctx, inst, 0xc09e0f);
    }
}

static void handle_neg(Context *ctx, Inst *inst) {
    Var *dst = LIST_REF(inst->args, 0);
    Var *src = LIST_REF(inst->args, 1);
    load_rax(ctx, src);
    // NOT eax
    emit2(ctx, 0xd0f7);
    save_rax(ctx, dst);
}

static void handle_inst3(Context *ctx, Inst *inst, u64 op) {
    Var *dst = LIST_REF(inst->args, 0);
    Var *src0 = LIST_REF(inst->args, 1);
    Var *src1 = LIST_REF(inst->args, 2);
    load_rax(ctx, src0);
    load_r11(ctx, src1);
    emit3(ctx, op);
    save_rax(ctx, dst);
}

static void handle_and(Context *ctx, Inst *inst) {
    // AND rax, r11
    handle_inst3(ctx, inst, 0xd8214c);
}

static void handle_shift(Context *ctx, Inst *inst, u64 op) {
    Var *dst = LIST_REF(inst->args, 0);
    Var *src0 = LIST_REF(inst->args, 1);
    Var *src1 = LIST_REF(inst->args, 2);
    load_rax(ctx, src1);
    // MOV ecx, eax
    emit2(ctx, 0xc189);
    load_rax(ctx, src0);
    emit3(ctx, op);
    save_rax(ctx, dst);
}

static void handle_shl(Context *ctx, Inst *inst) {
    // SHL rax, cl
    handle_shift(ctx, inst, 0xe0d348);
}

static void handle_shr(Context *ctx, Inst *inst) {
    // SHR rax, cl
    handle_shift(ctx, inst, 0xe8d348);
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
    Var *var = LIST_REF(inst->args, 0);
    Var *val = LIST_REF(inst->args, 1);
    assert(var->ctype->type != CTYPE_ARRAY);
    assert(val);
    if (is_flonum(((Var *)LIST_REF(inst->args, 1))->ctype)) {
        load_xmm0(ctx, val);
        save_xmm0(ctx, var);
    } else {
        load_rax(ctx, val);
        save_rax(ctx, var);
    }
}

static void handle_equal(Context *ctx, Inst *inst, bool eq) {
    // SETE al or SETNE al
    if (is_flonum(((Var *)LIST_REF(inst->args, 1))->ctype)) {
        emit_fcmp(ctx, inst, eq ? 0xc0940f : 0xc0950f);
    } else {
        emit_cmp(ctx, inst, eq ? 0xc0940f : 0xc0950f);
    }
}

static void handle_address(Context *ctx, Inst *inst) {
    Var *p = LIST_REF(inst->args, 0);
    Var *v = LIST_REF(inst->args, 1);
    if (v->stype == VAR_IMM && v->ctype->type == CTYPE_ARRAY) {
        load_rax(ctx, v);
    } else {
        int off = var_stack_pos(ctx, v);
        // LEA rax, [ebp+off]
        emit3(ctx, 0x858d48);
        emit4(ctx, off);
    }
    save_rax(ctx, p);
}

static void handle_deref(Context *ctx, Inst *inst) {
    Var *v = LIST_REF(inst->args, 0);
    Var *p = LIST_REF(inst->args, 1);
    load_rax(ctx, p);
    // MOV rax, [rax]
    emit3(ctx, 0x008b48);
    save_rax(ctx, v);
}

static void handle_assign_deref(Context *ctx, Inst *inst) {
    Var *loc = LIST_REF(inst->args, 0);
    Var *v = LIST_REF(inst->args, 1);
    load_rax(ctx, v);
    load_r11(ctx, loc);
    // MOV [r11], rax
    emit3(ctx, 0x038949);
}

static void handle_if(Context *ctx, Inst *inst) {
    Var *cond = LIST_REF(inst->args, 0);
    Block *then = LIST_REF(inst->args, 1);
    Block *els = LIST_REF(inst->args, 2);
    Block *cont = LIST_REF(inst->args, 3);
    load_rax(ctx, cond);

    // TEST rax, rax
    emit2(ctx, 0xc085);

    // JE offset
    emit2(ctx, 0x840f);
    emit4(ctx, 0); // filled later
    int pos0 = STRING_LEN(ctx->text);
    handle_block(ctx, then);

    int pos1;
    if (els) {
        // JMP offset
        emit1(ctx, 0xe9);
        emit4(ctx, 0); // filled later
        pos1 = STRING_LEN(ctx->text);
        handle_block(ctx, els);
    }

    // Backfill
    int save = STRING_LEN(ctx->text);
    string_seek(ctx->text, pos0 - 4);
    if (els) {
        emit4(ctx, pos1 - pos0);
        string_seek(ctx->text, pos1 - 4);
        emit4(ctx, save - pos1);
    } else {
        emit4(ctx, save - pos0);
    }
    string_seek(ctx->text, save);

    handle_block(ctx, cont);
}

static void handle_alloc(Context *ctx, Inst *inst) {
    Var *v = LIST_REF(inst->args, 0);
    var_stack_pos(ctx, v);
}

static void jump_to(Context *ctx, int pos) {
    // JMP offset
    emit1(ctx, 0xe9);
    emit4(ctx, pos - STRING_LEN(ctx->text) - 4);
}

static void handle_jmp(Context *ctx, Inst *inst) {
    Block *dst = LIST_REF(inst->args, 0);
    if (dst->pos < 0) {
        handle_block(ctx, dst);
    } else {
        jump_to(ctx, dst->pos);
    }
}

static void handle_return(Context *ctx, Inst *inst) {
    Var *retval = LIST_REF(inst->args, 0);
    load_rax(ctx, retval);
    emit1(ctx, 0xc9); // LEAVE
    emit1(ctx, 0xc3); // RET
}

static void handle_block(Context *ctx, Block *block) {
    if (block->pos >= 0) {
        jump_to(ctx, block->pos);
        return;
    }
    block->pos = STRING_LEN(ctx->text);
    for (int i = 0; i < LIST_LEN(block->code); i++) {
        Inst *inst = LIST_REF(block->code, i);
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
        case OP_SHL:
            handle_shl(ctx, inst);
            break;
        case OP_SHR:
            handle_shr(ctx, inst);
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
        case OP_ALLOC:
            handle_alloc(ctx, inst);
            break;
        case OP_I2F:
            handle_int_to_float(ctx, inst);
            break;
        case OP_F2I:
            handle_float_to_int(ctx, inst);
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
        emit1(ctx, 0x90);
    }
}

static void save_params(Context *ctx, Function *func) {
    for (int i = 0; i < LIST_LEN(func->params); i++) {
        Var *param = LIST_REF(func->params, i);
        emit3(ctx, pop_arg[i]);
        save_rax(ctx, param);
    }
}

static void assemble1(Context *ctx, Function *func) {
    emit1(ctx, 0x55); // PUSH rbp
    emit3(ctx, 0xe58948); // MOV rbp, rsp

    // SUB rsp, 0
    emit3(ctx, 0xec8148);
    int pos = STRING_LEN(ctx->text);
    emit4(ctx, 0); // filled later
    emit1(ctx, 0x90); // NOP

    save_params(ctx, func);
    handle_block(ctx, func->entry);

    // Backfill SUB rsp, 0
    int off = ((ctx->sp + 2) & ~1) * 8; // need to be 16-byte aligned
    string_seek(ctx->text, pos);
    emit4(ctx, off);
    string_seek(ctx->text, STRING_LEN(ctx->text));
}

void assemble(Elf *elf, List *fns) {
    Section *text = find_section(elf, ".text");
    Dict *dict = make_string_dict();
    List *tbf = make_list();
    for (int i = 0; i < LIST_LEN(fns); i++) {
        Context *ctx = make_context(elf);
        ctx->func_tbf = tbf;
        Function *func = LIST_REF(fns, i);

        Symbol *fsym = make_symbol(func->name, text, STRING_LEN(ctx->text), STB_GLOBAL, STT_NOTYPE, 1);
        dict_put(ctx->elf->syms, func->name, fsym);

        dict_put(dict, func->name, (void *)(intptr)STRING_LEN(ctx->text));
        assemble1(ctx, func);
    }
    finish_func_call(elf, dict, tbf);
}

/*
 * gen.c - code generator
 *
 * Copyright 2010 Rui Ueyama <rui314@gmail.com>.  All rights reserved.
 * This code is available under the simplified BSD license.  See LICENSE for details.
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

#define RESERVED ((void *)-1)
#define USABLE(ctx, reg) ((ctx)->var[reg] != RESERVED && !(ctx)->var[reg])
#define INUSE(ctx, reg) ((ctx)->var[reg] != RESERVED && (ctx)->var[reg])

typedef struct Context {
    Elf *elf;
    String *text;
    Dict *stack;
    Dict *global;
    List *func_tbf;
    int sp;
    // For register allocation
    Var *var[16];
    int lastuse[16];
    int serial;
} Context;

static void reset_context(Context *ctx) {
    static void *init[] = { RESERVED, 0, 0, 0, RESERVED, RESERVED, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
    for (int i = 0; i < 16; i++) {
        ctx->var[i] = init[i];
        ctx->lastuse[i] = 0;
    }
    ctx->serial = 1;
}

static Context *make_context(Elf *elf) {
    Context *r = malloc(sizeof(Context));
    r->elf = elf;
    r->text = find_section(elf, ".text")->body;
    r->stack = make_address_dict();
    r->global = make_address_dict();
    r->func_tbf = NULL;
    r->sp = 0;
    reset_context(r);
    return r;
}

Section *find_section(Elf *elf, char *name) {
    for (int i = 0; i < LIST_LEN(elf->sections); i++) {
        Section *sect = LIST_REF(elf->sections, i);
        if (!strcmp(sect->name, name))
            return sect;
    }
    panic("cannot find section '%s'", name);
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

static Reloc *make_reloc(long off, char *sym, Section *section, int type, u64 addend) {
    Reloc *rel = malloc(sizeof(Reloc));
    rel->off = off;
    rel->sym = sym;
    rel->section = section;
    rel->type = type;
    rel->addend = addend;
    return rel;
}

/*==============================================================================
 * X86 machine code generation.
 */

static void emit1(Context *ctx, u8 b)  { o1(ctx->text, b); }
static void emit2(Context *ctx, u16 w) { o2(ctx->text, w); }
static void emit3(Context *ctx, u32 d) { o3(ctx->text, d); }
static void emit4(Context *ctx, u32 d) { o4(ctx->text, d); }
static void emit8(Context *ctx, u64 q) { o8(ctx->text, q); }

static void spill(Context *ctx, int reg);

/*
 * Mod R/M and SIB
 *
 * Many x86 instructions takes Mod R/M and optional SIB and displacement bytes.
 * As described in Chapter 2 in the Intel 64 and IA-32 Architecture Software
 * Developer's Manual, Volume 2A, there are four different use cases of these
 * bytes.
 *
 *   - Memory addressing without a SIB byte
 *   - Register-register addressing
 *   - Memory addressing with a SIB byte
 *   - Register operand coded in opcode byte
 *
 * For the details of x86 machine code format, refer to the Intel Developer's
 * Manuals.
 */

static void emit_prefix(Context *ctx, int size, int reg0, int reg1) {
    if (size == 2)
        emit1(ctx, 0x66);
    if (size != 8 && !EXT_REG(reg0) && !EXT_REG(reg1))
        return;
    int rex = 0x40;
    if (size == 8) rex |= 8;
    if (EXT_REG(reg0)) rex |= 4;
    if (EXT_REG(reg1)) rex |= 1;
    o1(ctx->text, rex);
}

static void emit_op(Context *ctx, int op) {
    if (op < 0x100)
        emit1(ctx, op);
    else if (op < 0x10000)
        emit2(ctx, op);
    else
        emit3(ctx, op);
}

static void emit_modrm(Context *ctx, int mod, int reg, int rm) {
    int byte = (mod << 6) | ((reg & 7) << 3) | (rm & 7);
    o1(ctx->text, byte);
}

static void emit_memop(Context *ctx, int size, int op, int reg0, int reg1, int off) {
    emit_prefix(ctx, size, reg0, reg1);
    emit_op(ctx, op);
    int oneword = -128 <= off && off <= 127;
    emit_modrm(ctx, oneword ? 1 : 2, reg0, reg1);
    if (oneword)
        emit1(ctx, off);
    else
        emit4(ctx, off);
}

static void emit_regop(Context *ctx, int size, int op, int reg0, int reg1) {
    emit_prefix(ctx, size, reg0, reg1);
    emit_op(ctx, op);
    emit_modrm(ctx, 3, reg0, reg1);
}

/*======================================================================
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
    Inst *r = make_inst(op, 0);
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

static void add_reloc(Section *text, long off, char *sym, Section *section, int type, u64 addend) {
    Reloc *rel = make_reloc(off, sym, section, type, addend);
    list_push(text->rels, rel);
}


// Registers for function argument passing.
static const int grp_arg[] = { RDI, RSI, RDX, RCX, R8, R9 };
static const int xmm_arg[] = { XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7 };

static void load_imm(Context *ctx, int reg, Var *var) {
    if (var->ctype->type == CTYPE_INT) {
        // MOV reg, imm
        emit_prefix(ctx, 8, 0, reg);
        emit1(ctx, 0xb8 | (reg & 7));
        emit8(ctx, var->val.i);
        return;
    }
    if (var->ctype->type == CTYPE_ARRAY) {
        Section *text = find_section(ctx->elf, ".text");
        Section *data = find_section(ctx->elf, ".data");
        // MOV rax/r11, imm
        emit_prefix(ctx, 8, 0, reg);
        emit1(ctx, 0xb8 | (reg & 7));
        int off = var_abs_pos(ctx, var);
        add_reloc(text, STRING_LEN(ctx->text), NULL, data, R_X86_64_64, off);
        emit8(ctx, 0);
        return;
    }
    panic("unsupported IMM ctype: %d", var->ctype->type);
}

static void assign(Context *ctx, Var *var, int reg);

static void load(Context *ctx, int reg, Var *var) {
    spill(ctx, reg);
    if (var->stype == VAR_IMM) {
        load_imm(ctx, reg, var);
        return;
    }
    for (int i = 0; i < 16; i++)
        if (INUSE(ctx, i) && reg != i && ctx->var[i] == var) {
            emit_regop(ctx, 8, 0x8b, reg, i);
            ctx->var[i] = NULL;
            assign(ctx, var, i);
            return;
        }

    int off = var_stack_pos(ctx, var);
    int size = ctype_sizeof(var->ctype);
    int signedp = var->ctype->signedp;
    // MOVSX/MOVZX reg, [rbp+off]
    if (size == 1)
        emit_memop(ctx, size, signedp ? 0xbe0f : 0xb60f, reg, RBP, off);
    else if (size == 2)
        emit_memop(ctx, size, signedp ? 0xbf0f : 0xb70f, reg, RBP, off);
    else if (size == 4)
        emit_memop(ctx, signedp ? 8 : 4, signedp ? 0x63 : 0x8b, reg, RBP, off);
    else
        emit_memop(ctx, size, 0x8b, reg, RBP, off);
}

static void save(Context *ctx, Var *dst, int reg) {
    int off = var_stack_pos(ctx, dst);
    int size = ctype_sizeof(dst->ctype);
    // MOV [rbp+off], reg
    if (size == 1)
        emit_memop(ctx, 1, 0x88, reg, RBP, off);
    else
        emit_memop(ctx, size, 0x89, reg, RBP, off);
}

static u64 flonum_to_u64(double d) {
    u64 *p = (u64 *)&d;
    return *p;
}

static void load_xmm_imm(Context *ctx, int xmmreg, Var *var) {
    assert(is_flonum(var->ctype));
    // SUB rsp, 8
    emit3(ctx, 0xec8148);
    emit4(ctx, 8);
    // MOV rax, imm
    emit2(ctx, 0xb848);
    emit8(ctx, flonum_to_u64(var->val.f));
    // MOV [rsp], rax
    emit4(ctx, 0x24048948);
    // MOVSD reg, [rsp]
    emit_prefix(ctx, 4, 0, xmmreg);
    emit3(ctx, 0x100ff2);
    emit_modrm(ctx, 0, xmmreg, RSP);
    emit1(ctx, 0x24);
    // ADD rsp, 8
    emit3(ctx, 0xc48148);
    emit4(ctx, 8);
}

static void load_xmm(Context *ctx, int reg, Var *var) {
    spill(ctx, RAX);
    if (var->stype == VAR_IMM) {
        load_xmm_imm(ctx, reg, var);
        return;
    }
    int off = var_stack_pos(ctx, var);
    if (var->ctype->type == CTYPE_FLOAT) {
        // MOVSS reg, [rbp+off]
        emit_prefix(ctx, 4, reg, reg);
        emit_memop(ctx, 4, 0x100ff3, reg, RBP, off);
        // CVTPS2PD reg, reg
        emit_prefix(ctx, 4, reg, reg);
        emit2(ctx, 0x5a0f);
        emit_modrm(ctx, 3, reg, reg);
    } else {
        // MOVSD reg, [rbp+off]
        emit_prefix(ctx, 4, reg, reg);
        emit_memop(ctx, 4, 0x100ff2, reg, RBP, off);
    }
}

static void save_xmm(Context *ctx, Var *var, int reg) {
    assert(is_flonum(var->ctype));
    int off = var_stack_pos(ctx, var);
    if (var->ctype->type == CTYPE_FLOAT) {
        // CVTPD2PS reg, reg
        emit_prefix(ctx, 4, reg, reg);
        emit3(ctx, 0x5a0f66);
        emit_modrm(ctx, 3, reg, reg);
        // MOVSS off, reg
        emit_prefix(ctx, 4, reg, reg);
        emit_memop(ctx, 4, 0x110ff3, reg, RBP, off);
    } else {
        // MOVSD off, reg
        emit_prefix(ctx, 4, reg, reg);
        emit_memop(ctx, 4, 0x110ff2, reg, RBP, off);
    }
}

/*==============================================================================
 * Register allocation
 */

static void assign(Context *ctx, Var *var, int reg) {
    for (int i = 0; i < 16; i++)
        if (ctx->var[i] == var)
            ctx->var[i] = NULL;
    ctx->var[reg] = var;
    ctx->lastuse[reg] = ctx->serial++;
    ctx->var[reg]->need_save = true;
}

static void spill(Context *ctx, int reg) {
    if (!INUSE(ctx, reg))
        return;
    if (!ctx->var[reg]->need_save)
        return;
    save(ctx, ctx->var[reg], reg);
    ctx->var[reg] = NULL;
}

static void save_all(Context *ctx) {
    for (int i = 0; i < 16; i++)
        spill(ctx, i);
}

static void save_var(Context *ctx, Var *var) {
    for (int i = 0; i < 16; i++)
        if (ctx->var[i] == var) {
            spill(ctx, i);
            return;
        }
}

static int load_reg_replace(Context *ctx, int reg, Var *var) {
    spill(ctx, reg);
    load(ctx, reg, var);
    ctx->var[reg] = var;
    ctx->lastuse[reg] = ctx->serial++;
    var->need_save = false;
    return reg;
}

static int load_reg(Context *ctx, Var *var) {
    int reg;
    for (reg = 1; reg < 16; reg++)
        if (ctx->var[reg] == var)
            return load_reg_replace(ctx, reg, var);
    for (reg = 1; reg < 16; reg++)
        if (USABLE(ctx, reg))
            return load_reg_replace(ctx, reg, var);

    int serial = INT_MAX;
    int oldest = 0;
    for (reg = 1; reg < 16; reg++)
        if (INUSE(ctx, reg) && !ctx->var[reg]->need_save && ctx->lastuse[reg] < serial) {
            serial = ctx->lastuse[reg];
            oldest = reg;
        }
    if (serial != INT_MAX)
        return load_reg_replace(ctx, oldest, var);

    serial = INT_MAX;
    for (reg = 1; reg < 16; reg++)
        if (INUSE(ctx, reg) && ctx->lastuse[reg] < serial) {
            serial = ctx->lastuse[reg];
            oldest = reg;
        }
    return load_reg_replace(ctx, reg, var);
}

/*==============================================================================
 * IL handlers
 */

static void handle_int_to_float(Context *ctx, Inst *inst) {
    Var *dst = LIST_REF(inst->args, 0);
    Var *src = LIST_REF(inst->args, 1);
    assert(is_flonum(dst->ctype));
    assert(src->ctype->type == CTYPE_INT);
    load(ctx, RAX, src);
    // cvtsi2sd xmm0, rax
    emit4(ctx, 0x2a0f48f2);
    emit1(ctx, 0xc0);
    save_xmm(ctx, dst, XMM0);
}

static void handle_float_to_int(Context *ctx, Inst *inst) {
    Var *dst = LIST_REF(inst->args, 0);
    Var *src = LIST_REF(inst->args, 1);
    assert(dst->ctype->type == CTYPE_INT);
    assert(is_flonum(src->ctype));
    spill(ctx, RAX);
    load_xmm(ctx, XMM0, src);
    // CVTTSD2SI eax, xmm0
    emit4(ctx, 0xc02c0ff2);
    save(ctx, dst, RAX);
}

static void handle_func_call(Context *ctx, Inst *inst) {
    Var *fn = LIST_REF(inst->args, 0);
    Section *text = find_section(ctx->elf, ".text");
    int gpr = 0;
    int xmm = 0;

    save_all(ctx);
    for (int i = 2; i < LIST_LEN(inst->args); i++) {
        Var *var = LIST_REF(inst->args, i);
        if (is_flonum(var->ctype))
            load_xmm(ctx, xmm_arg[xmm++], var);
        else
            load(ctx, grp_arg[gpr++], var);
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
    save(ctx, rval, RAX);
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
        load_xmm(ctx, XMM0, src0);
        load_xmm(ctx, XMM7, src1);
        // ADDSD/SUBSD xmm0, xmm7
        emit4(ctx, add ? 0xc7580ff2 : 0xc75c0ff2);
        save_xmm(ctx, dst, XMM0);
        return;
    }
    int reg0 = load_reg(ctx, src0);
    int reg1 = load_reg(ctx, src1);
    spill(ctx, reg0);
    // ADD/SUB reg0, reg1
    emit_regop(ctx, 8, add ? 1 : 0x29, reg1, reg0);
    ctx->var[reg0] = dst;
}

static void handle_imul(Context *ctx, Inst *inst) {
    Var *dst = LIST_REF(inst->args, 0);
    Var *src0 = LIST_REF(inst->args, 1);
    Var *src1 = LIST_REF(inst->args, 2);
    if (is_flonum(src0->ctype)) {
        load_xmm(ctx, XMM0, src0);
        load_xmm(ctx, XMM7, src1);
        // MULSD xmm0 xmm7
        emit4(ctx, 0xc7590ff2);
        save_xmm(ctx, dst, XMM0);
        return;
    }
    int reg0 = load_reg(ctx, src0);
    int reg1 = load_reg(ctx, src1);
    // IMUL reg0, reg1
    emit_regop(ctx, 8, 0xaf0f, reg0, reg1);
    assign(ctx, dst, reg0);
    save_all(ctx);
}

static void handle_idiv(Context *ctx, Inst *inst) {
    Var *dst = LIST_REF(inst->args, 0);
    Var *src0 = LIST_REF(inst->args, 1);
    Var *src1 = LIST_REF(inst->args, 2);
    if (is_flonum(src0->ctype)) {
        load_xmm(ctx, XMM0, src0);
        load_xmm(ctx, XMM7, src1);
        // DIVSD xmm0, xmm7
        emit4(ctx, 0xc75e0ff2);
        save_xmm(ctx, dst, XMM0);
        return;
    }
    spill(ctx, RAX);
    spill(ctx, RDX);
    // XOR edx, edx
    emit_regop(ctx, 4, 0x31, RDX, RDX);

    ctx->var[RAX] = ctx->var[RDX] = RESERVED;
    load(ctx, RAX, src0);
    int reg = load_reg(ctx, src1);
    ctx->var[RAX] = ctx->var[RDX] = NULL;

    // IDIV reg
    emit_regop(ctx, 8, 0xf7, 7, reg);
    assign(ctx, dst, RAX);
}

static void handle_not(Context *ctx, Inst *inst) {
    Var *dst = LIST_REF(inst->args, 0);
    Var *src = LIST_REF(inst->args, 1);
    load(ctx, RAX, src);
    // TEST eax, eax
    emit2(ctx, 0xc085);
    // SETE al
    emit3(ctx, 0xc0940f);
    // MOVZBL eax, al
    emit3(ctx, 0xc0b60f);
    assign(ctx, dst, RAX);
}

static void emit_cmp(Context *ctx, Inst *inst, int op) {
    Var *dst = LIST_REF(inst->args, 0);
    Var *src0 = LIST_REF(inst->args, 1);
    Var *src1 = LIST_REF(inst->args, 2);
    save_all(ctx);
    load(ctx, RAX, src0);
    int reg1 = load_reg(ctx, src1);
    // CMP rax, reg1
    emit_regop(ctx, 8, 0x39, reg1, RAX);
    emit_regop(ctx, 4, op, 0, RAX);
    // MOVZX eax, al
    emit_regop(ctx, 4, 0xb60f, 0, RAX);
    assign(ctx, dst, RAX);
}

static void emit_fcmp(Context *ctx, Inst *inst, u32 op) {
    Var *dst = LIST_REF(inst->args, 0);
    Var *src0 = LIST_REF(inst->args, 1);
    Var *src1 = LIST_REF(inst->args, 2);
    spill(ctx, RAX);
    load_xmm(ctx, XMM0, src1);
    load_xmm(ctx, XMM7, src0);
    // UCOMISD xmm0, xmm7
    emit4(ctx, 0xc72e0f66);
    emit3(ctx, op);
    // MOVZX eax, al
    emit3(ctx, 0xc0b60f);
    assign(ctx, dst, RAX);
}

static void handle_less(Context *ctx, Inst *inst) {
    if (is_flonum(((Var *)LIST_REF(inst->args, 1))->ctype)) {
        // SETL al
        // emit_fcmp(ctx, inst, 0xc09c0f);
        // SETA al
        emit_fcmp(ctx, inst, 0xc0970f);
    } else {
        // SETL al
        emit_cmp(ctx, inst, 0x9c0f);
    }
}

static void handle_less_equal(Context *ctx, Inst *inst) {
    if (is_flonum(((Var *)LIST_REF(inst->args, 1))->ctype)) {
        // SETAE al
        emit_fcmp(ctx, inst, 0xc0930f);
    } else {
        // SETLE al
        emit_cmp(ctx, inst, 0x9e0f);
    }
}

static void handle_neg(Context *ctx, Inst *inst) {
    Var *dst = LIST_REF(inst->args, 0);
    Var *src = LIST_REF(inst->args, 1);
    int reg = load_reg(ctx, src);
    // NOT reg
    emit_regop(ctx, 4, 0xf7, 2, reg);
    assign(ctx, dst, reg);
}

static void handle_inst3(Context *ctx, Inst *inst, int op) {
    Var *dst = LIST_REF(inst->args, 0);
    Var *src0 = LIST_REF(inst->args, 1);
    Var *src1 = LIST_REF(inst->args, 2);
    int reg0 = load_reg(ctx, src0);
    int reg1 = load_reg(ctx, src1);
    emit_regop(ctx, 4, op, reg1, reg0);
    assign(ctx, dst, reg0);
}

static void handle_and(Context *ctx, Inst *inst) {
    handle_inst3(ctx, inst, 0x21);
}

static void handle_or(Context *ctx, Inst *inst) {
    handle_inst3(ctx, inst, 0x09);
}

static void handle_xor(Context *ctx, Inst *inst) {
    handle_inst3(ctx, inst, 0x31);
}

static void handle_shift(Context *ctx, Inst *inst, int op1) {
    Var *dst = LIST_REF(inst->args, 0);
    Var *src0 = LIST_REF(inst->args, 1);
    Var *src1 = LIST_REF(inst->args, 2);
    load_reg_replace(ctx, RCX, src1);
    int reg = load_reg(ctx, src0);
    emit_regop(ctx, 8, 0xd3, op1, reg);
    assign(ctx, dst, reg);
}

static void handle_shl(Context *ctx, Inst *inst) {
    handle_shift(ctx, inst, 4);
}

static void handle_shr(Context *ctx, Inst *inst) {
    handle_shift(ctx, inst, 5);
}

static void handle_assign(Context *ctx, Inst *inst) {
    Var *var = LIST_REF(inst->args, 0);
    Var *val = LIST_REF(inst->args, 1);
    assert(var->ctype->type != CTYPE_ARRAY);
    assert(val);
    if (is_flonum(((Var *)LIST_REF(inst->args, 1))->ctype)) {
        load_xmm(ctx, XMM0, val);
        save_xmm(ctx, var, XMM0);
    } else {
        int reg = load_reg(ctx, val);
        assign(ctx, var, reg);
    }
}

static void handle_equal(Context *ctx, Inst *inst, bool eq) {
    // SETE al or SETNE al
    if (is_flonum(((Var *)LIST_REF(inst->args, 1))->ctype)) {
        emit_fcmp(ctx, inst, eq ? 0xc0940f : 0xc0950f);
    } else {
        emit_cmp(ctx, inst, eq ? 0x940f : 0x950f);
    }
}

static void handle_address(Context *ctx, Inst *inst) {
    Var *p = LIST_REF(inst->args, 0);
    Var *v = LIST_REF(inst->args, 1);
    spill(ctx, RAX);
    if (v->stype == VAR_IMM && v->ctype->type == CTYPE_ARRAY) {
        load(ctx, RAX, v);
    } else {
        save_var(ctx, v);
        int off = var_stack_pos(ctx, v);
        // LEA rax, [ebp+off]
        emit_memop(ctx, 8, 0x8d, RAX, RBP, off);
    }
    assign(ctx, p, RAX);
}

static void handle_deref(Context *ctx, Inst *inst) {
    Var *v = LIST_REF(inst->args, 0);
    Var *p = LIST_REF(inst->args, 1);
    int reg = load_reg(ctx, p);
    // MOV rax, [rax]
    emit_memop(ctx, 8, 0x8b, reg, reg, 0);
    assign(ctx, v, reg);
}

static void handle_assign_deref(Context *ctx, Inst *inst) {
    Var *loc = LIST_REF(inst->args, 0);
    Var *v = LIST_REF(inst->args, 1);
    save_all(ctx);
    int reg = load_reg(ctx, v);
    load(ctx, R11, loc);
    // MOV [R11], reg
    emit_memop(ctx, 8, 0x89, reg, R11, 0);
}

static void handle_if(Context *ctx, Inst *inst) {
    Var *cond = LIST_REF(inst->args, 0);
    Block *then = LIST_REF(inst->args, 1);
    Block *els = LIST_REF(inst->args, 2);
    Block *cont = LIST_REF(inst->args, 3);

    // TEST reg, reg
    int reg = load_reg(ctx, cond);
    emit_regop(ctx, 4, 0x85, reg, reg);
    save_all(ctx);

    // JE offset
    emit2(ctx, 0x840f);
    emit4(ctx, 0); // filled later
    int pos0 = STRING_LEN(ctx->text);
    handle_block(ctx, then);
    save_all(ctx);

    int pos1;
    if (els) {
        // JMP offset
        emit1(ctx, 0xe9);
        emit4(ctx, 0); // filled later
        pos1 = STRING_LEN(ctx->text);
        handle_block(ctx, els);
        save_all(ctx);
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
    save_all(ctx);
    if (dst->pos < 0) {
        handle_block(ctx, dst);
    } else {
        jump_to(ctx, dst->pos);
    }
}

static void handle_return(Context *ctx, Inst *inst) {
    Var *retval = LIST_REF(inst->args, 0);
    load(ctx, RAX, retval);
    emit1(ctx, 0xc9); // LEAVE
    emit1(ctx, 0xc3); // RET
}

static void handle_flush(Context *ctx) {
    for (int i = 0; i < 16; i++)
        if (INUSE(ctx, i) && ctx->var[i]->is_temp)
            ctx->var[i] = NULL;
}

static void handle_block(Context *ctx, Block *block) {
    save_all(ctx);
    reset_context(ctx);
    if (block->pos >= 0) {
        jump_to(ctx, block->pos);
        return;
    }
    block->pos = STRING_LEN(ctx->text);
    for (int i = 0; i < LIST_LEN(block->code); i++) {
        Inst *inst = LIST_REF(block->code, i);
        switch (inst->op) {
        case OP_FLUSH:
            handle_flush(ctx);
            break;
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
        save(ctx, param, grp_arg[i]);
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

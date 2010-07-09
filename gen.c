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

CompiledVar *make_compiled_var(int off) {
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

Context *make_context(Elf *elf) {
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
        Section *sect = LIST_ELEM(elf->sections, i);
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

static void emit1(Context *ctx, u8 b)  { o1(ctx->text, b); }
static void emit2(Context *ctx, u16 w) { o2(ctx->text, w); }
static void emit3(Context *ctx, u32 d) { o3(ctx->text, d); }
static void emit4(Context *ctx, u32 d) { o4(ctx->text, d); }
static void emit8(Context *ctx, u64 q) { o8(ctx->text, q); }

static int var_size(Ctype *ctype) {
    if (ctype->type == CTYPE_ARRAY) {
        return ctype->size * var_size(ctype->ptr);
    }
    return 1;
}

static Var *resolve_alias(Var *var) {
    while (var->stype == VAR_ALIAS)
        var = var->loc;
    return var;
}

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
    var = resolve_alias(var);
    CompiledVar *cvar = dict_get(ctx->stack, var);
    if (cvar == NULL) {
        ctx->sp += var_size(var->ctype);
        cvar = make_compiled_var(ctx->sp * -8);
        dict_put(ctx->stack, var, cvar);
    }
    return cvar->off;
}

static void add_reloc(Section *text, long off, char *sym, char *section, int type, u64 addend) {
    Reloc *rel = make_reloc(off, sym, section, type, addend);
    list_push(text->rels, rel);
}


int type_bits(Ctype *ctype) {
    switch (ctype->type) {
    case CTYPE_FLOAT: return 64;
    case CTYPE_PTR:   return 64;
    case CTYPE_ARRAY: return 64;
    case CTYPE_LONG:  return 64;
    case CTYPE_INT:   return 32;
    case CTYPE_SHORT: return 16;
    case CTYPE_CHAR:  return  8;
    default: panic("unknown type: %d", ctype->type);
    }
}

// MOV rdi/rsi/rdx/rcx/r8/r9, imm
// static u16 push_arg_imm[] = { 0xbf48, 0xbe48, 0xba48, 0xb948, 0xb849, 0xb949 };

// MOVSD xmm0/xmm1/xmm2/xmm3/xmm4/xmm5/xmm6/xmm7, imm
// static u32 push_arg_xmm_imm[] = { 0x05100ff2, 0x0d100ff2, 0x15100ff2, 0x1d100ff2,
//                                   0x25100ff2, 0x2d100ff2, 0x35100ff2, 0x3d100ff2 };

// MOV rdi/rsi/rdx/rcx/r8/r9, rax
static u32 push_arg[] = { 0xc78948, 0xc68948, 0xc28948, 0xc18948, 0xc08949, 0xc18949 };

// MOV rax, rdi/rsi/rdx/rcx/r8/r9
static u32 pop_arg[] = { 0xf88948, 0xf08948, 0xd08948, 0xc88948, 0xc0894c, 0xc8894c };

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

    var = resolve_alias(var);
    int off = var_stack_pos(ctx, var);
    int bits = type_bits(var->ctype);
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

    var = resolve_alias(var);
    int off = var_stack_pos(ctx, var);
    int bits = type_bits(var->ctype);
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
    int bits = type_bits(dst->ctype);
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

static void handle_func_call(Context *ctx, Inst *inst) {
    Var *fn = LIST_ELEM(inst->args, 0);
    Section *text = find_section(ctx->elf, ".text");
    int gpr = 0;
    int xmm = 0;

    for (int i = 2; i < LIST_LEN(inst->args); i++) {
        Var *var = LIST_ELEM(inst->args, i);
        load_rax(ctx, var);
        emit3(ctx, push_arg[gpr++]);
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
    Var *src0 = LIST_ELEM(inst->args, 1);
    Var *src1 = LIST_ELEM(inst->args, 2);
    load_rax(ctx, src0);
    load_r11(ctx, src1);
    // ADD/SUB rax, r11
    emit3(ctx, add ? 0xd8014c : 0xd8294c);
    save_rax(ctx, LIST_ELEM(inst->args, 0));
}

static void handle_imul(Context *ctx, Inst *inst) {
    Var *src0 = LIST_ELEM(inst->args, 1);
    Var *src1 = LIST_ELEM(inst->args, 2);
    load_rax(ctx, src0);
    load_r11(ctx, src1);
    // IMUL rax, r11
    emit4(ctx, 0xc3af0f49);
    save_rax(ctx, LIST_ELEM(inst->args, 0));
}

static void handle_idiv(Context *ctx, Inst *inst) {
    Var *src0 = LIST_ELEM(inst->args, 1);
    Var *src1 = LIST_ELEM(inst->args, 2);
    load_rax(ctx, src0);
    load_r11(ctx, src1);
    // XOR edx, edx
    emit2(ctx, 0xd231);
    // IDIV r11
    emit3(ctx, 0xfbf749);
    save_rax(ctx, LIST_ELEM(inst->args, 0));
}

static void handle_not(Context *ctx, Inst *inst) {
    Var *dst = LIST_ELEM(inst->args, 0);
    Var *src = LIST_ELEM(inst->args, 1);
    load_rax(ctx, src);
    // TEST eax, eax
    emit2(ctx, 0xc085);
    // SETE al
    emit3(ctx, 0xc0940f);
    // MOVZBL eax, al
    emit3(ctx, 0xc0b60f);
    save_rax(ctx, dst);
}

static void emit_cmp(Context *ctx, Inst *inst) {
    Var *src0 = LIST_ELEM(inst->args, 1);
    Var *src1 = LIST_ELEM(inst->args, 2);
    load_rax(ctx, src0);
    load_r11(ctx, src1);
    // CMP rax, r11
    emit3(ctx, 0xd8394c);
}

static void handle_less(Context *ctx, Inst *inst) {
    Var *dst = LIST_ELEM(inst->args, 0);
    emit_cmp(ctx, inst);
    // SETL al
    emit3(ctx, 0xc09c0f);
    // MOVZX eax, al
    emit3(ctx, 0xc0b60f);
    save_rax(ctx, dst);
}

static void handle_less_equal(Context *ctx, Inst *inst) {
    Var *dst = LIST_ELEM(inst->args, 0);
    emit_cmp(ctx, inst);
    // SETLE al
    emit3(ctx, 0xc09e0f);
    // MOVZX eax, al
    emit3(ctx, 0xc0b60f);
    save_rax(ctx, dst);
}

static void handle_neg(Context *ctx, Inst *inst) {
    Var *dst = LIST_ELEM(inst->args, 0);
    Var *src = LIST_ELEM(inst->args, 1);
    load_rax(ctx, src);
    // NOT eax
    emit2(ctx, 0xd0f7);
    save_rax(ctx, dst);
}

static void handle_inst3(Context *ctx, Inst *inst, u64 op) {
    Var *dst = LIST_ELEM(inst->args, 0);
    Var *src0 = LIST_ELEM(inst->args, 1);
    Var *src1 = LIST_ELEM(inst->args, 2);
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
    Var *dst = LIST_ELEM(inst->args, 0);
    Var *src0 = LIST_ELEM(inst->args, 1);
    Var *src1 = LIST_ELEM(inst->args, 2);
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
    Var *var = LIST_ELEM(inst->args, 0);
    Var *val = LIST_ELEM(inst->args, 1);
    if (!val) {
        var_stack_pos(ctx, var);
        return;
    }
    load_rax(ctx, val);
    save_rax(ctx, var);
}

static void handle_equal(Context *ctx, Inst *inst, bool eq) {
    Var *dst = LIST_ELEM(inst->args, 0);
    emit_cmp(ctx, inst);
    // SETE al or SETNE al
    emit3(ctx, eq ? 0xc0940f : 0xc0950f);
    // MOVZX eax, al
    emit3(ctx, 0xc0b60f);
    save_rax(ctx, dst);
}

static void handle_address(Context *ctx, Inst *inst) {
    Var *p = LIST_ELEM(inst->args, 0);
    Var *v = LIST_ELEM(inst->args, 1);
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
    Var *v = LIST_ELEM(inst->args, 0);
    Var *p = LIST_ELEM(inst->args, 1);
    load_rax(ctx, p);
    // MOV rax, [rax]
    emit3(ctx, 0x008b48);
    save_rax(ctx, v);
}

static void handle_assign_deref(Context *ctx, Inst *inst) {
    Var *loc = LIST_ELEM(inst->args, 0);
    Var *v = LIST_ELEM(inst->args, 1);
    load_rax(ctx, v);
    load_r11(ctx, loc);
    // MOV [r11], rax
    emit3(ctx, 0x038949);
}

static void handle_if(Context *ctx, Inst *inst) {
    Var *cond = LIST_ELEM(inst->args, 0);
    Block *then = LIST_ELEM(inst->args, 1);
    Block *els = LIST_ELEM(inst->args, 2);
    Block *cont = LIST_ELEM(inst->args, 3);
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

static void jump_to(Context *ctx, int pos) {
    // JMP offset
    emit1(ctx, 0xe9);
    emit4(ctx, pos - STRING_LEN(ctx->text) - 4);
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

void save_params(Context *ctx, Function *func) {
    for (int i = 0; i < LIST_LEN(func->params); i++) {
        Var *param = LIST_ELEM(func->params, i);
        emit3(ctx, pop_arg[i]);
        save_rax(ctx, param);
    }
}

void assemble1(Context *ctx, Function *func) {
    emit1(ctx, 0x55); // PUSH rbp
    emit3(ctx, 0xe58948); // MOV rbp, rsp

    // SUB rsp, 0
    emit3(ctx, 0xec8148);
    int pos = STRING_LEN(ctx->text);
    emit4(ctx, 0); // filled later

    save_params(ctx, func);
    handle_block(ctx, func->entry);

    // Backfill SUB rsp, 0
    string_seek(ctx->text, pos);
    emit4(ctx, (ctx->sp + 1) * 8);
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

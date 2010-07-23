#include "unittest.h"
#include "../cpp.c"

static ReadContext *make_test_read_context(char *str) {
    FILE *stream = create_file(str);
    File *file = make_file(stream, "-");
    Elf *elf = new_elf();
    CppContext *cppctx = make_cpp_context(file);
    return make_read_context(file, elf, cppctx);
}

static CppContext *make_test_cpp_context(char *str) {
    FILE *stream = create_file(str);
    File *file = make_file(stream, "-");
    return make_cpp_context(file);
}

static List *parse_string(char *str) {
    ReadContext *ctx = make_test_read_context(str);
    List *expanded = make_list();
    for (Token *tok = read_token(ctx); tok; tok = read_token(ctx))
        list_push(expanded, tok);
    return expanded;
}

static bool list_equal(List *list0, List *list1) {
    if (LIST_LEN(list0) != LIST_LEN(list1))
        return false;
    for (int i = 0; i < LIST_LEN(list0); i++) {
        Token *t0 = LIST_REF(list0, i);
        Token *t1 = LIST_REF(list1, i);
        if (t0->toktype != t1->toktype)
            return false;
        switch (t0->toktype) {
        case TOKTYPE_IDENT:
        case TOKTYPE_CPPNUM:
        case TOKTYPE_STRING:
            if (!string_equal(t0->val.str, t1->val.str))
                return false;
            break;
        case TOKTYPE_KEYWORD:
        case TOKTYPE_CHAR:
        case TOKTYPE_INT:
        case TOKTYPE_PUNCT:
            if (t0->val.i != t1->val.i)
                return false;
            break;
        case TOKTYPE_FLOAT:
            if (t0->val.f != t1->val.f)
                return false;
            break;
        default:
            panic("invalid token: '%d'", t0->toktype);
        }
    }
    return true;
}

static char *token_list_to_string(List *list) {
    String *b = make_string();
    for (int i = 0; i < LIST_LEN(list); i++) {
        if (STRING_LEN(b))
            o1(b, ' ');
        Token *t = LIST_REF(list, i);
        string_printf(b, "%s", token_to_string(t));
    }
    return STRING_BODY(b);
}

static void test(char *expect, char *input) {
    List *list0 = parse_string(expect);
    List *list1 = parse_string(input);
    if (!list_equal(list0, list1))
        error("'%s' expected, but got '%s'\n", expect, token_list_to_string(list1));
}

/*
 * #define
 */
TEST(cpp_define) {
    test("\"%d\" 3",
         "#define MSG \"%d\"\n"
         " # define NUM  3 \n"
         " MSG NUM");

    test("3",
         "#define FOO(x) x\n"
         "FOO(3)");

    test("3",
         "#define FOO() 3\n"
         "FOO()");

    test("3",
         "#define FOO(x) 3\n"
         "FOO()");

    test("3",
         "#define FOO(x) 3\n"
         "FOO(bar)");

    test("int x=5; (x);",
         "#define FOO (x)\n"
         "int x=5; FOO;");

    test("3, 4",
         "#define FOO(x, y) x, y\n"
         "FOO(3, 4)");
}

/*
 * Recursively expanded macros.
 */
TEST(cpp_recursive_macro) {
    test("\"ok\"",
         "#define FOO() \"ok\"\n"
         "#define BAR FOO()\n"
         "BAR");

    test("\"ok\"",
         "#define FOO() \"ok\"\n"
         "#define BAR() FOO()\n"
         "BAR()");

    // Examples in 6.10.3.5
    test("\"vers2\"",
         "#define str(s) # s\n"
         "#define xstr(s) str(s)\n"
         "#define INCFILE(n) vers ## n\n"
         "xstr(INCFILE(2))");

    test("int foo=1; foo, 3",
         "int foo = 1;\n"
         "#define foo foo, 3\n"
         "foo");

    test("\"hello\", \"hello\" \", world\"",
         "#define glue(a, b) a ## b\n"
         "#define xglue(a, b) glue(a, b)\n"
         "#define HIGHLOW \"hello\"\n"
         "#define LOW LOW \", world\"\n"
         "glue(HIGH, LOW), xglue(HIGH, LOW)");

    test("123 45 67 89 10 11 12",
         "#define t(x,y,z) x ## y ## z\n"
         "t(1,2,3) t(,4,5) t(6,,7) t(8,9,) t(10,,) t(,11,) t(,,12) t(,,)");

    test("(1-1)",
         "#define OBJ_LIKE (1-1)\n"
         "OBJ_LIKE");

    test("(1-1)",
         "#define OBJ_LIKE /* white space */ (1-1) /* other */\n"
         "OBJ_LIKE");

    test("(7)",
         "#define FUNC_LIKE(a) ( a )\n"
         "FUNC_LIKE(7)");

    test("(7)",
         "#define FUNC_LIKE( a )( /* note the white space */ \\\n"
         "a /* other stuff on this line\n"
         "*/ )\n"
         "FUNC_LIKE(7)");

    // More tests
    test("int A=1; 1+A",
         "int A=1;\n"
         "#define A 1+A\n"
         "#define B(x) x\n"
         "B(A)");
}

/*
 * Variable argument list.
 */
TEST(cpp_va_args) {
    test("foo, bar, baz",
         "#define p(...) foo, __VA_ARGS__\n"
         "p(bar, baz)");
}

/*
 * # operator
 */
TEST(cpp_sharp) {
    test("\"abc\"",
         "#define STR(x) #x\n"
         "STR(abc)");

    test("1, \"abc\"",
         "#define STR(x, y) x, #y\n"
         "STR(1, abc)");

    test("\"123\"",
         "#define STR(x) #x\n"
         "STR(123)");

    test("\"123 abc\"",
         "#define STR(x) #x\n"
         "STR(123 abc)");

    test("\"'0' '\\\\xfe'\"",
         "#define STR(x) #x\n"
         "STR('0' '\\xfe')");

    test("\"\\\"a\\\\\\\"b\\\"\"",
         "#define STR(x) #x\n"
         "STR(\"a\\\"b\")");

    test("\"()\"",
         "#define C(x) #x\n"
         "C(())");

    test("4.4",
         "#define CAT(x, y) x ## y\n"
         "CAT(4, .4)");
}

/*
 * ## operator
 */
TEST(cpp_twosharps) {
    test("ab",
         "#define CONC(x, y) x ## y\n"
         "CONC(a, b)");

    /*
     * A tricky function-like macro in C99 spec.
     */
    test("\"x ## y\"",
         "#define hash_hash # ## #\n"
         "#define mkstr(a) # a\n"
         "#define in_between(a) mkstr(a)\n"
         "#define join(c, d) in_between(c hash_hash d)\n"
         "join(x, y)");
}

/*
 * #undef
 */
TEST(cpp_undef) {
    test("X",
         "#define X 17\n"
         "#undef X\n"
         "X");

    // It is not an error if undef's argument is not defined
    test("X",
         "#undef X\n"
         "X");
}

/*
 * #if, #elif, #else and #endif
 */
TEST(cpp_conditional_include) {
    test("a",
         "#define X\n"
         "#if defined(X)\n"
         "a\n"
         "#endif");

    test("\"a\"",
         "#define X\n"
         "#if defined(X)\n"
         "\"a\"\n"
         "#else\n"
         "\"b\"\n"
         "#endif");

    test("b",
         "#if defined(X)\n"
         "a\n"
         "#endif\n"
         "b");

    test("'b'",
         "#if defined(X)\n"
         "'a'\n"
         "#else\n"
         "'b'\n"
         "#endif");

    test("b",
         "#if defined(X)\n"
         "# if foo\n"
         "# else\n"
         "# endif\n"
         "# if bar\n"
         "# endif\n"
         "a\n"
         "#else\n"
         "b\n"
         "#endif");

    test("b",
         "#if defined(X)\n"
         "# ifdef\n"
         "# endif\n"
         "# ifndef\n"
         "# endif\n"
         "a\n"
         "#else\n"
         "b\n"
         "#endif");

    test("b",
         "#ifdef X\n"
         "      # ifdef\n"
         "# endif\n"
         "# ifndef\n"
         "# endif\n"
         "a\n"
         "#else\n"
         "b\n"
         "#endif");

    test("a",
         "#ifndef X\n"
         "a\n"
         "#else\n"
         "b\n"
         "#endif");

    test("\"c\"",
         "#if defined(X)\n"
         "a\n"
         "#elif defined(Y)\n"
         "b\n"
         "#else\n"
         "\"c\"\n"
         "#endif");
}

/*
 * Null directive
 */
TEST(cpp_null_directive) {
    test("X",
         "#\n"
         "X");
}

/*
 * #line
 */
TEST(cpp_line_directive) {
    test("50",
         "#line 50\n"
         "__LINE__");

    test("50 \"foo\"",
         "#line 50 \"foo\"\n"
         "__LINE__ __FILE__");
}

/*
 * Predefined macros
 */
TEST(cpp_predefined_macros) {
    test("1", "__8CC__");
    // test("$(date '+%b %d %Y')", "__DATE__");
    test("\"-\"", "__FILE__");
    test("1", "__LINE__");
    test("1", "__STDC__");
    test("1", "__STDC_HOSTED__");
    test("199901", "__STDC_VERSION__");
}

/*
 * Bigraphs
 */
TEST(cpp_bigraph) {
    test("[ ] { } # ##;",
         "<: :> <% %> %: %:%:;");
}

/*
 * #include
 */
TEST(cpp_include) {
    bool std;

    CppContext *ctx = make_test_cpp_context("<foo>");
    String *header = read_cpp_header_name(ctx, &std);
    EQ_STR("foo", STRING_BODY(header));
    EQ(std, true);

    ctx = make_test_cpp_context("\"bar\"");
    header = read_cpp_header_name(ctx, &std);
    EQ_STR("bar", STRING_BODY(header));
    EQ(std, false);
}

TEST(cpp_include_buffered) {
    bool std;

    CppContext *ctx = make_test_cpp_context("<foo>");
    peek_cpp_token(ctx);
    String *header = read_cpp_header_name(ctx, &std);
    EQ_STR("foo", STRING_BODY(header));
    EQ(std, true);

    ctx = make_test_cpp_context("\"bar\"");
    peek_cpp_token(ctx);
    header = read_cpp_header_name(ctx, &std);
    EQ_STR("bar", STRING_BODY(header));
    EQ(std, false);
}

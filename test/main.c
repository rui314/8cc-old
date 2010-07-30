/*
 * Copyright 2010 Rui Ueyama <rui314@gmail.com>.  All rights reserved.
 * This code is available under the simplified BSD license.  See LICENSE for details.
 */

#include "unittest.h"
#include <pthread.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>

List* test_funcs;

static bool run_in_memory;

void eq_str(int line, char *expected, char *got) {
    if (strcmp(expected, got))
        error("line %d: \"%s\" expected, but got \"%s\"", line, expected, got);
}

void eq_str1(int line, char *expected, char *got, char *msg) {
    if (strcmp(expected, got))
        error("line %d: \"%s\" expected, but got \"%s\"\n  %s", line, expected, got, msg);
}

void eq_char(int line, int expected, int got) {
    if (expected != got)
        error("line %d: '%c' expected, but got '%c'", line, expected, got);
}

void contains(int line, char *expected, char *got) {
    if (!strstr(got, expected))
        error("line %d: '%s' expected, but got '%s'", line, expected, got);
}

File *mkfile(char *str) {
    return make_string_file(to_string(str));
}

/*
 * Concurrency
 */

static pthread_mutex_t test_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t test_cond = PTHREAD_COND_INITIALIZER;
static int nthreads;

static void *thread_main(void* ignore) {
    for (;;) {
        pthread_mutex_lock(&test_lock);
        if (!LIST_LEN(test_funcs)) {
            nthreads--;
            if (!nthreads)
                pthread_cond_signal(&test_cond);
            pthread_mutex_unlock(&test_lock);
            return NULL;
        } else {
            char *name = (char *)list_pop(test_funcs);
            printf("  %s\n", name);
            void (*fn)(void) = list_pop(test_funcs);
            pthread_mutex_unlock(&test_lock);
            fn();
        }
    }
}

static void start_test_threads(int n) {
    for (int i = 0; i < n; i++) {
        pthread_t thread;
        pthread_create(&thread, NULL, thread_main, NULL);
    }
}

/*
 * Compile and run
 */

static void run_command(char *command, ...) {
    char *args[10];
    int i = 0;
    va_list ap;
    va_start(ap, command);
    args[i++] = command;
    char *arg;
    do {
        arg = va_arg(ap, char *);
        args[i++] = arg;
    } while (arg);

    pid_t pid;
    int status;
    if ( (pid = fork()) )
        do {
            if (waitpid(pid, &status, 0) < 0) {
                perror("waitpid failed:");
                exit(-1);
            }
        } while (!WIFEXITED(status));
    else
        execvp(command, args);
    if (WEXITSTATUS(status))
        error("'%s' failed", command);
}

static String *read_fd(int fd) {
    char buf[512];
    String *b = make_string();
    for (;;) {
        int nread = read(fd, buf, sizeof(buf));
        if (nread == 0) break;
        out(b, buf, nread);
    }
    o1(b, '\0');
    close(fd);
    return b;
}

static int wait_child(pid_t pid) {
    int status;
    do {
        if (waitpid(pid, &status, 0) < 0) {
            perror("waitpid failed:");
            exit(-1);
        }
    } while (!WIFEXITED(status));
    return WEXITSTATUS(status);
}

static String *run_command_string(char *command) {
    int pipefd[2];
    pid_t pid;
    pipe(pipefd);
    if ( (pid = fork()) ) {
        close(pipefd[1]);
        String *b = read_fd(pipefd[0]);
        if (wait_child(pid))
            error("'%s' failed", command);
        return b;
    } else {
        close(pipefd[0]);
        dup2(pipefd[1], 1);
        close(pipefd[1]);
        execlp(command, command, (char *)NULL);
        return NULL; // dummy
    }
}

static void run_fast_test(char *expected, char *input) {
    pid_t pid;
    int pipefd[2];
    pipe(pipefd);
    if ( (pid = fork()) ) {
        close(pipefd[1]);
        String *b = read_fd(pipefd[0]);
        if (wait_child(pid))
            error("'%s' failed", input);
        EQ_STR1(expected, STRING_BODY(b), input);
    } else {
        close(pipefd[0]);
        dup2(pipefd[1], 1);
        close(pipefd[1]);
        exit(run_string(input));
    }
}

static void test(char *expected, char *input) {
    if (run_in_memory) {
        run_fast_test(expected, input);
        return;
    }

    char source[] = ".tmpTEST-src-XXXXXX";
    int fd = mkstemp(source);
    FILE *file = fdopen(fd, "w");
    fwrite(input, 1, strlen(input), file);
    fclose(file);

    char object[] = ".tmpTEST-obj-XXXXXX";
    fd = mkstemp(object);
    close(fd);

    char exec[] = "./.tmpTEST-exec-XXXXXX";
    fd = mkstemp(exec);
    close(fd);

    struct stat statbuf;
    if (stat("../8cc", &statbuf) == 0)
        run_command("../8cc", source, object, (char *)NULL);
    else if (stat("./8cc", &statbuf) == 0)
        run_command("./8cc", source, object, (char *)NULL);
    else
        panic("8cc not found");
    unlink(source);
    run_command("gcc", "-o", exec, object, (char *)NULL);
    unlink(object);
    String *s = run_command_string(exec);
    unlink(exec);
    EQ_STR1(expected, STRING_BODY(s), input);
}

/*==============================================================================
 * Various tests
 */

/*
 * Basic tests
 */
TEST(basic_types) {
    test("", "main(){-1;}");
    test("Hello, world!", "main(){printf(\"Hello, world!\");}");
    test("Hello, world!", "main(){printf(\"Hello, %s\", \"world!\");}");
    test("3", "main(){int i=3; printf(\"%d\", i);}");
    test("6", "main(){int i=3; int j=0; j=i+3; printf(\"%d\", j);}");
    test("50", "main(){int i=atoi(\"50\"); int j = i; printf(\"%d\", j);}");
    test("15", "main(){int i=3; int j=i+5+7; printf(\"%d\", j);}");
    test("-5", "main(){int i=3; int j=5-i-7; printf(\"%d\", j);}");
    test("0", "main(){int i=3; int j=5-i-7; printf(\"%d\", j+5);}");
    test("0", "main(){int i=3; int j=5-i-7; printf(\"%d\", j+5);}");
    test("3.5", "main(){printf(\"%.1f\", 3.0 + 0.5);}");
    test("2.5", "main(){printf(\"%.1f\", 3.0 - 0.5);}");
    test("9.9", "main(){printf(\"%.1f\", 1.1 * 9.0);}");
    test("3.0", "main(){printf(\"%.1f\", 9.9 / 3.3);}");
}

/*
 * Operator precedences
 */
TEST(op_precedence) {
    test("10", "main(){int i=3; int j=1+i*3; printf(\"%d\", j);}");
    test("5", "main(){int i=9; int j=1+i/3+9/i; printf(\"%d\", j);}");
}

/*
 * Assignment
 */
TEST(assignment) {
    test("3", "main(){int i=3; int j=i; printf(\"%d\", j);}");
    test("5", "main(){int i=3; i+=2; printf(\"%d\", i);}");
    test("1", "main(){int i=3; i-=2; printf(\"%d\", i);}");
    test("6", "main(){int i=3; i*=2; printf(\"%d\", i);}");
    test("2", "main(){int i=6; i/=3; printf(\"%d\", i);}");
}

/*
 * Comma operator
 */
TEST(comma_op) {
    test("321", "main(){int i=3; while (i) printf(\"%d\", i), i=i-1;}");
}

/*
 * Parenthesized operator
 */
TEST(parenthesized_op) {
    test("25", "main(){int i=2; int j=(i+3)*5; printf(\"%d\", j);}");
}

/*
 * "if" statement
 */
TEST(if_op) {
    test("true", "main(){int i=1; if (i) { printf(\"true\"); } else { printf(\"false\"); }}");
    test("false", "main(){int i=0; if (i) { printf(\"true\"); } else { printf(\"false\"); }}");
    test("true", "main(){int i=1; if (i)   printf(\"true\");   else   printf(\"false\");}");
}

/*
 * "while" statement
 */
TEST(while_stmt) {
    test("54321a", "main(){int i=5; while (i) { printf(\"%d\", i); i=i-1; } printf(\"a\");}");
    test("54321a", "main(){int i=5; while (i)   printf(\"%d\", i), i=i-1;   printf(\"a\");}");
}

/*
 * "for" statement
 */
TEST(for_stmt) {
    test("321", "main(){int i=0; for (i=3;i;i=i-1) { printf(\"%d\", i); }}");
    test("321", "main(){int i=0; for (i=3;i;i=i-1)   printf(\"%d\", i);}");
    test("321", "main(){for (int i=3;i;i=i-1) printf(\"%d\", i);}");
}

/*
 * "do" statement
 */
TEST(do_stmt) {
    test("321", "main(){int i=3; do { printf(\"%d\", i); i=i-1;} while (i);}");
    test("321", "main(){int i=3; do   printf(\"%d\", i), i=i-1;  while (i);}");
}

/*
 * "==" and "!=" operators
 */
TEST(eq_and_ne) {
    test("1", "main(){int i=5; int j=5; int k=i==j; printf(\"%d\", k);}");
    test("0", "main(){int i=3; int j=5; int k=i==j; printf(\"%d\", k);}");
    test("true", "main(){int i=5; int j=5; if (i==j) { printf(\"true\"); } else { printf(\"false\"); }}");
    test("false", "main(){int i=3; int j=5; if (i==j) { printf(\"true\"); } else { printf(\"false\"); }}");
    // "!="
    test("0", "main(){int i=5; int j=5; int k=i!=j; printf(\"%d\", k);}");
    test("1", "main(){int i=3; int j=5; int k=i!=j; printf(\"%d\", k);}");
    test("false", "main(){int i=5; int j=5; if (i!=j) { printf(\"true\"); } else { printf(\"false\"); }}");
    test("true", "main(){int i=3; int j=5; if (i!=j) { printf(\"true\"); } else { printf(\"false\"); }}");
    // Flonum
    test("1", "main(){printf(\"%d\", 1.2 == 1.2);}");
    test("0", "main(){printf(\"%d\", 1.2 == 1.0);}");
    test("0", "main(){printf(\"%d\", 1.2 != 1.2);}");
    test("1", "main(){printf(\"%d\", 1.2 != 1.0);}");
}

/*
 * "!" operator
 */
TEST(not_op) {
    test("1", "main(){int i=0; printf(\"%d\", !i);}");
    test("0", "main(){int i=1; printf(\"%d\", !i);}");
    test("0", "main(){int i=0; printf(\"%d\", !!i);}");
    test("1", "main(){int i=9; printf(\"%d\", !!i);}");
}

/*
 * "~" operator
 */
TEST(bit_not) {
    test("-1 -2", "main(){printf(\"%d %d\", ~0, ~1);}");
    test("-1 -2", "main(){int i=0; int j=1; printf(\"%d %d\", ~i, ~j);}");
}

/*
 * "&" operator
 */
TEST(bit_and) {
    test("20", "main(){printf(\"%d\", 30&21);}");
    test("20", "main(){int i=30; i&=21; printf(\"%d\", i);}");
}

/*
 * "|" operator
 */
TEST(bit_or) {
    test("30", "main(){printf(\"%d\", 20|26);}");
    test("30", "main(){int i=20; i|=26; printf(\"%d\", i);}");
}

/*
 * "^" operator
 */
TEST(xor) {
    test("4", "main(){printf(\"%d\", 7^3);}");
    test("7", "main(){int i=4; i^=3; printf(\"%d\", i);}");
}

/*
 * Shift operators
 */
TEST(shift) {
    test("1", "main(){printf(\"%d\", 1<<0);}");
    test("4", "main(){printf(\"%d\", 1<<2);}");
    test("1", "main(){printf(\"%d\", 1>>0);}");
    test("1", "main(){printf(\"%d\", 4>>2);}");
    test("16", "main(){int i=4; i<<=2; printf(\"%d\", i);}");
    test("2", "main(){int i=8; i>>=2; printf(\"%d\", i);}");
}

/*
 * comparison operators
 */
TEST(comparison) {
    test("1", "main(){int i=3; int j=5; printf(\"%d\", i<j);}");
    test("0", "main(){int i=3; int j=5; printf(\"%d\", i>j);}");
    test("0", "main(){int i=3; int j=3; printf(\"%d\", i<j);}");
    test("0", "main(){int i=3; int j=3; printf(\"%d\", i>j);}");
    test("1", "main(){int i=3; int j=5; printf(\"%d\", i<=j);}");
    test("0", "main(){int i=3; int j=5; printf(\"%d\", i>=j);}");
    test("1", "main(){int i=3; int j=3; printf(\"%d\", i<=j);}");
    test("1", "main(){int i=3; int j=3; printf(\"%d\", i>=j);}");
    test("1", "main(){printf(\"%d\", 0<=1);}");
    test("0", "main(){printf(\"%d\", 1<=0);}");
    test("1", "main(){printf(\"%d\", 1>=0);}");
    test("0", "main(){printf(\"%d\", 0>=1);}");
    test("0", "main(){printf(\"%d\", 0<=-1);}");
    test("1", "main(){printf(\"%d\", -1<=0);}");
    test("0", "main(){printf(\"%d\", -1>=0);}");
    test("1", "main(){printf(\"%d\", 0>=-1);}");
    // Floating point numbers
    test("1", "main(){float i=3.0; float j=5.0; printf(\"%d\", i<j);}");
    test("0", "main(){float i=3.0; float j=5.0; printf(\"%d\", i>j);}");
    test("0", "main(){float i=3.0; float j=3.0; printf(\"%d\", i<j);}");
    test("0", "main(){float i=3.0; float j=3.0; printf(\"%d\", i>j);}");
    test("1", "main(){float i=3.0; float j=5.0; printf(\"%d\", i<=j);}");
    test("0", "main(){float i=3.0; float j=5.0; printf(\"%d\", i>=j);}");
    test("1", "main(){float i=3.0; float j=3.0; printf(\"%d\", i<=j);}");
    test("1", "main(){float i=3.0; float j=3.0; printf(\"%d\", i>=j);}");
    test("1", "main(){printf(\"%d\", 0.0<=1.0);}");
    test("0", "main(){printf(\"%d\", 1.0<=0.0);}");
    test("1", "main(){printf(\"%d\", 1.0>=0.0);}");
    test("0", "main(){printf(\"%d\", 0.0>=1.0);}");
    test("0", "main(){printf(\"%d\", 0.0<=-1.0);}");
    test("1", "main(){printf(\"%d\", -1.0<=0.0);}");
    test("0", "main(){printf(\"%d\", -1.0>=0.0);}");
    test("1", "main(){printf(\"%d\", 0.0>=-1.0);}");
}

/*
 * "?:" operator
 */
TEST(ternary) {
    test("17", "main(){int i=1; printf(\"%d\", i?17:42);}");
    test("42", "main(){int i=0; printf(\"%d\", i?17:42);}");
    test("2", "main(){int i=1; int j=i?i+1:i-1; printf(\"%d\", j);}");
    test("0", "main(){int i=1; int j=i-1?i+1:i-1; printf(\"%d\", j);}");
    test("-1", "main(){int i=0; int j=i?i+1:i-1; printf(\"%d\", j);}");
}

/*
 * && operator
 */
TEST(log_and) {
    test("ab", "main(){1 && printf(\"a\"); printf(\"b\");}");
    test("b", "main(){0 && printf(\"a\"); printf(\"b\");}");
    test("3", "main(){int i = 1 && 3; printf(\"%d\", i);}");
    test("0", "main(){int i = 5 && 3 && 0; printf(\"%d\", i);}");
    test("0", "main(){int i = 0 && 0; printf(\"%d\", i);}");
}

/*
 * || operator
 */
TEST(log_or) {
    test("b", "main(){1 || printf(\"a\"); printf(\"b\");}");
    test("ab", "main(){0 || printf(\"a\"); printf(\"b\");}");
    test("1", "main(){int i = 1 || 3; printf(\"%d\", i);}");
    test("3", "main(){int i = 0 || 3 || 5; printf(\"%d\", i);}");
    test("0", "main(){int i = 0 || 0; printf(\"%d\", i);}");
}

/*
 * "++" and "--" operators
 */
TEST(inc_and_dec) {
    test("12", "main(){int i=1; printf(\"%d\", i++);printf(\"%d\", i);}");
    test("22", "main(){int i=1; printf(\"%d\", ++i);printf(\"%d\", i);}");
    test("54", "main(){int i=5; printf(\"%d\", i--);printf(\"%d\", i);}");
    test("44", "main(){int i=5; printf(\"%d\", --i);printf(\"%d\", i);}");
}

/*
 * "break" and "continue"
 */
TEST(break_and_continue) {
    test("bar", "main(){int i=1; while (1) { if (i) { break; } printf(\"foo\"); } printf(\"bar\");}");
    test("aac", "main(){int i=2; while (i) { if (i) { printf(\"a\"); i=i-1; continue; } printf(\"b\"); } printf(\"c\");}");
    test("32a", "main(){for (int i=3;i;i=i-1) { printf(\"%d\", i); if (i==2) { break; } } printf(\"a\");}");
    test("321a", "main(){for (int i=3;i;i) { if (i) { printf(\"%d\", i); i=i-1; continue; } } printf(\"a\");}");
}

/*
 * "goto" statement
 */
TEST(goto_stmt) {
    test("acbd", "main(){A: printf(\"a\"); goto C; B: printf(\"b\"); goto D; C: printf(\"c\"); goto B; D: printf(\"d\");}");
}

/*
 * "return" statement
 */
TEST(return_stmt) {
    test("a", "main(){printf(\"a\"); return 0;}");
}

/*
 * Function call
 */
TEST(function_call) {
    test("foo", "main() { bar(); } bar() { printf(\"foo\"); }");
    test("foo", "bar() { printf(\"foo\"); } main() { bar(); }");
    test("17", "main() { printf(\"%d\", bar()); } bar() { return 17; }");
    // functions taking parameters
    test("1 2", "main() { bar(1, 2); } bar(int i, int j) { printf(\"%d %d\", i, j); }");
    test("17 42", "main() { int p[3]; p[0]=17; p[1]=42; bar(p); } bar(int *p) { printf(\"%d %d\", p[0], p[1]); }");
}

/*
 * Pointer operations
 */
TEST(pointer_op) {
    test("17", "main(){long i=17; long *j=&i; printf(\"%d\", *j);}");
    test("17", "main(){long i=17; long *j=&i; long **k=&j; printf(\"%d\", **k);}");
    test("42", "main(){long i=17; long *j=&i; *j=42; printf(\"%d\", *j);}");
    test("42", "main(){long i=17; long *j=&i; long **k=&j; **k=42; printf(\"%d\", **k);}");
}

/*
 * Array
 */
TEST(array) {
    test("17", "main(){int i[20]; printf(\"17\");}");
    test("17 42", "main(){int i[20]; i[0]=17; i[19]=42; printf(\"%d %d\", i[0], i[19]);}");
    test("17 42", "main(){int i[20]; int *p=i; p[0]=17; p[1]=42; printf(\"%d %d\", *p, p[1]);}");
    test("5", "main(){int i[20]; int *p=i; int *q=p+5; printf(\"%d\", q-p);}");
    test("123", "main(){ int a[3][3]; a[0][1]=1; a[2][0]=2; a[2][2]=3; printf(\"%d%d%d\", a[0][1], a[2][0], a[2][2]);}");
    test("012345678", "main(){int a[3][3]; for (int i=0;i<3;i++) for (int j=0;j<3;j++) a[i][j]=i*3+j; for (int i=0;i<9;i++) printf(\"%d\",*(*a+i));}");
    test("bx", "main(){printf(0 ? \"a\" : \"b\"); printf(1 ? \"x\" : \"y\");}");
}

/*
 * Aray and pointer arithmetic
 */
TEST(array_and_pointer_arith) {
    test("17 42", "main(){int i[20]; i[0]=17; i[1]=42; int *j=i; printf(\"%d \", *j); j++; printf(\"%d\", *j);}");
}

/*
 * Array and function parameter
 */
TEST(array_as_function_param) {
    test("012345678", "main() { int a[9]; for (int i=0;i<9;i++) a[i]=i; f(a); } f(int a[][3]) { for (int i=0;i<3;i++) for (int j=0;j<3;j++) printf(\"%d\", a[i][j]); }");
    test("012345678", "main() { int a[9]; for (int i=0;i<9;i++) a[i]=i; f(a); } f(int *a[3])  { for (int i=0;i<3;i++) for (int j=0;j<3;j++) printf(\"%d\", a[i][j]); }");
}

/*
 * Char type
 */
TEST(char_type) {
    test("3 257", "main(){char c=3; int i=c+254; printf(\"%d %d\", c, i);}");
    test("2", "main(){char c=255+3; printf(\"%d\", c);}");
    test("-1", "main(){char c=255; printf(\"%d\", c);}");
    test("255", "main(){unsigned char c=255; printf(\"%d\", c);}");
}

/*
 * Literal string
 */
TEST(literal_string) {
    test("Hello", "main(){char *p=\"Hello\"; printf(\"%s\", p);}");
    test("Hello world", "main(){char *p=\"Hello \" \"world\"; printf(\"%s\", p);}");
}

/*
 * Type conversion between floating point and integer
 */
TEST(float_to_int) {
    test("3.0", "main(){float f=3; printf(\"%.1f\", f);}");
    test("3", "main(){int i=3.0; printf(\"%d\", i);}");
}

/*
 * Binary, octal, and hexadecimal numbers
 */
TEST(numbers) {
    test("511", "main(){printf(\"%d\", 0777);}");
    test("255 255", "main(){printf(\"%d %d\", 0xff, 0XFF);}");
    test("7 7", "main(){printf(\"%d %d\", 0b111, 0B111);}");
}

/*==============================================================================
 * Entry point
 */

int main(int argc, char **argv) {
    printf("Running unit tests ...\n");
    if (argc == 2 && !strcmp(argv[1], "-n")) {
        run_in_memory = false;
        thread_main(NULL);
    } else {
        run_in_memory = true;
        pthread_mutex_lock(&test_lock);
        nthreads = 1;
        start_test_threads(nthreads);
        while (nthreads)
            pthread_cond_wait(&test_cond, &test_lock);
    }
    printf("ALL TESTS PASSED\n");
}

/*
 * init.c - program initializer
 *
 * Copyright 2010 Rui Ueyama <rui314@gmail.com>.  All rights reserved.
 * This code is available under the simplified BSD license.  See LICENSE for details.
 */

#include "8cc.h"
#include <signal.h>

static void sigsegv_handler(int signo) {
    struct sigaction act;
    act.sa_handler = SIG_DFL;
    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
    if (sigaction(SIGSEGV, &act, NULL)) {
        perror("sigaction failed: ");
        exit(-1);
    }
    fprintf(stderr, "\n");
    print_stack_trace_safe();
}

static void set_signal_handler(void) {
    struct sigaction act;
    act.sa_handler = sigsegv_handler;
    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
    if (sigaction(SIGSEGV, &act, NULL)) {
        perror("sigaction failed: ");
        exit(-1);
    }
}

void eightcc_init(void) {
    set_signal_handler();
}

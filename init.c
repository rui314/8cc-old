/*
 * init.c - program initializer
 *
 * Copyright 2010 Rui Ueyama <rui314@gmail.com>.  All rights reserved.
 * This code is available under the simplified BSD license.  See LICENSE for details.
 */

#include "8cc.h"
#include <signal.h>

typedef void (*sighandler)(int);

static void set_signal_handler(sighandler handler) {
    struct sigaction act;
    act.sa_handler = handler;
    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
    if (sigaction(SIGSEGV, &act, NULL)) {
        perror("sigaction failed: ");
        exit(-1);
    }
}

static void sigsegv_handler(int signo) {
    set_signal_handler(SIG_DFL);
    fprintf(stderr, "\n");
    print_stack_trace_safe();
}

void eightcc_init(void) {
    set_signal_handler(sigsegv_handler);
}

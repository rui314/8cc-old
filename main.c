/*
 * main.c - main program
 *
 * Copyright 2010 Rui Ueyama <rui314@gmail.com>.  All rights reserved.
 * This code is available under the simplified BSD license.  See LICENSE for details.
 */

#include "8cc.h"

static void usage(void) {
    fprintf(stderr,
            "Usage: 8cc [ -d ] [ -run ] <infile>\n"
            "       8cc [ -d ] <infile> <outfile>\n");
    exit(-1);
}

int main(int argc, char **argv) {
    eightcc_init();

    bool flag_cscript = false;
    char *infile = NULL;
    char *outfile = NULL;

    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "-run"))
            flag_cscript = true;
        else if (!strcmp(argv[i], "-d"))
            flag_debug = true;
        else if (argv[i][0] == '-' && argv[i][1] != '\0')
            usage();
        else if (!infile) {
            infile = argv[i];
        }
        else if (!outfile)
            outfile = argv[i];
        else
            usage();
    }

    if (!infile)
        usage();
    if (flag_cscript && outfile)
        usage();
    if (!flag_cscript && !outfile)
        usage();

    File *in = open_file(infile);
    Elf *elf = new_elf();
    List *fns = parse(in, elf);
    assemble(elf, fns);

    if (flag_cscript) {
        run_main(elf, argc - 1, argv + 1);
    } else {
        FILE *out = fopen(outfile, "w");
        write_elf(out, elf);
    }
}

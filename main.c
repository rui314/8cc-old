/*
 * main.c - main program
 *
 * Copyright 2010 Rui Ueyama <rui314@gmail.com>.  All rights reserved.
 * This code is available under the simplified BSD license.  See LICENSE for details.
 */

#include "8cc.h"

int main(int argc, char **argv) {
    bool cscript = false;
    File *infile;
    if (argc >= 2 && !strcmp(argv[1], "-run")) {
        cscript = true;
        infile = open_file(argv[2]);
    } else if (argc != 3) {
        fprintf(stderr,
                "Usage: 8cc <infile>\n"
                "       8cc <infile> <outfile>\n");
        exit(-1);
    } else {
        infile = open_file(argv[1]);
    }

    Elf *elf = new_elf();
    List *fns = parse(infile, elf);
    assemble(elf, fns);

    if (cscript) {
        run_main(elf, argc - 1, argv + 1);
    } else {
        FILE *outfile = fopen(argv[2], "w");
        write_elf(outfile, elf);
    }
}

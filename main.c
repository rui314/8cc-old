/*
 * main.c - main program
 *
 * Copyright 2010 Rui Ueyama <rui314@gmail.com>.  All rights reserved.
 * This code is available under the simplified BSD license.  See LICENSE for details.
 */

#include "8cc.h"

int main(int argc, char **argv) {
    if (argc != 3) {
        fprintf(stderr, "Usage: 8cc <infile> <outfile>\n");
        exit(-1);
    }

    File *infile = open_file(argv[1]);
    FILE *outfile = fopen(argv[2], "w");
    Elf *elf = new_elf();
    List *fns = parse(infile, elf);
    assemble(elf, fns);
    write_elf(outfile, elf);
}

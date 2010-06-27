#include "8cc.h"

int main(int argc, char **argv) {
    if (argc != 3) {
        fprintf(stderr, "Usage: 8cc <infile> <outfile>\n");
        exit(-1);
    }

    File *infile = open_file(argv[1]);
    FILE *outfile = fopen(argv[2], "w");
    Elf *elf = new_elf();

    Section *data = make_section(".data", SHT_PROGBITS);
    data->flags = SHF_ALLOC | SHF_WRITE;
    data->align = 4;
    add_section(elf, data);

    Section *text = make_section(".text", SHT_PROGBITS);
    text->flags = SHF_ALLOC | SHF_EXECINSTR;
    text->align = 16;
    add_section(elf, text);

    List *insts = parse(infile, elf);
    assemble(elf, insts);
    write_elf(outfile, elf);
}

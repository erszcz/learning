#include <elf.h>
#include <err.h>
#include <stdio.h>

typedef union {
    Elf32_Ehdr ehdr32;
    Elf64_Ehdr ehdr64;
} Elf_Ehdr;

int main(int argc, const char *argv[]) {
    FILE*       f;
    Elf_Ehdr    _ehdr;
    Elf64_Ehdr* ehdr;

    if (argc < 2) {
        errx(1, "no ELF file supplied");
    }

    f = fopen(argv[1], "rb");
    if (fread(&_ehdr, sizeof(_ehdr), 1, f) == 0) {
        err(2, "error reading file");
    }
    fclose(f);

    int8_t* e_ident = (int8_t*)_ehdr.ehdr32.e_ident;
    if (e_ident[EI_MAG0] != ELFMAG0 ||
        e_ident[EI_MAG1] != ELFMAG1 ||
        e_ident[EI_MAG2] != ELFMAG2 ||
        e_ident[EI_MAG3] != ELFMAG3) {
        errx(3, "not an ELF file");
    }

    if (e_ident[EI_CLASS] != ELFCLASS64) {
        errx(4, "not ELF64");
    }

    ehdr = (Elf64_Ehdr*)&_ehdr;
    printf("e_entry: 0x%lx\n", ehdr->e_entry);
    printf("e_entry: 0x%lx\n", (int)ehdr->e_entry);

    return 0;
}

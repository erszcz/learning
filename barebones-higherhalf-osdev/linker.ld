ENTRY(loader)
OUTPUT_FORMAT(elf32-i386)

SECTIONS {
   /* The kernel will live at 3GB + 1MB in the virtual
      address space, which will be mapped to 1MB in the
      physical address space. */
   . = 0xc0100000;

   .text : AT(ADDR(.text) - 0xc0000000) {
       *(.text)
       *(.rodata*)
   }

   .data ALIGN (0x1000) : AT(ADDR(.data) - 0xc0000000) {
       *(.data)
   }

   .bss : AT(ADDR(.bss) - 0xc0000000) {
       _sbss = .;
       *(COMMON)
       *(.bss)
       _ebss = .;
   }
}


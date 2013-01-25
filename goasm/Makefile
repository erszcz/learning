main: main.go hello.o
	gccgo hello.o main.go -o main

hello.o: hello.asm
	nasm -f elf64 -o hello.o hello.asm

clean:
	rm -rf _obj *.o *~ *.6 *.gch a.out main

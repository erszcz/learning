echo-stdin: echo-stdin.o
	gcc -o $@ $< -lev

example: example.o
	gcc -o $@ $< -lev

%.o: %.c
	gcc -c $< -lev

client: client.o
	gcc -o $@ $< -lev

server: server.o
	gcc -o $@ $< -lev -lbsd

%.o: %.c
	gcc -c $<

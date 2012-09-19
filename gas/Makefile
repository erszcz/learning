DEBUG = -g

main: asmain.o cmain.o
	gcc $(DEBUG) -o $@ $^

asmain.o: asmain.s
	as $(DEBUG) -o $@ $<

cmain.o: cmain.c
	gcc $(DEBUG) -c $<

clean:
	rm *.o main

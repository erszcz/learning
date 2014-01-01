CFLAGS := -std=c99

%: %.c

test_mergesort: mergesort.c
	$(CC) $(CFLAGS) -DTEST -o $@ $^

test: test_mergesort
	./test_mergesort

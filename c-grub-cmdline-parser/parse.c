#include <assert.h>
#include <stdio.h>
#include <bsd/string.h>
#include <string.h>

/*#include <sys/libkern.h>*/

#define SIZE 4096

/*extern*/ char multiboot_cmdline[SIZE];

/* stub for ksetenv */
static void ksetenv(char* key, char* val);

static void
multiboot_setup_kenv(void)
{
	char *key, *val;
	char *p = multiboot_cmdline;

	/* Did the bootloader pass a command line? */
	if (*p == '\0')
		return;

	/* For each key=val pair in the command line set a kenv with
	 * the same key and val. */
	while ((key = strsep(&p, " ")) != NULL) {
		/* Skip extra spaces. */
		if (*key == '\0')
			continue;
		val = key;
        strsep(&val, "=");

        // Was:
        //      ksetenv(key, val);
        // This leads to setting kenv "key" to value NULL which is not what
        // we want.

        // Should be:
		if (val != NULL)
            ksetenv(key, val);
        // This doesn't set the kenv for malformed command line entry.
	}
}

typedef void (*testcase_f)();

static char* gkey;
static char* gval;

static void ksetenv(char* key, char* val) {
    gkey = key;
    gval = val;
    printf("key: %s, val: %s\n", key, val);
}

static void init_per_testcase() {
    gkey = NULL;
    gval = NULL;
}

static void end_per_testcase() {
    memset(multiboot_cmdline, 0, SIZE);
}

void test1() {
    strlcpy(multiboot_cmdline, "k1=v1", SIZE);
    multiboot_setup_kenv();
    assert(!strncmp(gkey, "k1", 2));
    assert(!strncmp(gval, "v1", 2));
}

void test2() {
    strlcpy(multiboot_cmdline, "k1=", SIZE);
    multiboot_setup_kenv();
    assert(!strncmp(gkey, "k1", 2));
    assert(!strncmp(gval, "", 2));
}

void test3() {
    strlcpy(multiboot_cmdline, "k1", SIZE);
    multiboot_setup_kenv();
    assert(!gkey);
    assert(!gval);
}

testcase_f tests[] = {
    /*test1,*/
    /*test2,*/
    test3
};

main()
{
    int i;
    for (i = 0; i < sizeof(tests) / sizeof(tests[0]); i++) {
        if (!tests[i]) continue;
        init_per_testcase();
        tests[i]();
        end_per_testcase();
    }
}

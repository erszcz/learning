#ifndef SLIMTESTS_H
#define SLIMTESTS_H

typedef struct test_spec_struct {
    char*       name;
    bool        (*test)();
} test_spec;

#define TEST_SPEC(name) { #name, name }

static int
run_tests(test_spec* tests, size_t ntests) {
    int i;
    int failed = 0;
    for (i = 0; i < ntests; ++i) {
        if ( !(tests[i].test)() ) {
            failed += 1;
            fprintf(stderr, "failed \"%s\"\n", tests[i].name);
        } else
            fprintf(stderr, "ok \"%s\"\n", tests[i].name);
    }
    if (failed == 0) {
        fprintf(stderr, "\nAll %ld tests ok!\n", ntests);
        return 0;
    } else {
        fprintf(stderr, "\nFailed %d of %ld tests.\n", failed, ntests);
        return 1;
    }
}

#endif // SLIMTESTS_H

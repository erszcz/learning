#include <stdbool.h>
#include <stdio.h>

#include "piu.h"

// From piu.c
bool is_subset(int*, size_t, int*, size_t);
bool does_user_match(user*, user*);
bool does_piu_match(piu*, piu*);

#define size(arr) sizeof(arr) / sizeof(arr[0])

bool sanity_check() { return true; }

int john_following[] = {2, 4, 6, 8};
int john_blocked[]   = {3, 5, 7};
user john = {
    .user_id        = 1,
    .when_created   = 1417651200,
    .last_activity  = 1417726806,
    .name           = "John Noobson",
    .URL_of_avatar  = "http://myavatar.com/pic.jpg",
    .num_following  = size(john_following),
    .ids_following  = john_following,
    .num_blocked    = size(john_blocked),
    .ids_blocked    = john_blocked
};

int jane_following[] = {1};
int jane_blocked[] = {1};
user jane = {
    .user_id        = 2,
    .when_created   = 1417641200,
    .last_activity  = 1417716806,
    .name           = "Jane Goodall",
    .URL_of_avatar  = "http://donkeykong.com/pic.jpg",
    .num_following  = size(jane_following),
    .ids_following  = jane_following,
    .num_blocked    = size(jane_blocked),
    .ids_blocked    = jane_blocked
};

int pattern_following[] = {2, 4, 6};
int pattern_blocked[]   = {3, 5};
user user_pattern = {
    .user_id        = 0,
    .when_created   = 0,
    .last_activity  = 0,
    .name           = NULL,
    .URL_of_avatar  = NULL,
    .num_following  = size(pattern_following),
    .ids_following  = pattern_following,
    .num_blocked    = size(pattern_blocked),
    .ids_blocked    = pattern_blocked
};

piu piu_pattern = {
    .piu_id = 0,
    .piu_id_of_repiu = 0,
    .user_id_of_repiu = 0,
    .user_id_of_poster = 0,
    .poster = 0,
    .piu_text_utf8 = {0},
    .piu_length = 0,
    .visible_only_to_followers = 0
};

bool subset() {
    int as[] = {2,3,6};
    int bs[] = {1,2,3,4,5,6};
    return is_subset(as, size(as), bs, size(bs));
}

bool not_subset() {
    int as[] = {2,3,6,7,8};
    int bs[] = {1,2,3,4,5,6};
    return !is_subset(as, size(as), bs, size(bs));
}

bool substring1() {
}

// - All ids_following and ids_blocked in the user pattern are followed/blocked
//   by the input Piu’s user

bool user_matches_wildcard_pattern() {
    return does_user_match(&john, &user_pattern);
}

bool user_does_not_match_wildcard_pattern() {
    return !does_user_match(&jane, &user_pattern);
}

bool piu_matches_request_not_followed_not_blocked() {
    return does_piu_match(NULL, NULL);
}

typedef struct test_spec_struct {
    char*       name;
    bool        (*test)();
} test_spec;

#define TEST_SPEC(name) { #name, name }

test_spec tests[] = {
    TEST_SPEC(sanity_check),
    TEST_SPEC(subset),
    TEST_SPEC(not_subset),
    TEST_SPEC(user_matches_wildcard_pattern),
    TEST_SPEC(user_does_not_match_wildcard_pattern),
    TEST_SPEC(piu_matches_request_not_followed_not_blocked)
};

int run_tests(test_spec* tests, size_t ntests) {
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
        fprintf(stderr, "\nAll ok!\n");
        return 0;
    } else {
        fprintf(stderr, "\nFailed %d of %ld tests.\n", failed, ntests);
        return 1;
    }
}

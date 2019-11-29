#include <cassert>
#include <cstdbool>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include "slimtests.h"

//static ERL_NIF_TERM
//from_binary_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
//{
//    if(argc != 1) {
//        return enif_make_badarg(env);
//    }

//    ErlNifBinary bin;
//    if (!enif_inspect_binary(env, argv[0], &bin)) {
//        return enif_make_badarg(env);
//    }

//    const unsigned size = bin.size;
//    unsigned commercial_at = -1;
//    unsigned slash = size;
//    for (unsigned i = 0; i < size ; ++i) {
//        switch(bin.data[i]) {
//            case '/':
//                if (slash == size) {
//                    slash = i;
//                    goto end_loop;
//                }
//                break;
//            case '@':
//                if (commercial_at == -1) {
//                    commercial_at = i;
//                } else
//                    return mk_error(env);
//                break;
//        }
//    }
//end_loop:
//    if (commercial_at == 0 || slash == 0) {
//        return mk_error(env);
//    }

//    unsigned host_size = slash - commercial_at - 1;
//    if (host_size == 0) return mk_error(env);
//    ERL_NIF_TERM host;
//    unsigned char *host_data = enif_make_new_binary(env, host_size, &host);
//    std::memcpy(host_data, &(bin.data[commercial_at+1]), host_size);

//    ERL_NIF_TERM resource;
//    unsigned res_size = slash >= size-1 ? 0 : size-1-slash;
//    unsigned char *res_data = enif_make_new_binary(env, res_size, &resource);
//    std::memcpy(res_data, &(bin.data[slash + 1]), res_size);

//    ERL_NIF_TERM user;
//    unsigned user_size = commercial_at == -1 ? 0 : commercial_at;
//    unsigned char *user_data = enif_make_new_binary(env, user_size, &user);
//    std::memcpy(user_data, &(bin.data[0]), user_size);

//    return enif_make_tuple3(
//            env,
//            user,
//            host,
//            resource);
//}

#define JID_T_LEN 256
typedef struct {
    char        data[JID_T_LEN];
    const char* user;
    const char* server;
    const char* res;
} jid_t;

int mk_error() {
    abort();
    return 1;
}

int binary_to_jid_v1(const char* data_in, jid_t* jid)
{
    bzero(jid->data, JID_T_LEN);
    memcpy(jid->data, data_in, strnlen(data_in, JID_T_LEN-1));
    char* data = jid->data;
    const unsigned size = strlen(data);
    unsigned commercial_at = -1;
    unsigned slash = size;
    for (unsigned i = 0; i < size ; ++i) {
        switch(data[i]) {
            case '/':
                if (slash == size) {
                    slash = i;
                    goto end_loop;
                }
                break;
            case '@':
                if (commercial_at == -1) {
                    commercial_at = i;
                } else
                    return mk_error();
                break;
        }
    }
end_loop:
    if (commercial_at == 0 || slash == 0) {
        return mk_error();
    }
    data[commercial_at] = 0;
    data[slash] = 0;
    jid->user = data;
    jid->server = data+commercial_at+1;
    jid->res = data+slash+1;
    return 0;
}

int binary_to_jid_v2(const char* data_in, jid_t* jid)
{
    bzero(jid->data, JID_T_LEN);
    memcpy(jid->data, data_in, strnlen(data_in, JID_T_LEN-1));
    char* data = jid->data;
    jid->user = data;
    jid->server = strpbrk(data, "@/");
    data[jid->server - data] = 0;
    jid->server += 1;
    jid->res = strpbrk(jid->server, "@/");
    data[jid->res - data] = 0;
    jid->res += 1;
    return 0;
}

typedef int (*binary_to_jid_t)(const char*, jid_t*);

binary_to_jid_t binary_to_jid;

const char* jid1 = "alice@example.com/res1";
const char* jid2 = "example.com/res1";
const char* jid3 = "example.com";
const char* jid4 = "example.com/heres-the-@-sign";

bool parses_user_server_resource() {
    jid_t jid;
    binary_to_jid(jid1, &jid);
    printf(".user   = %s\n", jid.user);
    printf(".server = %s\n", jid.server);
    printf(".res    = %s\n", jid.res);
    assert(0 == strcmp("alice", jid.user));
    assert(0 == strcmp("example.com", jid.server));
    assert(0 == strcmp("res1", jid.res));
    return true;
}

test_spec tests[] = {
    TEST_SPEC(parses_user_server_resource)
};

#define size(arr) (sizeof(arr) / sizeof(arr[0]))

int main() {

    fprintf(stderr, "Testing custom loop\n");
    binary_to_jid = binary_to_jid_v1;
    run_tests(tests, size(tests));

    fprintf(stderr, "\n");
    fprintf(stderr, "Testing strpbrk()\n");
    binary_to_jid = binary_to_jid_v2;
    run_tests(tests, size(tests));
}

#include <cstdlib>
#include <cassert>
#include <cstring>
#include <cstdio>

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

int mk_error() {
    abort();
    return 1;
}

int v1()
{
    char* data = "alice@localhost/res1";
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
    char* user = data;
    char* server = data+commercial_at+1;
    char* resource = data+slash+1;
    printf("%s\n", user);
    printf("%s\n", server);
    printf("%s\n", resource);
    return 0;
}

int v2()
{
    char* data = "alice@localhost/res1";
    char* user = data;
    char* server = strpbrk(data, "@/");
    //assert(server[0] == '@');
    char* resource = strpbrk(server+1, "@/");
    //assert(resource[0] == '/');
    printf("%s\n", user);
    printf("%s\n", server);
    printf("%s\n", resource);
    return 0;
}

int main() {
    v2();
}

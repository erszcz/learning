#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUFSIZE 4096
#define UNIX_PATH_MAX 108

#define handle_error(msg) \
    do { perror(msg); exit(EXIT_FAILURE); } while (0)

static int
do_connect(const char* host, const char* serv);

static void
cli_loop(int sfd);

int
main(int argc, const char *argv[])
{
    int sfd;

    if (argc < 3) {
        fprintf(stderr, "usage: %s host port\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    sfd = do_connect(argv[1], argv[2]);

    cli_loop(sfd);

    return 0;
}

static int
do_connect(const char* host, const char* serv)
{
    struct addrinfo hints;
    struct addrinfo *result, *rp;
    int sfd, e;

    bzero(&hints, sizeof hints);
    hints.ai_family   = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_protocol = 0;

    e = getaddrinfo(host, serv, &hints, &result);
    if (e != 0) {
        fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(e));
        exit(EXIT_FAILURE);
    }

    for (rp = result; rp != NULL; rp = rp->ai_next) {
        sfd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
        if (sfd == -1)
            continue;

        if (connect(sfd, rp->ai_addr, rp->ai_addrlen) != -1)
            /* Success! */
            break;

        close(sfd);
    }

    if (rp == NULL) {
        fprintf(stderr, "could not connect\n");
        exit(EXIT_FAILURE);
    }

    freeaddrinfo(result);

    return sfd;
}

static void
cli_loop(int sfd)
{
    char* r;
    ssize_t nread;
    char buf[BUFSIZE];
    printf("send> ");
    while ((r = fgets(buf, BUFSIZE, stdin)) != NULL) {
        send(sfd, buf, strlen(buf), 0);
        nread = recv(sfd, buf, BUFSIZE, 0);
        if (nread > 0) {
            buf[nread] = '\0';
            printf("recv: %s\n", buf);
            printf("send> ");
        } else if (nread == -1)
            handle_error("recv");
        else {
            fprintf(stdin, "connection closed\n");
            exit(EXIT_FAILURE);
        }
    }
    if (r == NULL) {
        fprintf(stderr, "\nclosing connection\n");
        shutdown(sfd, SHUT_RDWR);
    }
}

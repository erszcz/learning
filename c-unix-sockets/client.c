#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdio.h>
#include <stdlib.h>

#define BUFSIZE 4096
#define UNIX_PATH_MAX 108

#define handle_error(msg) \
    do { perror(msg); exit(EXIT_FAILURE); } while (0)

#define min(res, a, b) \
    do { int __a = (a); \
         int __b = (b); \
         res = __a < __b ? __a : __b; } while (0)

void cli_loop(int sfd);

int main(int argc, const char *argv[])
{
    int sfd;
    int n;

    if (argc < 2) {
        fprintf(stderr, "Usage: %s unix_socket\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    struct sockaddr_un sun;
    bzero(&sun, sizeof(struct sockaddr_un));
    sun.sun_family = AF_UNIX;
    min(n, sizeof(sun.sun_path) - 1, strlen(argv[1]));
    strncpy(sun.sun_path, argv[1], n);

    printf("%d\n", sfd);
    if ((sfd = socket(AF_UNIX, SOCK_STREAM, 0)) == -1)
        handle_error("socket");
    printf("%d\n", sfd);

    if (connect(sfd, (struct sockaddr*) &sun, sizeof(struct sockaddr_un)) == -1)
        handle_error("connect");
    printf("%d\n", sfd);

    cli_loop(sfd);

    return 0;
}

void
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

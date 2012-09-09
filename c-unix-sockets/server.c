#include <ev.h>
#include <bsd/string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#define MY_SOCK_PATH "/tmp/thesocket"
#define LISTEN_BACKLOG 50

#define handle_error(msg) \
    do { perror(msg); exit(EXIT_FAILURE); } while (0)

#define BUFSIZE 4096

static int
bind_and_listen (const char *path);

ev_io listen_watcher;

static void
listen_cb (EV_P_ ev_io *w, int revents);

static void
client_cb (EV_P_ ev_io *w, int revents);

int
main (int argc, char *argv[])
{
    int sfd;
    struct ev_loop *loop = EV_DEFAULT;

    if (access(MY_SOCK_PATH, F_OK) == 0)
        unlink(MY_SOCK_PATH);

    sfd = bind_and_listen(MY_SOCK_PATH);

    /* set sfd to non-blocking mode before using with libev */

    /* how to store sfd in the watcher? */
    ev_io_init (&listen_watcher, listen_cb, sfd, EV_READ);
    ev_io_start (loop, &listen_watcher);

    /* Now we can accept incoming connections one
       at a time using accept(2) */


    /* When no longer required, the socket pathname, MY_SOCK_PATH
       should be deleted using unlink(2) or remove(3) */
    unlink(MY_SOCK_PATH);
}

static inline min(int a, int b) { return a < b ? a : b; }

static int
bind_and_listen(const char *path)
{
    int sfd;
    struct sockaddr_un addr;

    sfd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (sfd == -1)
        handle_error("socket");

    bzero(&addr, sizeof addr);
    addr.sun_family = AF_UNIX;
    strlcpy(addr.sun_path, path, sizeof addr.sun_path);
    if (bind(sfd, (struct sockaddr*) &addr, sizeof addr) == -1)
        handle_error("bind");

    if (listen(sfd, LISTEN_BACKLOG) == -1)
        handle_error("listen");

    return sfd;
}

static void
listen_cb (EV_P_ ev_io *w, int revents)
{
    for(;;) {
        peer_addr_size = sizeof(struct sockaddr_un);
        cfd = accept(sfd, (struct sockaddr *) &peer_addr,
                     &peer_addr_size);
        if (cfd == -1)
            handle_error("accept");

        /* Code to deal with incoming connection(s)... */
        pid_t pid = fork();
        if (pid == 0)
            handle_connection(cfd);
        else if (pid == -1)
            handle_error("fork");
    }
}

void
handle_connection(int sfd)
{
    char buf[BUFSIZE];
    int result;
    while ((result = recv(sfd, buf, BUFSIZE, 0)) > 0) {
        send(sfd, buf, result, 0);
    }
    if (result == -1)
        handle_error("recv");
}

#include <errno.h>
#include <ev.h>
#include <fcntl.h>
#include <bsd/string.h>
#include <poll.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#define MY_SOCK_PATH "/tmp/thesocket"
#define LISTEN_BACKLOG 50

#define handle_error(msg) \
    do { fprintf(stderr, "%s:%d: ", __FILE__, __LINE__); \
         perror(msg); exit(EXIT_FAILURE); } while (0)

#define BUFSIZE 256

static int
bind_and_listen (const char *path);

static void
set_nonblock(int fd);

static ev_io listen_watcher;

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

    //printf("main: sfd = %d\n", sfd);

    /* Set sfd to non-blocking mode before using with libev */
    set_nonblock(sfd);

    ev_io_init (&listen_watcher, listen_cb, sfd, EV_READ);
    ev_io_start (EV_A_ &listen_watcher);

    /* Start the event loop */
    ev_run (EV_A_ 0);

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
set_nonblock(int fd)
{
    long flags;
    if ((flags = fcntl(fd, F_GETFL) == -1))
        handle_error("fcntl");
    flags |= O_NONBLOCK;
    if (fcntl(fd, F_SETFL, flags) == -1)
        handle_error("fcntl");
}

static void
listen_cb (EV_P_ ev_io *w, int revents)
{
    int cfd;
    int r;
    struct pollfd pfd;
    struct sockaddr_un peer_addr;
    socklen_t peer_addr_size;
    ev_io* client_watcher;

    //printf("listen_cb: w->fd = %d\n", w->fd);

    /* Without this block the accept call often fails with EINVAL */
    bzero(&pfd, sizeof pfd);
    pfd.fd = w->fd;
    pfd.events |= POLLIN;
    r = poll(&pfd, 1, /* timeout in ms */ 10);
    if (r == -1)
        handle_error("poll");
    else if (r == 0) {
        /* no events to read */
        fprintf(stderr, "poll: no events to read\n");
        return;
    }

    cfd = accept4(w->fd, (struct sockaddr*) &peer_addr,
                  &peer_addr_size, SOCK_NONBLOCK);
    //printf("cfd=%d\terrno=%d\n", cfd, errno);
    if (cfd == -1)
        handle_error("accept");

    client_watcher = (ev_io*) malloc (sizeof(ev_io));
    if (! client_watcher)
        handle_error("malloc");

    ev_io_init (client_watcher, client_cb, cfd, EV_READ);
    ev_io_start (EV_A_ client_watcher);

    fprintf(stderr, "accepted connection\n");
}

static void
client_cb (EV_P_ ev_io *w, int revents)
{
    char buf[BUFSIZE];
    char* p;
    ssize_t nread;
    p = buf;
    while ((nread = recv(w->fd, p, buf+BUFSIZE-p, 0)) > 0)
        p += nread;
    if (nread == 0) {
        /* Orderly shutdown of the other endpoint */
        close(w->fd);
        ev_io_stop(EV_A_ w);
        /* Not that nice, should first check ev_is_active and ev_is_pending
         * on the watcher. Maybe tomorrow ;) */
        free(w);
        fprintf(stderr, "closed connection\n");
    }
    if (nread == -1 && errno != EAGAIN)
        /* Non-blocking mode so -1 doesn't have to be an error */
        handle_error("recv");
    if (p != buf)
        /* We actually received something */
        send(w->fd, buf, p-buf, 0);
}

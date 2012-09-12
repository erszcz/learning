#include <errno.h>
#include <ev.h>
#include <fcntl.h>
#include <bsd/string.h>
#include <poll.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#define LISTEN_BACKLOG 50

#define ERROR_PREFIX "%s:%d: "
#define handle_error(msg) \
    do { fprintf(stderr, ERROR_PREFIX, __FILE__, __LINE__); \
         perror(msg); exit(EXIT_FAILURE); } while (0)

#define progname(argv0) (argv0[0] == '.' ? argv0+2 : argv0)

#define BUFSIZE 256

static int
bind_and_listen (const char* service);

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

    if (argc < 2) {
        fprintf(stderr, "usage: %s port\n", progname(argv[0]));
        exit(EXIT_FAILURE);
    }

    sfd = bind_and_listen(argv[1]);

    //printf("main: sfd = %d\n", sfd);

    /* Set sfd to non-blocking mode before using with libev */
    set_nonblock(sfd);

    ev_io_init (&listen_watcher, listen_cb, sfd, EV_READ);
    ev_io_start (EV_A_ &listen_watcher);

    /* Start the event loop */
    ev_run (EV_A_ 0);

    return EXIT_SUCCESS;
}

static inline min(int a, int b) { return a < b ? a : b; }

static int
bind_and_listen(const char* service)
{
    int sfd, e;
    struct addrinfo hints;
    struct addrinfo *result, *rp;
    char host[BUFSIZE], serv[BUFSIZE];

    bzero(&hints, sizeof(hints));
    hints.ai_family   = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags    = AI_PASSIVE;
    hints.ai_protocol = 0;

    e = getaddrinfo(NULL, service, &hints, &result);
    if (e != 0) {
        fprintf(stderr, ERROR_PREFIX "getaddrinfo: %s\n", __FILE__,
                __LINE__, gai_strerror(e));
        exit(EXIT_FAILURE);
    }

    for (rp = result; rp != NULL; rp = rp->ai_next) {
        sfd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
        if (sfd == -1)
            continue;

        if (bind(sfd, rp->ai_addr, rp->ai_addrlen) == 0)
            /* Success! */
            break;

        close(sfd);
    }

    if (rp == NULL) {
        fprintf(stderr, ERROR_PREFIX "could not bind\n", __FILE__, __LINE__);
        exit(EXIT_FAILURE);
    }

    e = getnameinfo(rp->ai_addr, rp->ai_addrlen,
                    host, BUFSIZE, serv, BUFSIZE, NI_NOFQDN);
    if (e != 0) {
        fprintf(stderr, ERROR_PREFIX "getnameinfo: %s\n", __FILE__,
                __LINE__, gai_strerror(e));
        exit(EXIT_FAILURE);
    }
    fprintf(stderr, "bound on %s port %s\n", host, serv);

    freeaddrinfo(result);

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
    struct sockaddr_storage peer_addr;
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

#include <ev.h>
#include <stdio.h>
#include <unistd.h>

#define BUFSIZE 1024

static char buf[BUFSIZE];
ev_io stdin_watcher;

// all watcher callbacks have a similar signature
// this callback is called when data is readable on stdin
static void
stdin_cb (EV_P_ ev_io *w, int revents)
{
    if (fgets(buf, BUFSIZE, stdin))
    {
        if (strncmp("quit\n", buf, 4) == 0)
        {
            ev_io_stop(EV_A_ w);
            ev_break(EV_A_ EVBREAK_ALL);
        }
        fputs(buf, stdout);
    }
}

int
main (void)
{
    struct ev_loop *loop = EV_DEFAULT;

    ev_io_init (&stdin_watcher, stdin_cb, STDIN_FILENO, EV_READ);
    ev_io_start (loop, &stdin_watcher);

    // now wait for events to arrive
    ev_run (loop, 0);

    // break was called, so exit
    return 0;
}

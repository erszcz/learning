#!/usr/bin/env python
# encoding: utf-8

from multiprocessing import Manager, Pool
import logging
import os

"""
This modules shows how to get around the limitation of multiprocessing
not allowing methods to be called in other processes.

The idea is to call a wrapping function and pass `self` explicitly.
The function will in turn call the method.

Keep in mind that `self` can't contain references to pool or queue.
The pool must be module global (to be visible from a method and outside
the class) while the queue passed in as an argument.
"""

log = logging.getLogger("queue")

class Multiplier(object):

    def multiply_all(self, args):
        for arg in args:
            pool.apply_async(mp_multiply, (self, arg, queue))

    def multiply(self, arg, queue):
        queue.put(arg * 3)

def mp_multiply(self, *args):
    Multiplier.multiply(self, *args)

def main():
    global man, queue, pool

    man = Manager()
    queue = man.Queue()
    pool = Pool()

    #logging.basicConfig(level=logging.INFO, filename="queue.log")
    logging.basicConfig(level=logging.INFO)
    log.info("started, parent: %s" % os.getpid())

    m = Multiplier()

    m.multiply_all([1,2,3])
    for i in xrange(3):
        log.info(queue.get())

    pool.close()
    pool.join()

if __name__ == '__main__':
    main()

#!/usr/bin/env python
# encoding: utf-8

from multiprocessing import Manager, Pool
import logging
import os

"""
This is a simple application of multiprocessing to calling methods
in the context of other processes.

Alas, due to limitations of multiprocessing this approach doesn't work.
"""

log = logging.getLogger("queue")

class Multiplier(object):

    q = None
    p = None

    def multiply_all(self, args):
        for arg in args:
            p = self.p
            self.p = None
            p.apply_async(mp_multiply, (self, arg,))
            self.p = p

    def multiply(self, arg):
        self.q.put(arg * 3)

def mp_multiply(self, *args):
    Multiplier.multiply(self, *args)

def init(q):
    log.info("[%s] init" % os.getpid())
    queue = q

def multiply(a):
    log.info("[%s] multiply(%s)" % (os.getpid(), a))
    queue.put(a * 3)

if __name__ == '__main__':
    #logging.basicConfig(level=logging.INFO, filename="queue.log")
    logging.basicConfig(level=logging.INFO)
    log.info("started, parent: %s" % os.getpid())

    man = Manager()
    queue = man.Queue()
    pool = Pool()

    m = Multiplier()
    m.q = queue
    m.p = pool

    m.multiply_all([1,2,3])
    for i in xrange(3):
        log.info(queue.get())
    pool.close()
    pool.join()

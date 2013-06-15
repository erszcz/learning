#!/usr/bin/env python
# encoding: utf-8

from multiprocessing import Manager, Pool
import logging
import os

"""
This module shows that logging is indeed multiprocessing-aware.
All logging output will end up in 'queue.log' regardless
of the process it comes from.
"""

log = logging.getLogger("queue")

queue = None

def init(q):
    log.info("[%s] init" % os.getpid())
    queue = q

def multiply(a):
    log.info("[%s] multiply(%s)" % (os.getpid(), a))
    queue.put(a*3)

if __name__ == '__main__':
    #logging.basicConfig(level=logging.INFO, filename="queue.log")
    logging.basicConfig(level=logging.INFO)
    log.info("started, parent: %s" % os.getpid())

    man = Manager()
    queue = man.Queue()

    pool = Pool(initializer=init, initargs=(queue,))
    pool.apply_async(multiply, (3,))
    pool.apply_async(multiply, (2,))
    pool.apply_async(multiply, (1,))
    for i in xrange(3):
        log.info(queue.get())
    pool.close()
    pool.join()

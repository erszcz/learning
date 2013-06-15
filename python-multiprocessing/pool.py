#!/usr/bin/env python
# encoding: utf-8

from multiprocessing import Pool
import logging
import os

"""
This module shows that logging might be used with multiprocessing,
though no process-shared locks are used so output from different
processes might get intermixed.

All logging output will end up in 'pool.log' regardless
of the process it comes from.
"""

log = logging.getLogger("pool")

def s(a):
    for i in xrange(5):
        log.info("proc %d: iteration: %d, arg: %s" % (os.getpid(), i, a))

if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO, filename="pool.log")
    log.info("started, parent: %s" % os.getpid())
    pool = Pool(processes=2)
    pool.map_async(s,[1,2,3])
    pool.close()
    pool.join()

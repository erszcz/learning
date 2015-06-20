#!/usr/bin/env python

from pylab import *

# multiple series on a single plot
plotfile("square.csv", cols=(0,1))
plotfile("square.csv", cols=(0,2), newfig=False)
show()

# bar chart
plotfile("square.csv", (0,1), plotfuns={1: 'bar'})
show()

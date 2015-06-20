#!/usr/bin/env python
# encoding: utf-8

# There's a possibly very long sequence of numbers, like:
# 1 5 2 2 1 5 1 1 1 5 2 ...
# Its length is n.
# It's guaranteed that there are only 3 distinct numbers in the sequence
# like 1,2,5 in the example above.
# Using a sliding window of length k (k << n) calculate consecutive medians
# over the sequence. I.e. for k==3:
# 1 5 2 -> 2
#   5 2 2 -> 2
#     2 2 1 -> 2
#       2 1 5 -> 2
#         1 5 1 -> 1

# I've written a sliding window counting the distinct element types
# in a dictionary, but didn't manage to get the median from the dictionary.
# Computing the median was along the lines of:

#def median(counts):
#    l = sum(counts.values())
#    i = 0
#    c = 0
#    while i < len(counts):
#        if i == 0 and counts.values()[i] >= l / 2:
#            return counts.keys()[i]
#        elif i == len(counts) - 1 and counts.values()[i] >= l / 2:
#            return counts.keys()[i]
#        elif c < l/2 and c + counts.values()[i] >= l/2:
#            return counts.keys()[i]
#        c += counts.values()[i]
#        i += 1
#    raise Exception("fcuk")

def median(counts):
    keys = sorted(counts.keys())
    c = 0
    total = sum(counts.values())
    midpoint = total / 2
    if all((v == counts[keys[0]] for v in counts.values())):
        return keys[len(keys) / 2]
    for i in xrange(len(keys)):
        k = keys[i]
        # middle element
        if i > 0 and i < len(keys) - 1:
            if c < midpoint and c + counts[k] > midpoint:
                return k
        # first element
        elif i == 0:
            if counts[k] >= midpoint:
                return k
        # last element
        else:
            if total - counts[k] <= midpoint:
                return k
        c += counts[k]
    return -1

def main():
    assert_equal(median({"a": 4, "b": 8, "c": 3}), "b")
    assert_equal(median({"a": 1, "b": 1, "c": 1}), "b")
    assert_equal(median({"a": 4, "b": 1, "c": 1}), "a")
    assert_equal(median({"a": 1, "b": 1, "c": 3}), "c")
    assert_equal(median({"a": 5, "b": 5, "c": 5}), "c")
    assert_equal(median({"a": 5, "b": 5, "c": 5, "d": 5}), "b")

def assert_equal(actual, expected):
    assert expected == actual, "%s not equal to %s" % (expected, actual)

if __name__ == '__main__':
    main()

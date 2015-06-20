#!/usr/bin/env python

def reverse(n):
    s = str(n)
    def rec(s,acc):
        if s:
            acc.insert(0, s[0])
            return rec(s[1:], acc)
        else:
            i = 0
            while acc[i] == '0':
                i += 1
            return acc[i:]
    return int("".join(rec(s, [])))

def test():
    assert reverse(reverse(123)) == 123
    assert reverse(23) == 32
    assert reverse(230) == 32

if __name__ == '__main__':
    test()

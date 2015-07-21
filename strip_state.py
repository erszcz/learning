#!/usr/bin/env python

#def main():
#    import sys
#    with sys.stdin as f:
#        print f.read(),

def parse_state_tuple(content, from_):
    nopen = 1
    to = from_
    for i in xrange(from_+1, len(content)):
        if nopen == 0:
            break
        c = content[i]
        if c == '{':
            nopen += 1
        elif c == '}':
            nopen -= 1
            to = i+1
    assert to != from_
    return from_, to

def parse_state_tuple_test():
    p = parse_state_tuple
    def t(expected, string):
        return expected == p(string, string.find("{"))
    assert t((0, 10), "{state, x}")
    assert t((0, 16), "{state, {1,2,3}}")
    assert t((10, 20), "some shit {state, x} some more shit")

def main():
    import sys
    with sys.stdin as f:
        content = f.read()
        i = content.find("{state,")
        while i != -1:
            from_, to = parse_state_tuple(content, i)
            content = content[:from_] + "{state, '$stripped_out$'}" + content[to:]
            i = content.find("{state,", i+1)
        print content,

if __name__ == '__main__':
    main()

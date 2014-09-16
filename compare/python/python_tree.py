#! /usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import time

sys.setrecursionlimit(1000 * sys.getrecursionlimit())


class Leaf(object):
    __slots__ = ("val",)
    def __init__(self, val):
        self.val = val
    def __eq__(self, other):
        return (isinstance(other, self.__class__) and
                other.val == self.val)
    def __hash__(self):
        return hash(self.val)
    def is_leaf(self):
        return True

class Node(object):
    __slots__ = ("left", "val", "right")
    def __init__(self, left, val, right):
        self.left = left
        self.val = val
        self.right = right

    def __eq__(self, other):
        return (isinstance(other, self.__class__) and
                other.left == self.left and
                other.val == self.val and
                other.right == self.right)

    def __hash__(self):
        return hash(self.left) ^ hash(self.val) ^ hash(self.right)

    def is_leaf(self):
        return False

class Element(object):
    pass

E = Element()
F = Element()

def make(item, d):
    if d == 0:
        return Leaf(item)
    d2 = d - 1
    left = make(item, d2)
    right = make(item, d2)
    return Node(left, item, right)

def check(t):
    if t.is_leaf():
        return E
    check(t.left)
    return check(t.right)

min_depth = 3


def python_tree(num):
    max_depth = num
    stretch_depth = max_depth + 1
    _ = make(E, stretch_depth)
    long_lived_tree = make(E, max_depth)

    for d in range(min_depth, max_depth):
        iterations = 2 ** (max_depth - d)
        for i in range(1, iterations + 1):
            _1 = make(E, d)
            _2 = make(E, d)
    return check(long_lived_tree)

def main(args):
    num = (int(args[1]) / 1000000) if len(args) > 1 else 18
    t1 = time.clock()
    res = python_tree(num)
    t2 = time.clock()
    t = (t2 - t1) * 1000
    print "0:RESULT-cpu:ms: %s\n0:RESULT-total:ms: %s\n0:RESULT-gc:ms: 0.0\n" % (t, t)
    assert res == E
    return 0

if __name__ == '__main__':
    try:
        sys.exit(main(sys.argv))
    except SystemExit:
        pass
    except:
        import pdb, traceback
        _type, value, tb = sys.exc_info()
        traceback.print_exception(_type, value, tb)
        pdb.post_mortem(tb)

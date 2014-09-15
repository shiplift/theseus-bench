#! /usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import time

sys.setrecursionlimit(1000 * sys.getrecursionlimit())


class Cons(object):
    __slots__ = ("car", "cdr")
    def __init__(self, car, cdr):
        self.car = car
        self.cdr = cdr
    def __eq__(self, other):
        return (isinstance(other, self.__class__) and
                other.car == self.car and
                other.cdr == self.cdr)
    def __hash__(self):
        return hash(self.car) ^ hash(self.cdr)
    def head(self):
        return self.car
    def tail(self):
        return self.cdr
    def is_null(self):
        return self is null

    def append(self, other):
        aux = null
        while not self.is_null():
            aux = Cons(self.head(), aux)
            self = self.tail()
        self = other
        while not aux.is_null():
            self = Cons(aux.head(), self)
            aux = aux.tail()
        return self

null = Cons(None, None)

class Element(object):
    pass

E = Element()
F = Element()

def make_list(number, acc=null):
    while number >= 0:
        acc = Cons(E, acc)
        number -= 1
    return acc

def main(args):
    num = int(args[1]) if len(args) > 1 else 10000000
    lst = make_list(num)
    lst2 = make_list(num)
    t1 = time.clock()
    res = lst.append(lst2)
    t2 = time.clock()
    t = (t2 - t1) * 1000
    print "0:RESULT-cpu:ms: %s\n0:RESULT-total:ms: %s\n0:RESULT-gc:ms: 0.0\n" % (t, t)
    assert not res.is_null()
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

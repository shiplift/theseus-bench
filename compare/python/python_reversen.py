#! /usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import time
import gcreport

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

    def reverse(self):
        aux = null
        while not self.is_null():
            aux = Cons(self.head(), aux)
            self = self.tail()
        return aux

null = Cons(None, None)

E = 17

def make_list(number, acc=null):
    while number >= 0:
        acc = Cons(E, acc)
        number -= 1
    return acc

def main(args):
    num = int(args[1]) if len(args) > 1 else 20000000
    lst = make_list(num)
    lst2 = make_list(num)
    gc1 = gcreport.current_gc_time()
    t1 = time.clock()
    res = lst.reverse()
    t2 = time.clock()
    gc2 = gcreport.current_gc_time()
    t = (t2 - t1) * 1000
    gc = (gc2 - gc1) * 1.0
    print "0:RESULT-cpu:ms: %s\n0:RESULT-total:ms: %s\n0:RESULT-gc:ms: %s\n" % (t, t, gc)
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

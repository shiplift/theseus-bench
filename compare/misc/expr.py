#!/usr/bin/env python
# -*- coding: utf-8; -*-

import sys
import operator
import time

sys.setrecursionlimit(1000 * sys.getrecursionlimit())

class ParseError(Exception):
    pass

def expect(tok, toks):
    if accept(tok, toks):
        return toks.pop(0)
    else:
        raise ParseError

def accept(tok, toks):
    return len(toks) and tok == toks[0]

class Node(object):
    __slots__ = ()
    def __repr__(self):
        return str(self.__class__.__name__)
    def evaluate(self):
        raise NotImplementedError("Abstract Base Class")
    def transform(self):
        return self


class Expr(Node):
    op = None

    def __init__(self, l, r=None):
        self.left = l
        self.right = r

    def __repr__(self):
        s = super(Expr, self).__repr__()
        if self.right is None:
            return "".join([s, "(", repr(self.left), ")"])
        else:
            return "".join([s, "(", repr(self.left), ", ", repr(self.right), ")"])

    def __str__(self):
        if self.right is None:
            return "".join(["(", str(self.left), ")"])
        else:
            return "".join(["(", str(self.left), " ", self.op, " ", str(self.right), ")"])

    def transform(self):
        if self.right is None:
            return self.left.transform()
        else:
            return self.__class__(self.left.transform(), self.right.transform())

    @classmethod
    def make(cls, toks):
        toks = lstrip(toks)
        (l, rest) = cls.next.make(toks)
        r = None
        if accept(cls.op, rest):
            rest.pop(0)
            (r, rest) = cls.next.make(lstrip(rest))
        return cls(l, r), rest

class Mul(Expr):
    op = "*"
    def evaluate(self):
        return self.left.evaluate() * self.right.evaluate()

class Add(Expr):
    op = "+"
    def evaluate(self):
        return self.left.evaluate() + self.right.evaluate()

class Num(Expr):

    def transform(self):
        assert self.right is None
        if isinstance(self.left, Expr):
            e = self.left
            assert e.right is None
            return e.left.transform()
        elif isinstance(self.left, NUM):
            return self.left.transform()
        else:
            raise RuntimeError("dont know what to do")

    @classmethod
    def make(cls, toks):
        toks = lstrip(toks)
        if accept("(", toks):
            toks = lstrip(toks)
            expect("(", toks)
            e, rest = Expr.make(lstrip(toks))
            expect(")", rest)
        else:
            lit = ""
            while toks[0].isdigit():
                lit += toks.pop(0)
            e, rest = NUM(int(lit)), lstrip(toks)
        return cls(e), lstrip(rest)

class NUM(Node):
    def __repr__(self):
        s = super(NUM, self).__repr__()
        return "".join([s, "(%i)" % self.value])
    def __str__(self):
        return "%i" % self.value

    def __init__(self, val):
        self.value = val

    def evaluate(self):
        return self.value

    def transform(self):
        return self

Expr.next = Mul
Mul.next = Add
Add.next = Num

def lstrip(toks):
    if len(toks):
        return toks if not toks[0].isspace() else lstrip(toks[1:])
    return toks

def evaluate(ast):
    # print "ast>>", ast
    # print "ast>>", repr(ast)
    return ast.evaluate()

def transform(st):
    # print "st>>", st
    # print "ast>>", repr(st)
    return st.transform()

def parse(string):
    expr, rest = Expr.make(list(string.strip()))
    if len(rest) != 0:
        raise ParseError
    return expr

from random import random, randint, choice

decay = 1.05
def randomExpression(prob):
    p = random()
    if p > prob:
        return NUM(randint(1, 9))
    elif randint(0, 1) == 0:
        return Expr(randomExpression(prob / decay))
    else:
        left = randomExpression(prob / decay)
        op = choice([Mul, Add])
        right = randomExpression(prob / decay)
        return op(left, right)

    
def generate_main(argv):
    size = int(argv[1]) if len(argv) > 1 else 100
    
    r = transform(randomExpression(1))
    while len(str(r)) < size:
        r = transform(Add(r, randomExpression(1)))
    
    print r
    return 0

def main(argv):
    if len(argv) > 1 and argv[1] == "--generate":
        return generate_main(argv[1:])
        
    
    if len(argv) > 1:
        with open(argv[1]) as f:
            inp = f.read()
    else:
        inp = sys.stdin.read()

    ast = parse(inp)
    
    t1 = time.clock()
    st = transform(ast)
    t2 = time.clock()
    
    val = evaluate(st)
#    print val
#    assert val
    
    t = (t2 - t1) * 1000
    print "0:RESULT-cpu:ms: %s\n0:RESULT-total:ms: %s\n0:RESULT-gc:ms: 0.0\n" % (t, t)

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

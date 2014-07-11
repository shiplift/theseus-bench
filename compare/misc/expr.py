#!/usr/bin/env python
# -*- coding: utf-8; -*-

import sys
import operator

operator_map = {
    "+": operator.add,
    "*": operator.mul,
}


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
    def __str__(self):
        return "%s" % self.__class__.__name__
    def evaluate(self):
        raise NotImplementedError("Abstract Base Class")

class Expr(Node):
    op = None

    def __init__(self, l, r=None):
        self.left = l
        self.right = r

    def __str__(self):
        s = super(Expr, self).__str__()
        if self.right is None:
            return s + "(%s)" % self.left
        else:
            return s + "(%s, %s)" % (self.left, self.right)

    def evaluate(self):
        if self.right is None:
            return self.left.evaluate()
        else:
            return operator_map[self.op](
                self.left.evaluate(),self.right.evaluate())

    @classmethod
    def make(cls, toks):
        (l, rest) = cls.next.make(toks)
        r = None
        if accept(cls.op, rest):
            rest.pop(0)
            (r, rest) = cls.next.make(lstrip(rest))
        return cls(l, r), rest

class Mul(Expr):
    pass

class Add(Expr):
    pass

class Num(Expr):

    @classmethod
    def make(cls, toks):
        if accept("(", toks):
            expect("(", toks)
            e, rest = Expr.make(toks)
            expect(")", rest)
        else:
            lit = ""
            while toks[0].isdigit():
                lit += toks.pop(0)
            e, rest = NUM(int(lit)), lstrip(toks)
        return cls(e), rest

class NUM(Node):
    def __str__(self):
        s = super(NUM, self).__str__()
        return s + "(%i)" % self.value

    def __init__(self, val):
        self.value = val

    def evaluate(self):
        return self.value

Expr.next = Mul
Mul.next = Add
Mul.op = "*"
Add.next = Num
Add.op = "+"

def lstrip(toks):
    return toks if not toks[0].isspace() else lstrip(toks[1:])

def evaluate(ast):
    print "ast>>", ast
    return ast.evaluate()

def transform(st):
    print "st>>", st
    return st

def parse(string):
    expr, rest = Expr.make(list(string.strip()))
    if len(rest) != 0:
        raise ParseError
    return expr

def main(argv):
    if len(argv) > 1:
        with open(argv[1]) as f:
            inp = f.read()
    else:
        inp = sys.stdin.read()

    # try:
    val = evaluate(transform(parse(inp)))
    print "val>>", val

    return 0
    # except ParseError:
    #     return 1

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

import re

class Keyword:
    def __init__(self, s):
        self.val = s
    def __repr__(self):
        return self.val
    def __eq__(self, k):
        return type(k) == type(self) and self.val == k.val

class Symbol:
    def __init__(self, s):
        self.val = s
    def __repr__(self):
        return self.val
    def __eq__(self, k):
        return type(k) == type(self) and self.val == k.val

def key(s):
    return Keyword(s)

def sym(s):
    return Symbol(s)

def s_symbol(scanner, token): return sym(token)
def s_keyword(scanner, token): return key(token)
def s_int(scanner, token): return int(token)

scanner = sre.Scanner([
    (r"[a-zA-Z\-]+", s_symbol),
    (r"\:[a-zA-Z\-]+", s_keyword),
    (r"\d+", s_int),
    (r"\".+\"", s_float),
    (r"=|\+|-|\*|/", s_operator),
    (r"\s+", None),
    ])

# sanity check
test('scanner.scan("sum = 3*foo + 312.50 + bar")',
     (['sum', 'op=', 3, 'op*', 'foo', 'op+', 312.5, 'op+', 'bar'], ''))

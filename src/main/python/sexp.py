import re

def read(s):
    "Read a Scheme expression from a string."
    return read_from(tokenize(s))

def tokenize(s):
    "Convert a string into a list of tokens."
    return s.replace('(',' ( ').replace(')',' ) ').split()

def read_from(tokens):
    "Read an expression from a sequence of tokens."
    if len(tokens) == 0:
        raise SyntaxError('unexpected EOF while reading')
    token = tokens.pop(0)
    if '(' == token:
        L = []
        while len(tokens) > 0 and tokens[0] != ')':
            L.append(read_from(tokens))
        if len(tokens) == 0:
          raise SyntaxError('unexpected EOF while reading list.')
        tokens.pop(0) # pop off ')'
        return L
    elif ')' == token:
        raise SyntaxError('unexpected )')
    else:
        return atom(token)

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

def atom(token):
    ch = token[0]
    if token == "t":
        return True
    if token == "nil":
        return False
    elif ch == ":":
        return Keyword(token)
    elif ch == "\"":
        return token[1:-1].replace("\\\\", "\\")
    elif ch.isdigit():
        return int(token)
    else:
        return Symbol(token)

def to_string(exp):
    "Convert a Python object back into a Lisp-readable string."
    return '('+' '.join(map(to_string, exp))+')' if type(exp) == type([]) else atom_to_str(exp)

def atom_to_str(exp):
    if exp and (type(exp) == type(True)):
        return "t"
    elif (not exp) and (type(exp) == type(False)):
        return "nil"
    elif type(exp) == type(""):
        return "\"" + exp.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
    else:
        return str(exp)

def repl(prompt='lis.py> '):
    "A prompt-read-eval-print loop."
    while True:
        val = eval(parse(raw_input(prompt)))
        if val is not None: print to_string(val)


if __name__ == "__main__":
  print(str(read("nil")))

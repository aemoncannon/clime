import sys
import os
import sexp
from sexp import key
from sexp import sym
import re, fileinput
import pyPEG
from pyPEG import parseLine
from pyPEG import keyword, _and, _not
import traceback


def tok(str):
  return re.compile(str)

def name(): return re.compile(r"(?:[a-zA-Z0-9_]+::)*[~a-zA-Z0-9_]+")
def type_basic_name(): return [tok("long long"),
                               tok("long double"),
                               tok("unsigned int"),
                               tok("unsigned long long"),
                               name]
def operator_name(): return re.compile("operator[^ (]{1,2}")
def function_name(): return [operator_name,name]
def ref_or_ptr(): return [tok("&"),tok("\\*")]
def type_formal_param(): return "<#",["typename","class"],name,"#>"
def typearg(): return [typedecl,type_formal_param]
def type_param_list(): return typearg,-1,(",", typearg)
def typename(): return type_basic_name,0,("<",type_param_list,">")
def typedecl(): return 0,const,typename,0,ref_or_ptr
def const(): return tok("const")
def param(): return "<#",typedecl,0,name,"#>"
def optional_param_list(): return ["{#,", "{#"],param,-1,(",", param),"#}"
def required_param_list(): return param,-1,(",", param)
def param_list(): return 0,required_param_list,0,optional_param_list
def field(): return ("[#",typedecl,"#]"),name
def const_func_mod(): return "[#",const,"#]"
def scope(): return "[#",typename,"::","#]"
def function(): return ("[#",typedecl,"#]"),0,scope,function_name,"(",param_list,")",0,const_func_mod
def constructor(): return typename,"(",param_list,")"
def member(): return [function,field,constructor,typename]

# For debugging
# ----
#pyPEG.print_trace = True

class Opt:
  def __init__(self,obj):
    self.obj = obj;

def match(ast, pattern):
  return match_worker(ast, pattern, {})

def match_worker(ast, pattern, binding):
  #  print "matching..."
  #  print ast
  #  print pattern
  skip = 0
  if isinstance(ast,list):
    for i in range(0,len(pattern)):
      b = pattern[i]
      if (i - skip) < len(ast):
        a = ast[i - skip]
        if isinstance(b, Opt) and match_worker(a,b.obj,{}):
          match_worker(a, b.obj, binding)
        elif isinstance(b, Opt):
          skip = skip + 1
        elif isinstance(a,list) and isinstance(b, list):
          if not match_worker(a, b, binding):
            return None
        elif isinstance(b, basestring) and b[0] == ":":
          binding[b] = a
        elif a == b: 
          pass
        else:
          return None
      elif isinstance(b, basestring) and b[0] == ":":
        binding[b] = None

  else:
    return None

  return binding


def flatten_ast(ast):
  if ast == None:
    return ""
  elif isinstance(ast, (basestring)):
    return ast
  elif isinstance(ast, (list, tuple)):
    if len(ast) == 0:
      return ""
    elif len(ast) == 1:
      return flatten_ast(ast[0])
    elif isinstance(ast[1], (list, tuple)):
      return " ".join([flatten_ast(ea) for ea in ast[1]])
    else:
      return str(ast[1])



type_decl_pat = ['typedecl', [
    Opt(['const', ":CONST"]),
    ['typename', [":BASIC_NAME", ['type_param_list', ":TYPE_ARGS"]]], 
    ":REF_OR_PTR"]]

typename_pat = ['typename', [":BASIC_NAME", ['type_param_list', ":TYPE_ARGS"]]]

class Type:

  def __init__(self, ast):
    m = match(ast,type_decl_pat)
    if m:
      if ":CONST" in m:
        self.const = "const"
      else:
        self.const = None
  
      if ":TYPE_ARGS" in m:
        self.type_args = [flatten_ast(ast) for ast in m[":TYPE_ARGS"]]
      else:
        self.type_args = None
  
      if ":REF_OR_PTR" in m:
        self.ref_or_ptr = flatten_ast(m[":REF_OR_PTR"])
      else:
        self.ref_or_ptr = None
  
      self.name = flatten_ast(m[":BASIC_NAME"])
      if self.type_args:
        self.name = self.name + "<" + ", ".join(self.type_args) + ">"
    else:
      m = match(ast,typename_pat)
      if m:
        self.const = None
        self.ref_or_ptr = None
        if ":TYPE_ARGS" in m:
          self.type_args = [flatten_ast(ast) for ast in m[":TYPE_ARGS"]]
        else:
          self.type_args = None
    
        self.name = flatten_ast(m[":BASIC_NAME"])

        if self.type_args:
          self.name = self.name + "<" + ", ".join(self.type_args) + ">"
      else:
        raise StandardError("Pattern match failed!")

  def to_sexp_list(self):
    return [ key(":const"),self.const,
             key(":name"),self.name,
             key(":type"),[key(":name"),self.name]]

  def __repr__(self):
    return (self.name + (" const" if self.const else "") + 
            (" " + self.ref_or_ptr if self.ref_or_ptr else ""))

  def is_callable(self):
    return False

  def args_placeholder(self):
    return ""


param_pat = ['param', [":TYPE", ":NAME"]]

class Param:

  def __init__(self, ast):
    m = match(ast, param_pat)
    if m:
      self.type = Type(m[":TYPE"])
      self.name = flatten_ast(m[":NAME"]) if ":NAME" in m else None
    else:
      raise StandardError("Pattern match failed!")

  def to_sexp_list(self, ):
    return [key(":name"),self.name,
            key(":type"),self.type.to_sexp_list()]

  def __repr__(self):
    return str(self.type) + (" " + self.name if self.name else "")


field_pat = ['field',[
    ":TYPE",
    ":NAME"
    ]]

class Field:
  def __init__(self, ast):
    m = match(ast, field_pat)
    if m:
      self.type = Type(m[":TYPE"])
      self.name = flatten_ast(m[":NAME"])
    else:
      raise StandardError("Pattern match failed!")

  def to_sexp_list(self, ):
    return [key(":name"),self.name,
            key(":type"),self.type.to_sexp_list()]

  def __repr__(self):
    return (str(self.type) + " " + self.name)

  def args_placeholder(self):
    return ""

  def is_callable(self):
    return False


def param_section_str(params, opt_params):
  return (", ".join([str(p) for p in params]) + 
          (("[, " + ", ".join([str(p) for p in opt_params]) + "]") if len(opt_params) > 0 else ""))


function_pat = ['function',[
    ":RETURN_TYPE",
    Opt(["scope", ":SCOPE"]),
    ":FUNCTION_NAME", 
    ['param_list', [Opt(["required_param_list", ":PARAMS"]),
                    ["optional_param_list", ":OPTIONAL_PARAMS"]]],
    ['const_func_mod', ":CONST"],
    ]]

class Function:

  def __init__(self, ast):
    m = match(ast, function_pat)
    if m:
      self.type = Type(m[":RETURN_TYPE"])
      self.name = flatten_ast(m[":FUNCTION_NAME"])

      if ":PARAMS" in m:
        self.params = [Param(ast) for ast in m[":PARAMS"]]
      else:
        self.params = []

      if ":OPTIONAL_PARAMS" in m:
        self.opt_params = [Param(ast) for ast in m[":OPTIONAL_PARAMS"]]
      else:
        self.opt_params = []

      if ":CONST" in m:
        self.const = flatten_ast(m[":CONST"])
      else:
        self.const = None
    else:
      raise StandardError("Pattern match failed!")
    

  def to_sexp_list(self, ):
    return [key(":const"),self.const,
            key(":name"),self.name,
            key(":type"),self.type.to_sexp_list(),
            key(":params"),[p.to_sexp_list() for p in self.params],
            key(":optional-params"),[p.to_sexp_list() for p in self.opt_params]]

  def __repr__(self):
    return (str(self.type) + " " + self.name + 
            "(" + param_section_str(self.params, self.opt_params) + ")" + 
            (" const" if self.const else ""))

  def args_placeholder(self):
    return param_section_str(self.params, self.opt_params)

  def is_callable(self):
    return True


constructor_pat = ['constructor',[
    ":TYPE",
    ['param_list', [Opt(["required_param_list", ":PARAMS"]),
                    ["optional_param_list", ":OPTIONAL_PARAMS"]]]
    ]]

class Constructor:
  def __init__(self, ast):
    m = match(ast, constructor_pat)
    if m:
      self.type = Type(m[":TYPE"])
      self.name = self.type.name
      if ":PARAMS" in m:
        self.params = [Param(ast) for ast in m[":PARAMS"]]
      else:
        self.params = []
      if ":OPTIONAL_PARAMS" in m:
        self.opt_params = [Param(ast) for ast in m[":OPTIONAL_PARAMS"]]
      else:
        self.opt_params = []
    else:
      raise StandardError("Pattern match failed!")
  

  def to_sexp_list(self, ):
    return [key(":name"),self.type.name,
            key(":params"),[p.to_sexp_list() for p in self.params]]

  def __repr__(self):
    return (self.type.name + "(" + param_section_str(self.params, self.opt_params)) + ")"

  def args_placeholder(self):
    return param_section_str(self.params, self.opt_params)

  def is_callable(self):
    return True



def make_member(completion_str):
  try:
    (ast,remainder) = parseLine(completion_str, member, 
                                resultSoFar=[], skipWS=True, 
                                skipComments=None,packrat=False)
    if len(remainder) == 0:
      ast = ast[0][1][0]
      if ast[0] == "function":
        return Function(ast)
      elif ast[0] == "field":
        return Field(ast)
      elif ast[0] == "constructor":
        return Constructor(ast)
      elif ast[0] == "typename":
        return Type(ast)
      else:
        return None
    else:
      print "Failed to parse member: " + completion_str
      return None

  except Exception as e:
    print "Failed to build member: " + completion_str
    traceback.print_exc(file=sys.stdout)
    return None

def test(input, expected):
  s = str(make_member(input))
  assert expected == s, "Expected: " + expected + ", found " + s

if __name__ == "__main__":
  test("[#void#]insertRows(<#int row#>, <#const QList<QStandardItem *> &items#>)",
       "void insertRows(int row, QList<QStandardItem *> const & items)")
  test("[#void#]insertRows(<#int row#>, <#int count#>)",
       "void insertRows(int row, int count)")
  test("[#bool#]isDragEnabled()[# const#]",
       "bool isDragEnabled() const")
  test("[#QBrush#]background()[# const#]",
       "QBrush background() const")
  test("[#QScopedPointer<QStandardItemPrivate>#]d_ptr",
       "QScopedPointer<QStandardItemPrivate> d_ptr")
  test("[#QStandardItem &#]operator=(<#const QStandardItem &other#>)",
       "QStandardItem & operator=(QStandardItem const & other)")
  test("[#bool#]operator<(<#const QStandardItem &other#>)[# const#]",
       "bool operator<(QStandardItem const & other) const")
  test("[#QStandardItem *#]child(<#int row#>{#, <#int column#>#})[# const#]",
       "QStandardItem * child(int row[, int column]) const")
  test("ApeOutlineModel(<#const long long customerId#>{#, <#QObject *parent#>#})",
       "ApeOutlineModel(long long const customerId[, QObject * parent])")
  test("[#void#]selectionChangedSlot(<#const QItemSelection &#>, <#const QItemSelection &#>)",
       "void selectionChangedSlot(QItemSelection const &, QItemSelection const &)")
  test("QMap<<#class Key#>, <#class T#>>(<#const QMap<Key, T> &other#>)",
       "QMap<Key, T>(QMap<Key, T> const & other)")
  test("QMap<<#class Key#>, <#class T#>>",
       "QMap<Key, T>")
  test("QMatrix(<#Qt::Initialization#>)",
       "QMatrix(Qt::Initialization)")
  test("[#void#]setConnectOptions({#<#const QString &options#>#})",
       "void setConnectOptions([, QString const & options])")

  test("[#void#]~QSqlDatabase()",
       "void ~QSqlDatabase()")

  test("[#void#][#QAbstractItemView::#]setEditTriggers(<#EditTriggers triggers#>)",
       "void setEditTriggers(EditTriggers triggers)")

  test("[#void#][#QList<QString>::#]pop_front()","void pop_front()")

  print "All tests passed."

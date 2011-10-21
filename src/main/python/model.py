import sys
import os
import sexp
from sexp import key
from sexp import sym
import re


class Type:

  def __init__(self, type_str):
    tpe_re = re.compile("(const )?(.+)")
    m = tpe_re.match(type_str)
    self.const = m.group(1)
    self.name = m.group(2)
    if self.name.endswith(" &"):
      self.name = self.name[:-2] + "&"
    elif self.name.endswith(" *"):
      self.name = self.name[:-2] + "*"
    elif self.name.endswith(" "):
      self.name = self.name[:-1]

  def to_sexp_list(self):
    return [ key(":const"),self.const,
             key(":name"),self.name]

  def __repr__(self):
    return ("const " if self.const else "") + self.name



class Param:

  def __init__(self, name, type):
    self.name = name
    self.type = type

  def to_sexp_list(self, ):
    return [key(":name"),self.name,
            key(":type"),self.type.to_sexp_list]

  def __repr__(self):
    return str(self.type) + " " + self.name


def parse_params(str):
  result = []
  parts = str.split(", ")
  param_re = re.compile("<#(.+? (?:&|\\*)?)([A-z]+)?#>")
  for p in parts:
    m = param_re.match(p)
    if m:
      tpe = m.group(1)
      name = m.group(2) or "x"
      result.append(Param(name, Type(tpe)))
  return result


class Field:
  # Parsing lines of the form:
  # [#void#]insertRows(<#int row#>, <#const QList<QStandardItem *> &items#>)
  # [#void#]insertRows(<#int row#>, <#int count#>)
  # [#bool#]isDragEnabled()[# const#]
  def __init__(self, type, name):
    self.type = type
    self.name = name

  def to_sexp_list(self, ):
    return [key(":name"),self.name,
            key(":type"),self.type.to_sexp_list]

  def __repr__(self):
    return (str(self.type) + " " + self.name)

  def args_placeholder(self):
    return ""

  def is_callable(self):
    return False


class Function:
  def __init__(self, type, name, params, const):
    self.type = type
    self.name = name
    self.const = const
    self.params = params

  def to_sexp_list(self, ):
    return [key(":const"),self.const,
            key(":name"),self.name,
            key(":type"),self.type.to_sexp_list,
            key(":params"),[p.to_sexp_list for p in self.params]]

  def __repr__(self):
    return (str(self.type) + " " + self.name + 
            "(" + ", ".join([str(p) for p in self.params]) + ")" + 
            (" const" if self.const else ""))

  def args_placeholder(self):
    return ", ".join([str(p) for p in self.params])

  def is_callable(self):
    return True


class Constructor:
  def __init__(self, name, params):
    self.name = name
    self.params = params

  def to_sexp_list(self, ):
    return [key(":name"),self.name,
            key(":params"),[p.to_sexp_list for p in self.params]]

  def __repr__(self):
    return (self.name + 
            "(" + ", ".join([str(p) for p in self.params]) + ")")

  def args_placeholder(self):
    return ", ".join([str(p) for p in self.params])

  def is_callable(self):
    return True




# Parsing lines of the form:
# [#void#]insertRows(<#int row#>, <#const QList<QStandardItem *> &items#>)
# [#void#]insertRows(<#int row#>, <#int count#>)
# [#bool#]isDragEnabled()[# const#]
def make_member(completion_str):
  
  # For now, let's just ignore optional args.
  completion_str = completion_str.replace("{#", "")
  completion_str = completion_str.replace("#}", "")

  method_re = re.compile("(?:\\[#([^\\]]+)#\\])?(?:\\[#([^\\]]+)#\\])?([^ \\(]+)(\\(((?:<#[^\\]]+#>(?:, )?)*)\\))?(\\[# const#\\])?")
  m = method_re.match(completion_str)
  if m:
    type = Type(m.group(1)) if m.group(1) else None
    scope = m.group(2)
    name = m.group(3)
    const = m.group(6)
    if type and m.group(4):
      params = parse_params(m.group(5))
      return Function(type, name, params, const)
    elif not type and m.group(4):
      params = parse_params(m.group(5))
      return Constructor(name, params)
    else:
      return Field(type, name)
  else:
    None


if __name__ == "__main__":
  assert (str(make_member("[#void#]insertRows(<#int row#>, <#const QList<QStandardItem *> &items#>)"))
          == "void insertRows(int row, const QList<QStandardItem *>& items)")
  assert (str(make_member("[#void#]insertRows(<#int row#>, <#int count#>)"))
          == "void insertRows(int row, int count)")
  assert (str(make_member("[#bool#]isDragEnabled()[# const#]"))
          == "bool isDragEnabled() const")
  assert (str(make_member("[#QBrush#]background()[# const#]"))
          == "QBrush background() const")
  assert (str(make_member("[#QScopedPointer<QStandardItemPrivate>#]d_ptr"))
          == "QScopedPointer<QStandardItemPrivate> d_ptr")
  assert (str(make_member("[#QStandardItem &#]operator=(<#const QStandardItem &other#>)"))
          == "QStandardItem& operator=(const QStandardItem& other)")
  assert (str(make_member("[#bool#]operator<(<#const QStandardItem &other#>)[# const#]"))
          == "bool operator<(const QStandardItem& other) const")
  assert (str(make_member("[#QStandardItem *#]child(<#int row#>{#, <#int column#>#})[# const#]"))
          == "QStandardItem* child(int row, int column) const")
  assert (str(make_member("ApeOutlineModel(<#const long long customerId#>{#, <#QObject *parent#>#})"))
          == "ApeOutlineModel(const long long customerId, QObject* parent)")
  assert (str(make_member("[#void#]selectionChangedSlot(<#const QItemSelection &#>, <#const QItemSelection &#>)"))
          == "void selectionChangedSlot(const QItemSelection& x, const QItemSelection& x)")












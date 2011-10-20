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
  param_re = re.compile("<#(.+? (?:&|\\*)?)([A-z]+)#>")
  for p in parts:
    m = param_re.match(p)
    if m:
      tpe = m.group(1)
      name = m.group(2)
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



# Parsing lines of the form:
# [#void#]insertRows(<#int row#>, <#const QList<QStandardItem *> &items#>)
# [#void#]insertRows(<#int row#>, <#int count#>)
# [#bool#]isDragEnabled()[# const#]
def make_member(completion_str):
  method_re = re.compile("\\[#([^\\]]+)#\\]([^ \\(]+)(\\(((?:<#[^\\]]+#>(?:, )?)*)\\))?(\\[# const#\\])?")
  m = method_re.match(completion_str)
  if m:
    type = Type(m.group(1))
    name = m.group(2)
    const = m.group(5)
    if m.group(3):
      params = parse_params(m.group(4))
      return Function(type, name, params, const)
    else:
      return Field(type, name)
  else:
    None


if __name__ == "__main__":
  print str(make_member("[#void#]insertRows(<#int row#>, <#const QList<QStandardItem *> &items#>)"))
  print str(make_member("[#void#]insertRows(<#int row#>, <#int count#>)"))
  print str(make_member("[#bool#]isDragEnabled()[# const#]"))
  print str(make_member("[#QBrush#]background()[# const#]"))
  print str(make_member("[#QScopedPointer<QStandardItemPrivate>#]d_ptr"))
  print str(make_member("[#QStandardItem &#]operator=(<#const QStandardItem &other#>)"))
  print str(make_member("[#bool#]operator<(<#const QStandardItem &other#>)[# const#]"))
  print str(make_member("[#QStandardItem *#]child(<#int row#>{#, <#int column#>#})[# const#]"))






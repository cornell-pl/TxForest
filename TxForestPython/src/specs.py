
import re

from fetchrep import *
from log import *

class Spec():
  def __init__(self):
    pass

  def fetch(self, fs, p, log):
    pass

  def __str__(self):
    return ""


class NullRep(Spec):
  def __init__(self):
    Spec.__init__(self)

  def fetch(self, fs, p, log):
    return None

  def __str__(self):
    return "nullrep"

class File(Spec):
  def __init__(self):
    Spec.__init__(self)

  def fetch(self, fs, p, log):
    contents = fs[p]
    u = contents.get_u()
    log.append(ReadFile(p))
    return FileRep(u)

  def __str__(self):
    return "file"

class Dir(Spec):
  def __init__(self):
    Spec.__init__(self)

  def fetch(self, fs, p, log):
    contents = fs[p]
    lst = contents.get_lst()
    log.append(ReadFile(p))
    return DirRep(lst)

  def __str__(self):
    return "dir"

class Path(Spec):
  def __init__(self, name, s):
    Spec.__init__(self)
    self.name = name
    self.s = s

  def get_exp(self):
    return self.name

  def get_subspec(self):
    return self.s

  def fetch(self, fs, p, log):
    return PathRep(self.name)

  def __str__(self):
    return self.name + ' :: ' + self.s.__str__()

class Pair(Spec):
  def __init__(self, s1, x_to_s2):
    Spec.__init__(self)
    self.s1 = s1
    self.x_to_s2 = x_to_s2

  def leftspec(self):
    return self.s1

  def rightspec(self):
    return self.x_to_s2

  def fetch(self, fs, p, log):
    return PairRep()

  def __str__(self):
    return '( ' + self.s1.__str__() + ', ' + self.x_to_s2(self.s1).__str__() + ' )'


class Comp(Spec):
  def __init__(self, x_to_s, gen_xs):
    Spec.__init__(self)
    self.x_to_s = x_to_s
    self.gen_xs = gen_xs

  def get_subspec(self):
    return self.x_to_s

  def gen(self):
    return self.gen_xs()

  def fetch(self, fs, p, log):
    return CompRep(self.gen_xs())

  def desugar(self):
    return Pair(Dir(), lambda this: self)

  def __str__(self):
    # xs = self.gen_xs()
    # x_strings = [x + ' : ' + self.x_to_s(x).__str__() for x in xs]
    return '[' + str(self.x_to_s('var')) + ']'

class RegexComp(Comp):
  def __init__(self, x_to_s, gen_xs):
    Comp.__init__(self, x_to_s, gen_xs)

  def get_subspec(self):
    return self.x_to_s

  def gen(self, fs, p):
    all_files = fs[p].get_lst()
    return [f for f in all_files if re.match(self.gen_xs, f)]

  def fetch(self, fs, p, log):
    return CompRep(self.gen(fs, p))



class Opt(Spec):
  def __init__(self, s=None):
    Spec.__init__(self)
    self.s = s

  def get_subspec(self):
    return self.s

  def fetch(self, fs, p, log):
    log.append(ReadFile(p))
    return OptRep(p in fs)

  def __str__(self):
    if self.s == None:
      return 'none'
    else:
      return self.s.__str__()

class Pred(Spec):
  def __init__(self, test):
    Spec.__init__(self)
    self.test = test

  def gen(self):
    self.test()

  def fetch(self, fs, p, log):
    return PredRep(self.test())

  def __str__(self):
    return 'pred'

# note: they dont have an interation in order of insertion until
# python 3.7, so ordering must be used to ensure that the
# directory will be ordered as you think it will
class Directory(Spec):
  def __init__(self, names_to_specs={}, ordering=[]):
    Spec.__init__(self)
    self.names = names_to_specs.keys() if ordering == [] else ordering
    self.name_to_spec = {}
    self.name_to_zipper = {}
    for name in names_to_specs:
      self.name_to_spec[name] = names_to_specs[name]

  def add(self, name, to_spec):
    self.name_to_spec[name] = to_spec
    self.names.append(name)

  def desugar_helper_helper(self, name, x, i):
    self.name_to_zipper[name] = x
    return self.desugar_helper(i+1)

  def desugar_helper(self, i):
    if i == len(self.names):
      return NullRep()
    else:
      name = self.names[i]
      x_to_spec = self.name_to_spec[name]
      return Pair(
        x_to_spec(),
        lambda x : self.desugar_helper_helper(name, x, i)
      )

  def desugar(self):
    return self.desugar_helper(0)

  def __setitem__(self, name, to_spec):
    self.add(name, to_spec)

  def __getitem__(self, i):
    return self.name_to_zipper[i]
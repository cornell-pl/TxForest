class Spec():
  def __init__(self):
    pass

  def __str__(self):
    return ""


class NullRep(Spec):
  def __init__(self):
    Spec.__init__(self)

  def __str__(self):
    return "nullrep"

class File(Spec):
  def __init__(self):
    Spec.__init__(self)

  def __str__(self):
    return "file"

class Dir(Spec):
  def __init__(self):
    Spec.__init__(self)

  def __str__(self):
    return "dir"

class Path(Spec):
  def __init__(self, name, s):
    Spec.__init__(self)
    self.name = name
    self.s = s

  def __str__(self):
    return self.name + ' :: ' + self.s.__str__()

class Pair(Spec):
  def __init__(self, s1, x_to_s2):
    Spec.__init__(self)
    self.s1 = s1
    self.x_to_s2 = x_to_s2

  def __str__(self):
    return '( ' + self.s1.__str__() + ', ' + self.x_to_s2(self.s1).__str__() + ' )'


class Comp(Spec):
  def __init__(self, x_to_s, gen_xs):
    Spec.__init__(self)
    self.x_to_s = x_to_s
    self.gen_xs = gen_xs

  def __str__(self):
    xs = self.gen_xs()
    x_strings = [x + ' : ' + self.x_to_s(x).__str__() for x in xs]
    return '[ ' + ', '.join(x_strings) + ' ]'


class Opt(Spec):
  def __init__(self, s=None):
    Spec.__init__(self)
    self.s = s

  def __str__(self):
    if self.s == None:
      return 'none'
    else:
      return self.s.__str__()

class Pred(Spec):
  def __init__(self, test):
    Spec.__init__(self)
    self.test = test

  def __str__(self):
    return 'pred'

class Directory(Spec):
  def __init__(self, names_to_specs={}):
    Spec.__init__(self)
    self.names = []
    self.name_to_spec = {}
    self.name_to_zipper = {}
    for name in names_to_specs:
      self.add(name, names_to_specs[name])

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
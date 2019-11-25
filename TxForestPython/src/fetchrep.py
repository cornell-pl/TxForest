
class FetchRep():
   def __init__(self):
      pass

   def __str__(self):
      return ""


class FileRep(FetchRep):
   def __init__(self, contents):
      FetchRep.__init__(self)
      self.contents = contents

   def get_contents(self):
      return self.contents

   def __str__(self):
      return 'file ' + self.contents

class DirRep(FetchRep):
   def __init__(self, contents):
      FetchRep.__init__(self)
      self.contents = contents

   def get_contents(self):
      return self.contents

   def __str__(self):
      return 'dir ' + ', '.join(self.contents)


class PathRep(FetchRep):
   def __init__(self, u):
      FetchRep.__init__(self)
      self.u = u

   def get_u(self):
      return self.u

   def __str__(self):
      return 'path ' + self.u


class PairRep(FetchRep):
   def __init__(self):
      FetchRep.__init__(self)

   def __str__(self):
      return 'pair'


class CompRep(FetchRep):
   def __init__(self, xs):
      FetchRep.__init__(self)
      self.xs = xs

   def get_xs(self):
      return self.xs

   def __str__(self):
      return 'comp ' + ', '.join(self.xs)


class OptRep(FetchRep):
   def __init__(self, b):
      FetchRep.__init__(self)
      self.b = b

   def __str__(self):
      return 'opt ' + b

class PredRep(FetchRep):
   def __init__(self, b):
      FetchRep.__init__(self)
      self.b = b

   def __str__(self):
      return 'pred ' + b








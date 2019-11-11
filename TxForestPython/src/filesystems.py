

class Contents():
  def __init__(self):
    pass

  def __str__(self):
    return ""


class FileContents(Contents):
  def __init__(self, u):
    Contents.__init__(self)
    self.u = u

  def __str__(self):
    return 'file: ' + self.u

class DirContents(Contents):
  def __init__(self, s):
    Contents.__init__(self)
    self.s = s

  def __str__(self):
    return 'dir: ' + ', '.join(self.s)


class Filesystem():
  def __init__(self, path):
    self.path = path


class MemoryFilesystem(Filesystem):
  def __init__(self, path):
    Filesystem.__init__(self, path)
    self.fs = {}

  def __getitem__(self, i):
    return self.fs[i]


  def __setitem__(self, i, v):
    #TODO: realistically this is more complicated but this is the idea we want
    self.fs[i] = v

#TODO: realistically this is more complicated but this is the idea we want
class PosixFilesystem(Filesystem):
  def __init__(self, path):
    Filesystem.__init__(self, path)

  def __getitem__(self, i):
    return ""

  def __setitem__(self, i, v):
    pass

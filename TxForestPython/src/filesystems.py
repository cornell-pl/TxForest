
from os.path import *

class Contents():
  def __init__(self):
    pass

  def __str__(self):
    return ""


class FileContents(Contents):
  def __init__(self, u):
    Contents.__init__(self)
    self.u = u

  def get_u(self):
    return self.u

  def copy(self):
    return FileContents(self.u)

  def __str__(self):
    return 'file: ' + self.u

class DirContents(Contents):
  def __init__(self, s):
    Contents.__init__(self)
    self.s = s

  def get_lst(self):
    return self.s

  def copy(self):
    return DirContents(self.s)

  def __str__(self):
    return 'dir: ' + ', '.join(self.s)


class Filesystem():
  def __init__(self, path):
    self.path = path
    self.log = []

  def get_log(self):
    log = self.log
    self.log = []
    return log

  def _copy_fs(self):
    new_fs = {}
    for i in self.fs:
      new_fs[i] = self.fs[i].copy()
    return new_fs

  def commit(self):
    pass


global_fs = {
  '/': DirContents(['simple']),
  '/simple' : DirContents(['index.txt', 'dir']),
  '/simple/index.txt' : FileContents('a\nb\nc'),
  '/simple/dir' : DirContents(['a', 'b', 'c', 'd']),
  '/simple/dir/a' : FileContents('aaa'),
  '/simple/dir/b' : FileContents('bbb'),
  '/simple/dir/c' : FileContents('ccc'),
  '/simple/dir/d' : FileContents('ddd')
}

class MemoryFilesystem(Filesystem):
  def __init__(self, path):
    Filesystem.__init__(self, path)
    self.fs = global_fs
    self.fs = self._copy_fs()

  def __getitem__(self, i):
    return self.fs[i]

  def __contains__(self, i):
    return i in self.fs

  def _remove_dir(self, path):
    us = self.fs[path].get_lst()
    for u in us:
      child_path = join(path, u)
      if isinstance(self.fs[child_path], FileContents):
        del self.fs[child_path]
      else:
        self._remove_dir(child_path)
    del self.fs[path]

  def _simple_add(self, path, v):
    if isinstance(v, FileContents):
      self.fs[path] = v
    else:
      self.fs[path] = v
      new_lst = v.get_lst()
      for u in new_lst:
        child_path = join(path, u)
        if not child_path in self.fs:
          self.fs[child_path] = FileContents('')



  def __setitem__(self, i, v):
    if i in self.fs:
      if isinstance(v, FileContents) and isinstance(self.fs[i], DirContents):
        self._remove_dir(i)
      elif isinstance(v, DirContents) and isinstance(self.fs[i], DirContents):
        new_lst = v.get_lst()
        old_lst = self.fs[i].get_lst()
        for u in old_lst:
          if not u in new_lst:
            child_path = join(path, u)
            if isinstance(self.fs[child_path], FileContents):
              del self.fs[child_path]
            else:
              self._remove_dir(child_path)
    self._simple_add(i, v)

  def commit(self):
    global global_fs
    global_fs = self._copy_fs()

#TODO: realistically this is more complicated but this is the idea we want
class PosixFilesystem(Filesystem):
  def __init__(self, path):
    Filesystem.__init__(self, path)

  def __getitem__(self, i):
    return ""

  def __setitem__(self, i, v):
    pass

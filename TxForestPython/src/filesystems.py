

from os.path import *
import os
import shutil
import stat
import subprocess

from log import *

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

  def sync(self):
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

  # these dont actually do the thin I want them to, but they
  # are a stand in until I switch to the read fs
  def commit(self):
    global global_fs
    global_fs = self._copy_fs()

  def sync(self):
    global global_fs
    self.fs = global_fs
    self.fs = self._copy_fs()

#TODO: realistically this is more complicated but this is the idea we want
class PosixFilesystem(Filesystem):
  def __init__(self, path, cid):
    Filesystem.__init__(self, path)
    (self.root_path, root) = split(path)
    self.temp_path = join(self.root_path, 'temp' + str(cid))
    if exists(self.temp_path):
      shutil.rmtree(self.temp_path)
    os.mkdir(self.temp_path)
    self.temp_root = join(self.temp_path, root)
    self.real_root = path
    os.mkdir(self.temp_root)
    self._copy()

    self.commit_path = join(self.root_path, 'commit')
    self.commit_script = open(self.commit_path, 'w+')
    st = os.stat(self.commit_path)
    os.chmod(self.commit_path, st.st_mode | stat.S_IEXEC)
    self.commit_script.write('#!/bin/bash\n')
    self.commit_script.write("rsync -a " + self.temp_root + '/ ' + path + '/ \n')
    self.commit_script.flush()

  def _read_file(self, path):
    file = open(path, 'r')
    contents = file.read()
    file.close()
    return FileContents(contents)

  def _read_dir(self, path):
    dir_contents = os.listdir(path)
    dir_contents = [c for c in dir_contents if not c[0] == '.']
    return DirContents(dir_contents)

  def _read_path(self, path):
    if isfile(path):
      return self._read_file(path)
    elif isdir(path):
      return self._read_dir(path)
    else:
      raise Exception('path ' + path + ' is not a file or directory')

  def _remove_dir(self, path, real_path):
    shutil.rmtree(path)
    self.commit_script.write("rm -rf " + real_path + '\n')
    self.commit_script.flush()

  def _remove_file(self, path, real_path):
    os.remove(path)
    self.commit_script.write("rm " + real_path + '\n')
    self.commit_script.flush()

  def _remove(self, path, real_path):
    if isfile(path):
      self._remove_file(path, real_path)
    elif isdir(path):
      self._remove_dir(path, real_path)

  def _add(self, path, contents):
    if isinstance(contents, FileContents):
      file = open(path, 'w+')
      file.write(contents.get_u())
      file.flush()
      file.close()
    elif isinstance(contents, DirContents):
      if not exists(path):
        os.mkdir(path)
      new_lst = contents.get_lst()
      old_lst = self._read_dir(path).get_lst()
      for u in new_lst:
        if not u in old_lst:
          child_path = join(path, u)
          self._add(child_path, FileContents(''))

  def _write_path(self, path, contents, real_path):
    if isinstance(contents, FileContents) and isdir(path):
      self._remove_dir(path, real_path)
    elif isinstance(contents, DirContents) and isfile(path):
      self._remove_file(path, real_path)
      os.mkdir(path)
    elif isinstance(contents, DirContents) and isdir(path):
      new_lst = contents.get_lst()
      old_lst = self._read_dir(path).get_lst()
      for u in old_lst:
        if not u in new_lst:
          child_path = join(path, u)
          real_child_path = join(real_path, u)
          self._remove(child_path, real_child_path)
    self._add(path, contents)


  def __getitem__(self, i):
    self.log.append(ReadFile(i))
    rel_path = relpath(i, start=self.root_path)
    temp_path = join(self.temp_path, rel_path)
    return self._read_path(temp_path)

  def __setitem__(self, i, v):
    if isinstance(v, FileContents):
      self.log.append(WriteFile(i, v.get_u()))
    elif isinstance(v, DirContents):
      self.log.append(WriteFile(i, v.get_lst()))
    rel_path = relpath(i, start=self.root_path)
    temp_path = join(self.temp_path, rel_path)
    self._write_path(temp_path, v, i)

  # def add_child(self, i, child):
  #   if isdir(i):
  #     old_lst = self._read_dir(i).get_lst()
  #     if not child in old_lst:
  #       old_lst.append(child)
  #       contents = DirContents(old_lst)
  #       self.__setitem__(self, i, contents)
  #   elif isfile(i):
  #     contents = DirContents([child])
  #     self.__setitem__(self, i, contents)

  def _copy(self):
    # print 'copying path: ' + self.path
    rel_path = relpath(self.path, start=self.root_path)
    temp_path = join(self.temp_path, rel_path)
    children = self._read_dir(self.path).get_lst()
    # print 'children: '
    # print children
    for u in children:
      child_path = join(self.path, u)
      temp_child_path = join(temp_path, u)
      if not exists(temp_child_path):
        if isfile(child_path):
          contents = self._read_file(child_path)
          self._add(temp_child_path, contents)
        elif isdir(child_path):
          self._add(temp_child_path, DirContents([]))


  def goto(self, child):
    self.log.append(ReadFile(self.path))
    self.path = join(self.path, child)
    if isdir(self.path):
      self._copy()

  def up(self):
    self.path = dirname(self.path)

  def commit(self):
    self.commit_script.close()
    (commit_dir, commit_file) = split(self.commit_path)
    print self.commit_path
    subprocess.Popen(self.commit_path, env={"PATH": "$PATH:/bin:/usr/bin/"}).wait()
    # ex_file = open(self.commit_path, 'rb')
    # script = ex_file.read()
    # rc = subprocess.call(script, shell=True)
    self.commit_script = open(self.commit_path, 'w+')
    self.commit_script.write('#!/bin/bash\n')
    self.commit_script.write("rsync -a " + self.temp_root + '/ ' + self.real_root + '/ \n')
    self.commit_script.flush()
    self.commit_script.close()
    self.log = []
    shutil.rmtree(self.temp_root)
    os.mkdir(self.temp_root)
    subpaths = (relpath(self.path, start=self.root_path)).split('/')
    self.path = self.root_path
    for u in subpaths:
      self.goto(u)








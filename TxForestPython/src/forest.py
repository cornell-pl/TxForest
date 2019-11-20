

from fetchrep import *
from zipper import *
from filesystems import *
from log import *
from os.path import *
from specs import *


class Forest():
   def __init__(self, spec, path, ps=[], zipper=None, fs=None, log=None):
      self.spec = spec

      self.p = path
      self.ps = [path] if ps == [] else ps
      self.zipper = Zipper(cur = self.spec) if zipper == None else zipper
      self.fs = MemoryFilesystem(self.p) if fs == None else fs
      self.log = [] if log == None else log

   def up(self):
      self.p = dirname(self.p)
      self.z = self.z.ancestor()


   def down(self):
      cur = self.zipper.current()
      if isinstance(cur, Path):
         u = cur.get_exp()
         s = cur.get_subspec()

         self.log.append(ReadFile(self.p))
         self.p = self.p + '/' + u
         self.ps.append(self.p)
         self.zipper = Zipper(cur=s, anc=self.zipper)
      else:
         raise Exception('down not at a path')

   def into_pair(self):
      cur = self.zipper.current()
      if isinstance(cur, Pair):
         s1 = cur.leftspec()
         s2 = cur.rightspec()

         f1 = Forest(s1, self.p, self.ps, fs=self.fs, log=self.log)
         s2 = s2(f1)

         self.zipper = Zipper(cur=s1, right=[s2], anc=self.zipper)
      else:
         raise Exception('into_pair not at a pair')

   def into_comp(self):
      cur = self.zipper.current()
      if isinstance(cur, Comp):
         s = cur.get_subspec()
         us = cur.gen()
         cur_u = us[0]
         del us[0]
         s1 = s(cur_u)
         ss = [s(u) for u in us]

         self.zipper = Zipper(cur=s1, right=ss, anc=self.zipper)
      else:
         raise Exception('into_comp not at a comp')

   def into_opt(self):
      cur = self.zipper.current()
      if isinstance(cur, Opt):
         s = cur.get_subspec()
         self.zipper = Zipper(cur=s, anc=self.zipper)
      else:
         raise Exception('into_opt not at a opt')

   def out(self):
      self.zipper = self.zipper.ancestor()

   def general_into(self):
      cur = self.zipper.current()
      if isinstance(cur, Path):
         self.down()
      elif isinstance(cur, Pair):
         self.into_pair()
      elif isinstance(cur, Comp):
         self.into_comp()
      elif isinstance(cur, Opt):
         self.into_opt()
      else:
         raise Exception('general_into not at a path, pair, comp, opt')

   def general_out(self):
      anc = self.zipper.ancestor()
      if isinstance(anc, Path):
         self.up()
      elif isinstance(anc, Pair) or isinstance(anc, Comp) or isinstance(anc, Opt):
         self.out()
      else:
         raise Exception('general_out not at a path, pair, comp, opt')

   def next(self):
      self.zipper.right()

   def prev(self):
      self.zipper.left()

   def fetch_file(self):
      cur = self.zipper.current()
      if isinstance(cur, File):
         return self.fetch()
      else:
         raise Exception('fetch_file used not at a file')

   def fetch_dir(self):
      cur = self.zipper.current()
      if isinstance(cur, Dir):
         return self.fetch()
      else:
         raise Exception('fetch_dir used not at a dir')

   def fetch_pair(self):
      cur = self.zipper.current()
      if isinstance(cur, Pair):
         return self.fetch()
      else:
         raise Exception('fetch_pair used not at a pair')

   def fetch_path(self):
      cur = self.zipper.current()
      if isinstance(cur, Path):
         return self.fetch()
      else:
         raise Exception('fetch_path used not at a path')

   def fetch_comp(self):
      cur = self.zipper.current()
      if isinstance(cur, Comp):
         return self.fetch()
      else:
         raise Exception('fetch_comp used not at a comp')

   def fetch_opt(self):
      cur = self.zipper.current()
      if isinstance(cur, Opt):
         return self.fetch()
      else:
         raise Exception('fetch_opt used not at a opt')

   def fetch_pred(self):
      cur = self.zipper.current()
      if isinstance(cur, Pred):
         return self.fetch()
      else:
         raise Exception('fetch_pred used not at a pred')

   def fetch(self):
      cur = self.zipper.current()
      return cur.fetch(self.fs, self.p, self.log)

   def store_file(self, u):
      cur = self.zipper.current()
      if isinstance(cur, File):
         contents = FileContens(u)
         self.fs[self.p] = contents
         self.log = self.log + fs.get_log()
      else:
         raise Exception('store_file used not at a file')

   def store_dir(self, s):
      cur = self.zipper.current()
      if isinstance(cur, Dir):
         contents = DirContents(s)
         self.fs[self.p] = contents
         self.log = self.log + fs.get_log()
      else:
         raise Exception('store_dir used not at a dir')

   def create_path(self):
      cur = self.zipper.current()
      if isinstance(cur, Path):
         u = cur.get_exp()
         new_path = join(self.p, [u])
         contents = FileContens('')

         self.fs[new_path] = contents

         self.log = self.log + fs.get_log()
         self.log.append(Read(self.p))
      else:
         raise Exception('store_path used not at a path')

   def goto_name_comp(self, name):
      cur = self.zipper.current()
      if isinstance(cur, Comp):
         s = cur.get_subspec()
         us = cur.gen()
         i = us.index(name)
         left_us = [s(us[j]) for j in range(0, i)]
         s1 = s(name)
         right_us = [s(us[j]) for j in range(i+1, len(us))]

         self.zipper = Zipper(cur=s1, left=left_us, right=right_us, anc=self.zipper)
      else:
         raise Exception('goto_name_comp not at a comp')

   def goto_name(self):
      self.goto_name_comp()

   def goto_pos_comp(self, i):
      cur = self.zipper.current()
      if isinstance(cur, Comp):
         s = cur.get_subspec()
         us = cur.gen()
         left_us = [s(us[j]) for j in range(0, i)]
         s1 = s(us[i])
         right_us = [s(us[j]) for j in range(i+1, len(us))]

         self.zipper = Zipper(cur=s1, left=left_us, right=right_us, anc=self.zipper)
      else:
         raise Exception('goto_pos_comp not at a comp')

   def goto_pos_pairs(self, i):
      cur = self.zipper.current()
      if isinstance(cur, Pair):
         if i == 0:
            self.into_pair()
         else:
            self.into_pair()
            self.next()
            goto_pos_pairs(i-1)
      else:
         raise Exception('goto_pos_pairs not at a pair')

   def goto_position(self, i):
      cur = self.zipper.current()
      if isinstance(cur, Comp):
         self.goto_pos_comp(i)
      elif isinstance(cur, Pair):
         self.goto_pos_pairs(i)
      else:
         raise Exception('goto_name not at a comp or pair')

   def fold(self):
      pass

   def map(self):
      pass


   def commit(self):
      pass




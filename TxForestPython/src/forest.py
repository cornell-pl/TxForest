


from os.path import *

from fetchrep import *
from zipper import *
from filesystems import *
from log import *
from specs import *
from client import ForestClient

import os


class Forest():
   '''forest api: this is an object oriented twist on the forest dsl

      instead of executing commands with a conext as in the functional forest
      dsl, you construct an object which represents that context, and then
      call methods on it to alter than context.

      This class represents the context for forest.

      this is a higher order embeding of the langauage, so vraiables in forest
      turn into lambas where they are used

      In addition to the standard navigation, storage, fetches
      this also includes some nicer operations:
      - goto_position, go to the ith item in a comprehension, or the ith
      nested pair.
      - goto_name, goto the child with a certain name in a comprhension

      TODO: implement mapreduce for map and fold on this for nicer interaction

      constructor: to construct a forest obect pass in a Spec and string path
      for the root of the filesystem, the other optional parameters should
      not be passed in, they are only used when constructing a forest object
      from another (i.e. for dependent pairs)

      - example:
         spec = Spec(...)
         path = "/..."
         forest = Forest(spec, path)
   '''

   def __init__(self, spec, path, ps=[], zipper=None, fs=None, log=None):
      self.spec = spec

      self.client = ForestClient() if fs == None else None
      cid = None if self.client == None else self.client.getid()

      self.p = path
      self.ps = [path] if ps == [] else ps
      self.z = Zipper(cur = self.spec) if zipper == None else zipper
      self.fs = PosixFilesystem(self.p, cid) if fs == None else fs
      self.log = [] if log == None else log


   def up(self):
      '''Move out of the subspec of a path spec, the dual is down
         - returns: None
         - raises:

         - example:
            name = "..."
            subspec = Spec(...)
            spec = Path(name, subspec)
            path = "/..."
            forest = Forest(spec, path)
            forest.down()
            forest.up()
      '''
      self.p = dirname(self.p)
      self.z = self.z.ancestor()

   def down(self):
      '''Move into the subspec of a path spec, the dual is up
         - returns: None
         - raises:

         - example:
            name = "..."
            subspec = Spec(...)
            spec = Path(name, subspec)
            path = "/..."
            forest = Forest(spec, path)
            forest.down()
            forest.up()
      '''
      cur = self.z.current()
      if isinstance(cur, Path):
         u = cur.get_exp()
         s = cur.get_subspec()

         self.log.append(ReadFile(self.p))
         self.p = join(self.p, u)
         self.ps.append(self.p)
         self.fs.goto(u)
         self.z = Zipper(cur=s, anc=self.z)
         self.fetch()
      else:
         raise Exception('down not at a path')

   def into_pair(self):
      '''Move into the left subspec of a pair spec, the dual is out
         - returns: None
         - raises:

         - example:
            subspec = Spec(...)
            spec = Pair(subspec, subspec)
            path = "/..."
            forest = Forest(spec, path)
            forest.into_pair()
            forest.out()
      '''
      cur = self.z.current()
      if isinstance(cur, Pair):
         s1 = cur.leftspec()
         s2 = cur.rightspec()

         f1 = Forest(s1, self.p, self.ps, fs=self.fs, log=self.log)
         s2 = s2(f1)

         self.z = Zipper(cur=s1, right=[s2], anc=self.z)
      else:
         raise Exception('into_pair not at a pair')

   def into_comp(self):
      '''Move into the first subspec of a comp spec, the dual is out
         - returns: None
         - raises:

         - example:
            subspec = Spec(...)
            spec = Comp(subspec, [...])
            path = "/..."
            forest = Forest(spec, path)
            forest.into_comp()
            forest.out()
      '''
      cur = self.z.current()

      if isinstance(cur, RegexComp):
         s = cur.get_subspec()
         us = cur.gen(self.fs, self.p)
         cur_u = us[0]
         del us[0]
         s1 = s(cur_u)
         ss = [s(u) for u in us]

         self.z = Zipper(cur=s1, left=[], right=ss, anc=self.z)
      elif isinstance(cur, Comp):
         s = cur.get_subspec()
         us = cur.gen()
         cur_u = us[0]
         del us[0]
         s1 = s(cur_u)
         ss = [s(u) for u in us]

         self.z = Zipper(cur=s1, left=[], right=ss, anc=self.z)
      else:
         raise Exception('into_comp not at a comp')

   def into_opt(self):
      '''Move into the subspec of a opt spec, if it exists, the dual is out
         - returns: None
         - raises:

         -example:
            subspec = Spec(...)
            spec = Opt(subspec)
            path = "/..."
            forest = Forest(spec, path)
            forest.into_opt()
            forest.out()
      '''
      cur = self.z.current()
      if isinstance(cur, Opt):
         s = cur.get_subspec()
         self.z = Zipper(cur=s, anc=self.z)
      else:
         raise Exception('into_opt not at a opt')

   def out(self):
      '''Move out of a subspec of a non Path spec, the duals are
         into_pair, into_opt, into_comp
         - returns: None
         - raises:

         example:
         subspec = Spec(...)
         spec = Comp(subspec, [...])
         path = "/..."
         forest = Forest(spec, path)
         forest.into_comp()
         forest.out()
      '''
      self.z = self.z.ancestor()

   def general_into(self):
      '''Move into of a subspec of a spec, the dual is general_into
         - returns: None
         - raises:

         example:
         subspec = Spec(...)
         spec = Comp(subspec, [...])
         path = "/..."
         forest = Forest(spec, path)
         forest.general_into()
         forest.general_out()
      '''
      cur = self.z.current()
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
      '''Move out of a subspec of a spec, the dual is general_into
         - returns: None
         - raises:

         - example:
            subspec = Spec(...)
            spec = Comp(subspec, [...])
            path = "/..."
            forest = Forest(spec, path)
            forest.general_into()
            forest.general_out()
      '''
      anc = self.z.ancestor().current()
      if isinstance(anc, Path):
         self.up()
      elif isinstance(anc, Pair) or isinstance(anc, Comp) or isinstance(anc, Opt):
         self.out()
      else:
         raise Exception('general_out not at a path, pair, comp, opt')

   def next(self):
      '''Move to the next subspec of a pair or comprehension, the dual is prev
         - returns: None
         - raises:

         - example:
            subspec = Spec(...)
            spec = Comp(subspec, [...])
            path = "/..."
            forest = Forest(spec, path)
            forest.general_into()
            forest.next()
            forest.prev()
      '''
      self.z.right()

   def prev(self):
      '''Move to the previous subspec of a pair or comprehension, the dual
         is next
         - returns: None
         - raises:

         - example:
            subspec = Spec(...)
            spec = Comp(subspec, [...])
            path = "/..."
            forest = Forest(spec, path)
            forest.general_into()
            forest.next()
            forest.prev()
      '''
      self.z.left()

   def fetch_file(self):
      ''' Reads the contents of a File of a File spec
         - returns: FileRep(contents) where contents is a string of
                    the contents of the file
         - raises: Exception if the current spec is not a File

         - example:
            spec = File()
            path = "/..."
            forest = Forest(spec, path)
            forest.fetch_file()
      '''
      cur = self.z.current()
      if isinstance(cur, File):
         return self.fetch()
      else:
         raise Exception('fetch_file used not at a file')

   def fetch_dir(self):
      ''' Reads the contents of a directory of a Dir Spec
         - returns: DirRep(contents) where contents is a list of the
                    names of the files with in this directory
         - raises: Exception if the current spec is not a Dir

         - example:
            spec = Dir()
            path = "/..."
            forest = Forest(spec, path)
            forest.fetch_dir()
      '''
      cur = self.z.current()
      if isinstance(cur, Dir):
         return self.fetch()
      else:
         raise Exception('fetch_dir used not at a dir')

   def fetch_pair(self):
      ''' returns an empty class if the current spec is a Pair
         - returns: PairRep()
         - raises: Exception if the current spec is not a Pair

         - example:
            subspec = Spec(...)
            spec = Pair(subspec, lambda x: subspec )
            path = "/..."
            forest = Forest(spec, path)
            forest.fetch_pair()
      '''
      cur = self.z.current()
      if isinstance(cur, Pair):
         return self.fetch()
      else:
         raise Exception('fetch_pair used not at a pair')

   def fetch_path(self):
      ''' reads the name of the subspec of a Path spec
         - returns: PathRep(name) where name is the name of the subspec
         - raises: Exception if the current spec is not a PAth

         - example:
            name = "...""
            subspec = Spec(...)
            spec = Path(name, subspec )
            path = "/..."
            forest = Forest(spec, path)
            forest.fetch_path()
      '''
      cur = self.z.current()
      if isinstance(cur, Path):
         return self.fetch()
      else:
         raise Exception('fetch_path used not at a path')

   def fetch_comp(self):
      ''' reads the names of the children of a Comprehension spec
         - returns: CompRep(names) where names is a list are the string
                    names of the children of the comprehension
         - raises: Exception if the current spec is not a Comp

         - example:
            names = [...]
            subspec = Spec(...)
            spec = Comp(subspec, lambda: names)
            path = "/..."
            forest = Forest(spec, path)
            forest.fetch_comp()
      '''
      cur = self.z.current()
      if isinstance(cur, Comp):
         return self.fetch()
      else:
         raise Exception('fetch_comp used not at a comp')

   def fetch_opt(self):
      ''' reads a boolean representing if the subspec of an Opt exists
         - returns: OptRep(exist) where exist is a boolean indicating if
                    the subspec exists
         - raises: Exception if the current spec is not a Opt

         - example:
            subspec = Spec(...)
            spec = Opt(subspec)
            path = "/..."
            forest = Forest(spec, path)
            forest.fetch_opt()
      '''
      cur = self.z.current()
      if isinstance(cur, Opt):
         return self.fetch()
      else:
         raise Exception('fetch_opt used not at a opt')

   def fetch_pred(self):
      ''' reads a boolean representing if the predicate is true for
          the existing file system for an Pred spec
         - returns: PredRep(values) where value is a boolean indicating if
                    the predicate is true or false
         - raises: Exception if the current spec is not a Pred

         - example:
            predicate = lambda: ...
            spec = Pred(predicate)
            path = "/..."
            forest = Forest(spec, path)
            forest.fetch_pred()
      '''
      cur = self.z.current()
      if isinstance(cur, Pred):
         return self.fetch()
      else:
         raise Exception('fetch_pred used not at a pred')

   def fetch(self):
      ''' reads the current spec, see above fetches for more information
         - returns: FetchRep()
         - raises:

         - example:
            spec = File()
            path = "/..."
            forest = Forest(spec, path)
            forest.fetch()
      '''
      cur = self.z.current()
      return cur.fetch(self.fs, self.p, self.log)

   def store_file(self, u):
      '''writes the string [u] as the contents of the current Spec
         - returns: None
         - raises: Exception if the current spec is not a file

         - example:
            spec = File()
            path = "/..."
            forest = Forest(spec, path)
            forest.store_file('yay')
      '''
      cur = self.z.current()
      if isinstance(cur, File):
         contents = FileContents(u)
         self.fs[self.p] = contents
         self.log = self.log + self.fs.get_log()
      else:
         raise Exception('store_file used not at a file')

   def store_dir(self, s):
      '''creates files for the names in the list [s] as sub-children of the
         current spec
         - returns: None
         - raises: Exception if the current spec is a file (this should
           technically only work at a Dir, but for now it allows you to
           do this anywhere that could plausably be a dir. However writing a
           dir somewhere there shouldnt be could cause future errors)

         - example:
            spec = Dir()
            path = "/..."
            forest = Forest(spec, path)
            forest.store_dir(['yay', 'whoo'])
      '''
      cur = self.z.current()
      if not isinstance(cur, File):
         contents = DirContents(s)
         self.fs[self.p] = contents
         self.log = self.log + self.fs.get_log()
      else:
         raise Exception('store_dir used at a file')

   def create_path(self):
      '''creates a file at the current path
         - returns: None
         - raises: Exception if the current spec is not a Path

         - example:
            name = "..."
            subspec = Spec(...)
            spec = Path(name, Spec)
            path = "/..."
            forest = Forest(spec, path)
            forest.create_path()
      '''
      cur = self.z.current()
      if isinstance(cur, Path):
         u = cur.get_exp()
         new_path = join(self.p, u)
         contents = FileContents('')

         self.fs[new_path] = contents

         self.log = self.log + fs.get_log()
         self.log.append(Read(self.p))
      else:
         raise Exception('store_path used not at a path')

   def goto_name_comp(self, name):
      '''Like into_comp, but goes to the child with name [name] instead of the
         first child
         - returns: None
         - raises: Exception if the cirrent spec is not a Comp or name is not
                   a child of the current comprehension

         - example:
            subspec = Spec(...)
            spec = Comp(subspec, ['yay', ...])
            path = "/..."
            forest = Forest(spec, path)
            forest.goto_name_comp('yay')
      '''
      cur = self.z.current()
      if isinstance(cur, Comp):
         s = cur.get_subspec()
         us = cur.gen(self.fs, self.p) if isinstance(cur, RegexComp) else cur.gen()
         i = us.index(name)
         left_us = [s(us[j]) for j in range(0, i)]
         s1 = s(name)
         right_us = [s(us[j]) for j in range(i+1, len(us))]

         self.z = Zipper(cur=s1, left=left_us, right=right_us, anc=self.z)
      else:
         raise Exception('goto_name_comp not at a comp')

   def goto_name(self, name):
      '''see goto_name_comp
      '''
      self.goto_name_comp(name)

   def goto_pos_comp(self, i):
      '''Like into_comp, but goes to the [i]th child instead of the
         first child
         - returns: None
         - raises: Exception if the current spec is no a Comp or i
                   is larger than the number of children

         - example:
            subspec = Spec(...)
            spec = Comp(subspec, ['yay', 'whoo', ...])
            path = "/..."
            forest = Forest(spec, path)
            forest.goto_pos_comp(1)
      '''
      cur = self.z.current()
      if isinstance(cur, Comp):
         s = cur.get_subspec()
         us = cur.gen(self.fs, self.p) if isinstance(cur, RegexComp) else cur.gen()
         left_us = [s(us[j]) for j in range(0, i)]
         s1 = s(us[i])
         right_us = [s(us[j]) for j in range(i+1, len(us))]

         self.z = Zipper(cur=s1, left=left_us, right=right_us, anc=self.z)
      else:
         raise Exception('goto_pos_comp not at a comp')

   def goto_pos_pairs(self, i):
      '''goes to the [i]th subspec of a chain of nested pairs
         - returns: None
         - raises: Exception if the current spec is no a Pair, or there
                   is no ith nested subspec

         - example:
            subspec = Spec(...)
            spec = Pair(subspec, Pair(subspec, lambda x: subspec))
            path = "/..."
            forest = Forest(spec, path)
            forest.goto_pos_pairs(1)
      '''
      cur = self.z.current()
      if isinstance(cur, Pair):
         if i == 0:
            self.into_pair()
         else:
            self.into_pair()
            self.next()
            self.goto_pos_pairs(i-1)
      else:
         raise Exception('goto_pos_pairs not at a pair')

   def goto_position(self, i):
      '''see goto_pos_pairs and goto_pos_comp
      '''
      cur = self.z.current()
      if isinstance(cur, Comp):
         self.goto_pos_comp(i)
      elif isinstance(cur, Pair):
         self.goto_pos_pairs(i)
      else:
         raise Exception('goto_name not at a comp or pair')

   def commit(self):
      '''commits the global file system or abords depending on the
         severs permission
         - returns: None
         - raises:

         - example:
            subspec = Spec(...)
            spec = Pair(subspec, Pair(subspec, lambda x: subspec))
            path = "/..."
            forest = Forest(spec, path)
            forest.commit()
      '''
      if self.client == None:
         self.client = ForestClient()
      can_commit = self.client.send_commit(self.log)
      if can_commit:
         print 'commiting'
         self.fs.commit()
      else:
         print 'aborting'
         self.fs.sync()

      self.client.send_finish_commit()
      return can_commit


   # attempt at a tree traversal to fetch
   def fetch_tree_one(self): 
      list_dirs = os.walk(self) 
      for root, dirs, files in list_dirs: 
         for d in dirs:
               print os.path.join(root, d)
         for f in files: 
               print os.path.join(root, f) 

   def is_leaf(self, cur):
      # these aren't leaves
      if isinstance(cur, Dir): 
         return isinstance(cur, Dir)
      elif isinstance(cur, File): 
         return isinstance(cur, File)
      elif isinstance(cur, Pair): 
         return isinstance(cur, Pair)
      elif isinstance(cur, Path): 
         return isinstance(cur, Path)
      elif isinstance(cur, Comp): 
         return isinstance(cur, Comp)
      elif isinstance(cur, Opt): 
         return isinstance(cur, Opt)
      elif isinstance(cur, Pred): 
         return isinstance(cur, Pred)
      else: raise Exception('not a leaf')

   def get_children(self, cur):
      # inefficient/can i just use fetch?
      if self.is_leaf(cur):
         if isinstance(cur, Dir): 
            return str(self.fetch_dir())
         elif isinstance(cur, File): 
            return str(self.fetch_file())
         elif isinstance(cur, Pair): 
            return str(self.fetch_pair())
         elif isinstance(cur, Path): 
            return str(self.fetch_path())
         elif isinstance(cur, Comp): 
            return str(self.fetch_comp())
         elif isinstance(cur, Opt): 
            return str(self.fetch_opt())
         elif isinstance(cur, Pred): 
            return str(self.fetch_pred())
         else: raise Exception('can\'t fetch')
      else:
         return self
      # cur = self.z.current()
      # if self.is_leaf(cur):
      #     return str(self.fetch())
      # else: 
      #     return self

   def reduce_f(self, children, cur):
      result = ""
      if self.is_leaf(cur):
         return cur
      else:
         print(result.join(children))

   def fetch_tree_two(self):
      cur = self.z.current()
      self.reduce_f(self.get_children(cur), cur)



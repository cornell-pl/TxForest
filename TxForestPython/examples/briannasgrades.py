
import sys

from src.specs import *
from src.forest import Forest

# 1. Implement a simple function and add it to this interface
#    - Look at forest.py for instructions on what various commands do 
# 2. Implement a function that prints the filestore tree
#    - Hacky solution: Add a print_tree function to each spec in
#      specs.py
#    -- For most specs it would roughly print the contents and call
#       itself on any subspecification (with additional formatting)
#    - Better solution (eventually): Add a traverse function
#      to forest.py and pass in a function that will do the right
#      thing based on the node type
# 3. Make 'make test' work. You don't need to keep all of the tests,
#    just make a single one run (and it can be your own instead of one
#    that already exists).


# CS4999 = dir { 
#   fac is "Faculty" :: Faculty;
#   assign list is "Assignment List" :: assign_list;
#  }
# Faculty = dir { 
#   phd is "PhD" :: file;
#   ug is "Undergrad" :: file;
# }
# assign_list = [ hw :: Assignments | hw <- matches ".*" ]
# Assignments = [ a :: Student | a <- matches ".*"]
# Student = file

class BriannasGrades():
  def __init__(self, path):
    dstudent = File()
    dassignments = RegexComp( lambda a : Path(a, dstudent.desugar()), '.*')
    dassign_list = RegexComp( lambda hw : Path(hw, dassignments.desugar()), '.*')

    dfaculty = Directory({
      'phd' : lambda : Path('PhD.csv', File()),
      'ug' : lambda : Path('Undergrad.csv', File())
    })

    dcs4999 = Directory({
      'fac' : lambda : Path('Faculty', dfaculty.desugar()),
      'assign list' : lambda : Path('Assignment List', dassign_list.desugar())
    })

    # faculty = dfaculty.desugar()
    # assign_list = dassign_list.desugar()
    # assignments = dassignments.desugar()
    # student = dstudent.desugar()
    cs4999 = dcs4999.desugar()
    self.forest = Forest(cs4999, path)

  def ls_pair(self):
    res = self.forest.fetch()
    if isinstance(res, PairRep):
      self.forest.into_pair()
      child = self.forest.fetch()
      if isinstance(child, PathRep):
        child_name = child.get_u()
        self.forest.next()
        to_ret = self.ls_pair().append(child_name)
      else:
        self.forest.next()
        to_ret = self.ls_pair()
      self.forest.out()
      return to_ret
    else:
      return []

  def ls(self):
    res = self.forest.fetch()
    if isinstance(res, DirRep):
      return res.get_contents()
    if isinstance(res, CompRep):
      childrens = []
      num_children = len(res.get_xs())
      self.forest.into_comp()
      for i in range(0, num_children):
        child = self.forest.fetch()
        if isinstance(child, PathRep):
          childrens.append(child.get_u())
        if not i == num_children - 1:
          self.forest.next()
      self.forest.out()
      return childrens
    elif isinstance(res, PairRep):
      res = self.ls_pair()
      res.reverse()
      return res
    return []


  def run(self):
    while True:
      next_line = raw_input('> ')

      s = next_line.split()

      cmd = s[0]
      args = s[1:]

      if cmd == 'exit':
        break
      elif cmd == 'down':
        self.forest.general_into()
      elif cmd == 'up':
        self.forest.general_out()
      elif cmd == 'next':
        self.forest.next()
      elif cmd == 'prev':
        self.forest.prev()
      elif cmd == 'fetch':
        res = self.forest.fetch()
        print res
      elif cmd == 'ls':
        print '    '.join(self.ls())
      elif cmd == 'touch':
        file_name = args[0]
        cur_dir = self.forest.fetch().get_lst()
        cur_dir.append(file_name)
        self.forest.store_dir(cur_dir)
      elif cmd == 'update':
        v = args[0]
        self.forest.store_file(v)
      elif cmd == 'commit':
        self.forest.commit()


def run_operation(path):
  BriannasGrades(path).run()









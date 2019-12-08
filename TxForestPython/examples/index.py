
import sys

from src.forest import Forest
from src.specs import *


def lines(x):
  x.down()
  res = x.fetch_file().get_contents().split('\n')
  x.up()
  return res

dspec = Directory({
  'index' :  lambda : Path('index.txt', File()),
  'dir' :  lambda : Path(
    'dir',
    Comp(
      lambda x : Path(x, File()),
      lambda : lines(dspec['index'])
    ).desugar()
  )
})

spec = dspec.desugar()
# this should look like:
# Pair(
#   Path('index', File),
#   lambda index: Pair(
#     Path('dir', Pair(
#         Dir,
#         lambda this: Comp(
#           lambda name: Path(name, File()),
#           lambda : lines(index)
#         )
#       )
#     ),
#     labda dir: null
#   )
#)


def read_ith_entry(forest, i):
  commited = False
  result = None

  while not commited:

    #Pair(Path, Pair)
    forest.general_into()
    #Path -> Pair
    forest.next()
    #Pair(Path, Null)
    forest.general_into()
    #Path -> Null
    forest.general_into()
    #Pair -> null
    forest.general_into()
    #Dir -> Comp
    forest.next()
    #Dir <- Comp
    forest.goto_position(i)
    #Path
    forest.general_into()
    #File
    result = forest.fetch_file()
    forest.general_out()
    #Path
    forest.general_out()
    #Dir <- Comp
    forest.general_out()
    #Pair
    forest.general_out()
    #Path -> null
    forest.general_out()
    #Path <- Pair
    forest.general_out()
    #Pair(Path, Pair)
    commited = forest.commit()

  print result.get_contents()

def read_named_entry(forest, name):
  commited = False
  result = None

  while not commited:

    #Pair(Path, Pair)
    forest.general_into()
    #Path -> Pair
    forest.next()
    #Pair(Path, Null)
    forest.general_into()
    #Path -> Null
    forest.general_into()
    #Pair -> null
    forest.general_into()
    #Dir -> Comp
    forest.next()
    #Dir <- Comp
    forest.goto_name(name)
    #Path
    forest.general_into()
    #File
    result = forest.fetch_file()
    forest.general_out()
    #Path
    forest.general_out()
    #Dir <- Comp
    forest.general_out()
    #Pair
    forest.general_out()
    #Path -> null
    forest.general_out()
    #Path <- Pair
    forest.general_out()
    #Pair(Path, Pair)
    commited = forest.commit()

  print result.get_contents()

def update_ith_entry(forest, i, contents):
  commited = False
  result = None

  while not commited:

    #Pair(Path, Pair)
    forest.general_into()
    #Path -> Pair
    forest.next()
    #Pair(Path, Null)
    forest.general_into()
    #Path -> Null
    forest.general_into()
    #Pair -> null
    forest.general_into()
    #Dir -> Comp
    forest.next()
    #Dir <- Comp
    forest.goto_position(i)
    #Path
    forest.general_into()
    #File
    result = forest.store_file(contents)
    forest.general_out()
    #Path
    forest.general_out()
    #Dir <- Comp
    forest.general_out()
    #Pair
    forest.general_out()
    #Path -> null
    forest.general_out()
    #Path <- Pair
    forest.general_out()
    #Pair(Path, Pair)
    commited = forest.commit()


def update_named_entry(forest, name, contents):
  commited = False
  result = None

  while not commited:

    #Pair(Path, Pair)
    forest.general_into()
    #Path -> Pair
    forest.next()
    #Pair(Path, Null)
    forest.general_into()
    #Path -> Null
    forest.general_into()
    #Pair -> null
    forest.general_into()
    #Dir -> Comp
    forest.next()
    #Dir <- Comp
    forest.goto_name(name)
    #Path
    forest.general_into()
    #File
    result = forest.store_file(contents)
    forest.general_out()
    #Path
    forest.general_out()
    #Dir <- Comp
    forest.general_out()
    #Pair
    forest.general_out()
    #Path -> null
    forest.general_out()
    #Path <- Pair
    forest.general_out()
    #Pair(Path, Pair)
    commited = forest.commit()

def add_file(forest, name, contents):
  commited = False
  while not commited:
    #Pair(Path, Pair)
    forest.general_into()
    #Path -> Pair
    forest.next()
    #Pair(Path, Null)
    forest.general_into()
    #Path -> null
    forest.general_into()
    #Pair
    forest.general_into()
    #Dir
    old_contents = forest.fetch_dir().get_contents()
    old_contents.append(name)
    forest.store_dir(old_contents)
    forest.general_out()
    #Pair(Dir, Comp)
    forest.general_out()
    #Path
    forest.general_out()
    #Path <- Pair
    forest.prev()
    #Path -> Pair
    forest.general_into()
    #File
    current_contents = forest.fetch_file().get_contents()
    new_contents = current_contents.split()
    new_contents.append(name)
    new_contents = '\n'.join(new_contents)
    forest.store_file(new_contents)
    forest.general_out()
    #Path -> Pair
    forest.next()
    #Path <- Pair
    forest.general_into()
    #Path
    forest.general_into()
    #Pair
    forest.general_into()
    #Dir -> Comp
    forest.next()
    #Comp
    forest.goto_name(name)
    #Path
    forest.general_into()
    #File
    forest.store_file(contents)
    forest.general_out()
    #Path
    forest.general_out()
    #Comp
    forest.general_out()
    #Pair
    forest.general_out()
    #Path -> null
    forest.general_out()
    #Path <- Pair
    forest.general_out()
    #Pair
    commited = forest.commit()


def run_operation(path, op, args):
  forest = Forest(spec, path)
  if op == 0:
    read_ith_entry(forest, int(args[0]))
  elif op == 1:
    read_named_entry(forest, args[0])
  elif op == 2:
    update_ith_entry(forest, int(args[0]), args[1])
  elif op == 3:
    update_named_entry(forest, args[0], args[1])
  elif op == 4:
    add_file(forest, args[0], args[1])
  else:
    print 'Unknown Operation: ' + str(op)






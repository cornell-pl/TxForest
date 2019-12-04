
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
    )
  )
})

spec = dspec.desugar()



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
    #Comp -> Null
    forest.goto_position(i)
    #Path
    forest.general_into()
    #File
    result = forest.fetch_file()
    forest.general_out()
    #Path
    forest.general_out()
    #Comp -> Null
    forest.general_out()
    #Path <- Pair(Comp, Null)
    forest.general_out()
    #Pair(Path, Pair)
    commited = forest.commit()

  print result.get_contents()


def add_file(forest, name, contents):
  commited = False
  while not commited:
    #Pair(Path, Pair)
    forest.general_into()
    #Path -> Pair
    forest.next()
    #Pair(Path, Null)
    forest.general_into()
    #Path
    forest.general_into()
    #Comp
    num = len(forest.fetch_comp().get_contents())
    forest.general_into()
    #Path -> ...
    current_contents = []
    for i in range(num-1):
      current_contents.append(forest.fetch_path().get_contents())
      forest.next()
      #... <- Path -> ...
    current_contents.append(forest.fetch_path().get_contents())
    new_contents = current_contents
    new_contents.append(name)
    forest.general_out()
    #Comp
    forest.store_dir(new_contents)
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
    #Path
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
    add_file(forest, args[0], args[1])

  else:
    print 'Unknown Operation: ' + str(op)






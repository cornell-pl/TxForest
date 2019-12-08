

import sys

from src.forest import Forest
from src.specs import *

#[%%txforest {|
#  hws = directory {
#      max is "max" :: file;
#      students is [student :: file | student <- matches RE "[a-z]+[0-9]+"];
#    }
#
#  grades = [hw :: hws | hw <- matches RE "hw[0-9]+"]
#
#|}]

dhws = Directory(
  'max': lambda: Path('max', File()),
  'students' : lambda: RegexComp(
    lambda student: Path(student, File()),
    '[a-z][a-z_0-9]*'
  ).desugar()
)
hws = dhws.desugar()

dgrades = RegexComp(
  lambda hw: hws,
  'hw[0-9]+'
)
grades = dgrades.desugar()
#Pair(
#   Dir,
#   lambda this: RegexComp(
#     lambda hw: Pair(
#       Path('max', File()),
#       lambda max: Pair(
#         Pair(
#           Dir,
#           lambda this: RegexComp(
#             lambda student: Path(student, File()),
#             '[a-z][a-z_0-9]*'
#           )
#         ),
#         lambda students: null
#       )
#     ),
#     'hw[0-9]+'
#   )
#)



def goto_student(forest, hw, i):
  hw_name = 'hw' + str(hw)
  # Pair
  forest.general_into()
  #Dir -> RegexComp
  forest.next()
  #Dir <- RegexComp
  forest.goto_name(hw_name)
  #Pair
  forest.general_into()
  #Path -> Pair
  forest.next()
  #Path <- Pair
  forest.general_into()
  #Pair -> null
  forest.general_into()
  #Dir -> RegexComp
  forest.next()
  #Dir <- RegexComp
  forest.goto_position(i)
  #Path
  forest.general_into()
  #File


def exit_student(forest):
  #File
  forest.general_out()
  #Path
  forest.general_out()
  #Dir <- RegexComp
  forest.general_out()
  #Pair -> null
  forest.general_out()
  #Path <- Pair
  forest.general_out()
  #Pair
  forest.general_out()
  #RegexComp
  forest.general_out()
  #Pair


def perform_at_student(forest, hw, i, f):
  self.goto_student(forest, hw, i)
  f()
  self.exit_student(forest)

def read_score(forest, hw, i):
  def read():
    forest.fetch_file().get_contents()

  commited = False
  v = None

  while not commited:
    v = perform_at_student(
      forest,
      hw,
      i,
      read
    )

    commited = forest.commit()

  print 'Score: ' + str(v)


def run_operation(path, op, args):
  forest = Forest(grades, path)
  if op == 0:
    read_score(forest, int(args[0]), int(args[1]))
  elif op == 1:
    pass
  elif op == 2:
    pass
  elif op == 3:
    pass
  elif op == 4:
    pass
  else:
    print 'Unknown Operation: ' + str(op)
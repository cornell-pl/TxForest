

import sys

from src.forest import Forest
from src.specs import *

import numpy as np

#[%%txforest {|
#  hws = directory {
#      max is "max" :: file;
#      students is [student :: file | student <- matches RE "[a-z]+[0-9]+"];
#    }
#
#  grades = [hw :: hws | hw <- matches RE "hw[0-9]+"]
#
#|}]

dhws = Directory({
  'max': lambda: Path('max', File()),
  'students' : lambda: RegexComp(
    lambda student: Path(student, File()),
    '[a-z][a-z_0-9]*'
  ).desugar()
}, ['max', 'students'])
hws = dhws.desugar()

dgrades = RegexComp(
  lambda hw: Path(hw, hws),
  'hw[0-9]+'
)
grades = dgrades.desugar()
#Pair(
#   Dir,
#   lambda this: RegexComp(
#     lambda hw: Path(hw, Pair(
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
#     )),
#     'hw[0-9]+'
#   )
#)


def goto_hws(forest, hw_name):
  # Pair
  forest.general_into()
  #Dir -> RegexComp
  forest.next()
  #Dir <- RegexComp
  forest.goto_name(hw_name)
  #Path
  forest.general_into()
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

def exit_hws(forest):
  #Dir <- RegexComp
  forest.general_out()
  #Pair -> null
  forest.general_out()
  #Path <- Pair
  forest.general_out()
  #Pair
  forest.general_out()
  #Path
  forest.general_out()
  #RegexComp
  forest.general_out()
  #Pair

def goto_student(forest, hw, i):
  hw_name = 'hw' + str(hw)
  # Pair
  goto_hws(forest, hw_name)
  #Dir <- RegexComp
  forest.goto_position(i)
  #Path
  forest.general_into()
  #File

def goto_named_student(forest, hw, name):
  hw_name = 'hw' + str(hw)
  # Pair
  goto_hws(forest, hw_name)
  #Dir <- RegexComp
  forest.goto_name(name)
  #Path
  forest.general_into()
  #File

def exit_student(forest):
  #File
  forest.general_out()
  #Path
  forest.general_out()
  #Dir <- RegexComp
  exit_hws(forest)
  # Pair

def perform_at_student(forest, hw, i, f):
  goto_student(forest, hw, i)
  v = f()
  exit_student(forest)
  return v

def perform_at_named_student(forest, hw, name, f):
  goto_named_student(forest, hw, name)
  v = f()
  exit_student(forest)
  return v

def read_score(forest, hw, i):
  def read():
    return forest.fetch_file().get_contents()

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

def read_named_score(forest, hw, name):
  def read():
    return forest.fetch_file().get_contents()

  commited = False
  v = None

  while not commited:
    v = perform_at_named_student(
      forest,
      hw,
      name,
      read
    )

    commited = forest.commit()

  print 'Score: ' + str(v)

def change_score(forest, hw, i, new_score):
  def write():
    forest.store_file(str(new_score))

  commited = False

  while not commited:
    perform_at_student(
      forest,
      hw,
      i,
      write
    )

    commited = forest.commit()

def change_named_score(forest, hw, name, new_score):
  def write():
    forest.store_file(str(new_score))

  commited = False

  while not commited:
    perform_at_named_student(
      forest,
      hw,
      name,
      write
    )

    commited = forest.commit()

def get_hw_scores(forest, hw):
  hw_name = 'hw' + str(hw)
  commited = False
  scores = {}
  while not commited:
    #Pair
    goto_hws(forest, hw_name)
    #RegexComp
    students = forest.fetch_comp().get_contents()
    scores = {}
    for student in students:
      if student == 'max': continue
      #Regex
      forest.goto_name(student)
      #Path
      forest.down()
      #file
      scores[student] = int(forest.fetch_file().get_contents())
      #file
      forest.up()
      #Path
      forest.out()
      #RegexComp
    exit_hws(forest)
    commited = forest.commit()

  return scores

def get_stats(forest, hw):
  scores = get_hw_scores(forest, hw)
  scores = scores.values()
  print 'mean: ' + str(np.mean(scores))
  print 'std dev: ' + str(np.std(scores))
  print 'high: ' + str(max(scores))
  print 'low: ' + str(min(scores))

def run_operation(path, op, args):
  print grades

  forest = Forest(grades, path)

  if op == 0:
    read_score(forest, int(args[0]), int(args[1]))
  elif op == 1:
    read_named_score(forest, int(args[0]), args[1])
  elif op == 2:
    change_score(forest, int(args[0]), int(args[1]), int(args[2]))
  elif op == 3:
    change_named_score(forest, int(args[0]), args[1], int(args[2]))
  elif op == 4:
    get_stats(forest, int(args[0]))
  else:
    print 'Unknown Operation: ' + str(op)
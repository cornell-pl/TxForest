from src.specs import *
from src.forest import Forest

def lines(x):
  #fetch_file (down).split()
  x.down()
  res = x.fetch_file().get_contents().split('\n')
  x.up()
  return res

def test_spec0():
  c = Comp(
    lambda x : Path(x, File()),
    lambda : ['a', 'b', 'c', 'd', 'e']
  )
  print c


def test_spec1():
  a = Pair(NullRep(), lambda x: x)
  print a

def test_spec2():
  spec = Pair(
      Path('index.txt', File()),
      lambda index : Pair(
        Path(
          'dir',
          Comp(
            lambda x : Path(x, File()),
            lambda : lines(index)
          )
        ),
        lambda dir : NullRep()
      )
    )

  print spec


def test_spec3():
  #disclaimer, i am only able to pass strings a the spec for
  #printing purposes, for the __str__ function I only require
  #the subspec objects to have a __str__ function (which all
  #python objects do)
  b = Directory()
  b['a'] = lambda : 'a'
  b['b'] = lambda : 'b'
  b['c'] = lambda : b['b']

  print b.desugar()

def test_spec4():
  dspec = Directory()
  dspec['index'] = lambda : Path('index.txt', File())
  dspec['dir'] = lambda : Path(
    'dir',
    Comp(
      lambda x : Path(x, File()),
      lambda : lines(dspec['index'])
    )
  )

  print dspec.desugar()

def test_spec5():
  dspec_nicer = Directory({
    'index' :  lambda : Path('index.txt', File()),
    'dir' :  lambda : Path(
      'dir',
      Comp(
        lambda x : Path(x, File()),
        lambda : lines(dspec_nicer['index'])
      )
    )
  })

  spec = dspec_nicer.desugar()

  forest = Forest(spec, '/simple')

  print forest.fetch()
  forest.general_into()   # index :: File , Pair
  print forest.fetch()
  forest.general_into()   # File
  print forest.fetch()
  forest.general_out()    # index :: File , Pair
  forest.next()           # Pair
  print forest.fetch()
  forest.general_into()   # dir :: Comp, Null
  print forest.fetch()
  forest.general_into()   # Comp
  print forest.fetch()
  forest.general_into()   # a :: File
  print forest.fetch()
  forest.next()           # b :: File
  print forest.fetch()
  forest.general_into()   # File
  print forest.fetch()
  forest.general_out()    # b :: File
  print forest.fetch()
  forest.prev()           # a :: File
  print forest.fetch()
  forest.general_into()   # File
  print forest.fetch()
  forest.general_out()    # a :: File
  print forest.fetch()
  forest.general_out()    # Comp
  print forest.fetch()
  forest.goto_name('c')   # c :: File
  print forest.fetch()
  forest.general_into()   # File
  print forest.fetch()
  forest.general_out()    # c :: File
  print forest.fetch()
  forest.general_out()    # Comp
  print forest.fetch()
  forest.general_out()    # dir :: Comp, Null
  print forest.fetch()
  forest.general_out()    # index :: File, Pair
  print forest.fetch()
  forest.prev()           # index :: File, Pair
  print forest.fetch()
  forest.general_out()    # Pair
  print forest.fetch()
  forest.goto_position(1) # dir :: Comp, Null
  print forest.fetch()
  forest.down()           # Comp
  print forest.fetch()
  forest.goto_position(1) # b :: File
  print forest.fetch()
  forest.down()           # File
  forest.store_file('banana')
  print forest.fetch()
  forest.up()             # b :: File
  print forest.fetch()
  forest.general_out()    # Comp
  print forest.fetch()
  forest.general_out()    # dir :: Comp, Null
  print forest.fetch()
  forest.general_out()    # index :: File, Pair
  print forest.fetch()
  forest.prev()           # index :: File, Pair
  print forest.fetch()
  forest.down()           # File
  print forest.fetch()
  forest.store_file('a\nb\nc\nd')
  print forest.fetch()
  forest.up()             # index :: File, Pair
  print forest.fetch()
  forest.next()           # Pair
  print forest.fetch()
  forest.general_into()   # dir :: Comp, Null
  print forest.fetch()
  forest.general_into()   # Comp, Null
  print forest.fetch()
  forest.general_into()   # a :: File
  print forest.fetch()
  forest.next()           # b :: File
  print forest.fetch()
  forest.next()           # c :: File
  print forest.fetch()
  forest.next()           # d :: File
  print forest.fetch()
  forest.general_out()    # Comp
  forest.store_dir(['a', 'b', 'c', 'd', 'e'])
  print forest.fetch()
  forest.general_out()    #dir :: Comp, Null
  print forest.fetch()
  forest.general_out()    #index :: File, Pair
  print forest.fetch()
  forest.prev()           #index :: File, Pair
  print forest.fetch()
  forest.down()           #File
  print forest.fetch()
  forest.store_file('a\nb\nc\nd\ne')
  print forest.fetch()
  forest.up()             #index :: File, Pair
  print forest.fetch()
  forest.next()           #index :: File, Pair
  print forest.fetch()
  forest.general_into()   #dir :: Comp, Null
  print forest.fetch()
  forest.down()           #Comp
  print forest.fetch()
  forest.goto_name('e')   # e :: File
  print forest.fetch()
  forest.down()           # File
  print forest.fetch()



if __name__ == '__main__':
  # test_spec0()
  # test_spec1()
  # test_spec2()
  # test_spec3()
  # test_spec4()
  test_spec5()













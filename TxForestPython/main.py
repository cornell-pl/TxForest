from src.specs import *
from src.forest import Forest

def lines(x):
  #fetch_file (down).split()
  return x.fetch_file().get_contents().split('\n')


if __name__ == '__main__':
  # a = Pair(NullRep(), lambda x: x)
  # print a

  # #disclaimer, i am only able to pass strings a the spec for
  # #printing purposes, for the __str__ function I only require
  # #the subspec objects to have a __str__ function (which all
  # #python objects do)
  # b = Directory()
  # b['a'] = lambda : 'a'
  # b['b'] = lambda : 'b'
  # b['c'] = lambda : b['b']

  # print b.desugar()


  # c = Comp(
  #   lambda x : Path(x, File()),
  #   lambda : ['a', 'b', 'c', 'd', 'e']
  # )

  # print c

  # spec = Pair(
  #     Path('index.txt', File()),
  #     lambda index : Pair(
  #       Path(
  #         'dir',
  #         Comp(
  #           lambda x : Path(x, File()),
  #           lambda : lines(index)
  #         )
  #       ),
  #       lambda dir : NullRep()
  #     )
  #   )

  # print spec


  # dspec = Directory()
  # dspec['index'] = lambda : Path('index.txt', File())
  # dspec['dir'] = lambda : Path(
  #   'dir',
  #   Comp(
  #     lambda x : Path(x, File()),
  #     lambda : lines(dspec['index'])
  #   )
  # )

  # print dspec.desugar()

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

  forest = Forest(spec, '/')

  print forest.fetch()
  forest.general_into()
  print forest.fetch()
  forest.general_into()
  print forest.fetch()






from src.specs import *
from src.forest import Forest


def lines(x):
    #fetch_file (down).split()
    x.down()
    res = x.fetch_file().get_contents().split('\n')
    x.up()
    return res


def test_spec0():
    c = Comp(lambda x: Path(x, File()), lambda: ['a', 'b', 'c', 'd', 'e'])
    assert (str(c) == "[var :: file]")
    print "test 0 successful"


def test_spec1():
    a = Pair(NullRep(), lambda x: x)
    assert (str(a) == "( nullrep, nullrep )")
    print "test 1 successful"


def test_spec2():
    spec = Pair(
        Path('index.txt', File()), lambda index: Pair(
            Path('dir', Comp(lambda x: Path(x, File()), lambda: lines(index))),
            lambda dir: NullRep()))

    print spec


def test_spec3():
    #disclaimer, i am only able to pass strings a the spec for
    #printing purposes, for the __str__ function I only require
    #the subspec objects to have a __str__ function (which all
    #python objects do)
    b = Directory()
    b['a'] = lambda: 'a'
    b['b'] = lambda: 'b'
    b['c'] = lambda: b['b']

    print b.desugar()


def test_spec4():
    dspec = Directory()
    dspec['index'] = lambda: Path('index.txt', File())
    dspec['dir'] = lambda: Path(
        'dir', Comp(lambda x: Path(x, File()), lambda: lines(dspec['index'])))

    print dspec.desugar()


def test_spec5(path):
    dspec_nicer = Directory({
        'index':
        lambda: Path('index.txt', File()),
        'dir':
        lambda: Path(
            'dir',
            Comp(lambda x: Path(x, File()), lambda: lines(dspec_nicer['index'])
                 ))
    })

    spec = dspec_nicer.desugar()

    forest = Forest(spec, path)

    print forest.fetch()
    forest.general_into()  # index :: File , Pair
    print forest.fetch()
    # forest.general_into()  # File
    # print forest.fetch()
    # TODO: JDL FIX THIS
    # forest.general_out()  # index :: File , Pair
    forest.next()  # Pair
    print forest.fetch()
    forest.general_into()  # dir :: Comp, Null
    print forest.fetch()
    forest.general_into()  # Comp
    print forest.fetch()
    forest.general_into()  # a :: File
    print forest.fetch()
    forest.next()  # b :: File
    print forest.fetch()
    forest.general_into()  # File
    print forest.fetch()
    # forest.general_out()  # b :: File
    # print forest.fetch()
    # forest.prev()  # a :: File
    # print forest.fetch()
    # forest.general_into()  # File
    # print forest.fetch()
    # forest.general_out()  # a :: File
    # print forest.fetch()
    # forest.general_out()  # Comp
    # print forest.fetch()
    # forest.goto_name('c')  # c :: File
    # print forest.fetch()
    # forest.general_into()  # File
    # print forest.fetch()
    # forest.general_out()  # c :: File
    # print forest.fetch()
    # forest.general_out()  # Comp
    # print forest.fetch()
    # forest.general_out()  # dir :: Comp, Null
    # print forest.fetch()
    # forest.general_out()  # index :: File, Pair
    # print forest.fetch()
    # forest.prev()  # index :: File, Pair
    # print forest.fetch()
    # forest.general_out()  # Pair
    # print forest.fetch()
    # forest.goto_position(1)  # dir :: Comp, Null
    # print forest.fetch()
    # forest.down()  # Comp
    # print forest.fetch()
    # forest.goto_position(1)  # b :: File
    # print forest.fetch()
    # forest.down()  # File
    # forest.store_file('banana')
    # print forest.fetch()
    # forest.up()  # b :: File
    # print forest.fetch()
    # forest.general_out()  # Comp
    # print forest.fetch()
    # forest.general_out()  # dir :: Comp, Null
    # print forest.fetch()
    # forest.general_out()  # index :: File, Pair
    # print forest.fetch()
    # forest.prev()  # index :: File, Pair
    # print forest.fetch()
    # forest.down()  # File
    # print forest.fetch()
    # forest.store_file('a\nb\nc\nd')
    # print forest.fetch()
    # forest.up()  # index :: File, Pair
    # print forest.fetch()
    # forest.next()  # Pair
    # print forest.fetch()
    # forest.general_into()  # dir :: Comp, Null
    # print forest.fetch()
    # forest.general_into()  # Comp, Null
    # print forest.fetch()
    # forest.general_into()  # a :: File
    # print forest.fetch()
    # forest.next()  # b :: File
    # print forest.fetch()
    # forest.next()  # c :: File
    # print forest.fetch()
    # forest.next()  # d :: File
    # print forest.fetch()
    # forest.general_out()  # Comp
    # forest.store_dir(['a', 'b', 'c', 'd', 'e'])
    # print forest.fetch()
    # forest.general_out()  #dir :: Comp, Null
    # print forest.fetch()
    # forest.general_out()  #index :: File, Pair
    # print forest.fetch()
    # forest.prev()  #index :: File, Pair
    # print forest.fetch()
    # forest.down()  #File
    # print forest.fetch()
    # forest.store_file('a\nb\nc\nd\ne')
    # print forest.fetch()
    # forest.up()  #index :: File, Pair
    # print forest.fetch()
    # forest.next()  #index :: File, Pair
    # print forest.fetch()
    # forest.general_into()  #dir :: Comp, Null
    # print forest.fetch()
    # forest.down()  #Comp
    # print forest.fetch()
    # forest.goto_name('e')  # e :: File
    # print forest.fetch()
    # forest.down()  # File
    # print forest.fetch()


def test_spec6(path):
    dspec_nicer = Directory({
        'index':
        lambda: Path('index.txt', File()),
        'dir':
        lambda: Path(
            'dir',
            Comp(lambda x: Path(x, File()), lambda: lines(dspec_nicer['index'])
                 ))
    })

    spec = dspec_nicer.desugar()

    forest = Forest(spec, path)

    print forest.fetch()
    forest.general_into()  # index :: File , Pair
    print forest.fetch()
    forest.next()  # Pair
    print forest.fetch()
    forest.general_into()  # dir :: Comp, Null
    print forest.fetch()
    forest.general_into()  # Comp
    print forest.fetch()
    forest.general_into()  # a :: File
    print forest.fetch()
    forest.next()  # b :: File
    print forest.fetch()
    forest.general_into()  # File
    print forest.fetch()
    forest.commit()


def test_spec7(path):
    dspec_nicer = Directory({
        'index':
        lambda: Path('index.txt', File()),
        'dir':
        lambda: Path(
            'dir',
            Comp(lambda x: Path(x, File()), lambda: lines(dspec_nicer['index'])
                 ))
    })

    spec = dspec_nicer.desugar()

    forest = Forest(spec, path)

    print forest.fetch()
    forest.general_into()  # index :: File , Pair
    print forest.fetch()
    forest.next()  # Pair
    print forest.fetch()
    forest.general_into()  # dir :: Comp, Null
    print forest.fetch()
    forest.general_into()  # Comp
    print forest.fetch()
    forest.general_into()  # a :: File
    print forest.fetch()
    forest.next()  # b :: File
    print forest.fetch()
    forest.general_into()  # File
    print forest.fetch()
    forest.store_file('banana')
    print forest.fetch()
    forest.commit()


#################################


def test_spec8(path):
    print "begin test 8"
    dspec_nicer = Directory({'index': lambda: Path('index.txt', File())})
    spec = dspec_nicer.desugar()
    forest = Forest(spec, path)
    print forest.traverse()  # "pair\npath index.txt\nfile a\nb\nc\nd\n\nNone"


def test_spec9(path):
    print "begin test 9"
    dspec_nicer = Directory({
        'index1': lambda: Path('index.txt', File()),
        'index2': lambda: Path('index.txt', File())
    })
    spec = dspec_nicer.desugar()
    forest = Forest(spec, path)
    # "pair\npath index.txt\nfile a\nb\nc\nd\n
    #  pair\npath index.txt\nfile a\nb\nc\nd\nNone\nNone"
    print forest.traverse


def test_spec10(path):
    dspec_nicer = Directory({
        'index':
        lambda: Path('index.txt', File()),
        'dir':
        lambda: Path(
            'dir',
            Comp(lambda x: Path(x, File()), lambda: lines(dspec_nicer['index'])
                 ))
    })

    spec = dspec_nicer.desugar()
    forest = Forest(spec, path)
    print forest.traverse
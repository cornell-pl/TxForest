


Exposed Class hiearchy:

    Spec()
      File()
      Dir()
      Path(name : string , sub_spec : Spec )
      Pair(sub_spec1 : Spec, x_to_sub_spec2 : Spec -> Spec)
      Comp(x_to_sub_spec : string -> Spec, membership_function : () -> string list)
      Opt(sub_spec : Spec)
      Pred(test : bool)

    Forest(spec: Spec, path: string)
      - up
      - down
      - into
      - into____
      - ...
      - out
      - next
      - prev
      - fetch
      - fetch_____
      - ...
      - store_file
      - store_dir
      - create_path
      - commit
    some other nice things not in the core
      - goto_name
      - goto_position
      - fold
      - map

Background Classes:

    Zipper(cur: Spec,Env, left : Spec,Env list, right: Spec,Env list, ancestor: Zipper)
      - up
      - down
      - left
      - right
      - goto_name

    Log(path: string)

    Env()

    FileSystem(path : string)
      PosixFileSystem(path : string)
      InMemoryFileSystem(path : string)

Example:

Ocaml Version:
```
  spec = directory {
    index is "index.txt" :: file;
    dir is "dir" :: [x :: file | x <- $down index |> fetch_file |> lines$ ]
  }


  let get_info_for_name (name:string) z =
    goto "dir" z
    >>= down
    >>= goto_name name
    >>= down
    >>= print_file
```

Python Version:
```
spec =
    Pair(
      Path('index.txt', File()),
      lambda index : Pair(
        Path(
          'dir',
          Comp(
            lambda x : Path(x, File()),
            lambda () : lines (fetch_file (down index))
          )
        ),
        lambda x : null
      )
    )


function get_info_for_name (name : string) (forest : Forest) =
  forest.into_pair()
  forest.next()
  forest.into_pair()
  forest.down()
  forest.goto_name(name)
  forest.down()
  print forest.fetch_file()
```


To make this a little nicer we can add a `Directory` subclass to Spec.
We want this to take mapping from names to tunk to spec, which seems difficult in python, we will get there, but consider having to add each name spec pair. Then we can override the indexing functionality with something that returns the variable for zipper for the spec mapped to the name.

```
temp = Directory()
temp.add('index', lambda () : Path('index.txt', File()))
temp.add('dir', lambda () :
  Path(
    'dir',
    Comp(
      lambda x : Path(x, File()),
      lambda () : lines (fetch_file (down temp['index'])),
    )
  )
)
spec = temp.desugar()
```

Then we can just pass a dictionary into Directory on initalization and use iterative add for each of the items

```
temp = Directory({
    'index' :  lambda () : Path('index.txt', File()),
    'dir' :  lambda () : Path(
      'dir',
      Comp(
        lambda x : Path(x, File()),
        lambda () : lines(temp['index'])
      )
    )
  })
spec = temp.desugar()
```

We could also have subclasses like `RegexComp` or `GlobComp` matching the ocaml to help make writing things like this more suscinct


project plan:
- implment the spec hierarchy
- implement forest local transaction semantics (with logging)
- implement simple in memory filesystem to test on
- implement global log, and transaction log checking
- implement server/client like in ocaml
- write some examples on the python version
- implement fs to connect to system fs
- add some nice semantic things
  - pipe for functions would be v nice
  - paths with /






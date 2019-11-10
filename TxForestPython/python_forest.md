


Exposed Class hiearchy:

    Spec()
      File()
      Dir()
      Path(name : string , sub_spec : Spec )
      Pair(sub_spec1 : Spec, x_to_sub_spec2 : Spec -> Spec)
      Comp(x_to_sub_spec : Spec -> Spec, membership_function : () -> string list)
      Opt(sub_spec : Spec)
      Pred(test : () -> string list)

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


To make this a little nicer we can add `Directory(specs : string,Spec list)` to the Spec, which we just turn into the dpairs this chould make the spec looks a little nicer, not sure if this is possible with higher order embeding


```
spec = Directory()
spec.addToDirectory('index', lambda () : Path(lambda x : 'index.txt', File()))
spec.addToDirectory('dir', lambda () :
  Path(
    'dir',
    Comp(
      lambda x : Path(x, File()),
      lambda () : lines (fetch_file (down spec['index'])),
    )
  )
)
```

For comprehensions we could also have subclasses like `RegexComp` or `GlobComp` to help make writing things like this more suscinct


- higher order abstract syntax
- make the second part of the pair a function and the first part of the comprehension a function instead, so that the variables are more natural
to work with
- add back predicates




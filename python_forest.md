


Exposed Class hiearchy:

    Spec()
      File()
      Dir()
      Path(name : env -> string , sub_spec : Spec )
      Pair(var : string , sub_spec1 : Spec, sub_spec2 : Spec)
      Comp(sub_spec : Spec, var : string, membership_function : fs, env -> string list)
      Opt(sub_spec : Spec)

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
      'index'
      Path(lambda x : 'index.txt', File()),
      Pair(
        'dir',
        Path(
          lambda x : 'dir',
          Comp(
            Path(lambda env : env['x'], File()),
            'x',
            lambda env fs : lines (fetch_file (down env['index'])),
        null
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


To make this a little nicer we can add `Directory(specs : string,Spec list)` to the Spec, which we just turn into the dpairs this chould make the spec looks a little nicer


```
spec =
    Directory([
      ('index', Path(lambda x : 'index.txt', File())),
      ('dir',
        Path(
          lambda x : 'dir',
          Comp(
            Path(lambda env : env['x'], File()),
            'x',
            lambda env fs : lines (fetch_file (down env['index'])),
          )
        )
      )
    ])
```






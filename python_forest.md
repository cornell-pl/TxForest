


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
      - into____
      - out
      - next
      - prev
      - fetch
      - fetch_file
      - fetch ...
      - ...
      - store_file
      - store_dir
      - create_path
      - commit

Background Classes:

    Zipper(cur: Spec,Env, left : Spec,Env list, right: Spec,Env list, ancestor: Zipper)
      - up
      - down
      - left
      - right
      - goto



Example:

```
  d = directory {
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


```
spec =
  Pair(
    'd',
    Pair(
      'index'
      Path(_ -> 'index.txt', File()),
      Pair(
        'dir',
        Path(_ -> 'dir', Comp(Path(env -> env['x'], File()), 'x', env, fs -> lines (fetch_file (down env['index'])),
        null
      )
    ),
    null
  )


function get_info_for_name (name : string) (f : Forest) =
  forest.into_pair()
  forest.into_pair()
  forest.next()
  forest.into_pair()
  forest.goto(name)
  print forest.fetch_file()
```







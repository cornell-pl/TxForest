open Core
open Rawforest
open Utils
open Forest
open ForestIntf
open ForestIntf.ForestCoreExn
open Result

let i = "index"


[%%txforest {|
  d = directory {
      index is i :: file;
      data is [x :: file | x <- matches GL "*" , $x = "foo"$ ];
    }

  f = directory {
      x is "foo" :: file where $fetch_file this = "yay"$;
      y is $down x |> fetch_file$ :: file option
    }
|}]


[%%txforest {|

  hellofile = "hello" :: file

|}]

[%%txforest {|

  ofile = "hello" :: file option

|}]


[%%txforest {|

  afile = file

  d = directory { foo is "ack" :: file ;
                  baz is "bar" :: afile
                }

|}]


[%%txforest {|

  c1 = [ x :: file | x <- $String.Set.of_list ["foo";"bar";"etc"]$ ]

|}]


[%%txforest {|

  c2 = [x :: file | x <- matches GL "*.txt"]

|}]
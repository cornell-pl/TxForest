- so there are 2 concerns:
  1. converting the forest library into one accessable in python
  2. making nice syntax to use the library in python

1. there are options in two viens here
- some wrapper on the ocaml library that makes it usable in python
  - there are libraries to do this, like the one from jane street:
  [ocaml in python](https://github.com/janestreet/pythonlib)
  - most only handle simple types, and thus we have to serialize between the versions
  - something big to consider with this version is that the solutions to do this often require the person running the python to have ocaml
  - this would require users to do a lot of work to use the python, which defeats the purpose of implementing in python i think


- an alternative is to reimplement in python
 - though this requires duplication and we cant rely on what we have built
 - this would make the library readable for a user, if they wanna see whats going on
 - simplier install
 - if were going for making it easiest on the user this would probably be easier


2. this somewhat depends on what we do for 1.
- if we keep the ocaml around, we already will have a server in the ocaml version to allow multiple clients to connect at once.
  - we could push the specification from the client to server side, thus we could write the spec as a string in python, send it to ocaml where its converted to a full spec
  - then the python client just sends messages to the ocaml

- wether we keep the ocaml around there is the option to embed th language in python, but this would require recompiling the python interpreter and complicated install, which would be very prohibitive


- there defintly are ways to hyjack operators using pythons special functions here an example: [operators with special function](https://thepythonguru.com/python-operator-overloading/)
  - there are limits to this, for example with comrehensions, we would have to require the right part of the comrehension is writen as a function, so that we can have delayed evaluation, and it doesnt seem theres a way around this with only hyjacking operators



Overall I think the two nicest options are:

1. reimplement forest in python and overload operators using special functions to make the syntax nicer
  - con limit to how nice the spec syntax is
  - pro easy install, easier for a python person to use

2. keep the ocaml around and use the write the spec in a string, send this and commands to the ocaml server side
  - con our basic python programmer has to venture into the world of ocaml and opam to get the server side stuff running
  - pro: full syntax, and "simple" on python client side





&nbsp;

&nbsp;

&nbsp;

&nbsp;

&nbsp;

&nbsp;

&nbsp;

&nbsp;

&nbsp;

&nbsp;

&nbsp;

&nbsp;


&nbsp;

&nbsp;

&nbsp;

&nbsp;

&nbsp;

&nbsp;




- ok so there is something that should be noted:
  -  if we want to leverage the ocaml implementation, the user is gonna have to have ocaml, I havent found options that allow us to compile (transpile?) out the ocaml.
  - and tbh if we are targeting the normal python user, i dont think they are gonna wanna deal with going through the steps to install ocaml to run their python
  - if we really want the naive python user, it would probably be better to re write this as a python lib and then all they would have to do to use it is pip install which is familiar to them.
  - then we might be able to play some nice syntax tricks to make it easier to write the specification




- wrapper on the forest ocaml implementation (this could be supplemented with something like below to get nice syntax):
- here is a janestreet lib for doing so:
  - [ocaml in python](https://github.com/janestreet/pythonlib)
  - however this only works with the basic types (int float, string, list)
  - so we would have to serialize objects between the two
  - but functions would be problematic and so this may be an issue with comrehensions
  - this seesm to just use the ocaml top level, which is why it requires us to keep ocaml around

- theres another option in this vien which is a c wapper for ocaml and then that is called in python since its esier to link python and c. makes the process way more complicated, and seroalization would still be needed, though this might be able to remove the need for the end user to have ocamal and all installed


- extending the python langauge:
  1. requires recompiling python and is not cross platform, not ideal
  [extension binaryies](https://packaging.python.org/guides/packaging-binary-extensions/)
  2. here is a cross platform way to do the above, but also requires recompiling the python interpreter
  [c extensions](https://docs.python.org/3/extending/)
  3. theres some other options for embeing c in python see above but this does not really fit our needs


- embeding the python langauge in our langauge
  1. this is really only supported to embed python in C, which does not really fit the use case


- there seems to be a way to do this with out recompiling:
  - (cyrus's work embeding a langauge in python)[https://github.com/cyrus-/typy]
  - this seems complicated, im not sure if this fits our use case rn


- using the special functions:
  - it is complicated to do in practice
  - from what this article talks about its kinda dangerous to use these
  - because speical functions that you think should call eachother dont there is a lot of copy pasta in these, maybe to reduce the number of function calls and make them faster:
    [special functions](https://treyhunner.com/2019/04/why-you-shouldnt-inherit-from-list-and-dict-in-python/)
  - this means you have to reimplemnt all the functions, or at least all the ones that can interact, which could be quite large
  - additionally you can not use the comprehension sysntax for the object that extends the dictionary, which is more what we are interested in I belive
  - however this can also be used to overload operators which is nice, we could kinda use this to get nicer sytax between objects
    [operators with special function](https://thepythonguru.com/python-operator-overloading/)
    - this still means that they have to create objects



in ocaml we wrote:
```
[%%txforest {|

  d = directory {
    index is "index.txt" :: file;
    dir is "dir" :: [x :: file | x <- $down index |> fetch_file |> lines$ ]
  }

|}]
```

we could do something in python like, with the exception of how to get the zipper for index :(
```
spec = {
  'd' : Directory({
    'index' : Path('index.txt') / File(),
    'dir' : Path('dir') / Comp([Path(x) :: File() for x in lines (fetch_file (down 'index')) ])
  })
}
```

- it also looks like theres an already implemented way to get pipe between functions [pipe operator](http://0101.github.io/pipetools/doc/#contents)
- from looking at this pipe implemntation, it looks like we could also hyjack the / operator with out puting classes on everyting so getting somehthing like
```
spec = {
  'd' : Directory({
    'index' : 'index.txt' / File(),
    'dir' : 'dir' / [Path(x) :: File() for x in down 'index' | fetch_file | lines ]
  })
}
```
- so then to be able to get the zipper for index, we could make the / operator make a zipper and do something like
```
index = 'index.txt' / File(),
dir = 'dir' / [Path(x) :: File() for x in down 'index' | fetch_file | lines ]

d = 'd' / {index, dir}
}
```
- thats requires everything to be a path though, and the expressions will be executed right away, which defeats the purpose of the zipper, so i think the only way around this would be to have the user give functions that take a zipper and return a set for the comprhensions, which takes away some of the nice syntax, but i think would be accptable with the other nice syntax improvements




- another idea I had
- however we already will have a division in client and server in the ocaml version, so the python one could connect to the ocaml server
- would be to write the spec in a string in python, send to ocaml server, where there it uses ppx to convert this to ocaml, hmm actually im not sure about this, i think this is posisble, but idk, and then python client sends commands to the ocaml server, this would simplify our life greatly i think, thinking about it, this might be the way to go
- this requires that they have ocaml installed and be able to run the server, though this seens simple enough and such, but I dont think there is a way to compile (transpile?) out the ocaml, the embeding options that I have found still rely on having ocaml

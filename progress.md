
# Seperating the Forest and ZFS Repos
## previously done

- seperate the code and get them compiling on own with out eachother
  - mostly just dun files and things
- implement simple in memory fs for forest to run on
  - using hashmap like in utils
  - note: this has a bug with the paths that has yet to be fixed
- implement posix version of fs for forest to run on
  - uses Core.Unix and Core.Sys modules to interact with the file system
  - probably needs some extra things so that it can run on both mac and window
- re-organize types so they match the paper more closely and put some in
utils file so they can be shared between modules better
- implement in memory "copy" of posix file system for forest to run on before commiting a transaction (to be changed still)
- add Filesystems module, since forest needs a module for its temporary and
persisitant file systems
- add logging to above file system
- add logging to the forest repo to support transactionality
- add transaction checking logic for commiting / uncommiting in the forest repo
- add final commit logic for updating the global fs for forest
- updated ppx to work with the above scheme

## Last week

- make the local logs mutable to make merging logs easier, and evaluating expressions cleaner
- switch ppx back and update it to work with this version
- implemented server and a version of ForestIntf using this server
- started on the client
- tho disclaimer, im not sure about how correct this is, I didnt get a chance to test it yet, its prolly pretty buggy
  - the scheme i am using is haveing the client send their specification to the server, im not sure if this is the best way to go, but it seems like since the restrictions are enforced on the server side we want the spec to be there, but since we want the server to work with multiple specs we want the client to send to the server the spec

- look more into out options for ppx, it looks like the best approach will be to reimplement forest in python to avoid the python users having to do a complicated install to use our library, expose a bunch of objects for the user to work with and then overload some operators to make working with the objects nicer
  - theres anonther doc with all the stuff from this

## This week

- debuging the server and client
  - had some fun with marshaling not typing things
  - had an issue with the specs, because we have functions in the spec they
  can not be sent through the marshalling well
    - it is possible to send functions, but because the server and client are different pieces of code it messes with the way they serialize and deserialize functions.
    - thus sending the spec in its ocaml form is not really an option
    - I was playing with sending the raw string version of the spec to the servere and then parsing it at run time, but I was having trouble with
    using ppx at running time, it doesnt really seem like it can be used in this way
    - I settled on setting up a module that will contain all the specs, and has a mapping from identifier of spec to the spec.
    - client sends identifier and then server gets spec from the mapping
    - this isnt ideal, but seems like the most extensible way to do this.
  - I debuged all the traversal commands, fetch, and commit (only with a single client tho) and they seem to be behaving properly
  - there was a bug in the FS I had to fix, I was just not intializing the path correctly in the server

- python spec
  - drafted version I showed in the meeting
  - made updates to use higher order embeding
  - thoughts


## Whats next

- push the forst studd client side, so the spec an be there, the fs can be there etc.
- work through the directory add version

- write the updates to a temporary directory and then be moved to the rest of the file system on commit, so that we can spread the writes allong as we do the trasaction and the commiting process can be simplier and more atomic change to using

- clean up the code mode
- testing more

- make a project plan/ more detailed plan for this the python part of the project
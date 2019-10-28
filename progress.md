
# Seperating the Forest and ZFS Repos
## previously done

- seperate the code and get them compiling on own with out eachother
  - mostly just dun files and things
- implement simple in memory fs for forest to run on
  - using hashmap like in utils
  - note: this has a bug with the paths that has yet to be fixed
- implement posix version of fs for forest to run on
  - uses Core.Unix and Core.Sys modules to interact with the file system
  - probably needs some extra things so that it can run on both mac and windows


## Since last time

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

- make the local logs mutable to make merging logs easier, and evaluating expressions cleaner

- look more into out options for ppx, it looks like the best approach will be to reimplement forest in python to avoid the python users having to do a complicated install to use our library, expose a bunch of objects for the user to work with and then overload some operators to make working with the objects nicer

## Whats next

- write the updates to a temporary directory and then be moved to the rest of the file system on commit, so that we can spread the writes allong as we do the trasaction and the commiting process can be simplier and more atomic change to using

- clean up the code
- client and server to make it easier to force conflicts with transactions

- document a design for the object version of forest for python and rewrite some of the simple examples using this scheme
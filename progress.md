
# Seperating the Forest and ZFS Repos
- seperate the code and get them compiling on own with out eachother
  - mostly just dun files and things
- implement simple in memory fs for forest to run on
  - using hashmap like in utils
  - note: this has a bug with the paths that has yet to be fixed
- implement posix version of fs for forest to run on
  - uses Core.Unix and Core.Sys modules to interact with the file system
  - probably needs some extra things so that it can run on both mac and windows


## Updates

- re-organize types so they match the paper more closely and put some in
utils file so they can be shared between modules better
- implement in memory "copy" of posix file system for forest to run on before commiting a transaction
- add Filesystems module, since forest needs a module for its temporary and
persisitant file systems
- add logging to above file system
- add logging to the forest repo to support transactionality
- add transaction checking logic for commiting / uncommiting
- add commit logic for updating the global fs for forest


## Whats next

- need to more throughly go through the file systems and forest code to clean them up and test out a little more to see if any bugs
- also add a server like zfs to make it cleaner for examples to runn on forest?
- a client to make it easier to force conflicts like with zfs
- port all the examples on to this
- the code prolly could use some reoring, its all in the same directory, but spliting up the fs stuff, forest implemenation, server and api would prolly do some good

- look more into the way to make a python wrapper for ocaml
  - plan out what this part of the project will look like
- also look into all the embding options for python
  - should added to interpeter
  - should hyjack operators in python
  - should utilize pythons special functions
  - other options from existing work
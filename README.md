# distro-db
A distributed key-value store written in Haskell

## Usage
- Build a 3 process system (1 master, 2 slave) by the following
command:
```
ghc -Wall db.hs
./db slave 55551 & ./db slave 55552 & ./db master 55553
```
- This will bring a prompt for interacting with the 
database. Notable commands are listed below
```
GET <Key> : Get the value at a given key
SET <Key> <Value> : Set a given key to a given value
KILL <NodeId> : Kill a specific slave process, given its node ID
QUIT : Kill all slave processes and exit from the interactive
      session
```

## Building a Distributed Key-Value Store
_This project is based on an exercise from Simon Marlow's_
Parallel and Concurrent Programming in Haskell

Central to doing anything with the `Control.Distributed.Process`
package, is the `Process` monad. A `Process` is analogous to a
Thread, it has ProcessID and can operate concurrently with other
Processes. Unlike threads, it operates within the `Process` monad
as opposed to the `IO` monad - though `Process` is an instance of
`MonadIO` allowing use of IO functions within the `Process` monad.
The `Process` Monad allows for compl
-Some parallels to the Control.Concurrent package
-Much as forkIO is needed to create a newthread,
`spawn` is used to start a new process, given the
id of a node and a closure around a process.
```
spawn nid $(mkStaticClosure `datastore)
```
-The `set` and `get` commands take a given key and use channels
to interact with the datastore. Like with MVars/TVars, these channels
can carry messages to and from a specific `Process`, possibly updating
that `Process`
-`set` and `get` functions 
## Fault Tolerance
  -TODO: write up fault tolerance impplementations
  
## References
* Simon Marlow's [*Parallel and Concurrent Programming in Haskell*]
  (http://community.haskell.org/~simonmar/pcph/)
* Amazon's paper ['Dynamo: Amazon’s Highly Available Key-value Store']
 ( http://www.allthingsdistributed.com/files/amazon-dynamo-sosp2007.pdf)
* Hackage's page on [Cloud Haskell]
 ( https://wiki.haskell.org/Cloud_Haskell)

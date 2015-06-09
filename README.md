# distro-db
A distributed key-value store written in Haskell

## Usage
- Build a 3 process system (1 master, 2 slave) by the following
command:
```
$ ghc -Wall Main.hs
$ ./Main slave 55551 & ./Main slave 55552 & ./Main master 55553
```
- This should open something analogous to the following:
```
[3] 23321
[4] 23322
Populating Worker Processes...
Commands: GET <key>, SET <key> <value>, KILL <NodeId>, QUIT
Mon Jun  8 23:35:55 UTC 2015 pid://localhost:55553:0:11: spawning process on nid://localhost:55552:0
Mon Jun  8 23:35:55 UTC 2015 pid://localhost:55553:0:11: spawning process on nid://localhost:55551:0
>
```
The first two sets of numbers represent the slave processes, where
`22321` and `23322` are their `ProcessID`s. For each of these, there is
a corresponding announcement of the spawning of a datastore at each one
- This will bring a prompt for interacting with the 
database. Notable commands are listed below
```
GET <Key> : Get the value at a given key
SET <Key> <Value> : Set a given key to a given value
QUIT : Kill all slave processes and exit from the interactive
      session
```
- To kill one of the slave processes, merely open an additional terminal
and type `kill <ProcessID>` where the `ProcessID` is one of the slave
processes' process IDs
- One can now attempt to reuse the `GET` and `SET` commands, if the 
processes had a sibling process that also held a copy of the data, then
a `Just <value>` should be returned as opposed to `Nothing`

## Implementation
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
- Fault tolerance is important for any distributed key-value store--if one
machine goes down in a cluster, one does not wish for the data on that machine to
be unavailable for access.
- The first, naive attempt at fault tolerance merely calls for the copying of
the same data across two processes. Each process will have a buddy that will
mirror its data contents - should one go down, there is a back up.
- A second more nuanced version of this includes an awareness by the program
when a particular process dies. When a death is noticed, the key space is
redistributed across the remaining set of processes.
- A further evolution of this, which I plan to implement in the future, is
the Dynamo Ring, where processes are grouped in a ring formation where a given
worker's data contents are shared with its partners on both its left and right.
The program is aware when a worker exits the ring, and closes the gap between
the two neighbour workers by linking them and then redistributing the data over
the key space of the remaining processes as in the second attempt. This approach
is loosely based on the Amazon's paper on its distributed key-value store (see
references).
  
## Testing
- Considering the fact that much of the program runs from within the 
Process monad (as stated earlier, similar to IO), I opted for a more homebrewed
test function as opposed to Hspec or Quickcheck
- To run the test batch, first insure that all database-related processes
are dead (a simple `sudo netstat -lnp` command on the terminal line can
determine this). Then run the following code from `src`:
```
$ ghc TestDB.hs
$ ./TestDB slave 55554 & ./TestDB slave 55555 & ./TestDB master 55556
```
- One can then review the output as it appears at the terminal

## References
* Simon Marlow's [*Parallel and Concurrent Programming in Haskell*]
  (http://community.haskell.org/~simonmar/pcph/)
* Amazon's paper ['Dynamo: Amazon’s Highly Available Key-value Store']
 ( http://www.allthingsdistributed.com/files/amazon-dynamo-sosp2007.pdf)
* Hackage's page on [Cloud Haskell]
 ( https://wiki.haskell.org/Cloud_Haskell)

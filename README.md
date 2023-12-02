# Haskell Framework for autonomous vehicles 

This package contains: 
- 1 library called `IMC`; 
- and 3 + 1 executables:
  - For message generation as Haskell data types:
    - `extractIMC`
  - For vehicle control:
    - To follow a given target:
      - `subscribe`
      - `followSys`
    - To perform a given task:
      - `followForm`

The library is used by the vehicle control executables and is intended to be the main contribution of this package. It is structured as:

- IMC
  - Control
    - BehaviorTree
    - Subscribe
    - Utils
  - Maneuvers
    - FollowRef
  - Base
  - Messages
  - Network

The library comes with a set of messages pre-included, but you can customize it as you need. To (re)generate messages, check the executable `extractIMC`, by running for example:

```shell
$ cabal run extractIMC -- -?
```

## Usage

### As with any cabal package:

- To build:
```shell
$ cabal build
```
- To load the library into GHCi:
```shell
$ cabal repl
```
- Check the help of the executables with, for example:
```shell
$ cabal run followSys -- -?
```

- Profiling:
First, create a file called "cabal.project.local" and insert the following line:
```haskell
profiling: True
```
Then, run your program with:
```shell
cabal run --enable-profiling <executable> -- <arguments and options> +RTS -p
```
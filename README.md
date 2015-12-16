

# hlc - Hybrid Logical Clock in Erlang. #

Copyright (c) 2014-2015 Benoît Chesneau.

__Version:__ 2.0.0

hlc implements the Hybrid Logical Clock outlined in [Logical Physical Clocks
and Consistent Snapshots in Globally Distributed
Databases](http://www.cse.buffalo.edu/tech-reports/2014-04.pdf).

> Note: you can use it to have timestamps that are are a combination of both a
> physical and a logical component to support monotonic increments without
> degenerate cases causing timestamps to diverge from wall clock time. It's
> usefull to distribute transactions or such things.

[![Build Status](https://travis-ci.org/barrel-db/hlc.png?branch=master)](https://travis-ci.org/barrel-db/hlc)
[![Hex pm](http://img.shields.io/hexpm/v/hlc.svg?style=flat)](https://hex.pm/packages/hlc)

## Documentation

Full doc is available in the [`hlc`](http://github.com/barrel-db/hlc/blob/master/doc/hlc.md) module.

## Example of usage

Create a logical clock using `hlc:new/0`:

```
C = hlc:new()
```

> By default, it is using the physical clock (`erlang:timestamp/0` or `erlang:now/0` on old systems) , but you
> can use `hlc:new/1` to pass a function and use your own clock. Make sure to use the correct [time warp
> mode](http://www.erlang.org/doc/apps/erts/time_correction.html#Time_Warp) for your system.

You can update the current clock from the members of the cluster using `hlc:update/2`.

> :heavy_exclamation_mark: A clock is not locked, you need to make sure that only one user can update it at a time
> using the `now/1` or `update/2` functions.

### Comparaison

Compare 2 clocks using `hlc:ts_less/2` or `hlc:ts_equal/2`:

Ex, compare if A is inferior to B:

```
{MClock, MClockFun} = hlc:manual_clock(),
C = hlc:new(MClockFun),

A = hlc:timestamp(C),
B = hlc:timestamp(C),

?assert(A =:= B),

hlc:set_manual_clock(MClock, 1),
{B1, C2} = hlc:now(C),
true = hlc:less(A, B1).
```

To test if they are equal use `hlc:ts_equal/2`.

## Ownership and License

The contributors are listed in AUTHORS. This project uses the MPL v2
license, see LICENSE.

hlc uses the [C4.1 (Collective Code Construction
Contract)](http://rfc.zeromq.org/spec:22) process for contributions.

## Development

Under C4.1 process, you are more than welcome to help us by:

* join the discussion over anything from design to code style try out
* and [submit issue reports](https://github.com/refuge/hlc/issues/new)
* or feature requests pick a task in
* [issues](https://github.com/refuge/hlc/issues) and get it done fork
* the repository and have your own fixes send us pull requests and even
* star this project ^_^

To  run the test suite:

```
make test
```


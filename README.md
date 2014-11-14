

# hlc - Hybrid Logical Clock in Erlang. #

Copyright (c) 2014 Benoît Chesneau.

__Version:__ 0.1.0

hlc implements the Hybrid Logical Clock outlined in [Logical Physical Clocks
and Consistent Snapshots in Globally Distributed
Databases](http://www.cse.buffalo.edu/tech-reports/2014-04.pdf) and is based
on the [cockroach](https://github.com/cockroachdb/cockroach/blob/master/util/hlc/hlc.go) implementation.

## Example of usage

Create a logical clock using `enki_hlc:new/0`:

```
{ok, C} = enki_hlc:new()
```

> Note: by default, it is using the physical clock (`erlang:now/0`) , but you
> can use `enki_hlc:new/1` to pass a function and use your own clock.

You can update the current clock from the members using `hlc:update/2`.

### Comparaison

Compare 2 clocks using `hlc:ts_less/2` or `hlc:ts_equal/2`:

Ex, compare if A is inferior to B:

```
{MClock, MClockFun} = enki_hlc:manual_clock(),
{ok, C} = enki_hlc:new(MClockFun),

A = enki_hlc:timestamp(C),
B = enki_hlc:timestamp(C),

?assert(A =:= B),

enki_hlc:set_manual_clock(MClock, 1),
B1 = enki_hlc:now(C),
true = enki_util:ts_less(A, B1).`''
To test if they are equal use `hlc:ts_equal/2`.
## Ownership and License

The contributors are listed in AUTHORS. This project uses the MPL v2
license, see LICENSE.

rkvs uses the [C4.1 (Collective Code Construction
Contract)](http://rfc.zeromq.org/spec:22) process for contributions.

## Development

Under C4.1 process, you are more than welcome to help us by:

* join the discussion over anything from design to code style try out
* and [submit issue reports](https://github.com/refuge/rkvs/issues/new)
* or feature requests pick a task in
* [issues](https://github.com/refuge/rkvs/issues) and get it done fork
* the repository and have your own fixes send us pull requests and even
* star this project ^_^

To  run the test suite:

```
make test`''


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/refuge/hlc/blob/master/doc/hlc.md" class="module">hlc</a></td></tr></table>


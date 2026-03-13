# hlc - Hybrid Logical Clock in Erlang

[![CI](https://github.com/barrel-db/hlc/actions/workflows/ci.yml/badge.svg)](https://github.com/barrel-db/hlc/actions/workflows/ci.yml)
[![Hex.pm](https://img.shields.io/hexpm/v/hlc.svg?style=flat)](https://hex.pm/packages/hlc)

hlc implements the Hybrid Logical Clock outlined in [Logical Physical Clocks and Consistent Snapshots in Globally Distributed Databases](http://www.cse.buffalo.edu/tech-reports/2014-04.pdf).

Hybrid logical clocks provide timestamps that combine both physical and logical components to support monotonic increments without degenerate cases causing timestamps to diverge from wall clock time. This is useful for distributed transactions and ordering events across a cluster.

## Installation

Add `hlc` to your `rebar.config` dependencies:

```erlang
{deps, [
    {hlc, "3.0.2"}
]}.
```

Or with mix:

```elixir
{:hlc, "~> 3.0"}
```

## Usage

### Creating a Clock

```erlang
1> {ok, C} = hlc:start_link().
{ok, <0.158.0>}
```

### Getting Timestamps

Return a timestamp for the current time:

```erlang
2> Now = hlc:now(C).
{timestamp, 1511564016030, 0}
```

By default, `erlang:system_time(millisecond)` is used for the physical time.

### Updating from Remote Events

Update the clock when receiving timestamps from other cluster members:

```erlang
3> {ok, UpdatedTS} = hlc:update(C, RemoteTimestamp).
```

> **Note:** Clocks are not locked internally. Ensure only one process updates a clock at a time using `now/1` or `update/2`.

### Comparing Timestamps

Compare timestamps using `hlc:less/2` or `hlc:equal/2`:

```erlang
{MClock, MClockFun} = hlc:manual_clock(),
{ok, C} = hlc:start_link(MClockFun, 0),

A = hlc:timestamp(C),
B = hlc:timestamp(C),
true = hlc:equal(A, B),

hlc:set_manual_clock(MClock, 1),
B1 = hlc:now(C),
true = hlc:less(A, B1).
```

## API Reference

### Clock Management

- `hlc:start_link/0` - Start a clock with default physical clock
- `hlc:start_link/2` - Start with custom clock function and max offset
- `hlc:start_link/3` - Start with a registered name
- `hlc:stop/1` - Stop a clock

### Timestamps

- `hlc:now/1` - Get current timestamp (advances clock)
- `hlc:timestamp/1` - Get current timestamp (does not advance clock)
- `hlc:update/2` - Update clock from remote timestamp

### Comparison

- `hlc:less/2` - Check if timestamp A is before B
- `hlc:equal/2` - Check if two timestamps are equal

### Configuration

- `hlc:get_maxoffset/1` - Get maximum allowed clock offset
- `hlc:set_maxoffset/2` - Set maximum allowed clock offset

## Testing with Manual Clocks

For testing, you can create a manually controlled clock:

```erlang
{MClock, MClockFun} = hlc:manual_clock(),
{ok, C} = hlc:start_link(MClockFun, 0),
hlc:set_manual_clock(MClock, 42),
hlc:stop_manual_clock(MClock).  % Clean up when done
```

## Performance

Benchmark using `hlc_harness`:

```erlang
1> hlc_harness:timed_generate(1000000).
generating timestamp: 2.586 s
```

## License

Copyright (c) 2014-2026 Benoit Chesneau.

This project uses the MPL v2 license. See [LICENSE](LICENSE) for details.

## Contributing

Contributions welcome! Please submit issues and pull requests on [GitHub](https://github.com/barrel-db/hlc).

To run the test suite:

```
rebar3 eunit
```



# Module hlc #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

hlc.

<a name="description"></a>

## Description ##

implements the Hybrid Logical Clock outlined in
"Logical Physical Clocks and Consistent Snapshots in Globally
Distributed Databases", available online at
http://www.cse.buffalo.edu/tech-reports/2014-04.pdf.

An hybrid logical clock is available as a linked process.   Objects of this
type model causality while maintaining a relation  to physical time.
Roughly speaking, timestamps  consist of the largest wall clock time among
all  events, and a logical clock that ticks whenever  an event happens in
the future of the local physical  clock.

<a name="types"></a>

## Data Types ##




### <a name="type-clock">clock()</a> ###


<pre><code>
clock() = #clock{}
</code></pre>




### <a name="type-timestamp">timestamp()</a> ###


<pre><code>
timestamp() = #timestamp{}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#equal-2">equal/2</a></td><td>compare if 2 timestamps are equal.</td></tr><tr><td valign="top"><a href="#get_maxoffset-1">get_maxoffset/1</a></td><td>returns the maximal offset allowed.</td></tr><tr><td valign="top"><a href="#less-2">less/2</a></td><td>compare if one timestamps happen before the other.</td></tr><tr><td valign="top"><a href="#manual_clock-0">manual_clock/0</a></td><td>create a manually controlled physicl clock.</td></tr><tr><td valign="top"><a href="#manual_clock-1">manual_clock/1</a></td><td>create a manually controlled physicl clock and initialise it
with a default ts.</td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td>create a new hybrid logical clock.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>create a new hybrid logical clock with a custom physical clock function.</td></tr><tr><td valign="top"><a href="#now-1">now/1</a></td><td> returns a timestamp associated with an event from the local
machine that may be sent to other members of the distributed network.</td></tr><tr><td valign="top"><a href="#physical_clock-0">physical_clock/0</a></td><td>timestamp in milliseconds.</td></tr><tr><td valign="top"><a href="#set_manual_clock-2">set_manual_clock/2</a></td><td>change the value of the manually controlled physicall clock.</td></tr><tr><td valign="top"><a href="#set_maxoffset-2">set_maxoffset/2</a></td><td>Sets the maximal offset from the physical clock that a call to
Update may cause.</td></tr><tr><td valign="top"><a href="#timestamp-1">timestamp/1</a></td><td>return a copy of the clock timestamp without adjusting it.</td></tr><tr><td valign="top"><a href="#update-2">update/2</a></td><td>takes a hybrid timestamp, usually originating from an event
received from another member of a distributed system.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="equal-2"></a>

### equal/2 ###

<pre><code>
equal(TS::<a href="#type-timestamp">timestamp()</a>, X2::<a href="#type-timestamp">timestamp()</a>) -&gt; true | false
</code></pre>
<br />

compare if 2 timestamps are equal

<a name="get_maxoffset-1"></a>

### get_maxoffset/1 ###

<pre><code>
get_maxoffset(Clock::<a href="#type-clock">clock()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

returns the maximal offset allowed.
A value of 0 means offset checking is disabled.

<a name="less-2"></a>

### less/2 ###

<pre><code>
less(Timestamp::<a href="#type-timestamp">timestamp()</a>, X2::<a href="#type-timestamp">timestamp()</a>) -&gt; true | false
</code></pre>
<br />

compare if one timestamps happen before the other

<a name="manual_clock-0"></a>

### manual_clock/0 ###

<pre><code>
manual_clock() -&gt; {pid(), function()}
</code></pre>
<br />

create a manually controlled physicl clock

<a name="manual_clock-1"></a>

### manual_clock/1 ###

<pre><code>
manual_clock(TS0::integer()) -&gt; {pid(), function()}
</code></pre>
<br />

create a manually controlled physicl clock and initialise it
with a default ts.

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="#type-clock">clock()</a>
</code></pre>
<br />

create a new hybrid logical clock.

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(ClockFun::function()) -&gt; <a href="#type-clock">clock()</a>
</code></pre>
<br />

create a new hybrid logical clock with a custom physical clock function.

<a name="now-1"></a>

### now/1 ###

<pre><code>
now(Clock::<a href="#type-clock">clock()</a>) -&gt; {<a href="#type-timestamp">timestamp()</a>, <a href="#type-clock">clock()</a>}
</code></pre>
<br />

returns a timestamp associated with an event from the local
machine that may be sent to other members of the distributed network.
This is the counterpart of Update, which is passed a timestamp
received from another member of the distributed network.

<a name="physical_clock-0"></a>

### physical_clock/0 ###

<pre><code>
physical_clock() -&gt; non_neg_integer()
</code></pre>
<br />

timestamp in milliseconds

<a name="set_manual_clock-2"></a>

### set_manual_clock/2 ###

<pre><code>
set_manual_clock(Pid::pid(), TS::integer()) -&gt; ok
</code></pre>
<br />

change the value of the manually controlled physicall clock.

<a name="set_maxoffset-2"></a>

### set_maxoffset/2 ###

<pre><code>
set_maxoffset(Offset::non_neg_integer(), Clock::<a href="#type-clock">clock()</a>) -&gt; ok
</code></pre>
<br />

Sets the maximal offset from the physical clock that a call to
Update may cause. A well-chosen value is large enough to ignore a
reasonable amount of clock skew but will prevent ill-configured nodes
from dramatically skewing the wall time of the clock into the future.

A value of zero disables this safety feature.  The default value for
a new instance is zero.

<a name="timestamp-1"></a>

### timestamp/1 ###

<pre><code>
timestamp(Clock::<a href="#type-clock">clock()</a>) -&gt; <a href="#type-timestamp">timestamp()</a>
</code></pre>
<br />

return a copy of the clock timestamp without adjusting it

<a name="update-2"></a>

### update/2 ###

<pre><code>
update(RT::<a href="#type-timestamp">timestamp()</a>, Clock::<a href="#type-clock">clock()</a>) -&gt; {ok, <a href="#type-timestamp">timestamp()</a>, <a href="#type-clock">clock()</a>} | {timeahead, <a href="#type-timestamp">timestamp()</a>}
</code></pre>
<br />

takes a hybrid timestamp, usually originating from an event
received from another member of a distributed system. The clock is
updated and the hybrid timestamp  associated to the receipt of the
event returned.  An error may only occur if offset checking is active
and  the remote timestamp was rejected due to clock offset,  in which
case the state of the clock will not have been  altered. To timestamp
events of local origin, use Now instead.




# Module aggregator #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

aggregator public API.

<a name="types"></a>

## Data Types ##




### <a name="type-counts">counts()</a> ###


<pre><code>
counts() = #{<a href="#type-value">value()</a> =&gt; non_neg_integer()}
</code></pre>




### <a name="type-key">key()</a> ###


<pre><code>
key() = term()
</code></pre>




### <a name="type-value">value()</a> ###


<pre><code>
value() = term()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#count-2">count/2</a></td><td>Count how many of each value exists.</td></tr><tr><td valign="top"><a href="#fetch-2">fetch/2</a></td><td>Fetch value by the given key.</td></tr><tr><td valign="top"><a href="#put-3">put/3</a></td><td>Put the given value which associated with the caller.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>Start new aggregator process.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="count-2"></a>

### count/2 ###

<pre><code>
count(Pid::pid(), Key::<a href="#type-key">key()</a>) -&gt; {ok, <a href="#type-counts">counts()</a>} | {error, no_key}
</code></pre>
<br />

Count how many of each value exists.
This function uses `gen_server:call/2`.

<a name="fetch-2"></a>

### fetch/2 ###

<pre><code>
fetch(Pid::pid(), Key::<a href="#type-key">key()</a>) -&gt; {ok, <a href="#type-value">value()</a>} | {error, no_key}
</code></pre>
<br />

Fetch value by the given key.
You can only get the value that the caller putted before.

<a name="put-3"></a>

### put/3 ###

<pre><code>
put(Pid::pid(), Key::<a href="#type-key">key()</a>, Value::<a href="#type-value">value()</a>) -&gt; ok
</code></pre>
<br />

Put the given value which associated with the caller.
It will be removed when the caller is down.
This function uses `gen_server:cast/2`.

<a name="start-0"></a>

### start/0 ###

<pre><code>
start() -&gt; {ok, pid()}
</code></pre>
<br />

Start new aggregator process.

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(Pid::pid()) -&gt; ok
</code></pre>
<br />


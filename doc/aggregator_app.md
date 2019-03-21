

# Module aggregator_app #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

aggregator application.

__Behaviours:__ [`application`](application.md).

<a name="types"></a>

## Data Types ##




### <a name="type-state">state()</a> ###


<pre><code>
state() = pid()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#start-2">start/2</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="start-2"></a>

### start/2 ###

<pre><code>
start(StartType::term(), StartArgs::term()) -&gt; {ok, pid(), <a href="#type-state">state()</a>}
</code></pre>
<br />

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(Pid::<a href="#type-state">state()</a>) -&gt; ok
</code></pre>
<br />


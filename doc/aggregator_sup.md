

# Module aggregator_sup #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

aggregator top level supervisor.

__Behaviours:__ [`supervisor`](supervisor.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_child-0">start_child/0</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr><tr><td valign="top"><a href="#stop_child-1">stop_child/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="init-1"></a>

### init/1 ###

<pre><code>
init(X1::[]) -&gt; {ok, {<a href="supervisor.md#type-sup_flags">supervisor:sup_flags()</a>, [<a href="supervisor.md#type-child_spec">supervisor:child_spec()</a>]}}
</code></pre>
<br />

<a name="start_child-0"></a>

### start_child/0 ###

<pre><code>
start_child() -&gt; {ok, pid()}
</code></pre>
<br />

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; {ok, pid()} | ignore | {error, {already_started, pid()} | {shutdown, term()} | term()}
</code></pre>
<br />

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(Pid::pid()) -&gt; ok
</code></pre>
<br />

<a name="stop_child-1"></a>

### stop_child/1 ###

<pre><code>
stop_child(Pid::pid()) -&gt; ok | {error, Reason::term()}
</code></pre>
<br />


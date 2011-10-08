

#Module ecirca#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


This module provides an interface for circular arrays.

<a name="description"></a>

##Description##


**Note about implementation**: I hate repeating myself, so
some magic is involved in this module. I know about possibility
to replace all this macroses by something like
(type_to_module(Type)):foo(Bar), but it's 15% slower according
to my benchmarks.
<a name="types"></a>

##Data Types##




###<a name="type-ecirca">ecirca()</a>##



__abstract datatype__: `ecirca()`



###<a name="type-ecirca_atom_specs">ecirca_atom_specs()</a>##



<pre>ecirca_atom_specs() = [{atom(), strong | weak}]</pre>



###<a name="type-ecirca_type">ecirca_type()</a>##



<pre>ecirca_type() = last | max | min | avg | sum</pre>



###<a name="type-ecirca_value_size">ecirca_value_size()</a>##



<pre>ecirca_value_size() = small | medium | large</pre>



###<a name="type-maybe_value">maybe_value()</a>##



<pre>maybe_value() = [value()](#type-value) | empty</pre>



###<a name="type-nonneg">nonneg()</a>##



<pre>nonneg() = non_neg_integer()</pre>



###<a name="type-value">value()</a>##



<pre>value() = non_neg_integer()</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get-2">get/2</a></td><td>Returns a value.</td></tr><tr><td valign="top"><a href="#load-1">load/1</a></td><td>Loads ecirca from binary, assumes medium size of value.</td></tr><tr><td valign="top"><a href="#load-2">load/2</a></td><td>Loads ecirca from binary.</td></tr><tr><td valign="top"><a href="#max_size-0">max_size/0</a></td><td>Returns max allowed size of ecirca.</td></tr><tr><td valign="top"><a href="#max_slice-0">max_slice/0</a></td><td>Returns max allowed size of slice.</td></tr><tr><td valign="top"><a href="#max_value-1">max_value/1</a></td><td>Returns maximun value for given value size.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Returns new ecirca.</td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td>Returns new ecirca.</td></tr><tr><td valign="top"><a href="#new-4">new/4</a></td><td>Returns new ecirca.</td></tr><tr><td valign="top"><a href="#push-2">push/2</a></td><td>Push a value to ecirca.</td></tr><tr><td valign="top"><a href="#push_list-2">push_list/2</a></td><td>Push a list to ecirca.</td></tr><tr><td valign="top"><a href="#push_many-3">push_many/3</a></td><td>Push a value to ecirca N times.</td></tr><tr><td valign="top"><a href="#save-1">save/1</a></td><td>Saves ecirca to binary.</td></tr><tr><td valign="top"><a href="#set-3">set/3</a></td><td>Sets a value in ecirca.</td></tr><tr><td valign="top"><a href="#size-1">size/1</a></td><td>Returns a size of ecirca.</td></tr><tr><td valign="top"><a href="#slice-3">slice/3</a></td><td>Returns a slice of ecirca.</td></tr><tr><td valign="top"><a href="#update-3">update/3</a></td><td>Updates a value in ecirca, action is defined by type of ecirca.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="get-2"></a>

###get/2##




<pre>get(X1::[ecirca()](#type-ecirca), Position::pos_integer()) -&gt; {ok, [maybe_value()](#type-maybe_value)}</pre>
<br></br>




Returns a value.<a name="load-1"></a>

###load/1##




<pre>load(Binary::binary()) -&gt; {ok, [ecirca()](#type-ecirca)} | {error, wrong_ecirca_value_type} | {error, bad_binary} | {error, max_size}</pre>
<br></br>




Loads ecirca from binary, assumes medium size of value.<a name="load-2"></a>

###load/2##




<pre>load(Binary::binary(), X2::[ecirca_value_size()](#type-ecirca_value_size)) -&gt; {ok, [ecirca()](#type-ecirca)} | {error, wrong_ecirca_value_type} | {error, bad_binary} | {error, max_size}</pre>
<br></br>




Loads ecirca from binary.<a name="max_size-0"></a>

###max_size/0##




<pre>max_size() -&gt; {ok, pos_integer()}</pre>
<br></br>




Returns max allowed size of ecirca.<a name="max_slice-0"></a>

###max_slice/0##




<pre>max_slice() -&gt; {ok, pos_integer()}</pre>
<br></br>




Returns max allowed size of slice.<a name="max_value-1"></a>

###max_value/1##




<pre>max_value(X1::[ecirca_value_size()](#type-ecirca_value_size)) -&gt; pos_integer()</pre>
<br></br>




Returns maximun value for given value size.<a name="new-2"></a>

###new/2##




<pre>new(Size::pos_integer(), Type::[ecirca_type()](#type-ecirca_type)) -&gt; {ok, [ecirca()](#type-ecirca)} | {error, max_size}</pre>
<br></br>




Returns new ecirca. Takes size and type only, assumes medium
value size.<a name="new-3"></a>

###new/3##




<pre>new(Size::pos_integer(), Type::[ecirca_type()](#type-ecirca_type), ValueSize::[ecirca_value_size()](#type-ecirca_value_size)) -&gt; {ok, [ecirca()](#type-ecirca)} | {error, max_size}</pre>
<br></br>




Returns new ecirca. Takes size, type and value size.
Assumes empty atom spec list.<a name="new-4"></a>

###new/4##




<pre>new(Size::pos_integer(), Type::[ecirca_type()](#type-ecirca_type), X3::[ecirca_value_size()](#type-ecirca_value_size), AtomSpecs::[ecirca_atom_specs()](#type-ecirca_atom_specs)) -&gt; {ok, [ecirca()](#type-ecirca)} | {error, max_size}</pre>
<br></br>




Returns new ecirca. Takes size, type and value size.<a name="push-2"></a>

###push/2##




<pre>push(X1::[ecirca()](#type-ecirca), Value::[maybe_value()](#type-maybe_value)) -&gt; ok | {error, overflow} | {error, unknown_atom}</pre>
<br></br>




Push a value to ecirca.<a name="push_list-2"></a>

###push_list/2##




<pre>push_list(Ecirca::[ecirca()](#type-ecirca), Lst::[[maybe_value()](#type-maybe_value)]) -&gt; ok</pre>
<br></br>




Push a list to ecirca. Not implemented in C code yet,
provides an Erlang fallback.<a name="push_many-3"></a>

###push_many/3##




<pre>push_many(Ecirca::[ecirca()](#type-ecirca), N::[nonneg()](#type-nonneg), Val::[maybe_value()](#type-maybe_value)) -&gt; ok</pre>
<br></br>




Push a value to ecirca N times. Not implemented in C code yet,
provides an Erlang fallback.<a name="save-1"></a>

###save/1##




<pre>save(X1::[ecirca()](#type-ecirca)) -&gt; {ok, binary()}</pre>
<br></br>




Saves ecirca to binary.<a name="set-3"></a>

###set/3##




<pre>set(X1::[ecirca()](#type-ecirca), Position::pos_integer(), Value::[maybe_value()](#type-maybe_value)) -&gt; {ok, {[maybe_value()](#type-maybe_value), [maybe_value()](#type-maybe_value)}}</pre>
<br></br>




Sets a value in ecirca. Returns {ok, {old value, new value}} tuple.<a name="size-1"></a>

###size/1##




<pre>size(X1::[ecirca()](#type-ecirca)) -&gt; {ok, pos_integer()}</pre>
<br></br>




Returns a size of ecirca.<a name="slice-3"></a>

###slice/3##




<pre>slice(X1::[ecirca()](#type-ecirca), Start::pos_integer(), End::pos_integer()) -&gt; {ok, [[maybe_value()](#type-maybe_value)]} | {error, slice_too_big}</pre>
<br></br>




Returns a slice of ecirca.<a name="update-3"></a>

###update/3##




<pre>update(X1::[ecirca()](#type-ecirca), Position::pos_integer(), Value::[maybe_value()](#type-maybe_value)) -&gt; {ok, {[maybe_value()](#type-maybe_value), [maybe_value()](#type-maybe_value)}}</pre>
<br></br>




Updates a value in ecirca, action is defined by type of ecirca.
Returns {ok, {old value, new value}} tuple.
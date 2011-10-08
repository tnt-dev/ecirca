

#The ecirca application#

Ecirca: Erlang Circular Arrays  [![Build Status](https://secure.travis-ci.org/band115/ecirca.png)](http://travis-ci.org/band115/ecirca)
==============================.

__Authors:__ Alexander Pantyukhov ([`alwx.main@gmail.com`](mailto:alwx.main@gmail.com)), Dmitry Groshev ([`lambdadmitry@gmail.com`](mailto:lambdadmitry@gmail.com)).


Ecirca: Erlang Circular Arrays  [![Build Status](https://secure.travis-ci.org/band115/ecirca.png)](http://travis-ci.org/band115/ecirca)
==============================

License
-------
<pre>
Copyright (C) 2011 by Alexander Pantyukhov alwx.main@gmail.com
                      Dmitry Groshev       lambdadmitry@gmail.com
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
</pre>



Is it any good?
---------------



Yes.



What it is for?
---------------



Some day I had a need to store some limited amount of integer data local for
one Erlang process. Cost of serialization/deserialization was overwhelming, so
this library was created - initially as an experiment during local code contest,
and then it became a library that is used in production.



What can I do with it?
----------------------



- you can store integer values in an array of fixed length;
- you can set and get values of this array by index (which is in 1..n range);
- you can push some values to the start of this array, so an equal amount of  
values will be removed from the end of array;
- you can update a value instead of setting it. What will be done depends on  
the type of array that is set during creation (more on this later);
- you can define special atoms that can be used instead of integer value during  
set/push/update;
- export an entire array to binary and load it from binary.



Caller restriction
------------------



To prevent problems that can araise from abusing mutable nature of arrays
that are implemented in this library, all library functions that accept arrays
will return {error, creator_only} when called with array that was created in
a thread that is different from the thread where that function is called.



Endianess warning
-----------------



I was too lazy to implement endianess-independent serialization, so at this
moment binaries from save/1 should not be transferred between machines with
different endianesses.



Sizes of values
---------------



To save some space one can provide one of 3 sizes to new/3: small, medium and
large.
- **small**: one element of array use 2 bytes of memory and can store values             
up to (2^12 - 1)
- **medium**: 4 bytes per element, up to (2^28 - 1)
- **large**: 8 bytes per element, up to (2^60 - 1)
As one can see, 4 bits of each value is taken to save an additional information.
If one try to save value that is bigger than maximum allowed value, he can expect
{error, overflow} if value is less than int64 and badarg expression otherwise.



Atoms
-----



One can pass a list of {Atom, strong | weak} pairs (with up to 14 elements) to
new/4. As a result, any of that atoms can be passed instead of integer value to
any function that modifies the array. Atom value will be stored and returned
by any function that reads value (slice or get). There is also one special atom
that is always provided, 'empty' - it is returned if value wasn't initialized
and can be passed to setter functions.



The second value in tuple, strong or weak, defines behaviour of atom when it is
used in update/3 function. Following rules are used (let's say that A is a
value in array and B is a value that is passed to update/3):
- if A is empty, write B;
- if B is weak and A isn't an empty value, ignore it;
- if B is empty, ignore it;
- if B is integer and A is integer, update a value according to ecirca type  
(more on ecirca types in next section);
- if B is integer and A is weak atom, write B;
- if B is integer and A is strong atom, ignore it;
- if B is strong atom, write B.



Ecirca types
------------



Type of array can be passed in new/2,3,4. Type of array will affect the way how
update/3 will behave if it operates on integers:
- if type is **last**, update will just set new value;
- if type is  **max** or **min**, update will set value to max/min between  
provided and existing values;
- if type is **sum**, update will add new value to existing one if sum is lesser  
or equal than maximum value for this value type, returning {error, overflow}  
otherwise;
- if type is **avg**, update will update a value in such way that it will be an  
average between all values that was passed to update/3 function at this value.  
One should know that memory consumption of ecirca of type "avg" is increased  
by 8 bytes per value (there is an extra Double inside used to find true  
average).



Benchmarks
----------



Some code for doing basic benchmarks is provided in ecirca_bench module.
Repository also contais results of this benchmarks to give a tip how fast
(or slow) this library is.

Last updated
------------
Oct 8 2011 15:55:40


##Modules##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/band115/ecirca/blob/master/doc/ecirca.md" class="module">ecirca</a></td></tr>
<tr><td><a href="https://github.com/band115/ecirca/blob/master/doc/ecirca_bench.md" class="module">ecirca_bench</a></td></tr></table>


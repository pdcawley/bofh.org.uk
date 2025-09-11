+++
title = "Why I love Smalltalk"
slug = "why-i-love-smalltalk"
date = "2007-05-02T18:41:00+00:00"
draft = false

+++

From [Extreme UI Testing](http://www.cincomsmalltalk.com/blog/blogView?showComments=true&entry=3355580003):

> Method wrappers. Easily possible in VW and Squeak since you can subclass CompiledMethod and much with the method dictionaries \[...\] Niall uses a strategy pattern to automatically wrap threads that get forked off by looking up the stack to see if the code is under test (and if it is, make sure to install a wrapper)

So what if the language doesn't give me the callbacks I need, I'll just rejig the system classes until it does. Until Avi needed them for Seaside, Smalltalk didn't have continuations, but it's malleable enough that he could implement them.

Malleability rocks (even though rocks aren't exactly malleable.)

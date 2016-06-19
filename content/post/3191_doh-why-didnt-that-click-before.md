+++
title = "D'oh! Why didn't that click before?"
slug = "doh-why-didnt-that-click-before"
date = "2007-02-27T15:21:00+00:00"
draft = false

+++

There's a very cunning trick that most Smalltalk implementations use to avoid dereferencing pointers when they're dealing with 'small' integers. And for years, I've misunderstood it. For some reason I used to think that Squeak say looked at the value of a pointer to an object and, if it was &lt; some biggish number, it would be treated as an integer.

Which is cunning, I suppose, but nowhere near as cunning as what *really* happens. You see some clever chap (almost certainly a chap, I'm afraid) noticed that, since their data structures were at least two bytes long, their pointers were aligned on 2 byte boundaries. Which means that all pointers have 0 in their low bit. Which leaves all the odd numbers free. So, what Squeak does is to check the low bit of the object pointer and, if it's 1, all you have to do is `pointer >>  1` and you've got your integer.

Now that's what I *call* cunning.

So, who else uses this trick?

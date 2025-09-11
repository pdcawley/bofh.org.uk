+++
title = "For bad, map pretty"
slug = "for-bad-map-pretty"
date = "2007-02-08T04:32:00+00:00"
draft = false

+++

[Adam Turoff](http://www.panix.com/~ziggy/) is one of the good guys, a fine host who doesn't mind when your wife pulls his shower tap off the wall, a wise programmer, mine of information, and an enlightening man to talk to. It was Adam who popped up in an iChat window not long after Rails got released and suggested I take a look.

So, when he started up his [Notes on Haskell](http://notes-on-haskell.blogspot.com/) techblog, I subscribed immediately.

Today, Adam wrote about [What's wrong with the for loop](http://notes-on-haskell.blogspot.com/2007/02/whats-wrong-with-for-loop.html) and, as usual for him, he nailed it.

In summary, the for loop does too much. Or too little. It depends how you look at it. In a language with closures and higher order functions, there are forms of abstraction that hide the loop and bring intent to the fore, and unless you've been hiding under a rock for the past few years, anything that helps clarify your intent can only be a good thing.

I can't remember the last time I used a classic for loop in any of my Ruby code. In the past I've used

collection.each\_i {|item, i| ... }

when I've needed to iterate over parallel collections, but I discovered `zip` recently:

a = \[1, 2\]
b = \[:a, :b\]

b.zip(a) \# =&gt; \[\[:a, 1\], \[:b, 2\]\]
b.zip(a) {|(b\_item, a\_item)| ... }

which is so much nicer.

But, as usual, Adam puts it better than me, go read him.

+++
title = "Monads"
slug = "monads"
date = "2007-08-07T12:37:00+00:00"
draft = false

+++

I've been following Adam Turoff's excellent [Haskell tutorial](http://www.onlamp.com/pub/a/onlamp/2007/08/02/introduction-to-haskell-pure-functions.html) and he's just reached the part where he explains Monads.

To listen to a lot of people, Monads are the bit of Haskell that breaks their brains. As they're usually described, Monads are the part of Haskell that allow you to write code that has side effects. You know, stuff like reading a file or generating a random number.

What most of the tutorials I've read don't do is explain *why* Monads let you use side effects. Or, to put it another way, why you can't successfully use side effects in Haskell without them.

Monads wrap up the concept of a sequence of actions in something that looks, from outside the Monad, like an other datatype. It clicked for me when I remembered something from a course on Set Theory when I was an undergraduate mathematician. The thing about sets is, they're not ordered. {<i>a</i>, <i>b</i>} is the same set as {<i>b</i>, <i>a</i>}. However, when you're busily constructing all the entities that get used in mathematics from the Zermelo/Fraenkel axioms, you eventually need to build *ordered* pairs of numbers. Coordinate systems for example, make heavy use of ordered pairs.

So, if you've got sets, which are not ordered, and you need to build a set which can be interpreted as an ordered pair, how do you proceed?

The standard method is to represent the ordered pair, (<i>x</i>,<i>y</i>) as { {<i>x</i>}, {<i>x</i>, <i>y</i>} }. Look at the [Wikipedia article](http://en.wikipedia.org/wiki/Ordered_pair) if you want the gory details.

So, an ordered pair is just a handy notation for a slightly more complicated, unordered, set.

Monads are a generalization of this principle; in a sense, the details of how they work are irrelevant (in the same way that the innards of an ordered pair are irrelevant), what's important is that they provide a box in which ordered execution can happen. And ordered execution is what you need if you want to write code that uses side effects or works with impure 'functions' that don't always return the same value given the same input.

Monads can seem so mindbending because most of us come from a programming background where ordered execution is all there is. In pretty much every mainstream language, the idea that the programmer doesn't control the order in which code is evaluated seems utterly outlandish. Monads look weird, then, because we've never thought of the problem they solve as a problem in the first place. It's just how programs *are*.

Maybe it's time we tried to let go of that assumption. Or maybe I've completely misunderstood monads.

It's probably the latter.

### Updates

Someone [commenting on this post](http://programming.reddit.com/info/2d7ra/comments/c2d823) at reddit.com points out that monads aren't solely used for wrapping sequential processing; they can be used to wrap pretty much every model of computation you could come up with. Which, I must admit, hadn't quite clicked with me, despite realising that Parsec, a Haskell parser combinator library, was monadic but wasn't really about sequential processing because the Parser monad also handled backtracking.

So, it seems that I've incompletely misunderstood monads.

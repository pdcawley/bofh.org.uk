+++
title = "Theres Good Clever, and Bad Clever"
slug = "theres-good-clever-and-bad-clever"
date = "2007-06-29T14:01:00+00:00"
draft = false

+++

Have you noticed the difference between Good Clever and Bad Clever?

For instance, I recently spent a couple of hours working out to make a Genre model which `acts_as_nested_list` work in such a way that when ask one of the trunk genres for its tracks it finds all the tracks associated directly with the trunk or with any of the genre's sub genres. It's made doubly complicated by the fact that the genre is related to its tracks through a relationship model, and triply complicated that we're going to want to be able to be able to search within the basic set of results (because, usually, we don't want 300 tracks on a page...)

The solution I ended up with runs to about two emacs screenfuls and it's ugly as hell. We're having to declare custom 'select\_sql' for getting the basic collection, and then extending the association with a custom 'find' and method\_missing, and you can forget about making `genre.tracks.create` work because life is far too short.

There's no doubting that the solution is clever; it took a couple of hours of concentrated thought and experimentation to come up with it. However, it's definitely Bad Clever.

### What is Bad Clever?

Bad Clever is when something was hard to do, but it *looks* hard to do and, worse, is hard to understand. Bad clever is when the complexity of finding a solution pokes through into the form of that solution. Bad Clever is when you end up feeling like you haven't been clever enough. Bad Clever is the proof that [Four Colours Suffice](amazon:0691115338), Java's static type system, WS-Deathstar and inline Javascript. Bad Clever is "Dammit, if it was hard to do, it should be hard to understand!".

### What about Good Clever then?

Good Clever delights. Good Clever is modest, only revealing exactly how clever it is when you look more closely. Good Clever has a simple set of [Just Stories](http://www.bofh.org.uk/articles/2003/08/01/the-fine-art-of-complexity-management) - the complexity of the problem domain is exposed gradually. Good Clever has the Quality Without a Name. Good Clever is what happens when you have that simplifying insight that turns a bit of ugliness into something that isn't going to embarrass you.

Most of the time, Rails is Good Clever, Smalltalk has the Good Clever nature in abundance. Good clever is Cantor's proof that the real numbers are uncountable, Turing's proof that the Halting Problem can't be solved. Good clever is the [Atom Publishing Protocol](http://tools.ietf.org/wg/atompub/), [Haskell's](http://www.haskell.org/) static type system, and [Unobtrusive Javascript](http://en.wikipedia.org/wiki/Unobtrusive_JavaScript). Good Clever is [Origin of Species](amazon:0486450066) and Mick Jaggers phrasing on Sympathy for the Devil. Good clever *rocks*.

If only I could achieve it...

+++
title = "Reasons to be edgy..."
slug = "reasons-to-be-edgy"
date = "2006-09-29T02:50:00+00:00"
draft = false

+++

I've been busy with a new experimental branch of typo that plays well with edge rails, in part because I wanted to move to a more RESTful URL scheme for Typo.

But then I took a look at some of the things going on in Rails Core and my nipples exploded with delight. For instance, there's now a `ResourceFeeder` helper that pretty much magically deals with Atom and RSS feeds for Rails Resources, which would allow us to eliminate a whole pile of ugliness in the Typo source. So that's a win.

But then, I just discovered that Rails now has 'real' [around filters](http://dev.rubyonrails.org/changeset/5163) which allow you to implement a filter as a block which calls yield. Which means that it's now easy to write filters that clean up their resources properly, even in the face of exceptions being thrown further down the filter chain; you just catch the exception, do your cleanup and rethrow. Which allows us to eliminate a whole category of possible typo memory leaks *and* simplify a few things that are currently causing us pain. Result.

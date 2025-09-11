+++
title = "Listen to the Voice!"
slug = "listen-to-the-voice"
date = "2007-02-13T07:31:00+00:00"
draft = false

+++

You know that voice at the back of your head that says things like "That's not really a data structure, that's an object that is!"?

You should listen to it.

I've been battling away with having a million and one things monkeying with a result set that consists of an array of arrays, when really it should have been a `ResultSet` which has a collection of <code>ResultSet::Row</code>s. Once I bit that bullet, I got to do things like add `ActiveRecord::Errors` to each row and generally start to bring a bunch of big guns to bear on a problem.

So, having listened to the voice at last I'm going through and removing code with gay abandon. Sometimes I think removing code is one of the great joys of being a programmer - it's the code's way of telling you you got something right.

So, today's slogan is "Reify early and reify often".

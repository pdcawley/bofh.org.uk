+++
title = "Code Ownership Dies Hard"
slug = "code-ownership-dies-hard"
date = "2006-05-15T12:11:00+00:00"
draft = false

+++

Martin Fowler has posted an overview of the ways in which different projects can handle [Code Ownership](http://www.martinfowler.com/bliki/CodeOwnership.html) on his blog. As usual with Martin, it's a thoughtful piece coming down strongly in favour of an agile solution (Martin argues persuasively that strong code ownership makes it much harder to improve the code - or make it worse, but let's assume that we know what we're doing).

[Typo](http://www.typosphere.org/trac) is open source. In theory it has collective code ownership, but looking at the patches that come in, it's apparent that a lot of people still think in terms of strong code ownership. A typical typo patch tends to contain all its changes in a single class, even if that means writing a bunch of structural code - code that treats objects as if they were data structures - which is a [code smell](http://www.martinfowler.com/bliki/CodeSmell.html) if ever there was one. Still, I'd rather have the patches than not, even if it means that the usual process of accepting one is to apply it and then refactor before committing anything.

So, don't be afraid to submit Typo patches that alter multiple classes; the aim is to keep Typo as well factored as possible. Structural code tends to introduce unnecessary dependencies, and unnecessary dependencies make code brittle.

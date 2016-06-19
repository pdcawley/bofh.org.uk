+++
title = "Other viewpoints, and why they are important"
slug = "other-viewpoints-and-why-they-are-important"
date = "2006-08-16T02:50:00+00:00"
draft = false

+++

Many thanks to Paul Ingles who has just written a [fantastic article](http://www.oobaloo.co.uk/articles/2006/08/15/writing-a-typo-sidebar-test-first-in-rails) about writing a Typo sidebar in a test driven way.

For too long we've been assuming that Typo sidebars are, in fact, detestable ("If it's not testable, it's detestable" - someone at the Sydney XP group, by way of [Martin Fowler](http://www.martinfowler.com/bliki/Detestable.html)). Paul neatly demonstrates that sidebars are perfectly testable, we just haven't been testing them. Bad us.

So, I'm rolling back the changes in my current sidebar rejig and starting again by writing some tests for a few of the core sidebars, and then I can start my refactoring without quite so much of The Fear.

So, once again, thanks Paul. It's really great to occasionally see the curious outsider's view of what we're working on - it makes us question our assumptions and often shows us better ways.

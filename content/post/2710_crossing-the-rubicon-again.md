+++
title = "Crossing the Rubicon. Again"
slug = "crossing-the-rubicon-again"
date = "2006-09-21T07:33:34+00:00"
draft = false

+++

In 2001, using an idea from [James Duncan](http://www.whoot.org/), who blames [Damian Conway](http://damian.conway.org/) for getting him thinking on the right lines, I wrote a proof by implementation of the [Extract Method](http://www.refactoring.com/catalog/extractMethod.html) refactoring for Perl. Though I say so myself, this was a Big Deal - Martin Fowler calls Extract Method the [Refactoring Rubicon](http://www.martinfowler.com/articles/refactoringRubicon.html) - once you have a tool to help you do that refactoring automatically, you can probably implement the rest of the Smalltalk Refactoring Browser and free yourself to think more about the interesting aspects of programming.

And nothing happened.

Five years later, I mentioned this to [Jesse Vincent](http://fsck.com) at EuroOSCON. He got that "You interest me strangely![](" look, so I verbally sketched out how it worked, which induced the "I must hack now)" look. I headed off to another session and left Jesse to scratch his itch.

About 3 hours later, we hooked up again, and he showed me the vi plugin he's hacked up which implements an 'extract function' refactoring. So we sat down together and made it extract a method correctly.

At this point, magic happened. Jesse's original script was, putting it politely, *very* hacky. It's less hacky [now](http://fsck.com/~jesse/extract), in part because it's been refactored with the assistance of this cool new tool he'd just written to extract methods from Perl code.

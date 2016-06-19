+++
title = "Fluent Interfaces"
slug = "fluent-interfaces"
date = "2005-12-21T03:28:00+00:00"
draft = false

+++

\[Edited to correct an accidental misrepresentation, and again to correct an attribution, and again to correct some code\]

Martin Fowler is [talking about interfaces](http://martinfowler.com/bliki/FluentInterface.html) again and, as usual, he's mostly talking sense.

This time he's talking about what he's christened 'fluent interfaces' -- essentially interfaces that do a good job of removing hoopage ([James Duncan's](http://whoot.org) handy term for all the jumping through hoops you have to do in order to achieve something that ought to be a lot simpler) from the client side of the interface. In the examples Fowler gives, client side complexity (and in one case a subtle, but unpleasant code smell) is reduced by moving object construction behind a thoughtful, humane interface.

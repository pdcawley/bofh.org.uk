+++
title = "Test::Class::Sugar released"
slug = "test-class-sugar-released"
date = "2009-04-06T05:15:00+00:00"
draft = false

+++

I've just pushed the second version of [Test::Class::Sugar](http://search.cpan.org/dist/Test-Class-Sugar) (first discussed [here](http://www.bofh.org.uk/2009/03/11/writing-parsers-for-fun-and-convenience)). It's pretty much as discussed in the original article, but after some discussion with [David Wheeler](http://www.justatheory.com/), I've dropped the <code>+uses</code> clause from the `testclass` declaration in favour of less DWIMmy (and thus easier to explain behaviour).

I've also introduced a `defaults` argument at `use` time. The only working default in this release is one which lets you specify the prefix used to derive the test class name from the class/module under test. I've documented a couple of extra and so far unimplemented defaults as well.

Have a play, you might like it.

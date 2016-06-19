+++
title = "If you test it, I will patch"
slug = "if-you-test-it-i-will-patch"
date = "2006-03-12T13:17:00+00:00"
draft = false

+++

I've been working on [Typo](http://www.typosphere.org/) this weekend, mostly going through [open tickets](http://www.typosphere.org/trac/report/1) and deciding whether to apply patches.

There's plenty of potentially good patches in the queue, but too many of them give me The Fear.

They don't have tests!

I'm the first to admit that I've added untested code to my projects (and I've been bitten by it too), but I'm more sanguine about that risk because, when it goes wrong, I can often rack my brains and pull back some memory of what my intent was. Knowing my intent helps me write meaningful tests and fix the issue. However, I can't remember the intent of *your* code. The only thing I have to go on is what you wrote. If you've written tests, I can tell what you expect your code to do, which is far more important than being able to divine what your code actually does. Given a choice between code without tests and tests without code, I'll take the tests every time.

So, if you've got a typo patch on the trac and you want it to get accepted, you *must* write tests for it. If you're implementing something that's supposed to be conforming to a standard, then please reference those standards in your tests.

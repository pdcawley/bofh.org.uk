+++
title = "Testing Your Assumptions"
slug = "testing-your-assumptions"
date = "2005-12-04T09:37:00+00:00"
draft = false

+++

One of the joys of writing applications using Ruby on Rails is the way the framework is constantly evolving better ways of doing stuff. It's one of the dangers too.

Each release of Rails brings new and groovy features to the table, so it's nice to stay close to the edge. However, when you do that, a change in the framework can bring your whole app crashing down because a key assumption you made turns out not to be true any more.

This is especially common when the assumed behaviour is undocumented, or a surprising consequence of behaviour that *is* documented.

When this happens, your test suite can have failures everywhere; it's hard to work out precisely what the underlying problem is.

So, what to do?

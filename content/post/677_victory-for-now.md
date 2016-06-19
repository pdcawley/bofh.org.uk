+++
title = "Victory! For now"
slug = "victory-for-now"
date = "2006-08-20T03:43:20+00:00"
draft = false

+++

[Typo](http://www.typosphere.org/'s) Sidebar's are now controller free!

Which probably means nothing to the vast majority of people reading this. But it makes me very happy.

Until a couple of revisions ago, Typo sidebars were rendered through a convoluted route, which involved *n* + 1 calls to `render_component`, where *n* is the number of items on the sidebar. Calling `render_component` isn't something you really want to do even once if you can help it. It's not as slow as it used to be, but it's still unpleasant.

So, now a typo sidebar looks pretty much like any other Rails plugin. It lives in `vendor/plugins` with a model, a view, a test and an only mildly clever `init.rb`.

For the MVC purists among you, there's a small amount of shenanigans involved because the model carries information about where to find the sidebar's template, but I shall plead necessity and have done with it. Besides, the model never makes direct use of the information, it just holds onto it for the controller.

The upshot of this, and [Scott's changes](http://scottstuff.net/blog/articles/2006/08/17/the-typo-4-1-development-cycle-has-begun) is that Typo's [`components`](http://www.typosphere.org/trac/browser/trunk/components) directory is now empty. And you can call me Mr Happy!

You don't **have** to call me Mr Happy of course, but I reserve the right to be happy anyway.

Expect a quick introduction to writing your own sidebars soon.

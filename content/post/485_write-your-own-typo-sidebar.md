+++
title = "Write your own Typo sidebar"
slug = "write-your-own-typo-sidebar"
date = "2006-04-16T12:19:00+00:00"
draft = false

+++

If you've been following typo development on [the trac](http://www.typosphere.org/trac/timeline) you'll have seen that I've been niggling away at the Typo sidebar system and I haven't finished with it yet. The changes waiting in my current SVK repository are rather substantial so I'm going to give you a preview of them here.

### Why mess with sidebars?

Well, in the past, developing a sidebar required you to sling a bunch of boilerplate code in with the code that actually did stuff, which violates the DRY principle in all sorts of horrible ways. Also, the boilerplate code we were using breaks with IE6.

Sidebars have two 'sides' (a little like Apple's Dashboard widgets, but without the cool/annoying graphic effects when you switch between the config and content views). Most of the ugliness for the developer being found on the configuration side.

+++
title = "Deprecation! That's what you need!"
slug = "deprecation-thats-what-you-need"
date = "2007-05-31T10:04:00+00:00"
draft = false

+++

So, I'm quietly beavering away at Typo with an eye to slimming down our somewhat monolithic `ArticlesController` class, tweaking our routing to use the new `datestamped_resources` plugin that I've developed to help dry up our routes, making a couple of new resourceful controllers for comments and trackbacks and generally tidying the place up. Hopefully Typo's code is going to be much more habitable when I'm done.

The problem is themes. Typo has an awful lot of 'em. And what I'm planning to do will almost certainly break many of them. So, it's time to deprecate a bunch of controller and helper methods and start dropping huge warnings in the log files at the very least (I'm thinking evil thoughts about having deprecation warnings show up in the admin interface every time to go to make any changes too, but that needs a bit more thought).

So, for the next few revisions on the edge, expect to see a depressingly large amount of code duplication - I hope to keep it to a minimum by pushing everything I can down into the models, but it's still going to be ugly for a while.

The upside should be that we're going to be ready for Rails 2.0, it should be reasonably easy to add the Atom Publishing Protocol to Typo along with a handy dandy 'direct manipulation' AJAX based administration interface I've been busy designing in my head.

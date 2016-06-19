+++
title = "Typo on Rails 2"
slug = "typo-on-rails-2"
date = "2007-10-05T15:09:04+00:00"
draft = false

+++

Whee! Sat in my home directory is a version of typo that appears to work with Rails 2.0. I ended up giving up on the themer approach which proved to be very hard to get up and running transparently - things kept disappearing off around infinite loops, which is no fun at all, let me tell you.

So, ended up cheating and ended up monkeying with <code>@@view\_paths</code> directly, which is almost certainly against the rules, but has the cardinal virtue of *working*.

I'm sanguine about it because the way themes worked on rails 1.2.x also involved fiddling with undocumented methods which aren't there in edge rails.

So, it now remains to walk around the thing, kick the tyres, write a few more tests and generally get myself to the point where I'm happy that it all works, and merge my local branch with the main line. I'm going to wait until 2.0.0 proper has been released, but now that I've done the work I expect to be merging it sharpish.

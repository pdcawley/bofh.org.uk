+++
title = "Typo on Rails 1.2"
slug = "typo-on-rails-1-2"
date = "2006-11-24T05:00:00+00:00"
draft = false

+++

Typo users with longish memories will recall the absolute disaster we had when Rails 1.1 shipped and Typo wasn't even remotely compatible with it. Even edge Typo didn't work with 1.1. Chaos and confusion, strong words from DHH, all manner of unfun things.

So, this time, we're not going to let it happen. We hope.

For starters, the current Typo release, 4.0.3 is locked to Rails 1.1.6 - if you install it from the gem it will insist that you have an appropriate Rails gem installed too. If you install it from SVN, it will use the magic of `svn:externals` to stick the right version of Rails in `vendor/rails` and, if you're using SVK, we assume you have enough of a clue to populate `vendor/rails` appropriately.

### What about Edge Typo then?

Edge typo is currently fetching edge rails via `svn:externals`, but I'll be switching it to the Rails 1.2 branch in the next couple of changesets, which should mean that, as soon as Rails 1.2 is released, we'll be able to push out Typo 4.1.0 pretty promptly.

### What does Rails 1.2 mean for Typo?

Over time, Typo and Rails have diverged somewhat; in part it's because we were trying to do some stuff before Rails made it easy, and we didn't necessarily make the same choices as Rails did, so we've got a pretty huge code base that's doing an awful lot of heavy lifting that could (and should) be left to Rails. One of my goals in the next development cycle is to bring Typo more into line with the Rails way. Some things are sacrosanct of course: Article permalinks are sacrosanct - if an article has the url `http://www.bofh.org.uk/articles/2006/11/24/typo-on-rails-1-2` today, then it's going to continue to have it for as long as you're running typo and on the same server. And any new articles will have canonical URLs of a similar form.

Pretty much everything else is up for grabs though. We're going to try and keep well known URLs working but they may be redirects to a more RESTful canonical version, and we're not about to break your feeds any time soon, but the admin urls will certainly be changing (again we'll put redirects in for a few releases, but they won't be supported in perpetuity).

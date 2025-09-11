+++
title = "And... relax..."
slug = "and-relax"
date = "2006-03-31T14:38:00+00:00"
draft = false

+++

Rails 1.1 got released on Sunday.

Lots of hosting services upgraded.

[Typo](http://typosphere.org/trac), both the 'stable but old' and the bleading edge versions, didn't get on with Rails 1.1. For quite spectacular values of 'not getting on'. So, we ran around like headless chickens for a bit, gradually refining the incantations needed to get your existing typo installation working again - `rake freeze_edge VERSION=3303` does the trick if you're still having problems - and then [Scott](http://scottstuff.net/), [Kevin](http://kevin.sb.org/) and I spent the best part of a day nailing down the issues.

It turns out that there was rather a lot of 'cargo cult' code in older versions of Typo. Cargo cult code is code that isn't there because it works, but because it reassures the programmer into thinking it will work. Rails 1.0 was far more forgiving of our cargo cult (excessive use of `require_dependency`, an undocumented method that doesn't quite do what we thought it did) than 1.1.

So, with that done, and a few more bugs squashed and we're almost ready for a release (future distributions will come with rails baked into the tarball - but not if you get it from the repository), both of 2.6.0 with a frozen rails distribution and of the forthcoming 4.0.0. As I write their are two issues outstanding on the [4.0.0 milestone](http://www.typosphere.org/trac/query?status=new&status=assigned&status=reopened&milestone=4.0), the trickiest of which is Scott's plan to make typo installation substantially less user hostile. However, if you're happy to deal with the slightly tricksy installation, there's never really been a better time to jump onto the Trunk. [Grab](http://www.typosphere.org/trac/wiki/DownloadCurrent) a copy and start blogging. You know you want to.

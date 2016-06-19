+++
title = "Tips for data smugglers"
slug = "tips-for-data-smugglers"
date = "2007-01-25T02:25:00+00:00"
draft = false

+++

While I was working on the `acts_as_resource` plugin trying to fix things up so that the resource finding side of things works neatly, I realised that I needed some way to get at the ordered list of parameter keys that were matched by the routing system.

One way to do it would have been to parse the path again, but that smacked a little too much of repetition, after all, the routing system knows this stuff already, but how to get at it?

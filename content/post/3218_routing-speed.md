+++
title = "Routing Speed"
slug = "routing-speed"
date = "2007-04-27T02:39:00+00:00"
draft = false

+++

So, since I couldn't sleep, I've been hacking on the routing drop in replacement.

I've reached the point where things are mostly working and I can at least run the benchmark tests.

On this machine, Rails recognizes routes at a rate of around 22K url/s.

At the start of the night, with the recognizer `rparsec`, to do the recognition, my new routing system was running at around 5K url/s, which isn't exactly great. So, I rejigged things to build a regular expression and use that to parse the request URL and the recognition rate jumped to 12K url/s. I'm really pleased with that number for the time being.

Rails goes faster because it's doing some hairy `eval STRING` optimization that makes things rather hard to follow. If I can get up to better than half that speed without the hairiness, I'm happy.

For now.

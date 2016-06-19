+++
title = "Scratching an Itch"
slug = "scratching-an-itch"
date = "2006-04-09T03:29:00+00:00"
draft = false

+++

Some time ago, around the time of the [Typo themes contest](http://typogarden.org), someone, I think it may have been [Scott Laird](http://scottstuff.net/), wrote a nifty little [sparklines](http://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=0001OR&topic_id=1) textfilter.

It's a really cunning piece of code, the problem is that, to make it work, Scott had to make some dramatic changes to the way textfilters work which, in turn, meant we had to change the way we got at the htmlized versions of our content. Before sparklines, you could do `content.body_html` in your view and it would just work, generating the html as required. After the change you had to do `content.html(`controller, :body)`, which I'm sure you'll agree is rather gruesome. So we wrapped it up in a helper method, letting you do: `article\_html(`article, :body)`, which is only slightly less gruesome.

At a stroke, we managed to break almost all of the themes entered in the contest, the eventual winner included. Ho hum.

To be honest, screwing up the winning theme's almost beside the point, I've disliked the new interface for getting at html content ever since we introduced it, but I've liked cunning textfilters more, so we've bitten the bullet and accepted the fact that Typo 4.0 will have a different theme API from the last release.

Well... I say accepted. I've been looking for a way out of it in a desultory fashion ever since we introduced it. And today I cracked it. I knew - despite a few howls of protest - that introducing a Blog class was the right thing to do even if we never take the step to full multiblog capabiliity. Today, it paid off...

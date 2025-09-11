+++
title = "Cunning Typo Sidebar Tricks"
slug = "cunning-typo-sidebar-tricks"
date = "2007-04-14T03:36:16+00:00"
draft = false

+++

If you're reading this on the website rather than through a feedreader and you look to the left you'll see either a bunch of links to cited books or an Amazon 'self optimizing links' banner (though, if it keeps 'optimizing' like it has being I'll be changing it to something else). This is implemented using a mildly hacked version of the standard typo `amazon_sidebar` plugin.

"But," I hear you say, "The sidebar's over on the right, and Typo can only support one sidebar."

And you're right, Typo's admin interface can only handle configuring one sidebar, and I'm not entirely sure how to make it work with too - hopefully our new maintainer and usability advocate, [Fr&eacute;d&eacute;ric de Villamil](http://fredericdevillamil.com/) has some ideas along those lines. So, how does it work.

Let's take a look at a fragment of my `layouts/default.rhtml` file shall we?

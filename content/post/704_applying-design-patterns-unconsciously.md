+++
title = "Applying design patterns unconsciously"
slug = "applying-design-patterns-unconsciously"
date = "2006-08-21T07:33:00+00:00"
draft = false

+++

I just realised something about the workings of new style Typo Sidebars: it's just an application of the [Parameter Object](http://www.c2.com/cgi/wiki?ParameterObject) design pattern; the `render_sidebar` helper method takes a `Sidebar` parameter object and produces a chunk of HTML. The fact that we persist the parameter object using ActiveRecord is almost beside the point - the persistence is more important to theÂ  `render_sidebars` method than anything else.

Old style sidebars couldn't really be called an application of any design pattern. Maybe that's what bugged me about them, they lacked the [Quality Without a Name](/articles/2003/07/29/the-quality-without-a-name). I'm not sure that the new sidebar architecture has the Quality either, but it's much nearer to having it.

When I was learning to play [Go](http://en.wikipedia.org/wiki/Go) someone told me that it's easier to remember professional games than those of amateurs because the moves in a professional game are generally more 'right' than those of weaker players. To a good go player, the 'right' move has an obvious purpose (or, more likely, purposes). Bad moves don't.

Maybe something similar applies to code. In good code it's clear what each piece does - maybe it can be seen as an example of a design pattern, maybe not - and the pieces relate well to each other. In bad code, the intent and relationships are muddied.Â 

### Do Design Patterns Help?

Continuing the Go analogy, Design patterns can be likened to [*joseki*](http://senseis.xmp.net/?Joseki) - 'standard' patterns of good play in the opening of the game. It's useful to know them, but what's *really* important, and what takes time, practice and deep study to get good at, is using and adapting them well in the [*fuseki*](http://senseis.xml.net/?Fuseki) - the opening and its wider context. You can play a 'perfect' joseki in one little corner, but its influence radiates across the board and if you don't pay attention to what that influence is and use or adapt it well, you're screwed.

I think the analogy to the use of design patterns in programming is fairly obvious. Don't you?

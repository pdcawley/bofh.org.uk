+++
title = "... and relax"
slug = "and-relax"
date = "2010-11-12T13:06:00+00:00"
draft = false

+++

Crikey! What an intense few days.

Last Friday, I got some email from [Giles Bowkett](http://gilesbowkett.blogspot.com/) saying that he'd had to flake on a conference in Sweden and could I take his place. The brief was to "be interesting, and I know you can nail that in your sleep".

Last Saturday, I read it. And being flattered by Giles's silver tongue answered to say "Probably, when is it?".

<!--more-->

Then I did what I should possibly have done first, I Googled "Giles Bowkett Sweden", and discovered that he was scheduled to be speaking at Øredev in Malmö on the 11th of November. The rule is "Never refuse a gig", so I stalked the wily passport to its lair in the big stack of magazines and music books on my bedside table in the flat and set about thinking how I could fill two 45 minute talks with 'something interesting'. I reckoned that trotting out "Why I Came Back to Perl" at Øredev wasn't really going to fly (maybe if there were lightning talks), so that meant two new talks.

I can't quite believe that I just about pulled off a talk about higher order Javascript. I've not used Javascript for a couple of years now, and don't have particularly fond memories of the languages from when I did (I like pretty much everything about the language except the syntax, which is just icky and verbose in places where it shouldn't be), but thinking in Higher Order Functions is pretty much the same in any language that can support it.

As I wrote my first slide, I realised that there had to be a better way. I was writing a dead simple function to calculate the sum of a list of numbers (first rule of introducing higher order functions: stick with elementary mathematical stuff so far as possible. It's hard enough wrapping your head around what's going on without having to wrap it around some new domain at the same time) and I'd written this:

```javascript
    var sum = function (list) {
      var i;
      var r = 0;
      for (i = 0; i < list.length; i++) {
        r = r + list[i];
      }
      return r
    }
```

and I was banging hard against the right hand margin and had about reached the limit for the amount of legible code I'm prepared to fit on a slide. And ideally, I wanted to get an implementation of `sum` and of `product` on the same slide to show the similarities between the two of them. So, [CoffeeScript](http://jashkenas.github.com/coffee-script/) was my saviour. Suddenly I was writing:

```coffeescript
sum = (list) ->
  r = 0
  (r = r + i) for i in list
  r

product (list) ->
  r = 1
  (r = r * i) for i in list
  r
```

and most of the language noise went away and I could concentrate on getting the slides written and the knowledge transferred. My delivery wasn't everything I could have hoped for - I had a horrible patch early on where I lost where I was in the talk and then noticed some bugs in the slide code that left me slightly croggled, but putting a 'Questions?' slide in the middle of the talk turns out to have saved my bacon. Good questions that showed that people were still engaged with what I was trying to talk about, which helped me get my thoughts back in order and I think I did much better in the second half. Either that or my whistle stop introduction to memoization and using streams to shift the point at which stuff gets evaluated left people too baffled to walk out on me. Øredev has a very neat evaluation system, as people leave the room, they throw a red, green or yellow card into a basket. This means that evaluations are very fast. I can't remember how many yellows I got, but I had 26 greens and 20 reds, so if nothing else I had divided opinions. I take that response to mean that I wasn't boring.

The second talk was even sketchier. I was substituting for the rolling ball of awesome that is Giles' [Archaeopteryx](http://gilesbowkett.blogspot.com/2008/10/archaeopteryx-rubyfringe-presentation.html) presentation/performance. I went in with ten minutes of slides, 45 minutes to fill, a few ideas for demos, a rough idea of the direction I was headed in and no firm idea at all about whether there was going to be a conclusion at the end of it. By the end I had taught a room half full of programmers to sing a song about Space, to split up into parts and do it as a round, and to imitate a theremin by stealing a kick ass idea from the fabulous {{< marginnote >}}{{< embed "http://www.youtube.com/watch?v=ne6tB2KiZuk" >}}{{< /marginnote >}} of Bobby McFerrin at the World Science Festival last year[1]. I even managed to reach some kind of conclusion. And all the cards for that talk were green. I dance the dance of happy.

I met a bunch of good people, geeked out about singing with Kathy from [Panda Transport](http://www.pandatransport.com/), picked a few fights and generally tried not to get too much .net on me. And it wouldn't be a developer conference without the odd bit of sexism fail, it was just a shame that the source of it was Nolan "Atari" Bushnell. Okay, so Nolan's from the seventies, but the "random pretty girl" slide was an old joke when presentations still used honest to goodness slides. I wasn't entirely convinced by his futurism either - maybe seeing the Atari logo in Bladerunner went to his head.

[1] If you're trying to visualize me jumping up and down and running about like Bobby does in that video, I kind of left that bit out and relied on hand position instead. One day I shall be fit enough to do the whole body version, but I'm still buzzing on how well it worked without it. If you were in the talk, you were amazing.

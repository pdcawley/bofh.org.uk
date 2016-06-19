+++
title = "A quick Javascript formatting tip"
slug = "a-quick-javascript-formatting-tip"
date = "2008-04-16T04:01:00+00:00"
draft = false

+++

IE's a pain. The particular pain I want to write about is its pickiness about Javascript object literals. Consider the following Javascript object:

<code>

    { success: function () {...},
      failure: function () {...},
    }

</code>
If you're used to programming in Perl or Ruby, that trailing comma's perfectly fine, in fact leaving it there is sometimes considered good practice because it makes it easy to extend the hash, just add a new row and leave another trailing comma.

The trouble is, it's not strictly legal to do that in Javascript. Pretty much every implementation out there will allow it though.

Except IE.

So, I've taken a leaf out of Damian Conways <cite>Perl Best Practices</cite> and writing my object literals as:

<code>

    { success: function () {...}
    , failure: function () {...} 
    }

</code>
By sticking the comma at the beginning of the line, I'm never going to make an object that breaks IE, and adding a new line to the hash is straightforward too. Just stick the cursor in front of the <code>}</code>, type my leading comma, space, attribute name, and hit return when I'm finished.

I've also started using the same practice pretty much everywhere else that I've got a comma separated list of things:

<code>

    var foo
      , bar
      , baz
      ;
    $.each( anArray
          , function () { ... }
          );

</code>
It looks weird at first, but trust me, it grows on you.

### Update

In the comments, I make reference to tweaking Steve Yegge's excellent [js2-mode](http://code.google.com/p/js2-mode/) to handle leading comma style a little more gracefully. Since then, I've made it work and attached a diff to [this issue](http://code.google.com/p/js2-mode/issues/detail?id=64) on the project's issue tracker.

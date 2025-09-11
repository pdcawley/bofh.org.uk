+++
title = "Javascript heresy"
slug = "javascript-heresy"
date = "2008-10-29T08:16:00+00:00"
draft = false

+++

<p>
So, remind me, what's the rationale for always using the optional <code>;</code> when you're writing Javascript? The only reasons I can think of, off the top of my head are, "Because you'll break minifiers without it" and "Because Douglas Crockford doesn't like it". Well, broken minifiers that can't parse the language correctly can kiss my fat hairy arse and argument from authority cuts little ice here.

</p>
<p>
[Gareth Rushgrove](http://morethanseven.net/) pointed me at [an article](http://icanhaz.com/semicoloninsertion), which suggested that it's because Javascript will insert a semicolon after the `return` in:

    return
    { key: "value" }

</p>
<p>
But that's not exactly surprising, and falls squarely into the "Don't do that then" category of bugs, or putting it another way, the [Dominus](http://blog.plover.com) Doctrine ("You can't just make shit up and expect the computer to know what you mean, retardo!") applies.

</p>
<p>
Ruby also has an optional semicolon, but good style is avoid using them and we seem to survive. In fact, the Ruby parser is rather less capable than Javascript's:

    jQuery('.class')
      .addClass('whatever')
      .html('New inner HTML')

is legal Javascript, but, in Ruby you have to write:

    jQuery('.class') \
      .addClass('whatever') \
      .html('New inner HTML')

because if you don't the compiler throws its toys out of the pram and, for bonus points, the resulting parse error implies that the parser knows what you meant but decided to throw the error anyway. Ho hum.

<p>
<p>
Is there something I've missed? Or should I make a preemptive stand against incompetent minifiers and start writing my Javascript without semicolons?

</p>
<h3>
Updates

</h3>
<p>
In the comments, "James" offers a succinct piece of code using Prototype that demonstrates the problem rather neatly. In the absence of semicolons, code like:

    var foo = 3
    , bar = 2 + foo

    [foo, bar].each(function (i) { console.log(i) })

gets parsed as

    var foo = 3
    , bar = 2 + foo[foo, bar].each(...)

Which isn't exactly what you want. If I were feeling churlish, I might argue that such problems are one reason why the jQueryish way:

    var foo = 3
    , bar = 2 + foo
    $.each([foo, bar], function () {...})

is a better way of iterating over things, but I'm not entirely sure that it is.

</p>
<p>
Looks like I'll keep taking the semicolons.

</p>


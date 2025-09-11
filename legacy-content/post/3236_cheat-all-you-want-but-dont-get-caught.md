+++
title = "Cheat all you want, but don't get caught"
slug = "cheat-all-you-want-but-dont-get-caught"
date = "2007-06-20T17:16:00+00:00"
draft = false

+++

As far as I can tell, one of the Smalltalk optimizers' mottoes is "Cheat all you want, but don't get caught".

Well, this morning, I caught Squeak with its hand in the till.

One way I attempt to bootstrap myself towards understanding of code is to try and make it better, if that makes sense. So, I'd run SLint over the OmniBrowser package and was trying to shorten a method. One thing that struck me as rather ugly was a piece of code that ran like this:

<code>

<pre>
|selection|
...
selection := OBChoiceRequest prompt: nil labels: usersNames values: users.
selection ifNotNil: \[selection browse\].
</code>

So I thought I'd tweak `ifNotNil:` so that, if its receiver isn't nil, it will pass itself as an argument into the block, which will let me rewrite that ugly code with:

<code>

<pre>
(OBChoiceRequest
prompt: nil
labels: usersNames
values: users) ifNotNil: \[:selection| selection browse\].
</code>

So, I went to have a look at the implementation of `ifNotNil:` and found that it was already doing exactly what I was after.

At this point, I had a slight premonition of danger, so I brought up a workspace and tried to print the result of running `10 ifNotNil: [:i| i + 10]` and got a compiler error, complaining that `ifNotNil:` takes a 0 argument block. Which isn't what the implementation of `ifNotNil:` thinks.

I'd caught Squeak's optimizer cheating.

What appears to happen is that Squeak catches conditional code and rewrites it before passing it to the compiler. The rewritten code uses VM level primitives where possible. I needed to fix it so that it would only rewrite any calls to `ifNotNil:` with a zero argument block.

It took a while, but my local image now optimizes `ifNotNil:` correctly (the `ifNotNil:ifNil:` and `ifNil:ifNotNil:` forms are another matter though, but I shall live. Now, if I can just work out where to submit the changeset to...

I have mixed feelings about this. On the one hand, I've just changed something in the workings of Squeak, on the other, it's not been quite as easy as I'd expected it to be. It seems that, if you go poking around in methods that are defined on ProtoObject, don't be surprised if changing things doesn't quite do what you expect.

Maybe I should have just written a `ifNotNilDo:` taking a single argument block, but that just felt ugly... Ho hum.

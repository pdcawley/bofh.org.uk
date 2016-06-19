+++
title = "Twice now"
slug = "twice-now"
date = "2009-11-25T16:49:00+00:00"
draft = false

+++

In Ruby, when you're doing division on integers, things can get a little counter intuitive. For instance, <code>6/4</code> is obviously <code>1</code>. At least, it is until you decide that you'd rather have numbers that behave a little more like 'real' numbers and you do <code>require 'mathn'</code>, a module in the Ruby standard library (ie, it comes as part of ruby). Then you enter a whole new world of rational maths, where <code>6 / 4</code> returns <code>3/2</code>.

Several very fine and useful Ruby gems rely on the workings of `mathn`, including `ruby-units`, which is a spiffy tool for avoiding problems when one team is working in kilometres and the other in miles and it's no fun at all when your space probe is suddenly incommunicado.

Other fine and dandy Ruby gems include `ultrasphinx` and `webrat`. Both of these two (and no doubt others) rely on the the fact that <code>302/100 == 3</code>.

Hmm... can you see my problem?

Please, if you're working on a gem that you intend to publish widely, then adopt the practice of *never* trusting that dividing an integer by another integer will return a third integer. You're not even making yourself a hostage to some other gem, you're making yourself a hostage to the standard library. Always do <code>(an\_integer / another).to\_i</code> and your code will be so much more robust.

I've got a pull request and lighthouse ticket in for webrat and, once I've hit 'publish' on this post, I shall be doing the same thing for ultrasphinx, but I'm sure there are other gems out there with the same problems. Please people, check your assumptions.

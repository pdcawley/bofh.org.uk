+++
title = "A tale of two languages"
slug = "a-tale-of-two-languages"
date = "2010-07-25T20:02:00+00:00"
draft = false

+++

While I was at OSCON I found myself chatting with various non-perl and ex-perl folks and many of them had the same impression of Perl as a great language for hacking out short scripts to get stuff done, right now and possibly under severe time pressure. For these people, those scripts would very rapidly become unmaintainable. And it's easy to see why they came to that conclusion. Perl was originally designed and implemented by a systems administrator as a tool to make his every day tasks easier so there are a bunch of shortcuts and defaults that do exactly that - make Larry's life in 1987 easier. Which is great when you're whipping up a script to meet an immediate demand, but horrendously cryptic when you look at it later.

This is a perfectly valid way of looking at a language that I'll call 'desperate perl'. Desperate Perl scripts tend to be written by folks who don't necessarily know all of Perl and who certainly don't know all of its ecosystem. The scripts are often written to a tight deadline and to handle a specific task. They certainly weren't written with even half an eye to maintenance. And they live on. Boy, do they live on. There are CGI scripts written in 1995 that are still in every day use, never mind that they're buggy, insecure and well nigh impossible to maintain, still these zombies shamble on, colouring the opinion of everyone who ever has to deal with them.

If there's one thing we've learned from this, it's that code is forever. When we were young, we did stupid things, but we grew up and everyone but our nearest and dearest (who have the good grace never to mention it) forgets that, once upon a time, we set fire to the living room rug while playing with matches/ate cornflakes with HP sauce/had a brief flirtation with the young Conservatives/had a brief flirtation with a young Conservative. We are forgiven.

Not Perl.

Perl's reputation as a language that's fine for writing short scripts, but which rapidly becomes unmaintainable once you try and write larger systems is bestowed upon it by people who looked at `formmail.pl` once and ran screaming into the night (a not unreasonable response).

But Desperate Perl isn't the only language in the binary. Large scale programming with Perl uses, or should use, a different subset of the language. A key indicator that you're looking at large scale Perl is when the script or library starts with something along the lines of:

```perl
\#!/usr/bin/perl
use strict;
use warnings;
```

or, if it's a library:

```perl
package Whatever;
use Moose;
```
There are other signifiers, but the important thing is that the code is written in `strict` compliant Perl. `use strict` crops up so often that the recent crop of modern (or enlightened if you prefer) perl modules, like Moose, turn it on by default. Indeed, if you're using Perl 5, version 12 and you begin your code with `use 5.12.0`, Perl turns on strictures as well as enabling all the new features of the language.

We've had `strict` and `warnings` since forever and they've been strongly recommended for everything longer than about two lines since forever too. Those of us who program in Perl for a living get rather annoyed by the 'Desperate Perl' blinkers worn, however unwittingly, by so many outside our community. We write our code in a subtly different language. We write tests first, we have done for years. We write clean, maintainable, object oriented code. Nowadays we use Moose to help us with that; Perl's out of the box OO features are, let us say, idiosyncratic - Moose makes our lives better.

Large Scale Perl is about more than just what's in the distribution. Large Scale Perl has a wider culture than just the contents of the distribution. It's about learning to use the CPAN and the CPAN toolchain. When I start to work on anything perl related that I expect to live for more than I couple of minutes, I start by doing:

```perl
$ mkdir ProjectName
$ cd ProjectName
$ mkdir t lib
$ $EDITOR t/initial.t
```

Because I'm writing large scale perl, my modules go under `lib`, my tests go under `t`, any scripts I expect to install in my path go in script and they don't have a `.pl` suffix. I lay my code out this way because, well, that's just how it's done. I run my tests from the project directory by doing `prove -l ./t` and, when I realise I have something that's worth distributing (or deploying to the staging server), I add a `Module::Install` based `Makefile.PL` that lists my prerequisites, the scripts to install and various bits of metadata. By now, this is second nature - many clever people over the years have come to a collective decision about what a good Perl project looks like and they've written a fabulous toolchain for me as well.

If I distribute my work through the CPAN, there's a large community of CPANTesters who will attempt to build my module and run its tests on a bewildering variety of platforms, and they'll send me bug reports when those tests fail (bug reports which I can opt out of as well). Any documentation I write will be nicely formatted and accessible through [search.cpan.org](http://search.cpan.org) along with links to sites that'll draw me a tree of all the modules mine depends on and, if it becomes adopted by the community, nice lists of all the modules that depend on mine as well. My module will get its own section in [rt.cpan.org](http://rt.cpan.org) so there's a standard place for people to report bugs.

Large Scale Perl comes with an entourage. Tools like [Perl::Critic](http://search.cpan.org/dist/Perl-Critic/) can check my code for bad style, [Try::Tiny](http://search.cpan.org.uk/dist/Try-Tiny) helps me get exception handling (which is annoyingly tricky when you attempt to get it right barehanded) right, [Devel::NYTProf](http://search.cpan.org/dist/Devel-NYTProf) helps me work out why it's running slower than I expected. The list goes on.

But, all the world sees is Desperate Perl. Search for "Perl Tutorial" on google and the first few hits are horrendously out of date - when I checked, the number one hit was all about perl 4. Apparently, O'Reilly's biggest selling Perl title is the new edition of [Learning Perl](http://oreilly.com/catalog/9780596520113/), which was published in 2008 and is a cracking tutorial. But the second biggest Perl title is [Programming Perl](http://oreilly.com/catalog/9780596000271/) which is *ten years* old this month. It covers Perl 5 version <em>6.0.</em> I'm not even sure you can still build perl 5.6.2 - released more than 6 years ago - on modern systems. I certainly can't imagine why you might want to. Programming Perl is a great book, but there are things it discusses that aren't just deprecated, they've been recognized for the clusterfuck they were and excised from the language with extreme prejudice. As a community, we could really use a new edition. It would seem to make sense for someone to take the job on. A computer book that's still selling 10 years after its first publication, 8 years after the language it documents, 5.6 was superceded by 5.8 looks like an ideal candidate for a new edition to me.

But then, a glance at the release dates of major Perl versions shows us why O'Reilly might hesitate to revise the book. 5.6 got released in March 2000, 5.8 in July 2002, 5.10 in... December 2007, 5.10.1 (which might have been better named as 5.12.0, given how much got changed/fixed) in August 2009, 5.12 in April 2010. That 5 year gap is the sort of thing that might give any publisher pause. Still, the new strategy of releasing regularly and often is starting to bed in. It's getting easier to write `perldelta.pod` for each release and Jesse Vincent seems to be doing an excellent job of getting new release managers trained up and generally destressing the business of releasing a new perl. Who knows, by this time next year the perl release process may be no more difficult than releasing a module to CPAN. We do write good tools after all.

Reliable, regular releases with no big surprises are the sort of thing that gives programmers and organizations confidence that we're here for the long haul. We perl programmers know that all sorts of good things were happening on the CPAN in the long gap between 5.8 and 5.10, but as good as it is, CPAN's not as visible as the version number on the binary.

Hmm... I appear to have rambled some way from my starting point. Which probably means its time to stop. If I'm spared, I might manage to rustle up some kind of conclusion in a later post. Just remember, there are at least two perls: Desperate Perl, and Large Scale Perl. Both are excellent languages in their spaces, but you can't (or shouldn't) draw conclusions about one based on observations of the other.

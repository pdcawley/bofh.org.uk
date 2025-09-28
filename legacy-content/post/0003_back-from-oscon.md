+++
title = "Back from OSCON"
slug = "back-from-oscon"
date = "2003-07-17T09:03:00+00:00"
draft = false

+++

A good time was had by all

This was my first OSCON, I've usually been unable to make it over, due either to lack of time, finances, or both. This time there was a lack of finances and plenty of time, so we threw caution to the wind and headed off to sunny Portland.

And boy, was it sunny—I thought the Pacific Northwest was supposed to be rainy.

#### PONIE announced

The conference proper opened with Larry's State of the Onion speech where he announced <a href="http://www.fotango.com/">Fotango's</a> sponsorship of <a href="http://www.poniecode.org/">PONIE</a>, Arthur Bergman's attempt to port Perl 5's underlying data structures to Parrot. The idea is that, if everything goes well, we'll be able to keep Perl 5's front end (parser, opcode generation, etc), but use Parrot as the back end. Approaching things in this fashion should mean that the existing CPAN codebase of XS based modules will simply need a recompilation to run on Ponie, which is fantastic.

James Duncan of Fotango points out that sponsoring Arthur to do this work is naked self interest on Fotango's part; they have nearly 100,000 lines of Perl 5 code which depend to a greater or lesser extent on CPAN modules, the most obvious of which is the <a href="http://search.cpan.org/search?query=DBI&#38;mode=dist">DBI</a> family of modules, which provide standardized Perl access SQL databases. By porting Perl 5 to Parrot, Fotango will be able to jump to the new, JIT-enabled engine and then slowly port their Perl 5 codebase to Perl 6 as that language becomes available.

The beauty of Ponie is that, whether the port proves to be a success or not, Perl wins anyway because, in order to know if Ponie works properly, the team are going to have to write a comprehensive test and possibly documentation suite for Perl's internals, and those tests will be available in Perl 5, and better test coverage is always a good thing.

And Perl 6 wins too, if Ponie succeeds, we get the Perl 5 emulation for free, and the development of such a non-trivial Parrot application should really help shake out the bugs bottlenecks in Parrot, which should help Dan with the <a href="http://www.sidhe.org/~dan/blog/archives/000219.html">Pie-thon challenge</a> if nothing else.

#### Perl 6 Rules! (or do I mean Perl6::Rules?)

Ever since <a href="http://dev.perl.org/perl6/apocalypse/5">Apocalypse 5</a> outlined Perl 6 Rules and Grammars â€“ Larry's reinvention/replacement of regular expressions, there's been a good deal of anticipation and muttering. Everyone seems to want them, but the mutterers have doubted whether it will be possible to implement them.

They should doubt no more.

In his last talk of the conference, Damian presented an overview of Perl 6's rules system. With running code. Okay, it's not complete, it's not ready for public consumption, and Damian's just disappeared off for a well deserved month long holiday. But it will be ready for Christmas. Bets on which Christmas are not being accepted.

There are several amazing things about the module that Damian presented:

1.  It's in pure perl
2.  It's 700 lines long
3.  It comes with a comprehensive set of debugging methods
4.  It was entirely written over the course of OSCON (that's if we don't count the months of Damian's 'mind time' it's taken up while he planned it out...)

It's not (by any stretch of the imagination) complete, but it's a fantastic piece of work, I'm eager (nay panting) to see the completed module.

Once the module is released there's still a good deal of work to be done, but this is a vital step in the process of bootstrapping Perl 6 (in his later Apocalypses Larry has taken to using Perl 6 rules to describe Perl 6 syntax), both the language proper, but also the possibility of more, better, and complementary Perl6::\* modules implementing chunks of Perl 6 functionality in Perl 5 where people can experiment with the language before it exists for good.

#### I have a cunning Plot

On the Thursday of OSCON, inspired by Leon Brocard's "Little Languages in Parrot" talk and Dan's "What's new in Parrot" talk, I attempted to implement a tiny Scheme interpreter on Parrot in one day (we called it PLOT â€“ Parrot gets Lisp on OSCON Thursday, which is a lousy acronym but we were pressed for time). And failed dismally. But we do have the empty list implemented as a closure, so it's a simple matter of programming from here on...

#### And there's more

Of course, that's not everything, but it's certainly the highlights from the Perl 6 point of view. I shall probably write about some of the other sessions later.

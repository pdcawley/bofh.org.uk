+++
title = "Thinking about the virtues"
slug = "thinking-about-the-virtues"
date = "2009-03-15T02:19:00+00:00"
draft = false

+++

I got a bit of stick on IRC last night for some of the choices I'd made when I was writing [**Test::Class::Sugar**](http://github.com/pdcawley/test-class-sugar/), in particular because one of the prerequisites is [chromatic's](http://www.modernperlbooks.com/) handy and opinionated [**Modern::Perl**](http://search.cpan.org/dist/Modern-Perl) module. The 'controversial' aspect of Modern::Perl is that, when you use it, your code won't run on any Perl before Perl 5, version 10.

The thing is, I don't *care* about older perls any more. Version 10 features like <code><sub></sub></code> and <code>//</code> are too convenient to fart about writing circumlocutions just to run on a version of Perl that I have no intention of ever using again.

Actually, that's not quite true, those version 10 features are too convenient *for me* to fart about working around their absence. If you think that a module I write is useful enough that you want it to work on version 8, then of course I'll accept your patch. But don't be surprised if, when I start adding new features, I break the backward compatibility.

Also, on the happy day that version 10.1 escapes the pumpking patch, I'll be setting that as my minimum perl version even if chromatic doesn't bump the version number in Modern::Perl.

### It's all about the virtues

Your context is different from mine. I'm writing in Perl again for my own amusement more than anything. There are developments in modern Perl – tools like [**Moose**](http://search.cpan.org/dist/Moose) and Devel::Declare – that I think are exciting and important. The [**Announcements**](http://github.com/pdcawley/perl-announcements) project I started was as much about playing with the new tools as it was about trying to write something of wider utility. Test::Class::Sugar arose as a direct result of attempting to write Announcements and the desire to write test classes without hoopage. My principle drive then, is impatience to get Test::Class::Sugar to the point where I can get back to writing Announcements.

But then laziness and hubris kick in. So the code needs some polish. The parser and the code generator need to be disentangled, I need to get Adrian Howard to apply the little patch I had to make to Test::Class. Laziness demands I document it.

Impatience tells me to lean on the features of modern perl - that way I can get back to being a user of the new library as quickly as possible. Laziness tells me that I'm not going to need backwards compatibilty. Hubris tells me my work is good enough that someone who does will like it enough to send me a patch.

Everybody wins.

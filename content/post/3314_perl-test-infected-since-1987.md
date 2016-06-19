+++
title = "Perl: Test Infected since 1987"
slug = "perl-test-infected-since-1987"
date = "2009-03-04T16:33:00+00:00"
draft = false

+++

Here's something interesting. [This](http://github.com/github/perl/tree/8d063cd8450e59ea1c611a2f4f5a21059a2804f1/t) is the test directory from the very first version of Perl, released in 1987 and described by Larry as 'a "replacement" for awk and sed'. Day one of Perl, and we already have the bare bones of the [Test Anything Protocol](http://en.wikipedia.org/wiki/Test_Anything_Protocol) and a prototype for [Test::Harness](http://search.cpan.org/dist/Test-Harness/) in the [TEST](http://github.com/github/perl/blob/8d063cd8450e59ea1c611a2f4f5a21059a2804f1/t/TEST) file.

If we truffle about in the various other branches we find other useful milestones for module developers:

-   5.000, in 1994 came with the first iteration of `h2xs` which could be used to generate the basic boilerplate for a perl module. Even today, with more sophisticated module starters available, you won't find a CPAN module of repute that doesn't follow the basic directory structure laid down in this utility. `ExtUtils::MakeMaker` generates a Makefile with a test target which runs all the tests in the `t` subdirectory
-   5.002, in 1996, `h2xs` starts stubbing test.pl
-   5.003\_12, late 1996, first version of CPAN in the Perl distribution. From day one, CPAN would not install a module if any tests failed, unless you forced it.

Meanwhile, Ruby:

-   Has only recently embraced a language test suite
-   Appears to have no standard layout for gem distributions
-   Doesn't run tests as part of the installation process for a new gem

Is it any wonder that [chromatic](http://www.modernperlbooks.com/) gets a little [cranky](http://use.perl.org/~chromatic/journal/38494) when sweeping claims are made about how spiffy Ruby's testing culture is?

There are those who claim that CPAN is Perl's shining glory, but it's not really the collection of servers, it's the toolchain that enables it, and that toolchain can exist because so many libraries follow a pretty minimal set of conventions.

I'd love to see the Ruby community settle on a similar, single, set of conventions for the way things should work. Start with a guarantee of a either a top level `Rakefile` or `setup.rb` with `build`, `test` and `install` tasks. Make rubygems run the tests before installation, if the target is available, and halt the installation if they fail. Make it easy to send reports of test failures to module authors (look at the Perl [CPAN](http://www.cpan.org/) and [CPAN Testers](http://www.cpantesters.org/) sites, and their associated tooling for ideas).

I know... I should STFU And Write Some Code.

### Update

Further investigation shows that `gem install -t whatever` *does* run the tests as part of the installation process. The capability is there, but it's turned off by default. How depressing.

+++
title = "Wanted: module/starter.rb"
slug = "wanted-module-starter"
date = "2007-04-14T04:17:00+00:00"
draft = false

+++

I sometimes think that one of the reasons that CPAN is such a huge advantage for Perl is the ease with which you can *contribute* to it. It's all very well having a tool for installing libraries from the archive, any fool can do that, but CPAN has tools for getting started with a new library too.

If I want to start a new Perl project, I do:

module-starter --module=AuthenticationFairy

and the script goes away and builds me an AuthenticationFairy directory complete with all the boilerplate stuff filled in. I know that when I look in `AuthenticationFairy` I'll find a sensible directory structure, a `Makefile.PL` that will do the right thing, a `lib/AuthenticationFairy.pm` with the documentation boiler plate filled in and a test directory. And I rejoice because my yaks have been sh aved and I can get on with the interesting business of writing my first test and making sure it fails.

I've not found an equivalent for Ruby. Yet.

I have found `sow`, which is part of the [Hoe](http://blog.zenspider.com/archives/2007/02/hoe_version_120_has_been_released.html) package, which is definitely a start.

Are there any other tools along the lines of Module::Starter that I've missed?

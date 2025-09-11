+++
title = "Dear LazyWeb"
slug = "dear-lazyweb"
date = "2006-08-11T13:32:00+00:00"
draft = false

+++

I was just about to start writing a multimethod system for ruby when I realised how much I miss Perl tools like [**Module::Starter**](http://search.cpan.org/~petdance/Module-Starter-1.42/lib/Module/Starter.pm). [CPAN](http://www.cpan.org/) has a whole suite of tools which make it at least as easy to do the Right Thing when setting up your project than it is to succumb to ad hockery. Start your project using **Module::Starter**, and you get a sensibly laid out that works well with standard Perl build/installation tools, a stub of your module, with the various boilerplate bits of the documentation filled in, a test directory, README, etc...

It shouldn't be impossible to do the same thing for Ruby. Most of the tools are already here. Just use the Rails generator to build a standard stub directory layout, complete with gemspec, readme, license, tests, etc. To a certain extent, the only real issue is deciding on what a sensible project template should be.

Or maybe this already exists and I just haven't found it.

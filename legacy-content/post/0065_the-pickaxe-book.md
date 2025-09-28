+++
title = "The Pickaxe Book"
slug = "the-pickaxe-book"
date = "2005-08-01T14:20:00+00:00"
draft = false

+++

I can't remember when [Adam Turoff](http://use.perl.org/~ziggy/journal/) pointed me at [Ruby On Rails](http://www.rubyonrails.com/), but I'm still grateful. Blame Adam for the fact that this weblog is no longer running on Movable Type, but on [Typo](http://typo.leetsoft.com/trac/). My initial plan was to roll my own blogging software on top of Rails, but by the time I got my act together it just made sense to grab an existing package and extend it as required.

I first heard about Ruby when I read Andy Hunt and Dave Thomas's excellent book <cite asin="020161622X">The Pragmatic Programmer</cite>. I thought, "that looks interesting", so I read the first edition of Programming Ruby aka the Pickaxe Book, learned the syntax and some of the idioms and then sort of forget about it. As a programming language Ruby pushes a lot of my buttons: it has closures, objects all the way down, dynamic typing and its refreshingly concise. However, I never quite got round to writing anything in it (not the language's fault, I wasn't writing anything in anything at the time).

Then along came Ruby on Rails and it's lovely. I saw the first version of the [Rails Video](http://media.nextangle.com/rails/rails_setup.mov) and it knocked me out. Rails does so much *right*. Here was a web development tool that did most of the heavy lifting for you, and makes it easy to do things Right. David Heinemeier Hansson was obviously getting a great deal of leverage from Ruby's dynamic nature and I wanted some of that.

However, I found that Ruby had moved on since I skimmed the first Pickaxe so I got hold of a copy of the second edition of <cite asin="0974514055">Programming Ruby</cite>. What a cracking book it is. My personal benchmark of quality when it comes to a language reference is the first edition of <cite>Programming Perl</cite> which, as well as being a language reference was an introduction to a programming ethos. Whilst I don't think the Pickaxe is quite that good, it's definitely up there. The chapter on 'Duck Typing', for instance, is wonderful. It's a well made argument for dynamic typing and it deserves to be widely read. I sometimes think that there are two sorts of programmers in the world, those who think typed *values* are essential, and those who think typed *variables* are essential. I'm definitely a typed values kind of guy. Typed variables have their place, but that place is in the optimization toolbox next to the profiler.

Ruby's a great language. I don't *think* it'll ever be my primary language -- I still have high hopes for Perl 6, which will have all the things I like about Ruby whilst addressing some of the things about it that make me uncomfortable. But in the unlikely event that Perl 6 crashes and burns, Perl 5 finally has a competitor that I enjoy using if only because I'm sick of typing:

sub foo {
my $self = shift;
my($arg1, $arg2) = @\_;

at the start of every method.

def foo(arg1, arg2)

is just so much shorter. And until I can do

method foo($arg1, $arg2) {

Ruby will entice me. Maybe I should just get Emacs to fill in the Perl 5 boiler plate...

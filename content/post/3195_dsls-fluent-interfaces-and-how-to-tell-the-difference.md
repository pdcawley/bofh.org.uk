+++
title = "DSLs, Fluent Interfaces, and how to tell the difference"
slug = "dsls-fluent-interfaces-and-how-to-tell-the-difference"
date = "2007-03-15T10:39:00+00:00"
draft = false

+++

I'm getting heartily fed up of people banging on about Domain Specific Languages. It seems that every time someone writes a Ruby library that uses class methods, symbols and hashes reasonably sensibly they get delusions of grandeur and call the result a Domain Specific Language (or maybe an 'embedded' DSL).

In a sense, they're right, but it's a pretty compromised language simply because you're stuck with the Ruby parser. Scheme and Lisp hackers probably look at (say) ActiveRecord and sneer. Heck, even Perl programmers have grounds for [getting their sneer on](http://use.perl.org/~chromatic/journal/32583)

Now, before any Ruby programmers go getting on their high horse about Perl programmers being indisciplined louts, may I refer you to [Getopt::Euclid](http://search.cpan.org/~dconway/Getopt-Euclid-v0.1.0/lib/Getopt/Euclid.pm), an alternative to Perl's Getopt::Long library.

Getopt, or something like it, is pretty much a universal among programming languages. It's the library that makes it 'easy' to write commandline programs with unix style switches. It's often one of those functions that ends up taking an ugly argument string which defines all the possible flags your command could have. As interfaces go, it's often actively user hostile - the argument string is a DSL, but it's one that took its design cues from the notorious `sendmail.cf`.

So, Damian Conway fixed it. To use Getopt::Euclid you just import the library and then write your command's documentation using Perl's inline POD with a couple of extra Euclidean extensions and you're done. Getopt::Euclid treats your documentation as its specification and builds its option parser from that.

Now that's what I call a DSL. Entirely embedded in the Domain Specific (natural) Language of documenting a command line program.

Damian's a genius at this sort of thing. Check out [List::Maker](http://search.cpan.org/~dconway/List-Maker-v0.0.3/lib/List/Maker.pm), where he finds a little used part of perl's syntax and wedges in a bunch of cunning ways of building lists, including something that looks remarkably like list comprehensions.

And this is in Perl 5; the version of the language that doesn't have explicity support for syntax modification.

Another example of the kind of thing that's possible without monkeying with the parser is the stuff [Jifty](http://jifty.org/view/HomePage) (originally JFDI), in particular the way [Jifty Schemas](http://search.cpan.org/~jesse/Jifty-0.70117/lib/Jifty/Manual/Models.pod#Schema_definition_language) work.

### What's my point?

I'm not saying don't take the time to make your interfaces 'language like'. However, there's a lot to be learned from the way other languages have approached the idea of the DSL or 'little' language. Implementing something like Jifty's Schema's in Perl is far from easy (though the techniques needed are getting better understood all the time) and involve ferreting around in dusty corners of an already arcane syntax, but the beauty of getting it right is that you simply don't have to care about how its implemented. The neat bit, the bit that's worth pinching is the syntax of the resulting DSLs...

Oh yes, and, while you're about it, take a look at what Why the Lucky Stiff is doing with [hpricot](http://code.whytheluckystiff.net/hpricot/), definitely one of those libraries that goes out of its way to make life easy for its users.

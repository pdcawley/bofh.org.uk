+++
title = "Today's Noun Is: Notation"
slug = "todays-noun-is-notation"
date = "2007-08-28T16:20:00+00:00"
draft = false

+++

Remember back when I wrote about [metaprogramming and programming](http://www.bofh.org.uk/articles/2007/05/20/metaprogramming-programming) being the same thing?

Well, Libraries, Domain Specific Languages, [Domain Agnostic Languages](http://www.bofh.org.uk/articles/2007/05/19/domain-agnostic-languages), Pidgins, YAML, .INI files and the like are all the same thing. They're all notation.

That's the thing to remember. Good notation makes life easy. Bad (or inappropriate) notation makes it hard (or, in some cases, fun). There's no point striving to write a pidgin or DSL if your problem can be solved easily enough with a well factored set of loosely coupled classes. After all, even the neatest and cleanest of domain specific languages has to be learned. Meanwhile a class library that's respects the language of your problem domain can be easier to learn because the notation with which the library is implemented is plain old YFL (Your Favourite Language) and your team already knows that (and if it doesn't, you have bigger problems). Learning the class library is like learning about the problem domain and vice versa. Learning one supports learning the other.

I'm of the opinion that DSLs and pidgins are at their most when their domains are generic. So ActiveRecord's most useful language like interfaces are related to general problems like expressing the relationships between objects or describing how to validate an object. Backus-Naur Form, Perl compatible regular expressions, parser combinators are all useful for solving the generic parsing and lexing problems that crop up everywhere. Data serialization notations like YAML, FASTA, XML, CSV, JSON and all the others are useful when you want a generic way to pass data between systems. All of these notations are narrow in their focus, but broad in the sense that the things that they focus on are ubiquitous.

The most useful notation for your specific task is almost always a well factored set of classes or functions (depends on your language of choice) which reflects the language of your problem domain. Different parts of your program might well make use of a wide range of 'donor' notations, but it's a rare problem that's going to require to you implement a full blown language in order to solve it.

Don't worry about whether something's a DSL, pidgin or a library. What it is is notation. A better question is is "does this notation help me solve my problem?". If the answer is no, find better notation. Or keep plugging on with what you've got, but don't fool yourself about your reasons for using the notation - not everyone's here for the hunting after all.

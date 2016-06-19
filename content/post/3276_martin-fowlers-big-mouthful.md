+++
title = "Martin Fowler's big mouthful"
slug = "martin-fowlers-big-mouthful"
date = "2008-01-18T17:19:00+00:00"
draft = false

+++

Martin Fowler is writing a book about [Domain Specific Languages](http://martinfowler.com/dslwip/) and, because you could never accuse Martin of a lack of ambition, he's trying to write it in a reasonably (implementation) language agnostic fashion.

It's fairly easy to write an implementation language agnostic book about old school DSLs, what used to be called little languages - there's a fairly well established literature and theory to do with lexing, parsing and interpreting. These are all about algorithms, and algorithms are implementation language neutral by their very nature.

Where Martin has his work cut out for him is trying to talk about what he calls 'internal DSLs' and what I've been calling 'pidgins'. These are the sorts of languages where you don't write a lexer or parser but instead build a family of objects, methods, functions or whatever other bits and pieces your host language provides in order to create a part of your program that, while it is directly interpreted by the host language, feels like it's written in some new dialect.

The Lisp family of languages can be said to be all about this. A good 'bottom up' lisp programmer will shape a language to fit the problem space, essentially building a new lisp which makes it easy to solve the problem at hand. Lisp's minimal syntax, powerful macros and the way it blurs the boundary between code and data really support this style.

Once you move from Lisp to more 'syntaxy' languages, things get hairier. As Martin himself [says](http://martinfowler.com/bliki/BookCode.html)

> Another issue with book code is to beware of using obscure features of the language, where obscure means for my general reader rather than even someone fluent in the language I'm using. \[...\] this is much harder for a DSL book. Internal DSLs tend to rely on abusing the native syntax in order to get readability. Much of this abuse involves quirky corners of the language. Again I have to balance showing readable DSL code against wallowing in quirk.

He's dead right. When I'm thinking about writing a pidgin in Ruby for instance, my first thought is usually to start with some kind of tabula rasa object which I can use to `instance_eval` a block. That lets me start to shape my language by lexically scoping the change:

    in_pidgin do
    ...
    end

But, though it's easy to illustrate what I'd *do* with my tabula rasa, the implementation is somewhat tricky, and the tricks needed are unique to Ruby.

That sort of construct's not really available to someone trying to write a pidgin in Java or Perl. In Perl, there are other odd corners of the language that can be abused to good effect. Dynamic scoping can let you 'inject' methods into a block even though there's no Perl equivalent to `instance_eval`, or you can do some quite staggering things with the otherwise really annoying Perl function prototypes. For instance, here's part of a [Jifty](http://jifty.org/) definition of a persistent object:

    column title => 
    type is 'text',
    label is 'Title',
    default is 'Untitled post';

    column body => 
    type is 'text',
    label is 'Content',
    render_as 'Textarea';

Doesn't look much like Perl does it? But it's parsed and executed by perl with no source filters or `eval STRING` in sight. And there's no unsightly <code>:symbols</code> scattered about the place either come to that.

These things all work by making the language do something unexpected, and generally, the way to do that is by knowing your host language inside out and playing with it. One of Damian Conway's more inspired moments in recent years was [List::Maker](http://search.cpan.org/~dconway/List-Maker-v0.0.3/lib/List/Maker.pm), in which the good doctor managed to find a corner of Perl where he could wedge a proper old school, complete with full on parser to build the AST, Little Language right in the heart of Perl without it *looking* like he was taking a plain old string and interpreting it. So, having found this odd little corner, he proceeded to implement a remarkably neat tool for building complex lists that are beyond the capabilities of Perl's <code>..</code> operator.

    @odds   = <1..100 : N % 2 != 0 >;

    @primes = <3,5..99> : is_prime(N) >;

    @available = <1..$max : !allocated{N} >

You may not think that's all that sexy, but, and trust me on this, it's just *gorgeous*. Yet more proof that Damian Conway is an (evil) genius.

Frankly, once you've seen the best of the pidgins available in Perl, some of highly praised 'DSLs' in Ruby start to look a bit ordinary. Ruby makes a great deal of stuff that a pidgin breeder needs to do really easy. In Perl it's often rather hard with a huge amount of hoopage to deal with. But some of the things that are hard in Perl are impossible in Ruby.

Anyhoo... coming back to my point. I do find myself wondering if Martin's bitten off more than he can chew in attempting to write a book that covers implementing pidgins without getting bogged down in the nitty gritty of individual languages. The problem he's facing is that different languages don't just have different quirks, they have different idioms too. What reads naturally in the context of a Ruby program will read very weirdly in, say Java or a lisp. Any patterns of implementation beyond broad (but important) strokes like "Play to your host language's strengths" will surely end up as language specific patterns. Designing and implementing a good pidgin is *hard*. Doing it effectively means getting down and dirty with your host language and its runtime structures. And that's not the sort of thing you can cover effectively in a language agnostic book.

Martin, if you're reading this, good luck. I think you're going to need it. I look forward to being proved wrong.

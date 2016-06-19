+++
title = "Moose for Ruby programmers"
slug = "moose-for-ruby-programmers"
date = "2009-02-22T15:04:00+00:00"
draft = false

+++

'Moose for Ruby Programmers' was the programmed title for the talk I gave at the London.pm technical meet on Thursday, but that was something of a stalking horse for the real title, but I'll save that for the end of this writeup.

### Why Ruby?

I've said this before, and I'll no doubt say it again, but the main reason I started programming in Ruby was that I got fed up of unrolling @\_. From a Perl programmer's perspective, Ruby is like Perl 5, but with a way better object system out of the box.

### What's so awful about unrolling @\_?

Think about the way we program has changed over time. There's been a trend of replacing conventions with code or, to look at it another way, of replacing imperative code with declarative code.

#### An example

Here's some pseudo code explaining how we used to do code reuse back when everything got written in assembly language:

<code>

        save stuff we need later
        put the RETURN label on the stack
        put arguments on the stack
        goto FUNC_FOO
    RETURN:
        # return in accumulator
        # saved values on the stack

</code>
Every time we called another bit of code, we had to jump through all these hoops in order to manage the resources of the machine. The particular sequence of hoops used within a program or library is called a calling convention. Nowadays we don't have to do all that explicit control stuff, we just make a function call. Instead of a chunk of imperative code, we simply declare that we're making a function call and let the computer do the bookkeeping.

On the other side of the function call, our function fulfils its side of the conventional bargain:

<code>

    FUNC_FOO:
        pop arguments
        # maybe check types
        # do the real work of the function,
        put the result in the accumulator register
        pop continuation
        goto continuation

</code>
Of course, it's much easier in a high level language like Perl:

<code lang="perl">

    sub somefunc {
        my($argument) = @_;
        $argument->isa(Whatever) or die;
        # do stuff
        return $result;
    }

</code>
Hmm... actually... okay, so we're not having to manage the call stack in Perl, but we're still writing imperative code just to give names to our parameters. Meanwhile, in Ruby, we have:

<code lang="ruby">

    def somefunc(argument)
      argument.is_a?(Whatever) or raise
      # do stuff
      return result
    end

</code>
One line shorter, and we get to name our parameters in a declarative style. We're still stuck with imperative code to do the type checking, but we'll live.

It seems such a silly reason to switch programming languages doesn't it?

### Wading in treacle

The trouble is, unrolling @\_ is like wading through treacle. It exerts a great deal of drag. When every function you write carries with it the need to write a little bit of custom code just to do the simplest of tasks every time, it's awfully tempting to just stick another level of indentation into your function and leave your worries about the Single Responsibility Principle to one side. I mean, what's a 127 line method or two between friends?

And those little bits of custom code are great places for silly little bugs to hide. Most measures of defect rates seem to suggest that defects per line is pretty much constant across languages. There's a very real benefit in writing concise, expressive code and that benefit isn't simply the programmer's idea of elegance. Concise languages "make the easy things easy, and the hard things possible". If your programming style is heavily object oriented, Perl 5 doesn't make the easy things as easy as they could be (and are in other languages). Ruby, for me, hit that sweet spot -- it feels sufficiently perlish to make the transition easy, and its syntax for making classes and methods kicks Perl's arse.

Of course, after a while, you'll start hitting other limits and annoyances in Ruby (just as you will in any other language), it's software after all and rule 1 of software is "Software sucks".

### The Moose effect

Although, in recent years, I've been pretty much exclusively a Ruby programmer, I've kept my eye on Perl and it's been almost impossible not to notice the rise of [Moose](http://www.moose.org/). To a Ruby programmer, I'm sure that the very idea of Moose would seem silly. Having to install an entirely new package and its dependency chain just to get something that looks a bit like Ruby's class objects and accessor method auto generation? Are you kidding? Ruby's stuff is good enough.

I used to think Perl's OO was good enough.

It turns out that it is. Because if there's one glorious thing about Perl it's malleability. There's very little you can't change if you're prepared to delve into the weirder bits of the way the runtime is put together. And, for all Perl has a reputation for oodles of syntax and weird special cases, once you get down to the bones, it's consistent. The syntax to get there can be horrible, but the structure makes sense. So Perl has just enough object support for sufficiently clever programmers to build something really decent.

Here's a bit of noddy code to show you what I mean. First, a ruby version and then I'll show you the Moose equivalent.

<code lang="ruby">

    class Article
      attr_reader :title
      def title=(a_string)
        raise unless a_string.is_a?(String)
        @title = a_string
      end

      attr_reader :body
      def body=(a_string)
        raise unless a_string.is_a?(String)
        @body = a_string
      end

      def initialize(initial_values = {})
        self.title = initial_values[:title]
      end

      def print_on(html)
        html.h1(title)
        html.div(class => 'body') {
          body
        }
      end
    end


    And here's the Perl equivalent.


    package Article;
    use Moose;

    has title => (is => 'rw', isa => 'Str');
    has body  => (is => 'rw', isa => 'Str');

    sub print_on {
      my($self, $html) = @_;
      $html->h1($self->title);
      $html->div(
        $self->body,
        class => 'body'
      );
    }


    Note that the Ruby version omits the detailed error reporting of the Moose version, and if I were writing a _real_ Ruby class, I wouldn't bother with type checking something as simple as a string. It's not really a fair comparison (I could tilt things far more steeply in Moose's favour by using some of the more sophisticated attribute declaration possibilities that Moose offers).

    The important thing to remember is that accessor methods are the most boring bits of writing a class. The interesting bits are the methods that _do_ stuff. There's a school of thought that says that accessor methods should be private (or at least protected) - otherwise you're just using data structures with benefits. In any class worthy of the name, there will be more methods that do stuff than accessors after all, and I definitely prefer the Ruby version of @print_on@ to the Moose one.

    However, if we sprinkle a bit of magic dust on the Perl version, we have:

    use MooseX::Declare;

    class Article {
      has title => (is => 'rw', isa => 'Str');
      has body => (is => 'rw', isa => 'Str');

      method print_on (Document $html) {
        $html->h1($self->title);
        $html->div(
          $self->body,
          class => 'body'
        );
      }
    }

</code>
I still find myself pinching myself when I see that. I can't quite believe it's still Perl 5 and I haven't fallen through a wardrobe to a magical land where everything is flowers and ponies, and Perl 6 Christmas has arrived at last after too long a winter[1].

But it is Perl 5 and it's here now. Sort of (see [Caveats](#caveats) below).

### How does it work?

I neither know nor care[2]. As far as I'm concerned it's sufficiently advanced rocket science and I'll leave it at that. I'm just glad to be able to *use* it. About the only thing I can say with confidence about it is that Florian Ragwitz, Ash Berlin, Stevan Little and all the other who've made this possible are geniuses.

There's more to it, of course, but the [documentation](http://search.cpan.org/dist/MooseX-Declare) covers things in
more detail than I'm happy to write here, so I'd suggest you read that if you want to know more. Generally, things just work the way you'd expect them to.

### Caveats

This is seriously bleading edge code. There are bugs (though substantially fewer of them today than there were when I delivered the talk on Thursday), and you will trip over them. And when you do, the error reporting currently leaves something to be desired[3], but it's improving in leaps and bounds. The more people who are using it, the more people there are to submit patches (and a patch to the test suite with a bug and no fix is just as welcome as a patch that includes a fix).

So, if you're starting a new project, you should consider going one step further than Moose, and using MooseX::Declare. Heck, if you're feeling courageous, you should consider using it in existing projects as well. You can introduce it piecemeal after all - you can simply use `class Foo {...}` and only gradually introduce the extra `method` goodness as you refactor existing methods.

### The real title

I knew I'd forgotten something. The real title of my talk was <cite>MooseX::Declare -- Why I came back to Perl</cite>

[1] I can't quite believe I just used that metaphor.

[2] Not quite true. It's important that MooseX::Declare doesn't use source filters, and it doesn't, so that's all right.

[3] That's the error reporting that happens when you trip a bug, not when you, for example, call a method that that expects an argument of one type with an argument of the wrong type, which is pretty exemplary.

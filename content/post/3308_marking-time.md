+++
title = "Marking time..."
slug = "marking-time"
date = "2009-02-20T18:41:00+00:00"
draft = false

+++

So, it turns out that there's a recording of my [MooseX::Declare](http://search.cpan.org/dist/MooseX-Declare) talk at the London.pm techmeet last night. And, on listening to it, I sound rather less incoherent than I thought I did while I was delivering it (still plenty of places where I could improve. Must remember to always repeat the questions in full...), but I'm happy enough with it that I'm looking into synching it up with the slides and making a slidecast, which will probable take longer than is sensible.

So, while I do that (and write it up properly as well), I thought I'd cover something I mentioned as a throwaway about using MooseX::Declare to easily set up what I called nonce classes for testing. A nonce class, in this context, is a one shot class that you can use to test inheritance, or to provide just enough behaviour to test the class you're actually testing or whatever.

In ruby, it's pretty simple really:

<code>
    class Child1 < Announcements::Announcement
      attr_accessor :message
    end

    class Child2 < Announcements::Announcement; end

    class GrandKid < Child1; end

</code>

Because ruby was designed as 'Perl with better OO', it has a convenient, compact notation for making classes, which means it's easy and quick to define new classes, as you need them, near to where you're going to use them. Which is very helpful in tests.

Meanwhile, in Perl, its a wee bit more... clunky. Assuming we're using Moose, we have:

<code>
    package Child1;
    use Moose;
    extends 'Announcements::Announcement';

    has message => (is => 'rw', isa => 'str');
    no Moose;

    package Child2;
    use Moose;
    extends 'Announcements::Announcement';
    no Moose;

    package GrandKid
    use Moose;
    extends 'Child1';
    no Moose;

</code>

Yikes.

So, let's sprinkle some MooseX::Declare magic dust over things:

<code>
    use MooseX::Declare
    class Child1 extends Announcements::Announcement {
        has message => (is => 'rw', isa => 'str');
    }

    class Child2 extends Announcements::Announcement {
    }

    class GrandKid extends Child1 {
    }

</code>

So, even if you completely eschew the <em>really</em> sexy bits of MooseX::Declare and simply use its new syntax for declaring classes, you've still improved your golf score and reduced your hoop count.

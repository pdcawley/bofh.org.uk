+++
title = "Class decomposition and a handy delegation pattern"
slug = "class-decomposition-and-a-handy-delegation-pattern"
date = "2009-11-25T16:49:00+00:00"
draft = false

+++

There's something satisfying about reaching the point when you can't decompose an object any further and all your methods are tiny and do one thing - it's especially gratifying when you learn something new in the process. Sadly, it doesn't happen as often as I'd like, there's usually annoying bits and pieces where you have to placate the language in some fashion that breaks the flow of what you're writing.

As I get a better handle on the way [MooseX::Declare](http://search.cpan.org) has changed Perl, I'm finding I have to do much less in the way of placation.

Here's an example. For context, I'm writing a traffic shaping tool. The basic client interface needs to look something like:

```perl
$policy->current_weight # => a percentage between 0 and 100
```

Not much of an interface really. The requirements state that we should be able to specify weights with 15 minute granularity for every day of the week. Our problem becomes one of mapping from a time to a number between 0 and <code>7 (days) \* 24 (hours) \* 4 (quarters) - 1</code> and looking up the weight in an array.

Here's my first cut:

```perl
use MooseX::Declare;
class WeightVector {
use DateTime;

has vector => (
    isa => 'Array[Int]',
    is => 'ro',
    required => 1,
);

method current_weight {
    my $now = DateTime->now;
    my $offset = ($now->wday_0 * 7 * 24 + $now->hours) * 4 + int($now->minutes / 15);
    return $self->vector->[$offset];
}
}
```

Which is, I suppose, perfectly respectable. However, `current_weight` isn't filling me with delight. First it finds the current time, then it converts the time into an offset, then it uses the offset to lookup the weight in the vector. Let's introduce a method to find the weight at a specific time<span style="text-align:left;">[sup&gt;1</sup>](#decomp-motivation)</span>, the relevant code becomes:

```perl
method current_weight {
$self->weight_at(DateTime->now);
}

method weight_at (DateTime $time) {
my $offset = ($now->wday_0 * 7 * 24 + $now->hours) * 4 + int($now->minutes / 15);
return $self->vector->[$offset];
}
```

And again, we could rest here, but again, we're doing two things. We're converting from a time to an offset, then we're looking up the value in the vector. Type conversions tend to happen again and again, so it's good if we can specify them separately. We *could* write a `time_to_offset` helper method, but we're in Mooseland now; there's a better way. Let's introduce a formal Moose type and define a coercion for it. Here's the type definition stanza of the code. I've taken the opportunity to add types which do bounds checking for the vector as well, while I'm about it.<span style="text-align:left;">[sup&gt;2</sup>](#decomp-why-inline)</span>

```perl
use MooseX::Declare;
class WeightVector {
    use DateTime;
    use Moose::Autobox;
    use MooseX::Types -declare => [qw(SlotOffset VectorOfWeights PercentageInt)];
    use MooseX::Types::Moose qw(ArrayRef Int);

    use constant TOTAL_SLOTS = 7 * 24 * 4;

    BEGIN {
        subtype PercentageInt,
            as Int,
            where { 0 <= $_ && $_ <= 100 },
            message { "$_ does not is not an integeter between 0 and 100" };

        subtype VectorOfWeights,
            as ArrayRef[PercentageInt],
            where { $_->length == TOTAL_SLOTS }
            message { "Vector must have ".TOTAL_SLOTS." entries, not ".$_->length };

        subtype SlotOffset,
            as Int,
            where { 0 <= $_ && $_ < TOTAL_SLOTS };

        class_type 'DateTime';

        coerce SlotOffset,
            from 'DateTime',
            via { ($_->wday_0 * 7 * 24 + $_->hours) * 4 + int($_->minutes / 15) };

        # Let's allow clients not to care about using DateTime by allowing 
        # them to simply pass the results of calling 'time()' - It's not like it's
        # still 1970...

        coerce SlotOffset
            from subtype(as => Int, where { $_ > TOTAL_SLOTS }
            via { to_SlotOffset(DateTime->from_epoch(epoch => $_) };
    }

    has vector => (
        is => 'ro',
        isa => VectorOfWeights,
        required => 1,
    }

    ...
```

Now, if we were programming in plain old Moose, we could rewrite `weight_at` like so:

```perl
sub weight_at {
    my $self = shift;
    my $offset = to_SlotOffset(shift);
    $self->vector->[$offset]
}
```

Which would be pretty sweet, but we're using MooseX::Declare; there's an even better way:

```perl
method weight_at (SlotOffset $offset does coerce) {
    $self->vector->[$offset];
}
```

Sweet!

We could stop there, but I had an insight. What we've got here is basically a wrapper around a delegation to our vector, and Moose's new native types feature let us express the delegation to the vector quite neatly, like so:

```perl
has vector => (
    isa => 'VectorOfWeights',
    is => 'ro',
    required => 1,
    traits => ['Array'],
    handles {
        weight_at => 'get',
    },
);

...

around weight_at (SlotOffset $offset does coerce) {
    $self->$orig($offset);
}
```

This could be overkill when vector is a simple `ArrayRef` as we have here, but the pattern of delegating declaratively in the attribute definition and then munging arguments in an `around` handler is applicable to more than just argument transformation. A typical delegation pattern involves having the delegating object passing itself in as an argument to the method delegated to. The nature of Moose's `handles` declarations makes that impossible to do within the attribute declaration, but it's easy to fix with an around helper:

```perl
around delegated_method (Any @args) {
    $self->$orig($self, @args);
}
```

(If you're wrapping more than one method in this fashion, you should probably consider using a plain old Moose style `around` handler, which lets you wrap multiple methods with `around `delegated\_methods =&gt; sub {...}@

So, at the end of all that, and after we've extracted our Type declarations into WeightVector::Types, we have:

```perl
use MooseX::Declare;
class WeightVector {
    use WeightVector::Types qw(VectorOfWeights PercentageInt SlotOffset);

    has vector => (
        isa => 'VectorOfWeights',
        is => 'ro',
        required => 1,
        traits => ['Array'],
        handles {
            weight_at => 'get',
        },
    );

    method current_weight {
        $self->weight_at(time());
    }

    around weight_at (SlotOffset $offset does coerce) {
        $self->$orig($offset);
    }
}
```

And we've pushed all knowledge of DateTime off onto our type declarations and gained a boatload of handy bounds checking. We've also got a new tool for handling tricky delegation setups in the <code>handles</code>/<code>around</code> combo.

### Notes

#### Motivation

I realise that this looks like a radical decomposition of the class with very little motivation, but it was driven by tests and by some other requirements that I've removed from the body of the post. In particular, the type coercions were driven by the need to build particular vectors for testing, a key method being:

```perl
method set_weight (PercentageInt $weight, 
                   SlotOffset $from does coerce,
                   SlotOffset $to does coerce)
{
    ...
}
```

#### Type coercion is wonderful

Generally, I'm not a fan of static typing. I'm from the "duck type all the way" school of programming,<span style="text-align:left;">[sup&gt;3</sup>](#decomp-haskell)</span> so most of my method declarations have no type declarations. But type declarations, especially ones that coerce, make so much sense on methods that make up the public protocol of a class. I only use type declarations on internal methods when I need a narrower coercion, or if I'm using [MooseX::Multimethods](http://search.cpan.org/dist/MooseX-Multimethods), which I still haven't used for anything but exploration.

### Updates

Thanks to Chris Dolan for spotting that I'd got the `SlotOffset` coercion completely wrong. The real code's doing the right thing, but that's what comes of recreating code from memory.

<sup>1</sup> This was actually motivated by trying to write tests to verify that the weights were correctly set.

<sup>2</sup> I'm declaring these in a `BEGIN` block of the class itself mostly for explanatory purposes - there's a good case for moving them out into a separate file and pulling it in with `use`.

<sup>3</sup> Except during my periodic attempts to learn Haskell. I've learned Haskell at least three times now.

+++
title = "Reading 'The Traits Paper'"
slug = "reading-the-traits-paper"
date = "2013-04-14T10:18:00+00:00"
draft = false
description = "Where do Moose roles come from and what are they for?"
+++

There appear to be two camps around the way Moose::Roles busily arguing about whether the following code should emit a warning:

```perl
package Provider {
    use Moose::Role;
    
    sub foo { 'foo' };
    1;
}

package Comsumer {
    package Moose;

    with 'Provider';

    sub foo { 'no, bar' }
    1;
}
```

One camp holds that the code should at least emit a warning and ideally blow up at compile time. The other camp (Moose as implemented), holds that it shouldn't. The debate gets somewhat heated, people end up appealing to the [Traits paper](http://scg.unibe.ch/archive/papers/Scha03aTraits.pdf) as if it were some kind of holy writ. What's annoying is that the folk who appeal to that paper appear to have read a different paper from the one I remember reading. So I went and read it again, and here's what it has to say about overriding methods got from traits:

>Trait composition enjoys the flattening property. This property says that the semantics of a class defined using traits is exactly the same as that of a class constructed directly from all of the non-overridden methods of the traits. So, if class A is defined using trait T, and T defines methods a and b, then the semantics of A is the same as it would be if a and b were defined directly in the class A. Naturally, if the glue code of A defines a method b directly, then this b would override the method b obtained from T. Specifically, the flattening property implies that the keyword super has no special semantics for traits; it simply causes the method lookup to be started in the superclass of the class that uses the trait.

>Another property of trait composition is that the composition order is irrelevant, and hence conflicting trait methods must be explicitly disambiguated (cf. section 3.5). Conflicts between methods defined in classes and methods defined by incorporated traits are resolved using the following two precedence rules.

> *   Class methods take precedence over trait methods.
> *   Trait methods take precedence over superclass methods. This follows from the flattening property, which states that trait methods behave as if they were defined in the class itself.

Which is pretty much as I remember, and strongly implies that Moose is right not to issue a warning.

The paper has more to say on overriding trait implementations in its section on 'Evaluation against the identified problems':

>Method conflicts may be resolved within traits by explicitly selecting one of the conflicting methods, but more commonly conflicts are resolved in classes by overriding conflicts. 

And (relevant to another argument around role composition that's more or less current):

> ... sometimes a trait needs to access a conflicting feature, e.g., in order to resolve the conflict. These features are accessed by aliases, rather than by explicitly naming the trait that provides the desired feature. This leads to more robust trait hierarchies, since aliases remain outside the implementations of methods. Contrast this approach with multiple inheritance languages in which one must explicitly name the class that provides a method in order to resolve an ambiguity. The aliasing approach both avoids tangled class references in the source code, and eliminates code that is hard to understand and fragile with respect to change.

There are folk arguing for removing the aliasing support from Moose role composition, but I have to say that I find this argument compelling.

### Who's right

When it comes down to it, referencing the traits paper is just argument from authority, which is one of the classic logical fallacies. However, if you _are_ going to appeal to an authority, try to make sure that you're not misrepresenting what that authority says. The original traits paper does not suggest that overriding a method got from a trait should come with a warning. On the contrary, it recommends overriding as the right way to resolve conflicts between multiple composed roles. 

You may well think that this is problematic. You may be able to show examples where silent overriding has bit you on the arse. You may even have a good argument for introducing warnings. But "Because that's how the traits paper says you should do it!" is a lousy argument, made doubly lousy by the fact that is precisely _not_ what the traits paper says you should do. 

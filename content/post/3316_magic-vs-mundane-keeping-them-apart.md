+++
title = "Magic vs Mundane: Keeping them apart"
slug = "magic-vs-mundane-keeping-them-apart"
date = "2009-03-13T08:40:00+00:00"
draft = false

+++

In which your correspondent does magical battle with the guts of Perl and emerges bloodied, but unbowed with a useful principle to code by.

### Skip to the [conclusion](#conclusion) if you're uncomfortable with the guts of the Perl runtime

[`Test::Class`](http://search.cpan.org/dist/Test-Class) had me tearing my hair out earlier. There I was, happily transforming

<code>

<pre>
test something {
ok 1;
};
</code>

into something very like[1]:

<code>

<pre>
\*test\_something =
Sub::Name::subname(
'test\_something'
=&gt; sub : Test { ok 1 }
);
</code>

through the magic of [`Devel::Declare`](http://search.cpan.org/dist/Devel-Declare), but Test::Class didn't seem to be playing fair. Instead of letting my tests run happily, it was complaining that it:

> cannot test anonymous subs - you probably loaded a Test::Class too late (after the CHECK block was run). See 'A NOTE ON LOADING TEST CLASSES' in perldoc Test::Class for more details

The thing is, I wasn't loading Test::Class too late. The problem is that, at the point I applied the `Test` attribute to my sub, the sub didn't have a name and, because of the constraints you're operating under when you're using `Devel::Declare` to do code transformation, there was no obvious way to give it a name in time.

#### Incompatible magics

The trouble is, Test::Class does what it does through the magic of compile time code attributes, and, further, it relies on the fact that if a perl subroutine that gets inserted into the symbol table like this:

<code>

<pre>
sub has\_a\_name {...}
</code>

Then, when you get hold of a reference to that code by other means (say, in the subroutine that handles the setting of an attribute, that code ref knows its own name. However, if a subroutine that ends up in the symbol table like this:

<code>

<pre>
\*anonymous\_ref = sub {...};
</code>

Doesn't know its name, unless you take advantage of the [`Sub::Name`](http://search.cpan.org/dist/Sub-Name) module.

So, in my generated code, I was giving my coderef a name, but it was happening to late. At the point that `Test::Class::Test` method was seeing the coderef, the coderef was anonymous.

My magic and Test::Class's magic were incompatible.

The thing is, both sorts of magic are really just sugar for some pretty mundane donkey work. Test::Class does what it does through attributes because no flesh and blood programmer in their right mind would want to write something like this every time they wanted to write a test method:

<code>

<pre>
sub test\_something {
...
}
*PACKAGE*-&gt;mark\_as\_special\_method('test\_something', 'test', '3');
</code>

In fact, `mark_as_special_method` doesn't even exist as its own subroutine. The code that marks a method as special is just part of the body of the `Test` attribute handler.

### Conclusion

Which brings me neatly to my conclusion.

When you're designing a module that does anything magical, consider starting with a mundane core API that handles the business side of things. Then layer your magic on top of that API. Then document the API and the magic. Obviously the magic bits go up front in the docs, and the API goes in its own section (or even podfile) down at the bottom, where only eejits like me, who want their magic to work slightly different to yours, will bother reading it.

Obviously, I'm motivated by an issue I'm having with a particular module from CPAN, but the principle of separating the magic and the mundane is applicable everywhere. It's called Separation of Concerns, or The Single Responsiblity Pattern. I call it a [Just Story](http://www.bofh.org.uk/2003/08/01/the-fine-art-of-complexity-management).

You'll find the pattern in well designed websites that are using unobtrusive javascript to wave an AJAX wand over the site. You'll see it woven through books like The Structure and Interpretation of Computer Programmers - where it's called an Abstraction Barrier.

#### Patches sent

It turns out to be very easy to add `mark_as_special_method` (though I actually wrote it as 'add\_testinfo' in the patch) to Test::Class. It's about as straightforward an Extract Method refactoring as I've ever done - even without automated tools, I managed not to fuck it up. There's a patch in [Adrian Howard's](http://www.twitter.com/adrianh) inbox, and I'm hopeful that it'll be applied soon.

<br/>

Check out the [osfameron fork](http://www.github.com/osfameron/devel-declare) of Devel::Declare for the beginnings of some decent documentation which explains what's going on.

[1] Not exactly - that's the result of calling the shadowed `test` subroutine which was the result of the code transformation.

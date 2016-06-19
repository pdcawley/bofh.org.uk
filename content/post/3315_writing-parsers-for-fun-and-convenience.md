+++
title = "Writing parsers for fun and convenience"
slug = "writing-parsers-for-fun-and-convenience"
date = "2009-03-11T22:29:00+00:00"
draft = false

+++

One aspect of coming back to Perl for 'recreational' programming is that if, like me, you've declared war on <code>@\_</code> and boilerplate code, then testing can be somewhat trying. The Perl testing framework that best fits my head is [Test::Class](http://search.cpan.org/dist/Test-Class), which is an excellent Perlish implementation of xUnit style testing. If you're unfamiliar with the, library, [Ovid](http://publius-ovidius.livejournal.com/) is writing what's shaping up to be an [excellent series](http://xrl.us/bejgzr) of introductory articles about it at http://www.modernperlbooks.com/.

The problem I'm having with Test::Class at the moment is that I can't write:

<code>

<pre>
use MooseX::Declare
class Test::Person
extends Test::Class
{
use Test::Most;

method class\_under\_test {'Person'}

method startup : Test(startup =&gt; 1) {
use\_ok $test-&gt;class\_under\_test
}

...
}
</code>

Test::Class is doing too much in its initialization phase, and relies too heavily on code attributes, for it to play well with [MooseX::Declare](http://search.cpan.org/dist/MooseX-Declare). Drat.

On reflection though, this might be a good thing, because maybe MooseX::Declare isn't really what's needed. What I'd like to write is something like:

<code>

<pre>
use ...;

testclass Test::Person
exercises Person
{
startup class under test should be usable (1 test) {
use\_ok $test-&gt;class\_under\_test
}
}
</code>

And have the library '...' expand the `testclass` declaration into something that looks like the first code snippet. After all, if MooseX::Declare can work without source filters, it should be possible to come up with something nicely declarative for specifying test classes.

Obviously, there's nothing on CPAN that does this yet though. So I went fossicking through MooseX::Declare to see how it works[1] and discovered thing of Lovecraftian beauty that is...

### [Devel::Declare](http://search.cpan.org/dist/Devel-Declare)

Devel::Declare is possibly the most hostilely documented library I've ever come across. Its documentation only begins to make sense when you already understand enough about how it works that you don't really need the docs. What it does is to let you declare your own Perl keywords. You could, for instance use it to introduce `given/when` into versions of Perl that don't have it yet. You declare your keywords and associate them with parsers. When, during its compilation phase, perl hits one of your keywords in the right context, it hands off to your parser which can then do what the hell it likes in the way of code transformation, before handing control back to Perl, which then parses the transformed code as if that was what was there all along.

So, to want to transform that `testclass` syntax I just pulled out of my ass into a real Test::Class package, I just need to write an appropriate parser and code generator, perform the appropriate Devel::Declare incantations, and I'm laughing.

### Making progress

So far, I've got to the point where I have a working `testclass` keyword, but nothing yet for the 'inner' bits (`setup`, `test`, `teardown`, etc). I can write:

<code>

<pre>
testclass Test::Person
exercises Person
{
...
}

testclass Test::Person::Employee
extends Test::Person
exercises Person
{
...
}
</code>

and, as I write this, I'm realising that the syntax I'd cooked up for using extra test helper modules:

<code>

<pre>
testclass AnotherTest
helpers -More, -Exception, Carp
{
\# use Test::More;
\# use Test::Exception;
\# use Carp;

...
}
</code>

would probably read better as:

<code>

<pre>
testclass AnotherTest
+uses Carp
{
\# use Test::Most;
\# use Carp;

...
}
</code>

and also that I want this:

<code>

<pre>
testclass exercises Person {
...
}
</code>

to build me a Test::Person class.

What's still blowing my mind about Devel::Declare's possibilities is that I'm no longer constrained to writing a [Domain Specific Pidgin](http://www.bofh.org.uk/2007/08/08/domain-specific-pidgin) which works by building a tower of proxy objects and weird evaluation contexts to produce something that's legal code in the host language, but which has the feel of another language. With Devel::Declare, I control the horizontal *and* the vertical until I choose to hand control back to Perl. Right now that means my error reporting is disgracefully bad, but it also means that I can roll a syntax that makes sense without worrying about how I'm going to get perl to parse it.

One of the things I find frustrating about writing RSpec specifications is that `describe` and `it` both *want* to be first class keywords - it feels like you should be able to write:

<code>

<pre>
describe SomeClass, "in some context"
before each
\# set things up
end

it "should do something or another"
...
end
end
</code>

But, because RSpec works by taking advantage of Ruby's block magic, you have to write:

<code lang="ruby">

<pre>
describe SomeClass, "in some context" do
before :each do
\# set things up
end

it "should do something or another" do
...
end
end
</code>

I definitely prefer the version without the extraneous <code>do</code>s and the gratuitious <code>:</code> before `each` in the `before` declaration. Does anyone feel like writing <code>devel/declare.rb</code>?

### Show us the code!

If you want to see the current state of my Test::Class::Sugar art, the place to look is http://www.github.com/pdcawley/test-class-sugar. At the time of writing it relies on http://www.github.com/rafl/devel-declare and doesn't have anything so useful as documentation, a `Makefile.PL` or even any tests beyond the collection of code samples that is `t/initial.t`. Expect all those when and if I push it to CPAN.

### Caveats

Yes, I *know* that this sort of metasyntactic abstraction is trivial in a Lisp. I just happen to like syntax, okay?

### Update 20090312

<code>

<pre>
use Test::Class::Sugar

testclass exercises Foo
+uses -Warn
{
...
}
</code>

Now generates

<code>

<pre>
{
package Test::Foo;
use base qw/Test::Class/;
use Test::Most;
use Test::Warn

...
}
</code>

So that's one hurdle jumped. And I now know how to write the various method helpers and, when I get the appropriately shaped tuits, I shall actually write the damned things.

Then all I have to do is document it.

And write up a proposal about it for YAPC::Europe.

### Update 20090314

I now know what a plan looks like:

<code>

<pre>
test with multiple assertions &lt;&lt; 3 {...}
</code>

And, more importantly, I've implemented, and *documented* everything and am almost good to cut a 0.001 distribution. I need a few ducks up on CPAN, but once that's done, we're good and I can get on with parameterizing some of the assumptions that are hard coded at the moment.

[1] Something I swore blind I wasn't going to do in my London.pm presentation. Seems my word isn't to be trusted...

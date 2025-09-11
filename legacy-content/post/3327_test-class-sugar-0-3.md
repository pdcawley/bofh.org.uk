+++
title = "Test::Class::Sugar 0.3, no, 0.4"
slug = "test-class-sugar-0-3"
date = "2009-11-05T02:12:00+00:00"
draft = false

+++

**tap** **tap**... Is this thing on?

So, I recently noticed that [Test::Class](http://search.cpan.org.uk/dist/Test-Class-Sugar) 0.33 got released, which means that [Test::Class::Sugar](http://search.cpan.org/dist/Test-Class-Sugar) no longer needs to depend on a development release, and I also noticed that it was embarrassingly easy to throw Test::Class::Sugar into an infinite loop by forgetting which way the `>>` goes when you want to specify the number of subtests in a test method.

So, I've done a quick fix of the infinite loop problem as well and uploaded version 0.3 to PAUSE, so now you can write your tests like:

<code>

<pre>
testclass exercises ClassUnderTest {
test creation of the class under test {
lives\_and {
isa\_ok ClassUnderTest-&gt;new, $test-&gt;subject;
}
}
...
}
</code>

without having to jump through the hoops of downloading a development version of Test::Class or worry about accidental infinite loops...

Next up, fix the syntax to either allow both `<<` and `>>` as test count specifiers, or come up with a more memorable way of separating the count from the test name.

### Update:

Shortly after I released 0.3, Joel Bernstein asked if I'd be interested in a topic branch to make Test::Class::Sugar work with perl 5.8.

"Of course!" I said.

One day later, there it was. Thank you to Joel and to his employers, [NET-A-PORTER](http://www.net-a-porter.com/) for sponsoring his work. So now, Test::Class::Sugar 0.4 is winging its way to CPAN and now I have no excuse for not using it at work.

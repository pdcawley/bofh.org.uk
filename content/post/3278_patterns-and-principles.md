+++
title = "Patterns and principles"
slug = "patterns-and-principles"
date = "2008-02-16T09:57:00+00:00"
draft = false

+++

Recently I've been thinking about the way that patterns on different scales interact with each other. If you read Christopher Alexander's <cite>[A Pattern Language](amazon:0195019199</cite>), the first pattern in the book is Independent Regions, which are talked about within the context of a World Government, so it seems like a *huge* pattern. And it is, sort of, but it's scale invariant - it applies at the level of countries, but it also applies to states, cities, neighbourhoods, streets, houses and arguably even rooms within those houses.

Or maybe it emerges from the patterns that apply at those scales. As Kipling has it:

> As the creeper that girdles the tree trunk, the law runneth forward and back;<br/>
> For the strength of the pack is the wolf, and the strength of the wolf is the pack.

Can we see similar scale invariant patterns in our programming practice? Of course we can, but we tend to call them principles. Programmers reading this will, I hope, be familiar with "The DRY (Don't Repeat Yourself) Principle", which I first came across by that name in Hunt and Thomas's <cite>[The Pragmatic Programmer](amazon:020161622X</cite>). It's such a fundamental pattern that, it suffuses every pattern in Beck's <cite>[Smalltalk Best Practice Patterns](amazon:013476904X</cite>), but isn't actually expressed as a pattern there.

There are other macro patterns, one that I'm starting to appreciate more and more is:

### Fail Fast

Fail Fast is the principle that, when something starts going wrong, you shouldn't cover it up, but raise the issue as quickly as possible, hopefully to a level where it can be dealt with. As a pattern it informs almost every activity:

-   I want to go and photograph the Angel of the North. I plan to be at the angel half an hour before sunrise to get that magical pre dawn golden light. But the weather forecast says tomorrow will be entirely overcast, so I scratch that plan and decide to shoot some still life stuff in diffuse window light instead.

<!-- -->

-   I'm working on adding something to the work site and I realise that I'm not going to get it done in time, so I take it to the boss immediately. We work out how to reduce the scope of the change so that we'll still have something useful, but which can/should be extended in a future iteration.

<!-- -->

-   A low level method gets some data it didn't expect and doesn't know how to deal with, so it throws an exception and attaches what it knows about the problem - hopefully something up the caller chain will have enough information to deal with the problem.

<!-- -->

-   I'm looking for a new house. I check the details to see make sure there is room for our (huge) dining table, which means the room needs to be at least 18 feet long. If the dining room isn't big enough, I'm probably not going to like the rest of the house either, so I can reject houses quickly as I'm looking at details.

All reasonably obvious applications of the pattern, I hope you'll agree.

It gets fun is when Fail Fast affects other patterns. For instance, there's a pattern for choosing the name of the `each` block parameters. It says that you should always use the same name, usually `each` or <code>ea</code>. Many people rebel against the idea: surely it's better to reflect the parameter's type or rôle, or something. And they're right, sort of. However, those are rules for naming method parameters (type suggesting) and temporary variables (rôle suggesting or 'explaining'). If an each block gets long enough that you want to give the parameter a 'better' name, then it's time give the block a name too. Pull the body of the block out into a method, ideally on the parameter's class (which spares you the headache of naming the parameter - it's called `self` - and, if you're using ActiveSupport or something like it, you can replace the block with &lt;code&gt;&:method\_name</code>.

By naming your block parameters this way, you're applying the Fail Fast pattern. Your block becomes obviously ugly far sooner than it would if you gave it a more suggestive name, and getting ugly fast is often a good strategy. Similarly, if you're stuck with an old fashioned `for` loop, call your iterator <code>i</code> and when the body of the loop gets unwieldy, replace it with a method or function call that takes the counter and (probably) a [Collecting Parameter](http://c2.com/cgi/wiki?CollectingParameter) as arguments.

Fail Fast is why I'm using [Haml](http://haml.hamptoncatlin.com/) more and more in personal work. Haml recasts HTML in a YAML like structure, doing away with all the line noise involved in closing tags and letting me concentrate on the structure and content of the page. In an ERB template, it's all too easy to fall into the trap of writing complex logic in view code where it doesn't belong. In Haml, that gets ugly quickly, which makes me factor the logic into helpers. Then, because generating markup in a helper is a pain in the arse, it's easier to set things up so that the conditional logic simply selects which candidate partial to render. At the end of the process, the template, helpers and partials are working together, but each element is doing one thing and one thing only, and that makes for more comprehensible code. At least, it makes it more comprehensible to me.

Smalltalk people have been doing this sort of thing forever. A common complaint from new Smalltalkers is that the code editor isn't very capable compared to, say emacs, or vi, or whichever IDE the newbie is used to. Seasoned Smalltalkers will reply that, if you've reached the point where you wish you had a more capable editor, the method you're working on is probably too big. Limited text editing capability is just another way of failing faster, getting to the point where the code is telling you, loudly, that it needs to be better factored.

### When getting to ugly hurts

When a pattern or programming language starts to get ugly fast if you start down a dodgy road, the programmer wins. But sometimes the wrong sort of code gets ugly. When I'm asked why I don't code in Perl 5 any more ([Ruby 'til 6](http://www.bofh.org.uk/articles/2006/08/03/ruby-til-6) is still my motto) I usually reply that "I got fed up of unrolling <code>@\_</code>.")

For those unfortunates who are unfamiliar with Perl 5, Perl subroutines are odd in that they don't have named parameters. Almost every method ends up beginning like:

<code>

<pre>
sub some\_method {
my $self = shift;
my($other, $thing) = @\_;
...
}
</code>

There are arguments about whether or not to use `shift` to pull <code>$self</code> off the front of the parameter array, some folk argue for <code>my($self, $other, $thing) = @\_;</code> as the One True Way, but <strike>they are heathens and should be shunned</strike> it really comes down to taste and local coding standards.

The problem with this style of argument passing is that you have to do it for every bloody method. One or two lines of precious vertical space are always lost to unrolling the argument list. Vertical space is precious. Losing one or two lines of space for every method is fine when your methods are long, but well factored methods are anything but long. When your method bodies are usually 3 or 4 lines long, that repeated chunk of code is adding 25-30% to your line count, and those added lines are almost pure repetition. The temptation was always to let that method get a little bit longer, swallowing the extra complexity rather than waste another few precious lines on doing the same damned thing again. Perl 6's implicit `self` and named arguments are, on the face of it at least, only minor improvements, but they're the sort of improvements that make all the difference.

### What did I miss?

I'm sure you've got pet examples of this pattern, things that I've overlooked or never thought of. Tell us about it - comment here or blog it. Let's all start failing earlier and winning bigger.

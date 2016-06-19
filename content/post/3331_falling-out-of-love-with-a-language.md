+++
title = "Falling out of love with a language"
slug = "falling-out-of-love-with-a-language"
date = "2010-03-10T02:03:00+00:00"
draft = false

+++

So, [Giles Bowkett](http://gilesbowkett.blogspot.com/) asked me on facebook "Why Perl?". This is the long answer.

I'm a Perl hacker. I have been for around 16 years now. Around 5 years ago, prompted by the Pragmatic Programmer and Adam Turoff, I started looking at Ruby, and Ruby on Rails and sort of fell into maintaining Typo.

Why? I was getting hacked off with Perl.

I was coming to the end of my tenure/tether as Perl 6 Summarizer: watching a language that I still want to use before I die taking forever to get done gets wearing after a while, especially when you're spending 8 hours a week summarizing the activities of a development community that, in parts, was verging on the toxic (it's way better now).

I was also getting annoyed by small niggles in the way Perl 5 works. This was in the days before Moose. Stevan Little was just starting work on what became Class::MOP (as a prototype of Perl 6's metaobject protocol) and *way* before the days of Devel::Declare.

Ruby seemed to be, as Matz has described it, "Perl, done right". All the things that were pissing me off about Perl were so much easier in Ruby. I've joked, in a Ha Ha, only serious kind of way, that I left Perl for Ruby because I got fed up of unrolling <code>@\_</code>, but there's more to it than that. "Objects everywhere" fits my head. The language is dynamic. Ruby code had less boilerplate in it. Oh, and Ruby on Rails was looking very cool, even if David Heinemeier Hansson didn't seem to have a clue about RESTful/Resourceful principles (he has rather more of a clue now). The community looked vibrant. What's not to like?

I was a very happy Ruby programmer. It's a great language. If you haven't taken it out for a spin round the block, you should give it a try.

### A Perl 5 rennaissance

While I was away on my failed attempt to become a maths teacher and then my Ruby sojourn, Perl 5 was waking up. If Perl 6 were declared a failure tomorrow by the people who are actually working on it (as opposed to the people who aren't working on it, but bitch about it anyway - see some of chromatic's more intemperate posts about those people), then it will still have had value for the way it's inspired members of the Perl 5 community to nick the good bits and make them happen in Perl 5.

Stevan finished Class::MOP and it got used as the basis for Moose, which is essentially a layer of sensible defaults which sits on top of the insanely configurable and slightly more agnostic Class::MOP, which in turn sits on Perl's weird reduced instruction set object orientation. Moose even got fast enough that it's usable in polite company (it was always faster than Ruby), so people did. It grew an ecosystem.

Another, more recent development, is Devel::Declare. Devel::Declare is, in accordance with the long history of Perl, completely batshit insane. What it does is lets you trip the perl parser up and, while it's standing there looking at the pretty birds flying around its head, you can run ahead and rewrite the code that it's about to parse. Well, I say run, but what I mean is hobble ahead wearing narrow blinkers and mittens. You can accomplish amazing things in such circumstances, but it's not fun. Devel::Declare enabled my current favourite module on CPAN: MooseX::Declare, which I've talked about here:

<object width="400" height="300">
<param name="allowfullscreen" value="true" /><param name="allowscriptaccess" value="always" /><param name="movie" value="http://vimeo.com/moogaloop.swf?clip_id=4627327&amp;server=vimeo.com&amp;show_title=1&amp;show_byline=0&amp;show_portrait=0&amp;color=255&amp;fullscreen=1" /><embed src="http://vimeo.com/moogaloop.swf?clip_id=4627327&amp;server=vimeo.com&amp;show_title=1&amp;show_byline=0&amp;show_portrait=0&amp;color=255&amp;fullscreen=1" type="application/x-shockwave-flash" allowfullscreen="true" allowscriptaccess="always" width="400" height="300"></embed></object>

<p>
<a href="http://vimeo.com/4627327">An introduction to MooseX::Declare</a> from <a href="http://vimeo.com/user1158507">Piers Cawley</a> on <a href="http://vimeo.com">Vimeo</a>.

</p>
MooseX::Declare lets me kill boilerplate. Instead of writing:

<code>

<pre>
package Something;
use Moose;

has an\_attribute =&gt; (is =&gt; 'rw');

sub a\_method {
my($self, $with, $parameters)
= @\_;
...
}
</code>

I can write:

<code>

<pre>
use MooseX::Declare;
class Something {
has an\_attribute =&gt; (is =&gt; 'rw');

method a\_method($with, $parameters) {
...
}
}
</code>

which makes me happy. This is being done by a module which is written in Perl. It relies on a couple of modules that are implemented in C, but the bulk of the work is done in Perl itself.

So, that's addressed one of the annoyances that led me to Ruby. What about 'objects everwhere'. I got used writing things like:

<code>

<pre>
5.times { |each| ... }
</code>

Surely I'm not going to able to write:

<code>

<pre>
5-&gt;times(sub {...})
</code>

in perl?

Well...

<code>

<pre>
use Moose::Autobox;
sub Moose::Autobox::SCALAR::times {
my($count, $block) = @\_;
1-&gt;to($count)-&gt;each\_key($block);
}

5-&gt;times(sub {$\_-&gt;say});
</code>

Bingo! I'd like to be able to write <code>5-&gt;times { $\_-&gt;say }</code> but can't quite manage it. Give PerlX::MethodCallWithBlock a little more time though...

So, while I was away, most of the issues I had with Perl, issues that had driven me into the arms of Ruby, had been addressed.

### It's not about the bike

The new Perl tech is great, but all it does is removes barriers. I wasn't attached to the Perl language by some huge bungee cord which was crushing me against these barriers while I sojourned in Ruby land, all the while pining for Perl. I was loving Ruby. It's a great language.

What I am attached to is the group of friends I've made in the Perl community over the years. Many of whom I've never actually met face to face.

I felt like such a curmudgeonly old fart in the Ruby community (more specifically, the Rails community). Everything's shiny and new and awesome. And, I suppose when you're coming to the language from something like PHP, it *is* shiny and new and awesome. But it gets wearing after a while. And the arrogance? My dear! The arrogance! Admittedly, some people have a lot to be arrogant about - I still cheer DHH's "Fuck you![](" slide, but others... not so much. The kind of "Look) Ruby is uniquely suited to writing DSLs!" bullshit<a href='#tcs-example'><sup>1</sup></a> that sends chromatic off in fits of apoplexy annoyed me too, especially when I was looking at the kind of examples they were presenting as exemplary and remembering Perl from 5 years before that did the same thing but with cleaner language-like interfaces.

Coming back to Perl may well be a straightforward retreat to competence. I may be rationalizing like mad. But right now, it feels like I left Ruby because the ruby community, in the West at least, isn't a fun place for me to be. The sexism is just the icing on that particular cake.

<p id='tcs-example'>
<sup>1</sup> Here's my response to Rspec:

</p>
<code>

<pre>
testclass exercises Something {
test that we can create an object {
isa\_ok $test-&gt;subject-&gt;new, 'Something';
}
}
</code>

Note the lack of perlish furniture in the `test that we can create an object` part. No need to quote the strings, no meaningless <code>do</code>s scattered about the place, no <code>:symbol</code>s appearing at random. Okay, so I have to quote `'Something'` in the implementation block, but the implementation block is unadulterated Perl. I don't claim that only Perl can do this. I do claim that, right now at least, Ruby can't.

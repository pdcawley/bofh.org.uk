+++
title = "Getting the Rspec religion"
slug = "getting-the-rspec-religion"
date = "2006-11-26T02:52:00+00:00"
draft = false

+++

I've been eyeing the [rspec](http://rspec.rubyforge.org/index.html) and rspec on rails packages and thinking I should give them a go.

To my eye at least, something like:

<code>

<pre>
context 'Given a published article' do
fixtures :contents

setup { @article = contents(:published\_article) }

specify 'changing content invalidates the cache' do
`article.body = 'new body'
    `article.invalidates\_cache?.should\_be true
end
end

context 'Given an unpublished article' do
fixtures :contents

setup { @article = contents(:unpublished\_article) }

specify 'changing content keeps the cache' do
`article.body = 'new body'
    `article.invalidates\_cache?.should\_be false
end
end
</code>

reads far more fluently than the equivalent Test::Unit based tests:

<code>

<pre>
class CacheSupportTest &lt; Test::Unit::TestCase
fixtures :contents

def test\_changing\_published\_article\_invalidates\_the\_cache
art = contents(:published\_article)
art.body = 'new body'
assert art.invalidates\_cache?
end

def test\_changing\_unpublished\_article\_keeps\_the\_cache
art = contents(:unpublished\_article)
art.body = 'new body'
assert ! art.invalidates\_cache?
end
end
</code>

So, I installed everything and started to work on a new class in Typo using rspec. Rather annoyingly, this seemed to break the current test suite, so instead of working on my new model class, I set to porting the existing suite.

And, on about my third test suite, I found what I think is a bug in the suite. I'm not *sure* it's a bug, because, the way the test is written (by me, I admit it), masks the intent quite dramatically. I'm also finding that the freedom to name specifications and contexts in English rather than `method_names_that_go_on_for_ever` is forcing me to come up with much more useful descriptions of what I'm testing. I find myself working on making the spec runner output read reasonably well as English, and doing that casts light on what is and isn't being tested.

I've known for a while that Typo's test suite is, um, spotty, but the porting process is really helping me get familiar with what's being tested. I'm half tempted to start adding extra specs as I go, and if I could work out how to keep the existing tests working while I did it, I would, but my priority for now is to get to the point where I can check the specs and be confident that the new specs are no worse than the old tests.

Because I'm much more confident that I know what the specs are *doing*, I'm also confident that it won't be hard to revisit them to help specify typo's behaviour better. I'll just have to give myself the discipline of beginning each coding session with half an hour of fleshing out the specifications before I get back to adding behaviour.

+++
title = "Baby's first screencast"
slug = "babys-first-screencast"
date = "2008-03-14T14:29:00+00:00"
draft = false

+++

If you follow the Ruby blogs, you will probably have seen a bunch of programmers attempting to do something akin to Haskell's `maybe`, or the ObjectiveC style, message eating null.

Generally, by about the 3rd time you've written

<code>

<pre>
if foo.nil? ? nil : foo.bar
...
end
</code>

you're getting pretty tired of it. Especially when `foo` is a variable you've had to introduce solely to hold a value while you check that it's not nil. The pain really kicks in when you really want to call `foo.bar.baz`. You can end up writing monstrosities like <code>(tmp = foo.nil? ? nil : foo.bar).nil? ? nil : tmp.baz</code> (actually, if you were to write that in production code, you probably have bigger problems). One option is to just define `NilClass#method_missing` to behave like its Objective C equivalent, but I've never quite had the nerve to find out how that might work. I wanted to write

<code>

<pre>
if maybe { foo.bar.baz }
...
end
</code>

and have nil behave like an Objective C nil for the duration of the block, but no longer. So I wrote it. Then I thought about how to present it. I wrote the thing test first using rspec and the whole thing just flowed, but writing up a test first development process for a blog entry is painful, so I've made a very rough (but blessedly short) screencast of the process instead.

![](http://www.bofh.org.uk/images/monadcast.jpg):http://www.archive.org/download/RubyMaybeRoughcut/Monad2.mov

That's a slightly reduced thumbnail, the movie is substantially more readable. The bottom pane of the window is the output of autotest rerunning the spec every time either the spec or the implementation changes. The top pane alternates between the specs and the implementation. Generally, every time I edit the specs, a test starts failing and every time I edit the implementation it starts passing again. In the (any) real coding run, there were of course false starts, but generally the specs kept me pretty straight.

A word or two of warning: This is a completely unedited, silent, screen cast, there are typos, backtrackings and other embarrassments. I stopped recording once I'd got 4 tests passing, but this is far from release quality (it's perfectly usable if you know its limitations, but it's not entirely robust).

Please let me know what you think of this. I'm aiming to make a more polished version, complete with voice over and it would be good to know which bits are confusing and need addressing in more detail in the voice over.

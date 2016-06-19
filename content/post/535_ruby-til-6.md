+++
title = "Ruby 'til 6"
slug = "ruby-til-6"
date = "2006-08-03T02:34:00+00:00"
draft = false

+++

Oh, I say. It seems that [Sam Ruby](http://www.intertwingly.net/blog/2006/08/01/Python-vs-Ruby) is another member of the "Ruby 'til \[Perl\] 6" club.

I like Ruby a lot. For the kind dynamic OO/Functional coding style that I espouse, it's a better Perl than Perl simply because it's so much less verbose (I got *so* tired of always unpacking the argument list, it tended to put me off applying the [Composed Method](http://www.c2.com/cgi/wiki?ComposedMethod) pattern anywhere near often enough).

But it's not my One True Language. Perl 6 looks like it might be an awful lot closer to it. If nothing else, it has Lisp style macros.

A good macro system, especially when it's combined with an accessible and well abstracted runtime is an awfully useful thing. For instance, consider the Rails controller. In a rails controller, public methods are 'visible' as actions that can be accessed via the web (usually with a url of the form `/:controller/:action`). Protected and private methods aren't accessible in the same way.

But sometimes it's quite handy to have a method on the controller that shouldn't be deemed to be an action, but which you might want to call from a model. The canonical example here is when you're doing [Double Dispatch](http://www.c2.com/cgi/wiki?DoubleDispatch). Here's an example of bad code:

<code lang='ruby'>

<pre>
results = @search\_results.collect do |item|
case item
when Comment: extract\_comment\_metadata(item)
when Trackback: extract\_trackback\_metadata(item)
else
fail "Oops!"
end
end
</code>

Look, we're using a case statement that dispatches on the class of another object! This is a job for Polymorphism. Let's assume that the two `extract_*_metadata` methods need to do some of the things that only a controller can and can't simply be replaced with `extract_metadata` on the Comment and Trackback objects. Here's how I'd rejig the controller code:

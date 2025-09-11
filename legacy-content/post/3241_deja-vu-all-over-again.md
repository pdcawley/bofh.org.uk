+++
title = "Deja vu all over again"
slug = "deja-vu-all-over-again"
date = "2007-07-29T18:18:00+00:00"
draft = false

+++

Back when I was still programming Perl, one of the common mistakes that you'd see when people wrote lazily initialized object accessors was code like:

<code>

<pre>
sub get\_content {
my($self) = @\_;
$self-&gt;{content} ||= 'default content';
}
</code>

Code written like this would trundle along quite happily, until you had an attribute where, say, the empty string or 0 were legal values for the attribute. The problems were especially painful when the default value wasn't something that perl treated as false. The correct way of writing that code would be:

<code>

<pre>
sub get\_content {
my($self) = @\_;
unless (exists($self-&gt;{content})) {
$self-&gt;{content} = 'default content'
}
$self-&gt;{content}
}
</code>

Which is substantially uglier, but safe.

Safety's important, especially in building block code like accessor methods. An accessor method that works 99.99% of the time is like a compiler that produces correct code 99.99% of the time - useless.

### Why dÃ©jÃ  vu?

Recently, the usually spot on Jay Fields [wrote up](http://blog.jayfields.com/2007/07/ruby-lazily-initialized-attributes.html) the lazy initialization pattern for Ruby. I'm not entirely sure that I agree with his motivation for the pattern, but I am concerned by his suggested code transformation. He suggests writing your lazy initialization as:

<code>

<pre>
def content
@content ||= \[\]
end
</code>

Does that look familiar? This is subject to exactly the same potential bug as the perl code above. Admittedly, the number of possible 'bad' values is reduced to `nil` and `false`, but it only takes one. Here's the fix:

<code>

<pre>
def content
unless instance\_variable\_defined? :`content
    `content = \[\]
end
return @content
end
</code>

This code is guaranteed to work in all circumstances.

### Going a little bit further...

As Mark Jason Dominus has argued persuasively elsewhere, [patterns are a sign of weakness in a programming language](http://blog.plover.com/prog/design-patterns.html), so how can we go about incorporating this boilerplate code into our language[1].

How about something like this (as yet untested):

<code>

<pre>
class Module
class &lt;&lt; self
alias\_method :attr\_reader\_without\_default\_block, :attr\_reader
def attr\_reader\_with\_default\_block(\*args, &default\_block)
unless block\_given?
return attr\_reader\_without\_default\_block(\*args)
end
unless args.size == 1
raise ArgumentError, "Expected 1 argument"
end
var\_name = "@\#{args.first.to\_s}"
self.define\_method(args.first) do
unless instance\_variable\_defined?(var\_name)
self.set\_instance\_variable(var\_name, default\_block.call(self))
end
(class &lt;&lt; self; self; end).attr\_reader\_without\_default\_block(args.first)
return self.send(args.first)
end
end
alias\_method :attr\_reader, :attr\_reader\_with\_default\_block
end
end
</code>

This code is a little more complex than the boilerplate code. When the generated method is called, possibly initializing the attribute, the `(class << self; self; end).attr_reader_without_default_block(args.first)` part replaces the instance's accessor with the default `attr_reader` implementation and calls that instead. This is arguably premature optimization, but it's not all that evil...

Assuming I've not screwed anything up, that should allow you to write.:

<code>

<pre>
class Article
attr\_reader :content {'default content'}
end
</code>

and have your `content` lazily initialized. Extending this to let `attr_writer` take a block too is a reasonably obvious next step.

Extending this lazy initialization approach to work with ActiveRecord based classes is probably the next step after that. Making it work right probably involves a little bit of fossicking around in the workings of ActiveRecord, but it's far from impossible.

[1] I tend to take the Smalltalkish view that it's pointless to separate language from library, especially in a dynamic language. A sufficiently expressive language lets you blur the boundary between them.

+++
title = "What we have here is an opportunity to accelerate"
slug = "what-we-have-here-is-an-opportunity-to-accelerate"
date = "2006-09-13T12:24:46+00:00"
draft = false

+++

[Joel](http://www.joelonsoftware.com/items/2006/09/12.html) says Ruby is slow because it's dynamic.

[Avi](http://smallthought.com/avi/?p=16) explains that dynamic languages don't have to be slow and points out at cunning trick pulled by the Strongtalk VM to avoid hitting (slow) vtables on every method call. A trick which Ruby fails to pull.

Joel [declares victory](http://www.joelonsoftware.com/items/2006/09/12b.html).

### Hang on a minute...

Maybe it would be victory if Ruby were already pulling out all the stops, including trick Avi mentioned, and was still slow. In fact, Ruby is slow because it's using na&iuml;ve dispatching techniques. Something which can be pretty easily fixed by someone who does the C thing.

The Strongtalk trick involves profiling during the early part of a program's run to find out the most 'popular' implementation of a given method. Then, when the method is called again, execution jumps to the popular method, checks it's in the right place (which can be reduced to a very fast test) and only if it isn't the right place does it go through the process of doing a fully dynamic method dispatch.

I'm prepared to bet that one could see a marked improvement on Ruby's current performance without having to follow the full profiling step. Consider this (Rubyish) pseudo code:

<code lang='ruby'>

<pre>
def Kernel::dispatch(object, method\_name, \*args, &block)
$last\_method\_at\[method\_name\] ||= object.find\_method(method)
$last\_method\_at\[method\_name\].invoke\_on(object, args, &block)
end

def Method::invoke\_on(object, args, &block)
return self.raw\_call(object, args, &block) if right\_method\_for(object)

$last\_method\_at\[method\_name\] = nil
dispatch(object, self.name, \*args, &block)
end
</code>

So, the first time `foo` gets called, it gets looked up polymorphically, cached and then invoked. And of course it's the right method.

The next time `foo` gets called, we jump straight to the cached location and check we're in the right place (which we probably are) and continue to the method proper.

But `foo` might have multiple implementations and need a full polymorphic lookup, so we empty the cache and restart the dispatch.

For (probably) the bulk of all the method calls while our code is running, the cached method will be the right one. Cache misses will be pretty costly, but not too bad and there are probably more efficient cache management schemes that can be tried (stealing the Strongtalk profiling code might not be a bad idea, for instance).

But the beauty of a simple scheme like this is that it shouldn't be at all hard to implement. Even I, with the C skills of a teeny tiny child could probably do it. And I have a 3 hour train journey this afternoon with nothing much else to do... Wish me luck.

### And, as the train approaches King's Cross

I've been exploring. And there's definitely scope for adding an inline cache and a lightweight call mechanism. I've not implemented anything yet because there's an issue with the basic Ruby NODE structure -- there's no spare slots to hang the cache from, so adding it will break binary compatibility. Ho hum.

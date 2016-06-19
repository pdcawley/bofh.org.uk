+++
title = "Rails tip: Side effect filters"
slug = "rails-tip-side-effect-filters"
date = "2007-10-08T07:13:00+00:00"
draft = false

+++

Some bugs are easy to overlook. One that has a habit of catching me out is a Rails filter that returns false occasionally when it's being evaluated purely for its side effects. Here's how I've started working round the issue:

<code>

<pre>
def side\_effect\_filter
return if some\_conditions\_not\_met?
...
ensure
return true
end
</code>

What happens here is that the ensure catches any return and returns true instead. The catch is that if something throws an uncaught exception anywhere, it too gets caught by the ensure and true is returned. Which may not be what you were looking for. Here's how to fix that issue:

<code>

<pre>
def side\_effect\_filter
error = nil

return if some\_conditions\_not\_met?
...
rescue Exception =&gt; error
ensure
raise error if error
return true
end
</code>

This catches the exception in a rescue and stashes it in the `error` variable, then the ensure checks to see if an exception was thrown and rethrows it, otherwise, it just returns true. Which is bulletproof, but ugly. Let's wrap the ugliness up in a method:

<code>

<pre>
def self.side\_effect(method, &block)
def\_method(method) do
error = nil
begin
instance\_eval(&block)
rescue LocalJumpError \# catches an explicit return
rescue Exception =&gt; error
ensure
raise error if error
return true
end
end
end

side\_effect :side\_effect\_filter do
return if some\_conditions\_not\_met?
...
end
</code>

Again, not pretty inside, but all we actually *care* about anywhere else is that the interface is good and does what it's supposed to do. Encapsulated ugliness has its own beauty. Especially if you get the interface right.

### Homework

This should pluginize quite nicely, just install the method in ActionController::Base and ActiveRecord::Base and you have a very useful tool, but I'm still not sure that the method name is right, so I'm holding off on it. If someone were to come up with a bulletproof name and release a plugin, that would be wonderful though.

### Updates

Fixed a scoping issue in the encapsulated version of the code. Replaced `yield` with `instance_eval(&block)`

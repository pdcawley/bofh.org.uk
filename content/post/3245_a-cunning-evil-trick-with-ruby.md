+++
title = "A cunning (evil?) trick with Ruby"
slug = "a-cunning-evil-trick-with-ruby"
date = "2007-08-16T01:28:00+00:00"
draft = false

+++

One of the handy tools that Ruby makes available to us Domain Specific Pidgin builders is `instance_eval`. With `instance_eval` you can take a block that was created in one context, with all its lovely closed over local variables, and evaluate it in the context of an instance of an arbitrary object. Which means that any references to instance variables and calls to methods are made as if your block were a 0 argument method of that class. It's really potent, but at the same time, a little frustrating.

Frustrating? Why frustrating?

Well, it would be **really** cool if you could call \`instance\_eval\` with a block that took some arguments. That way, you could inject some values into the block from still another scope. (Yes, it's arcane, but in the places where it would be handy it would be **really** handy).

I just worked out how to do it:

<code>

<pre>
def my\_instance\_eval(\*args, &block)
return instance\_eval(\*args,&block) unless block\_given? && !block.arity.zero?
self.class.send(:define\_method, :**, &block)
returning(**(\*args)) do
self.class.send(:remove\_method, :**)
end
end
</code>

There's problems with that though, the most obvious one being that there's no guarantee that `__` won't already exist as a method or that our block won't *call* `__`. Here's a safer, if scarier option:

<code>

<pre>
def my\_instance\_eval(\*args, &block)
return instance\_eval(\*args,&block) unless block\_given? && !block.arity.zero?
old\_method = (self.class.instance\_method(:**) rescue nil)
self.class.send(:define\_method, :**, &block)
block\_method = self.class.instance\_method(:**)
if old\_method
self.class.send(:define\_method, :**, old\_method)
else
self.class.send(:remove\_method, :**)
end
block\_method.bind(self).call(\*args)
end
</code>

This should work even in the face of being called with something along the lines of `object.my_instance_eval(10) {|v| v + __}`. Of course, there would be no point in calling like that. You'd only really need it when you want to do something like `object.my_instance_eval(10,&a_block_param)`.

In the project I'm currently working, where the need for this arose, I shall probably extract the body of `my_instance_eval` to `Proc#to_unbound_method`, that way, instead of hanging onto Block objects I can hang onto UnboundMethod objects and avoid repeatedly shuffling methods.

### But... how does it work?

I hope the code's reasonably obvious. However...

The essence of it is that real methods get to take arguments, so what we need is some way of turning our block into a real method without altering the behaviour/interface of our object. Ruby does allow us create `UnboundMethod` objects which can be thought of as anonymous methods. To use them you have to bind them to a specific instance and then call them, which is the last thing we do in `my_instance_eval`. The intervening code is what turns our Proc into an `UnboundMethod`. First, we stash the old `__` method, or a `nil` if there wasn't one. Then we define a new `__` using our block, and immediately use `instance_method` to get it back as an `UnboundMethod`. Then we either replace the old definition of `__` or simply remove the new one, restoring the original behaviour of our class. Then we bind our new anonymous method to `self` and call it with our arguments. Easy.

### Exercises for the interested reader

-   What happens if our class has `method_removed` or `method_added` implemented? Can we 'hide' from them? How?
-   What does `Proc#to_unbound_method` look like? Does it need to take an argument?


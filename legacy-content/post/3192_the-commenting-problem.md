+++
title = "The commenting problem"
slug = "the-commenting-problem"
date = "2007-03-15T04:12:00+00:00"
draft = false

+++

On Monday I went to the London Ruby Users' Group March meeting. The theme of the meeting was code review, so I put up a chunk of code from my yet to be released Sudoku solver.

It ended up being more like a talk because I spent most of the time trying to explain how Amb, the continuation based backend worked.

At one point, someone noticed the lack of comments in the code and wondered if this was because I considered it self evident.

Well, no, it isn't. However, as far as I could tell, any sensibly short comment that I could write would founder on the fact that, if you understand continuations the code is reasonably straightforward, and if you don't, it's a maze of little, twisty passages, all different.

Then, on the train back I came up with what I hope might be a useful explanation of how a continuation works. So, without further ado: consider the following code:

def first(&predicate)
self.each do |i|
if predicate.call(i)
return i
end
end
raise "Predicate not satisfied"
end

A fairly straightforward piece of Ruby which iterates using each and returns the first thing in the collection that satisfies predicate or throws an exception if nothing matches. About the only thing that could possibly be thought of as magical is the workings of return - the block that contains the return is invoked over in the implementation of <code>each</code>, but it 'knows' to return from <code>first</code>.

Because you're an experienced computer programmer, you know that, although <code>return</code> *looks* a bit like a function that takes 0 or 1 arguments, it's actually a keyword of the language.

Strangely, a continuation looks like a function that takes 0 or 1 arguments... here's that code rewritten slightly:

def first(&predicate)
callcc do |k\_return|
self.each do |i|
if predicate.call(i)
k\_return.call(i)
end
end
raise "Predicate not satisfied"
end \# k\_return 'returns' here.
end

This function does exactly the same as the version that uses return. `k_return` is a continuation created by `callcc`. The continuation wraps up the process of returning a value from a block in a thing that can be passed around as if it, itself, were a block. Of course, if a continuation was just another way of writing 'return', there'd be nothing to them. Where they get weird is that you can use them more than once, and you can stash them away to use later, in an instance variable say.

So, in my sudoku solver, every time the solver makes a guess, it stashes away a continuation that would, in effect, guess differently. So when, some 20 cells later, it turns out that the guess was wrong, the solver uses the continuation to backtrack and try a different guess. This trick of keeping track of 'remaining' guesses is sufficiently general that it can all be wrapped up in a library. The solver itself only has to deal with methods like `one_of` and `assert`. Instead of having to write an explicit, twisty search loop, you can simply specify your values, make a bunch of assertions about them and then sit back and let the library handle the search.

For completeness' sake, and because I hope it might help you understand how continuations work a little better, here's a couple of other callcc based equivalents.

collection.each do |i|
...
break if ...
end

Becomes

callcc do |k\_break|
collection.each do |i|
...
k\_break.call if ...
end
end

And

collection.each do |i|
next if ...
...
end

Becomes

collection.each do |i|
callcc do |k\_next|
k\_next.call if ...
...
end
end

And, finally

collection.each do |i|
redo if ...
...
end

becomes

collection.each do |i|
k\_redo = nil
callcc {|cnt| k\_redo = cnt}
k\_redo.call if ...
...
end

That last one's the real key to getting a handle on how continuations work, and how you can use them to build interesting control structures. Notice that there's no limit to the number of times you can call `k_redo`. You could, for instance do (and yes, I *know* there are better ways to do this):

collection.each do |i|
k\_redo = nil
n = 10
callcc {|cnt| k\_redo = cnt}
puts n
n -= 1
k\_redo.call if n &gt; 0
...
end

which prints a countdown from 10 to 0 before getting on with whatever it's supposed to be getting on with.

### Solicitation

Please let me know if I've helped clear up your understanding of continuations. Or if I've just muddied the water still further. Thanks for you time.

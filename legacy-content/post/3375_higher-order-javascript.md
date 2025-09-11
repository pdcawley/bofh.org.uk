+++
title = "Higher Order Javascript"
slug = "higher-order-javascript"
date = "2010-11-18T14:52:00+00:00"
draft = false

+++

To my surprise, several people have asked for the slides from my [Øredev](http://oredev.org/2010) talk on Higher Order Javascript, and I've followed my usual practice of saying "Sorry, no". Slide decks are a terrible teaching medium - they're fine if they come with the presenter, but if they contain enough information to read as if they were a book, then I'm prepared to bet that they made a terrible presentation. Good presentations have a synergy; slides illustrate what the speaker is saying and neither the speech nor the slides should really stand alone. After all, if either could, why bother with the other?

But, the requests mean that some people found the information useful, so here's the content (or something like it) written up as a blog post instead.

### Higher order what?

Yes, yes. I know. I'm a Perl programmer. What am I doing writing about higher ordered javascript? In part it's because Higher Order Javascript was the title of the talk that [Giles Bowkett](http://gilesbowkett.blogspot.com) was due to give before he had to bale out and, while I'm sure I could give a Higher Order Perl talk, I doubt that it would have gone down particularly well in the webprogramming track of a conference that was heavily slanted towards Java, .NET and Agile. Nowadays Perl gets even less respect than Javascript and I simply wasn't prepared to go kicking that particular stone uphill with a bunch of programmers who made their minds up on that particular score ages ago.

Anyway, higher ordered programming is language agnostic. So long as your language has first class functions (and ideally lexical closures) then you're good to go. I first learned about higher order functions from Mark Jason Dominus's [Higher Order Perl](http://perl.plover.com/yak/ivory/) talk when he was still calling it "Stolen Secrets of the Wizards of the Ivory Tower" and some time before he'd written <a href="http://www.amazon.co.uk/gp/product/1558607013?ie=UTF8&tag=justasummary-21&linkCode=as2&camp=1634&creative=6738&creativeASIN=1558607013">the book</a><img src="http://www.assoc-amazon.co.uk/e/ir?t=justasummary-21&l=as2&o=2&a=1558607013" width="1" height="1" border="0" alt="" style="border:none !important; margin:0px !important;" /> on higher order programming in perl. In that talk, Dominus pointed me towards Abelson and Sussman on the [Structure and Interpretation of Computer Programs](http://mitpress.mit.edu/sicp/) (one of the *great* texts on programming - everyone who is serious about their craft should read it). Since then I've mucked about in Smalltalk, Ruby, Haskell, Scheme, Javascript and Clojure as well as Perl and higher order techniques had stood me in good stead in all of them.

### Before we start

There will be code snippets here which I don't pretend are written in what is normally considered good javascript style. In particular there's no error checking and variable names are short and almost devoid of meaning. The short variable names help avoid line wrapping headaches within the blog format and the lack of error checking is to enable us to focus on the higher ordered techniques that we're interested in. In the real world, we don't live on the happy path and you are expected to *think* before you code. And write tests.

I do try to follow the Haskell convention that a list or array of stuff will be called something like `xs` and `x` will the variable used to hold the current element of `xs` as we iterate over it.

### I've not seen that in Fowler...

Higher ordered programming is just another way of removing repetition from your program by extracting the 'shape' of an algorithm into a separate function. In the presentation I asked for a show of hands from people who'd done higher ordered programming before. A scattering of hands went up. Then I asked who'd used jQuery's `$.each(function {...})` and most hands in the room went up. Which means that most of the people in the room had used higher order programming and never realised it.

If you look at it right, `$.each` is just the result of applying the *Extract Method* refactoring on a for loop in a way that's impossible to do in, say, Java. Here's a barebones version of `$.each`

```javascript
each = function (xs, f) {
  var i, len, each;
  for(i = 0, len = xs.length; i < len; i++) {
    f(xs[i]);
  }
}
```

The jQuery each does rather more than this, but at it's core it's about turning an iterative loop into a function that takes a collection and a callback function which is applied to every element in the collection. What could be easier?

### Higher Leverage Javascript

What Javascript's designers thinking? Seriously, if you're going to provide a lovely language with first class functions, why on earth would you want to spell the keyword that makes a function '`function`'? Lisp is stuck with `lambda` because, well, because it's a bloody old language and they didn't know any better then. Arguably it's why they ended up with the macro capabilities they did - anything to avoid spelling out lambda every time. Anything *longer* than Lambda's just silly. So, for the rest of the examples I'm switching to [CoffeeScript](http://jashkenas.github.com/coffee-script/). There's a crib sheet over on that site which should allow you to work out what's going on, and if you take away nothing else from this article than "Investigate CoffeeScript", you'll be ahead of the game.

### Refactoring with higher order functions

Consider `sum` and `prod` below:

```coffeescript
sum = (xs) ->
  r = 0
  (r = r + x) for x in xs
  r

prod = (xs) ->
  r = 1
  (r = r * x) for x in xs
  r
```

Notice how they both have the same shape. They both set an initial result and then, for each element in the collection, the intermediate result with an element of the collection using an operator and, when they've finished iterating over the collection, they return the last value of the intermediate result.

Let's apply _Extract Temporary_ a few times to make the repetition even more obvious:

```coffeescript
sum = (xs) ->
  init = 0; 
  f = (a,b) -> a + b
  r = init
  (r = f(r,x)) for x in xs
  r

prod = (xs) ->
  init = 1;
  f = (a,b) -> a * b
  r = init
  (r = f(r,x) for x in xs
  r
```

Now we can just _Extract Method_ and replace the repetition:

```coffeescript
inject = (i, f, xs) -> 
  r = i
  (r = f(r,x)) for x in xs
  r

sum = (xs) -> inject 0, ((a,b) -> a+b), xs

prod = (xs) -> inject 1, ((a,b) -> a*b), xs
```

If we were writing this in a language that didn't have an OO feel to it, we'd probably call the extracted function `foldl`, but the OO languages that already have it, it tends to be called `inject` or, in Smalltalk `inject: into:` and implemented as part of the enumerating protocol of a collection. Smalltalkers will tell you that `inject:into:` is the fundamental method for enumerating a collection, pretty much everything else in the protocol can be implemented in terms of it (so it's a good idea to make it as fast as you possibly can). In Ruby, meanwhile, `inject` is implemented in terms of `each`, which seems the wrong way around for me. Here's a few typical functions for messing with collections, implemented in terms of `inject`:


```coffeescript
grep = (pred, xs) -> 
  inject [], ((rs, x) -> if pred x then rs + [x] else rs), xs

map = (f, xs) ->
  inject [], ((rs, x) -> rs + [f x]), xs

each = (f, xs) -> 
  inject undefined, ((_, x) -> f x; _), xs
```

### I see repetition!

Have you noticed it? Everything written in terms of `inject` has the form:

```coffeescript
something = (..., xs) -> inject ..., xs
```

If we weren't higher order programmers, we should probably just shrug our shoulders and give it up as a bad job, but we *are* higher order programmers, and we're working in a language that allows functions to return functions. We'd like to be able to write:

```coffeescript
    sum = injector 0, (a,b) -> a + b
```

where `injector` is a function like `inject` but which returns a function of one argument which will iterate over the supplied collection.

We *could* write injector directly, but where's the fun in that? Instead let's write:

```coffeescript
with_list = (f) ->
    arity = f.length
    (args...) ->
        if arity > 0 && args.length ![]( arity - 1
            throw "Oops)"
        (xs) -> f args..., xs
```

So, `with_list` is a function which takes a function, f, of n arguments an returns a function of n - 1 arguments (<em>args</em>) that returns a function of one argument (<em>xs</em>) that applies f to a list of arguments made up of appending *xs* to *args*.

So that's code that operates on code to write code that writes code. Confused? Don't worry - it gets worse.

Once we have `with_list` we can dispose of `sum` and `prod` by writing:

```coffeescript
injector = with_list inject

sum = injector 0, (a,b) -> a + b
prod = injector 1, (a,b) -> a \* b
```

It doesn't help us out with `grep`, `map` and `each`, but I'm not feeling clever enough to fix that at the moment so I'll leave it as an exercise for the interested reader.

### Functional vs. OO Style.

So far, I've been writing everything in a very functional style. If I'm honest, I prefer it that way for a lot of things, but I'm aware that I'm not in the majority on this front and some people would rather see `inject` and co as methods on objects like Allan Kay intended. So let's see if we can write something which will allow us to transform our functions into methods. Essentially, we need to get rid of the last argument (xs) and make the functions operate on `this` instead.

We already have `with_list` which transforms a function on n arguments into a function of n - 1 arguments. Maybe we can write `with_this` like so:

```coffeescript
with_this = (f) ->
    (args...) -> f args..., this
```

And, because I'm feeling mischievous, why not write:

```coffeescript
Function.prototype.to_method = with_this with_this
```

And then we have:

```coffeescript
Array::sum = sum.to_method()
Array::prod = prod.to_method()
Array::inject = inject.to_method()
Array::grep = grep.to_method()
Array::map = map.to_method()
Array::each = map.to_method()
```

Hopefully it's obvious what's going on here. If it isn't, you might want to reread everything so far and press on when you've got the hang of it. Remember, higher order programming arises from applying the following principles:

<dl>
<dt>Principle 1</dt> <dd>Functions can take functions as arguments</dd>

<dt>Principle 2</dt> <dd> Functions can return new functions</dd>
</dl>

That's all there is too it. Everything else is just working through the implications. Just remember that if `counter_from = (n) -> () -> n = n + 1`, then `c1 = counter_from 1` and `c2 = counter_from 2` are two different functions that do not share the same `n`. Easy, no?

Right, now you've got that straight, let's move on to

### Memoization

Let us suppose that you are a na&iuml;ve mathematician who has decided to implement a function to find the nth number in the fibonacci sequence. You know that Fib<sub>0</sub> is 0, Fib<sub>1</sub> is 1 and that Fib<sub>n</sub> is Fib<sub>n-1</sub> + Fib<sub>n-2</sub> otherwise. So you write:

```coffeescript
fib = (n) ->
    switch n
        when 0 then 0
        when 1 then 1
        else fib(n-1) + fib(n-2)
```

Obvious. Everything's fine for small values of n, but it takes an *age*, and an awful lot of memory to find Fib<sub>50</sub>. What can be wrong.

Because you know something about higher order functions, you write:

```coffeescript
calls = 0;
counted = (f) ->
    calls += 1
    f args...

fib = counted fib
```

You worry a little about that free variable, `calls`, but you're busily debugging and you're going to throw it away once you've worked out what's going on. Instead you run a test program:

```coffeescript
calls = 0;
print "#{fib 30} called 'fib' #{calls} times\\n"
calls = 0;
print "#{fib 31} called 'fib' #{calls} times\\n"
```
And you are shocked when the output is:

```coffeescript
832040 called 'fib' 2692537 times
1346269, called fib 4356617 times
```

Clearly you do not have an efficient algorithm.

You could, at this point, sit down and think and come up with a more efficient algorithm (which, frankly, isn't all that hard in the simple case of finding fibonacci numbers, but which can get harder in the case of, say, the [Ackermann function](http://en.wikipedia.org/wiki/Ackermann_function)). But, dammit, you've reduced the problem of finding Fib<sub>n</sub> to adding together two numbers you've already found, why not just make the computer remember that?

```coffeescript
memoize1 = (f) ->
    results = {}
    return (arg) ->
        if arg of results
            results[arg]
        else
            results[arg] = f arg

fib = memoize1 fib
```

And now when you run your test you get:

```
832040, called fib 31 times
1346269, called fib 1 times
```

I think we can safely say that that's an improvement. And it'll work for any function of one argument, where the argument is of a type that can be used as a key in a Javascript dictionary. Hmm... that's possibly not as wide as we'd like.

However, usually when we go to memoize a function, we know something about that function and its expected arguments. If we can write a function to normalize the function arguments into a valid key, then we can write a more general memoize that takes the function to memoize and an optional normalization function:

```coffeescript
memoize = (f, normalize) ->
    results = {}
    normalize ||= (x) -> x
        (args...) ->
            key = normalize args...
            if key of results
                results[key]
            else
                results[key] = f args...
```

Of course, given enough thought and/or access to Wikipedia, we could well come up with the closed form function to find Fib<sub>n</sub>

```coffeescript
fib = (n) ->
    Math.round(
        Math.pow(
            (1+Math.sqrt 5) / 2,
            n
        ) / Math.sqrt 5
    )
```

But for sheer readability, I'll take the memoized form.

### I promise to pay the bearer on demand...

Suppose you have 1000 things which are sortable, and you want to find the first ten. You *could* sort the entire list and just grab the first ten elements of the sorted list. But that's an O (<var>n</var>log<sub>2</sub><var>n</var>) algorithm. What if we had a means of only sorting the first <var>m</var> elements and then stopping (or pausing)?

We do. Or we will. And to do this, we're going to use a functional data structure called a Stream.

If you've spent any time around functional programmers, you will have come across a fundamental data structure called the Pair, which is usually used int list structure. A Pair is simply a pair of pointers. When pairs are used in what's called list structure, the first (`head`, or `car`) pointer points to the value of the cell, and the second (`tail` or `cdr`) points to the next element in the list, or to a special value called `nil` or 'the empty list'. Essentially a singly linked list then. When you're following most introductions to lisp, the pair is pretty much all you'll need to use because given that simple structure, you can build pretty much any structure you desire. Real world lisp programmers other, more specialised (faster) data structures available, but anything new or complicated tends to get built with pairs (including the data structure we're actually interested in right now).

A stream is like a pair in that it has a head value and a tail. But a stream's tail isn't another stream, it's a promise to compute another stream. So the head is akin to a lump of silver or gold - something with inherent value - and the tail is like a bank note, which is simply a 'promise to pay the bearer on demand' with another stream.

### Show me the code!

This is all a bit abstract, so let's look at a stream implementation.

```coffeescript
the_empty_stream = 
    is_empty: true
   take: (n) -> []

class Stream
    constructor: (@head, p) ->
        tail = p
        forced = false
        this.force = ->
            if forced then tail
            else
                forced = true
        tail = tail()
    take: (n) ->
        if n == 0
             []
        else
            [this.head].concat this.force().take n - 1
    is_empty: false

force = (s) -> s.force()
take = (n, s) -> s.take n
is_empty = (s) - s.is_empty
```

There you go. Admittedly, `force` is a little tricky. Calling `stream.force()` makes the stream evaluate the promise to calculate the tail, and returns that value. It's written the way it is so that, once we've evaluated the promise function, we drop our reference to it, which should make our memory usage a little more efficient.

None of this makes it clear how we'll actually *use* streams, so let's add a few utility functions:

```coffeescript
take_stream = (n, s) ->
    if is_empty(s) || (n == 0)
        the_empty_stream
    else
        new Stream s.head, ->; take_stream n-1, force s

filter_stream = (pred, s) ->
    if is_empty s
        the_empty_stream
    else if pred s.head
        new Stream s.head, ->
        filter_stream pred, force s
    else
        filter_stream pred, force s

map_stream = (f, s) ->
    if is_empty s
        the_empty_stream
    else
        new Stream f(s.head), ->
            map_stream f, force s

cat_streams = (s, t) ->
    if is_empty s
        t
    else
        new Stream s.head -> cat_streams force(s), t

print_stream = (s) ->
    if is_empty s
        return
    print "#{s.head}\n";
    t = force s
    if t.is_empty then return
    else print_stream t

Array::as_stream = ->
    switch this.length
    when 0 then the_empty_stream
    else
        [x,xs...] = this
        new Stream x, -> xs.as_stream()

array2stream = (xs) -> xs.as_stream()
```

It doesn't look like much so far, but there's more than enough to solve our top ten out of a thousand problem.

First let's build ourselves an array of 1000 integers chose at random.

```coffeescript
make_rands = (ceil) -> 
    new Stream Math.floor(Math.random() * ceil), -> make_rands max

big_list = take 1000, make_rands 1000
```

The function `make_rands` returns an infinite stream of random numbers and we take the first 1000. What we want to do now is a partial quicksort. First, we write `qspart` which partitions an array of integers into three arrays. All the members of the original array that are less than its first element, all the members that are equal to it and all the members that are greater than it.

```coffeescript
qspart = (xs) ->
    [p, ys...] = xs
    inject(
    [[], [p], []],
    ((bins, each) ->
        i =
            if each < p then 0
            else if each > p then 2
            else 1
        bins[i].push each
        bins
    ),
ys
)
```

This would turn the list `[2,3,5,2,1,0]` into `[[1,0],[2,2],[3,5]]`. Now we just have to work out how to make that into a sorted stream.

```coffeescript
make_sorted_stream = (xs) ->
    switch xs.length
    when 0 then the_empty_stream
    else
        [ls,[p,ps...],hs] = qspart xs
        cat_streams(
            make_sorted_stream(ls),
            new Stream p, ->
                cat_streams(
                    ps.as_stream(),
                    make_sorted_stream(hs)
                )
        )
```

We're partitioning the list into a list of those less than the pivot (<code>ls</code>), a list of elements equal to the pivot (<code>\[p,ps\]</code>) and a list of those greater than the pivot (<code>rs</code>). Then we build a stream by concatenating the sorted stream built on `ls` with stream whose head is the pivot and which promises to supply a stream made by concatenating the stream made from all the other elements equal to the pivot with the sorted stream made on `rs`. We're reasonably confident that this should terminate because the sorted stream on an empty array is the empty stream, and every sub list made by `qspart` is shorter than the list in its argument.

If we wrap `qspart` to get some kind of trace then get the top 3 entries from a list of 10 random numbers, like so:

```coffeescript
qs = ((f) (xs) ->
    res = f xs;
    print "# qspart([#{xs}] -> #{res[0]}|#{res[1]}|#{res[2]}\n"
    res)(qs)

print_stream(
    take_stream(
        3,
        make_sorted_stream(
            take(10, make_rands 50)
        )
    )
)
```

We see output like:

```coffeescript
1.  qspart([41,7,6,13,43,47,23,15,34,22]) # -> 7,6,13,23,15,34,22|41|43,47
2.  qspart([7,6,13,23,15,34,22])          # -> 6|7|13,23,15,34,22
3.  qspart([13,23,15,34,22])              # -> |13|23,15,34,22
4.  qspart([23,15,34,22])                 # -> 15,22|23|34
5.  qspart([15,22])                       # -> |15|22
```

Which gives us some idea of the shape of the computation. Notice how we only call `qspart` when necessary and we're doing the minimum amount of work needed to get the first three entries from the sorted list.

### Higher Order Javascript in the Real World

Now you know what higher order programming looks like, you'll start seeing it everywhere in libraries like [jQuery](http://jquery.org), [YUI](http://developer.yahoo.com/yui) and [Node.js](http://nodejs.org). I've not covered what's called 'continuation passing style', but it's almost impossible to do asynchronous stuff without it.

So, don your HOF sensing glasses and go play.

### One last thing

#### Why does this stuff get called higher order programming?

I think the idea is that functions that manipulate functions are of a higher order than functions that manipulate other 'lower' data structures. I tend to think that it belongs in a class with 'metaprogramming' - words and phrases that may lead one to infer that the meta- or higher order programmer is doing something inherently harder than the bread and butter programming indulged in by all those plain old programmers.

When it comes down to it, that higher order functions are just functions and metaprogramming is just programming and the sooner we learn that, the faster we can improve our programming practice.

### Update

For those of you who are finding the CoffeeScript a wee bit daunting (I personally like the lack of noise, but your mileage may vary), there's a thread on [sitepoint](http://www.sitepoint.com/forums/showthread.php?t=713645) which translates many of the examples here into more idiomatic Javascript than the CoffeeScript compiler can manage. You may find it useful.

I've written a followup post on [asynchronous streams](http://www.bofh.org.uk/2010/11/24/asynchronous-streams) which discusses using higher order techniques in the browser window to do something a little more 'real' than the examples shown here.

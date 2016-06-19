+++
title = "Javascript scoping makes my head hurt"
slug = "javascript-scoping-makes-my-head-hurt"
date = "2008-03-20T11:41:00+00:00"
draft = false

+++

Who came up with the javascript scoping rules? What were they smoking. Here's some Noddy Perl that demonstrates what I'm on about:

<code>

    my @subs; 
    for my $i (0..4) {
      push @subs, sub { $i }
    }

    print $subs[0]->(); # => 0;

</code>
Here's the equivalent (or what I thought should be the equivalent) in Javscript:

<code>

    var subs = [];
    for (var i in [0,1,2,3,4]) {
      subs[i] = function () {
        return i;
      }
    }
    alert subs[0]() // => 4

</code>
What's going on? In Perl, <code>$i</code> is scoped to the `for` block. Essentially, each time through the loop, a new variable is created, so the generated closures all refer to different <code>$i</code>s. In Javascript, `i` is scoped to the `for` loop's containing function. Each of the generated closures refer to the same `i`. Which means that, to get the same effect as the perl code, you must write:

<code>

    var subs = [];
    for (var shared_i in [0,1,2,3,4]) {
      (function (i) {
        subs[i] = function () {
          return i;
        };
      })(shared_i);
    }
    subs[0]() // => 0

</code>
h3. Dodgy Ruby scoping

I had initially planned to write the example "How it should work" code in Ruby, but it turns out that Ruby's `for` has the same problem:

<code>

    subs = [];
    for i in 0..4
      subs << lambda { i }
    end
    subs[0].call # => 4

</code>
Which is one reason why sensible Ruby programmers don't use `for`. If I were writing the snippet in 'real' Ruby, I'd write:

<code>

    subs = (0..4).collect { |i|
      lambda { i }
    }
    subs[0].call # => 0

</code>
h3. My conclusion

Javascript is weird. Okay, so you already know this. In so many ways it's a lovely language, but it does have some annoyingly odd corners.

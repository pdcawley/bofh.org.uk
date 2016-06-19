+++
title = "Code is data, and it always has been"
slug = "code-is-data-and-it-always-has-been"
date = "2008-04-07T00:12:00+00:00"
draft = false

+++

I'm just back from the first [Scotland on Rails](http://www.scotlandonrails.com) conference, and a jolly fine conference it was too. Much kudos is due to [Alan](http://www.alancfrancis.com), [Graeme](http://woss.name), Abdel and [Paul](http://merecomplexities.com/). It was hard to believe that this was the first conference these guys have run and I think all my fellow delegates hope it won't be the last. As I said in the Pub on Saturday night, I'd had a talk proposal knocked back and, in that situation, it's terribly easy to find yourself sitting in a session thinking "Bloody hell, my talk would have been better than this!", but not at this conference.

A phrase that cropped up a couple of times was the old saw that "Data == Code" - it's very hard to avoid the idea once you start talking about code generation or Domain Specific Pidgins, parsing... I first came across the idea in [The Structure And Interpretation of Computer Programs](amazon:0262510871) where it's elegantly phrased as "Data is just dumb code, and code is just smart data". Traditionally, the idea seems to be attributed to [John McCarthy](http://en.wikipedia.org/wiki/John_McCarthy_%28computer_scientist%29), the inventor of Lisp. But it's older than that. Way older than that. The idea is actually older than Computer Science. It lies at the core of [Turing's](http://en.wikipedia.org/wiki/Alan_Turing) original paper <span style="text-align:left;">[cite&gt;On Computable Numbers, with an Application to the Entscheidungsproblem</cite>](http://www.thocp.net/biographies/papers/turing_oncomputablenumbers_1936.pdf)</span> in which invents computer science on the way to proving that it's impossible to decide algorithmically whether a given statement of arithmetic is true or false.

In the course of the paper, Turing posits what has become known as the Halting Problem:

> Given a description of a program and a finite input, decide whether the program finishes running or will run forever, given that input.

Turing's proof runs something like this:

Suppose we have a subroutine <code>halts?(code,data)</code> which solves the halting problem. Let's use that to write something like:

<code>

<pre>
def counter\_example(code)
if halts? code, code
for (;;)
end
else
return
end
end

counter\_example(File.read(STDIN))
</code>

and ask the question "What happens when we run <code>counter\_example.rb &lt; counter\_example.rb</code>"? If <code>halts?</code> reckons that <code>counter\_example</code> would halt, given itself as input, then counter example will enter an infinite loop, but if <code>halts?</code> reckon that it would enter an infinite loop, then it would halt. Which is a contradiction. Which means that there can be no subroutine <code>halts?</code>, which means that maths is hard enough to be interesting and occasionally undecidable.

Look at how the proof works - it's built around the idea that code can be treated as data. In fact, you could say that the Turing Machine looks like it does because Turing was working backwards from this core idea to describe a sufficiently powerful machine that could obviously treat it's own description as data. Certainly when you compare the clarity of his proof that the halting problem is undecidable (given the idea of the universal Turing machine) with the contortions required to make mathematics swallow its own tail in similar fashion so that GÃ¶del could prove his Incompleteness Theorem.

So, if you want to know who the idea that code is data is due to, the answer (as is so often the case in our field) is Turing.

### Postscript

Incidentally, Turing is also responsible for the first ever bug - his original implementation of a Universal Turing Machine has a couple, one of which is probably a dumb typo (which even I could spot when I read the paper). Another is more subtle, but still fixable. Somewhat delightfully, a young grad student, ([Donald W Davies](http://en.wikipedia.org/wiki/Donald_Davies), who invented packet switching) spotted these bugs and told Turing:

> I ... found a number of quite bad programming errors, in effect, in the specification of the machine that he had written down, and I had worked out how to overcome these. I went along to tell him and I was rather cock-a-hoop ... I thought he would say 'Oh fine, I'll send along an addendum. But in fact he was very annoyed, and pointed out furiously that really it didn't matter, the thing was right in principle, and altogether I found him extremely touchy on this subject.

Nearly fifty years later Davies wrote and published a debugged version of the code, which you can find in [The Essential Turing](amazon:0198250797). One lesson to draw from the above is that getting annoyed at people pointing out trivial bugs in example code is also at least as old as computer science. Rather splendidly, there's also a story of the chap who wrote the first ever assembler getting a serious telling off from Turing because the computer's time was too valuable to waste it on converting symbols into machine code when humans were perfectly capable of doing it themselves. Who knows, maybe Turing's contention was actually true back in the days of the Manchester Baby...

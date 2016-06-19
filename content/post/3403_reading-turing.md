+++
title = "Reading Turing"
slug = "reading-turing"
date = "2013-03-17T12:49:00+00:00"
draft = false

+++

Some things never disappoint. And reading Alan Turing is one of those things. In an earlier post I told an incorrect anecdote about Turing, and Russ Cox pointed me at proof, in Turing's own words, that I was wrong. I don't know why it's taken me so long, but I finally got around to reading his [Lecture to the London Mathematical Society on 20 February 1947][turing1947].

Wow.

[turing1947]: http://www.vordenker.de/downloads/turing-vorlesung.pdf
<!--more-->

Seriously. Wow. He's talking about programming the [ACE][wpACE], the ['pilot'][wppilotACE] version of which didn't run its first program until 1950. And the [Manchester 'Baby'][wpManchesterBaby], the first stored program electronic computer, was more than a year away from running its first program. It sounds like it might be dreadfully speclative and either handwavy or as out there and daft as the usual crop of 'futurologist' type predictions.

As you can probably guess from the fact that I'm bothering to write this up, it was nothing of the sort. I suggest you nip off and read it for yourself. It won't take you long and it's well worth your time. Then come back here and find out if the same things struck you that struck me.

### Back in the day

Here's the sentence that brought me up short like a slap:

> Computers always spend just as long writing numbers down and deciding what to do next as they do in actual multiplications, and it is just the same with the ACE.

I got to the end of the sentence before it clicked that back then a computer was a human being performing a computation. What we think of today as 'a computer' was what Turing called 'the ACE' and back then it certainly deserved that definite article.

Then I read it again and recognised the deep truth of it. Back in Turing's day, the ACE was planned to have a memory store made up of 5 foot tubes full of mercury acting as an acoustic delay line. Each tube could hold 1K bits and an acoustic pulse took 1 millisecond to get from one end of a tube to the other, so the average access time for a single bit of memory was around 500 microseconds. When it was finally built, it was the fastest computer in the world, running at the mighty speed of 1MHz. Nowadays we think that a cache miss that costs 200 processor cycles is bad news and our compilers and processors are designed to do everything in their power to avoid such disasters. In Turing's day there were no caches, every time something was fetched from memory it cost 500 cycles. (Well, in 1947 that would be 500 cycles + a year and a half before there was a computer to fetch the memory from in the first place).

Curiously, the gold standard of high performance memory in Turing's day was the same circuit as you'll find in high speed SRAM today - the bistable flip flop - but done with valves and hope rather than by etching an arcane pattern on a bit of silicon.


### Subroutines and code reuse

Turing seems to have invented the idea of the subroutine. Admittedly it's implicit in his implementation of a Universal Turing machine in <cite>On Computable Numbers...</cite>, but it's explicitly described here. And, rather wonderfully, the pipedream of extensive code reuse is there in the computer science literature right from the start:

> The instructions for the job would therefore consist of a considerable number taken off the shelf together with a few made up specially for the job in question.

There are several moments when reading the paper where I found myself thinking "Hang on, he means that literally rather than figuratively doesn't he?" and this is one of them. When your code is embodied in punched Hollerith cards, a library is just that. Row upon row of shelves carefully indexed with reusable code stacked on them like so many books.

Elsewhere he says:

> It will be seen that the possibilities as to what one may do are immense. One of our difficulties will be the maintainence of an appropriate discipline, so that we do not lose track of what we are doing. _We shall need a number of efficient librarian types to keep us in order._

That's my emphasis, and ain't _that_ the truth? I'm not sure that Turing would have foreseen that the nearest thing we have to 'a number of efficient librarian types' would turn out to be Google's computers though. One wonders whether he'd be horrified or delighted.

### Descrimination

Here he is, having painstakingly explained how the use of loops can reduce the size of a program: 

> It looks however as if we were in danger of getting stuck in this cycle and unable to get out. The solution of this difficulty involves another tactical idea, that of 'descrimination'. ie. of deciding what to do next partly according to the results of the machine itself instead of according to data available to the programmer.

And there we have the nub of what makes computing so powerful and unpredictable. The behaviour of any program worth writing isn't necessarily what you expect because it's making decisions based on things you didn't already know (if you already knew them, you wouldn't have to compute them in the first place). This is why I'm optimistic about AI in the long run. I think that given that the behaviour of a single neuron is understandable and simulatable then, eventually we'll manage to connect up enough virtual neurons and sensors that the emergent behaviour of those simulated neurons is as near to a 'real' consciousness as makes no odds. I'm far less convinced that we're ever going to be able to upload our brains to silicon (or whatever the preferred computing substrate is by then). Whether we'll able to communicate with such a consciousness is another question entirely, mind.

### Job Security Code

> The masters are liable to get replaced because as soon as any technique becomes at all stereotyped it become possible to devise a ssystem of instruction tables which will enable the electronic computer to do it for itself. It may happen however that the masters will refuse to do this. They may be unwilling ot let their jobs be stolen from them in this way. In that case they would surround the whole of their work with mystery and make excuses, couched in well chosen gibberish, whenever any dangerous suggestions were made

Oh, did Turing nail it here. 1947 and he's already foreseen 'job security' code. I've seen this kind of behaviour all the time and it drives me up the wall. What the peddlars of well chosen gibberish always fail to see that, if you get it right, the computer ends up doing the _boring_ parts of your work for you. And your time is then free to be spent on more interesting areas of the problem domain. Software is never finished, it's always in a process of becoming. There's a never ending supply of new problems and a small talent pool of people able to solve them; if you're worth what you're paid today then you'll be worth it again tomorrow, no matter how much you've delegated today's work to the computer. And tomorrow's work will be more interesting too.

Automating the shitwork is what computers are _for._ It's why I hate the thought of being stuck writing code with an editor that I can't program. Why I love Perl projects like Moose and Moo. Why I'll spend half a day trawling [metacpan.org](http://metacpan.org/) looking to see if the work has already been done (or mostly done - an 80/20 solution gets me to 'interesting' so much quicker). 

Job security code makes me so bloody angry. There are precious few of us developers and so much work to be done. And we piss our time away on drudgery when we simply don't have to. We have at our fingertips the most powerful and flexible tool that humanity has ever built, and we use it like a slide rule. Programming is hard. It demands creativity and discipline. It demands the ability to dig down until we really understand the problem domain and what our users and customers are trying to do and to communicate the tradeoffs that are there to be made - users don't necessarily understand what's hard, but they're even less likely to understand what's easy. But its very difficulty is what makes it so rewarding. It's hard to beat the satisfaction of seeing a way to simplify a pile of repetitive code, or a neat way to carve a clean bit of testable behaviour off a ball of mud. Sure, the insight might entail a bunch of niggly code clean up to get things working the new way, but that's the kind of drudgery I can live with. What I can't stand is the equivalent of washing the bloody floor. Again. And again. And again. I'd rather be arguing with misogynists - at least there I might have a chance of changing something.

I'm not scared that I'm going to program myself out of a job. I'm more worried that I'm never going to be able to retire because as a society and a profession we're doing such a monumentally piss poor job of educating the next generation of programmers and some of us seem to be doing a bang up job of being unthinkingly hostile to the 50% of the talent pool who are blessed with two X chromosomes. But that's probably a rant for another day.

[wpACE]: http://en.wikipedia.org/wiki/Automatic_Computing_Engine
[wppilotACE]: http://en.wikipedia.org/wiki/Pilot_ACE
[wpManchesterBaby]: http://en.wikipedia.org/wiki/Manchester_Small-Scale_Experimental_Machine


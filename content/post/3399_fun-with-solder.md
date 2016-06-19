+++
title = "Fun with solder"
slug = "fun-with-solder"
date = "2013-03-13T22:49:00+00:00"
draft = false

+++

Where were we? Ah yes, I'd just unwired my Maltron, pulled out all the switches, ordered some Cherry MX brown stem keyswitches from a Deskthority [Group buy][mxgroupbuy] and a [Teensy++][teensy] from [Pieter Floris][floris]. Now all I had to do was work out how I was going to wire the thing up. Jesse's [article](http://blog.fsck.com/2012/12/building-a-keyboard-part-1.html) had some great pointers, but as I disassembled the Maltron wiring loom, I gained a great deal of respect for their decision to use fine enamelled wire (which a bit of googling revealed to be solderable copper magnet winding wire - I bought some 30SWG stuff from [wires.co.uk](http://wires.co.uk)) which, because it's thin and solid core is easy to bend into shape and, because the enamel coating melts into solder flux, is easy to solder without worrying about stripping insulation.

One thing that worried me about both Jesse and Maltron's wiring was the fiddly nature of the way the wired the diodes in. The Maltron wiring only had diodes on a few keys, but I was looking to experiment with some serious remapping and possibly chording layouts - hardwiring a limited set of modifier keys wasn't in my plan. 

### Diodes? Why diodes?

Before I talk about how I solved that problem, I'd best explain what the problem is. Consider the average computer keyboard with 105 or so keys. How do you work out which keys have been pressed without needing 105 I/O pins on your microcontroller? You arrange things in a matrix. We'll worry about the physical layout of the board later, but here's a schematic of a 5x5 key matrix which, with a bit of cunning, allows us to read which one of 25 keys is pressed with only 10 microcontroller pins

<a href="http://www.flickr.com/photos/pdcawley/8553198322/" title="A 5x5 simple keyboard matrix by pdcawley, on Flickr"><img src="http://farm9.staticflickr.com/8375/8553198322_88d24b6a8a.jpg" width="500" height="442" alt="A 5x5 simple keyboard matrix"></a>

Suppose we apply a signal to the first column then, by looking at the pins attached to the rows, we can tell which switches in the selected column have been pressed by checking for the signal. 

<a href="http://www.flickr.com/photos/pdcawley/8555988646/" title="S1-S13-Down-C1 by pdcawley, on Flickr"><img src="http://farm9.staticflickr.com/8243/8555988646_6da76315c0.jpg" width="500" height="435" alt="S1-S13-Down-C1"></a>

By cycling the symbol from column to column rapidly, we can scan the whole matrix.

<a href="http://www.flickr.com/photos/pdcawley/8555987578/" title="S1,-S13-Down-C3 by pdcawley, on Flickr"><img src="http://farm9.staticflickr.com/8238/8555987578_1578a99b55.jpg" width="500" height="435" alt="S1,-S13-Down-C3"></a>

When two keys are pressed at the same time, we can spot them with this arrangement, but what happens when, say, three keys are pressed? Let's press switches S1, S11 and S13 and find out. When we scan column one, all is well, we get the signal out on rows 1 and 3 as we expected, but when we scan column 3, which only has one key held down, we get a signal out on rows 1 and 3 as well. What's going on?

<a href="http://www.flickr.com/photos/pdcawley/8555989736/" title="S1-S11-S13-Down-C3 by pdcawley, on Flickr"><img src="http://farm9.staticflickr.com/8092/8555989736_220c9d60eb.jpg" width="500" height="435" alt="S1-S11-S13-Down-C3"></a>

Let's trace the signal and see if we can work out how it gets to row one. The signal comes in on column 3, through S13 and onto row 3. But S11 is also on row 3, which means that the signal can flow through S11 onto column 1 and once it's on column 1, it flows through S1, onto row 1 and confusion reigns.

Welcome to the wonderful world of ghosting.

If you're happy to live with only detecting when two keys are pressed simultaneously, you can correct for this in software by ignoring ambigous symbols (or by ignoring all signals which show more than two keys if you're feeling lazy) and you can correct things for WASD gamers with some careful matrix layout to make sure that most of the common 3+ key combos aren't ambiguous. Or you can spend a quid on a hundred 1N4148 diodes and invest some extra soldering time to wire things up like so:

<< Signal: C3. Closed: C1R1, C1R3, C3R1. All Diodes >> 

With the diodes in place, the signal can't go back through S1 and confuse things and your controller software can be much simpler. 

The original Maltron wiring doesn't put diodes on every key, just on all the modifier keys. Diodes may be cheap, but getting them wired into the matrix in the way Maltron did is a complete pain, doing it for over 100 keys was never going to be fun. But dammit, diodes on every key is just the Right Thing. There had to be a better way.

### Diodes without (much) pain

If you've ever spent time soldering stuff, you'll be aware that humans have a hard time soldering due to an acute shortage of hands. Generally you need to hold one component in one hand, another component in the other, the soldering iron in your other other hand and the solder wire in the... hmm... we appear to have run out of hands. Which is where things like vises come in handy. That way you can hold one component in the vise, lock the wire or component you're trying to attach to it by twisting, wrapping or beinding things, leaving you with hands free for the iron and solder. Wiring diodes up to the keyswitches, which have to be installed in the keyboard case while you're doing it, is the very definition of fiddly. 

However, if you go and read things like the MX keyswitch's datasheet, it makes reference to switches with diodes fitted and when you look at the bottom of a switch you'll see a diode symbol and four small holes.

<a href="http://www.flickr.com/photos/pdcawley/8542264267/" title="Cherry MX keyswitch by pdcawley, on Flickr"><img src="http://farm9.staticflickr.com/8532/8542264267_12237fbb36.jpg" width="500" height="445" alt="Cherry MX keyswitch"></a>

Time to crack a switch open

<a href="http://www.flickr.com/photos/pdcawley/8542869979/" title="Inside the switch by pdcawley, on Flickr"><img src="http://farm9.staticflickr.com/8389/8542869979_9f1ec3c7a2.jpg" width="500" height="465" alt="Inside the switch"></a>

There's quite a bit going on in there, but right where the four holes are is the interesting bit. We can bend the legs of a 1N4148 diode and feed 'em through the holes, pop the lid back on and it fits, clean as a whistle. We're onto something here.

I'd decided to go with a reduced layout based on the ['blue shift'][blueshift] layout that Jesse cooked up so, although it was fiddly it didn't take that long to pop another 59 keys and put diodes in there. After that, it was easy enough to wrap the anode lead around one of the switch's pins, solder it in place and clip off the excess wire:

<a href="http://www.flickr.com/photos/pdcawley/8542237927/" title="Steps along the way by pdcawley, on Flickr"><img src="http://farm9.staticflickr.com/8226/8542237927_7af6302b84.jpg" width="500" height="375" alt="Steps along the way"></a>

Now I had 60 keyswitches with diodes installed which I could pop into the keyboard case and wire up with magnet wire. I love solderable magnet wire. It's magic stuff. Just wrap it tight round a pin, or the cathode lead of the diode and it stays in place while you solder it in place. I'm not going to pretend it was the work of moments, but it was pretty straightforward and I haven't needed to resolder a single joint. There's something very satisfying about watching capillary action suck molten solder into the joint. Physics is awesome.

<a href="http://www.flickr.com/photos/pdcawley/8543338912/" title="The first half-row installed and partially wired by pdcawley, on Flickr"><img src="http://farm9.staticflickr.com/8234/8543338912_9a98ca177f.jpg" width="500" height="375" alt="The first half-row installed and partially wired"></a>

<a href="http://www.flickr.com/photos/pdcawley/8542240979/" title="Look! I made a rats' nest of my own by pdcawley, on Flickr"><img src="http://farm9.staticflickr.com/8382/8542240979_68186a386f.jpg" width="500" height="375" alt="Look! I made a rats' nest of my own"></a>

<a href="http://www.flickr.com/photos/pdcawley/8543339934/" title="Ribbon cable FTW! by pdcawley, on Flickr"><img src="http://farm9.staticflickr.com/8532/8543339934_6029472c9e.jpg" width="500" height="375" alt="Ribbon cable FTW!"></a>

<a href="http://www.flickr.com/photos/pdcawley/8543339470/" title="A working Maltronstein by pdcawley, on Flickr"><img src="http://farm9.staticflickr.com/8239/8543339470_e746943b2d.jpg" width="500" height="375" alt="A working Maltronstein"></a>

Tune in next time to really learn about the importance of the pull-up resistor, how to roll your own (or someone else's) keyboard firmware and some thoughts on where next.

[teensy]: http://www.pjrc.com/teensy/index.html "Tiny, Arduino compatible, microcontrollers"
[floris]: http://floris.cc/
[blueshift]: http://blog.fsck.com/2013/01/pinkies-and-your-brain.html
[mxgroupbuy]: http://deskthority.net/marketplace-f11/cherry-mx-taking-pre-orders-t2760.html


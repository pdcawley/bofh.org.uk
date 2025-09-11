+++
title = "In which Piers prepares to void the warranty..."
slug = "in-which-piers-prepares-to-void-the-warranty"
date = "2013-03-10T20:56:00+00:00"
draft = false

+++

Some years ago (I have the awful feeling it was 1999) I was stricken with a
bout of tingly numbness in my right hand. When you're a computer programmer, the thought of being unable to type, and thus unable to program isn't something you ever want to deal with. Terry Pratchett's words about gnawing the arse out of a dead badger if it would make it better spring to mind. So, I replaced my mouse with a trackball, got a better chair and invested three hundred and some pounds of my own money in a [Maltron][maltron] keyboard.

The Maltron design makes the Microsoft Natural split keyboard look like an achingly conservative piece of peripheral design (though the original MS natural keyboard had a very nice feature that the Maltron lacks in its reverse tilt). It looks like something from a science fiction film <span class="aside">(and maltron keyboards have been used as props in sf movies)</span> and once I'd got used to it, it was marvellous. Comfortable. Fast to type on. [Weirdly laid out][maltlayout]. 

But... slightly flimsy in feel. It's understandable really, the keyboards sell in such low volumes that each one has the feel of a 'production prototype'. Which is great when you're a programmer like me and want the layout tweaked slightly from the basic spec - Maltron were more than happy to wire things up so that, where their 'standard' model had a caps lock key, there was a control key, or to make a normally 'dead' key into an extra quote/double quote. After all, when you're forming a sheet of plastic over a plaster of paris mold, cutting holes in it with a punch and then wiring every keyswitch into the matrix by hand, there's not much more work in rerouting a couple of wires here and there, so at the cost of a slightly longer wait I got a customized keyboard.

Time went by, the tingles went away, I started coding on laptops, and Macbooks at that. Laptops that had neither AT or PS/2 keyboard interfaces, only USB. And all those keyboards that let you plug your PS/2 plug into a simple USB adaptor do it by having USB hardware onboard, they detect when they've been plugged into a USB socket and switch protocols. Not so much with keyboards made in the 20th century, so the Maltron got put away in its box.

When I was working for the BBC, the tingles came back, so I pulled the Maltron out of storage, hooked it up and discovered that the time in storage hadn't been kind to it. But the BBC is the kind of employer that really doesn't like the thought of an expensive programmer not being able to do his job and arranged for me to have a [Kinesis Advantage][kinesis].

The Kinesis is a problematic keyboard for us Maltron users. It's what the Maltron could have been, given the kind of investment and volume sales that small UK companies don't get. You only have to look at one to realise that its heavily inspired by the Maltron. Putting partisan issues to one side, the Kinesis is a more solid keyboard than the Maltron. When you open one up it's obvious why the Kinesis manages to feel solid and cost less. There's a flexible PCB in there, fewer 'real' keys and a heavier, more rigid, injection molded, case. The initial setup and tooling costs must've been phenomenal by comparison to the Maltron way of building, but once those are accounted for, the cost per unit is pulled way down. And Kinesis had a second mover advantage, they're not building a keyboard from scratch, they're developing an already existing design. And developing it very well. If you look at the 'bowl' of a Maltron keyboard, each column has its own radius, the angle of each key is tuned in the case, so the case shape is complex and presumably hard to make with injection molding. The Kinesis case shape is rather simpler; the 'middle' finger column has greater radius than the others, but apart from that the bowl is basically cylindrical. However, when you use the advantage each key feels as right as it does on a Maltron. Kinesis do this by altering the profile of the _keycaps_. If you pull the all the keycaps in a bowl off their stems and put 'em on a flat surface, you'll see that no two have the same shape. Developing that profile can't have been a simple process, but again, it's much easier to cast one hundred or so keycaps in different shapes than a whole case in a more complex shape. After all, if a keycap goes wrong when its cast, you can just dump that keycap and grab another one off the pile. If a case goes wrong, you've got rather more plastic to melt down.

So... once I'd remapped the Kinesis to use the Maltron layout (It may have been years since I'd used the Maltron, but fingers don't forget - when I'm typing on a flat keyboard, my fingers sit over ASDFJKL; but whey I'm typing on a contoured keyboard, they're mapped to ANISTHOR and I find typing in QWERTY mode nearly impossible; I'm reduced to hunt and peck.) I was very happy with it, and with the help it gave to my posture, I was soon typing comfortably again.

And then I left the Beeb and came to Cornwall. And, bless them, the Beeb held on to 'my' keyboard (I hope whoever it ended up with worked out how to remap it back to QWERTY). But, I wasn't hurting, so a flat keyboard was fine.

Until it wasn't and the tingles. And good as [Headforwards][HF] are as employers, they don't really have the kind of budget that the BBC has, so I've ended up buying my own Kinesis for work - if nothing else, I'm not going to have the problem of leaving my keyboard behind if I have to change jobs. 

But I'm a geek. I don't just code at work, I code and write at home. It's all very well having a lovely ergonomic keyboard at work and a painful flat one at home. So I dusted off the Maltron again, picked up an active USB protocol converter and discovered that time had definitely not been kind. Keys were stuck. Or not registering. The Shift Lock key wasn't like every other key, when you pressed it, the controller held odwn a virtual shift key until you pressed it again. And, because this was hard wired in a control software blown into an EPROM there was no way I could remap it. 

I was about to give up on getting a contoured keyboard up and working at home and resign myself to carrying the Kinesis back and forth from the office, but then my friend Nat Torkington's [Four Short Links][fourshort] post pointed me at my friend Jesse's fantastic [pair](http://blog.fsck.com/2012/12/building-a-keyboard-part-1.html) of blog [posts](http://blog.fsck.com/2012/12/building-a-keyboard-part-2.html) about building a flat, Maltron/Kinesis inspired keyboard from scratch and I was off to the on-line shops in search of [Teensy++][teensy] microcontrollers, diodes, soldering irons and new Cherry MX brown keyswitches; reading obssessive webfora like [GeekHack](http://geekhack.org) and [Deskthority](http://deskthority.net); and unscrewing the baseplate of my painfully expensive hand built custom keyboard and enthusiastically voiding the warranty with the aid of a pair of diagonal cutters.

<a href="http://www.flickr.com/photos/pdcawley/8542231487/" title="An almost disassembled keyboard matrix"><img src="http://farm9.staticflickr.com/8375/8542231487_a65b524970.jpg" width="500" height="375" alt="An almost disassembled keyboard matrix"></a>

<a href="http://www.flickr.com/photos/pdcawley/8542232041/" title="The photographer realises..."><img src="http://farm9.staticflickr.com/8388/8542232041_d8b3d69bba.jpg" width="500" height="375" alt="Untitled"></a>

<a href="http://www.flickr.com/photos/pdcawley/8542232459/" title="... he should've pulled out the camera earlier"><img src="http://farm9.staticflickr.com/8529/8542232459_59554fe3ed.jpg" width="500" height="375" alt="Untitled"></a>

Tune in next time to learn how a keyboard works, why magnet wire is lovely and the importance of the pull-up resistor.

[teensy]: http://www.pjrc.com/teensy/index.html "Tiny, Arduino compatible, microcontrollers"
[fourshort]: http://radar.oreilly.com/nat "Nat's O'Reilly Radar blog"
[HF]: http://www.headforwards.com/
[kinesis]: http://www.kinesis-ergo.com/advantage.htm
[maltron]: http://www.maltron.com/keyboard-info/dual-hand-fully-ergonomic-3d-keyboards.html
[maltlayout]: http://en.wikipedia.org/wiki/Maltron#Layouts


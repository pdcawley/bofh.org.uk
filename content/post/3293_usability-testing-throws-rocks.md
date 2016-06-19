+++
title = "Usability testing (throws) rocks"
slug = "usability-testing-throws-rocks"
date = "2008-07-03T04:55:00+00:00"
draft = false

+++

Usability testing is wonderful. But wow, its humiliating.

I've spent the last few weeks working on the [Amazingtunes](http://www.amazingtunes.com) in page player. Amazingtunes is a music site, so we need to play music. However, we don't like the way that most music sites work; either the music stops as you go from one page to another, or the player is a huge Flash app running in its own window. There has to be a better way. There needs to be a popup window if you want to eliminate stop/start behaviour, but there's surely no reason not to keep the controls on the main page.

So, we set about writing somthing that did just that. We settled on using Jeroen Wijering's excellent [flvPlayer](http://www.jeroenwijering.com/?item=JW_FLV_Player), which handles the media formats we need and has good Javascript control and communications. This sits in the child window and we use Javascript cross-window communication to have a player controller in the main window that looks something like:

<div class="thumbnail">
<a href="http://skitch.com/pdcawley/xy5m/piers-cawley-on-amazingtunes.com"><img src="http://img.skitch.com/20080703-58ianuwrptmeysscfkse3bm9i.preview.jpg" alt="Piers Cawley on amazingtunes.com" /></a><br /><span style="font-family: Lucida Grande, Trebuchet, sans-serif, Helvetica, Arial; font-size: 10px; color: #808080">Uploaded with <a href="http://plasq.com/">plasq</a>'s <a href="http://skitch.com">Skitch</a>!</span>

</div>
This is all done in HTML and and Javascript, the progress bar does the Safari trick of running behind the tune data links, the buttons do their AJAX magic and the whole thing is rather slick, though I say so myself.

At least, we thought it was slick until we pointed the [usertesting.com](http://www.usertesting.com/) legions at it. Without exception, they ignore the in page player, foreground the popup and use the teeny weeny controls on the flash player. Originally, the popup window didn't even display any transport controls, it just had a picture of some speakers and some text asking the user not to close it because it was playing the tunes. We added transport controls as a stopgap while we made the in page player work properly.

I sound like I'm whinging don't I? It's certainly a blow to the ego to see something we spent so much time and attention on being ignored by our sample users. On at least one occasion, while watching the screencasts I found myself boggling at the things the users did, and if I didn't shout "Just play some bloody music!" at the screen, then I came worryingly close.

It would be easy to retreat into a state of denial: "They're not our target users! They're stupid! They're American! If they would only magically intuit the way we think they should use the site!". And maybe it would be comforting to do so, for a while. The right thing to do is to suck it up - take away from those videos the sure and certain knowledge that bits of the site don't work and do something about it.

We may dislike the 'popup window for transport controls' model of controlling music playback, but users are cool with it. And it's not as if the work we did on making the in page player work is going to be wasted - widget is straightforwardly event driven so it'll work just as well in the popup window, and the communication protocol will be much simpler. Having the player in its own window means we'll be able to extend its interface in ways that would be hard when the player had to share window space with the rest of the page. In the end, it's all good.

But... damn that in page player was sweet. I learned Javascript as I wrote it (mostly by pretending it was Perl with odd syntas) and I'm bloody proud of it. I'll happily replace it with the next iteration (which I'm already working on), but it'll be with a pang of remorse all the same.

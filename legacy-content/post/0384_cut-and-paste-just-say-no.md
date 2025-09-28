+++
title = "Cut and Paste? Just say no!"
slug = "cut-and-paste-just-say-no"
date = "2005-12-11T16:57:00+00:00"
draft = false

+++

Today has been humbling. I had made some changes to the Typo database schema with the goal of making the code easier to follow and because, dammit, it's the Right Thing to do.

First, I updated the `published` flag, and everything went well.

Then, I decided to kill two birds with one stone, and update `allow_pings` and `allow_comments` at once. So, I copied the migration and simply called the method that I'd written to change a column type to boolean a couple of times in one migration. And it was fine.

On my, mysql based, setup.

But it was far from fine on Postgres based Typo installations.

S, I made some changes that involved ripping the body out of the method I wrote and sticking it inline in the body of the migration... There followed a comedy of errors of embarrassing proportions. I think I'm just about to apply the 4th or 5th patch to fix a problem caused by cutting and pasting code and then not making all the small changes required to make the code that worked in one place work in its new home.

I've done more damage thrashing around, trying to fix this issue than I did releasing the broken migration in the first place. At least the broken migration fell over in a recoverable way. Meanwhile, the version that I'm about to patch once [Adam 'My personal saviour' Greenfield](http://www.adamgreenfield.com/) has tested it one more time and posted to the <a href="http://typo.leetsoft.com/">Typo trac</a> simply and silently throws important data away.

Lesson learned: When you've done something stupid, it's a very good idea to stop, think and test in case you do something even more stupid in an attempt to fix the first stupidity. Which I knew before. I just didn't *know* it.

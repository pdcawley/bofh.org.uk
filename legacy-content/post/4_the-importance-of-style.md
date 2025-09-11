+++
title = "The Importance of Style"
slug = "the-importance-of-style"
date = "2003-07-18T16:13:00+00:00"
draft = false

+++

I was talking to <a href="http://use.perl.org/~gav/journal/">Gavin Estey</a> on iChat about the problems inherent in interviewing a new programmer. The cost of screwing up can be enormous. How do you find out whether the candidate is for real? How do you do it quickly?

Well, those are sticky questions, and there's a discussion of Perl certification and standards coming up once I've marshalled my thoughts properly.

Anyway, Gavin showed me one of the quiz questions they used in his organization:

What's wrong with the following code?

<code language="Perl">

    open FILE, "<$filename";
    print FILE '$parameter1, ';
    print FILE '$parameter2, ';
    print FILE '$parameter3\n';
    close FILE;

</code>

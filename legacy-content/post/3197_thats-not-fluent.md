+++
title = "That's not fluent..."
slug = "thats-not-fluent"
date = "2007-03-15T18:33:00+00:00"
draft = false

+++

So, I'm not a fan of static typing. It's okay in the likes of Haskell which does type inferencing and generally goes out of its way to reduce programmer pain, but Java? C\#? No ta. It's awfully tempting to conclude that anyone who chooses to use those languages deserves to be pointed out and laughed at.

It's especially hard to resist that temptation, when a C\# blogger plays right into my hands by describing the following code:

<code>

    Pattern findGamesPattern = Pattern.With.Literal(@"");

</code>
as "a very nice regular expression wrapper which allows you to define a regex using a readable syntax exposed via a very elegant fluent interface". It seemed so self-evidently silly that I read back in his blog to see if he was taking the piss. Depressingly, he appears to be serious. Ah well, I shall at least resist the temptation of supplying his name and URL.

Worse, he's using this regular expression (Rendered in Perl's `/x` style, which ignores whitespace and allows comments in the body of the regex)

<code>

    qr/
       
      /msx


    to parse XML (one of the canonical no nos that one). Badly. For instance, it will match this drivel:


</code>
but it won't match this perfectly valid xml:

<code>


      stuff
      

</code>
On reflection, that might not be matched for a couple of reasons - it depends whether `Anything` includes `"\n"`.

All of which is beside my main point. The sheer wordiness of the 'fluent' regex wrapper serves merely to obfuscate the intent of the pattern. You are tempted to conclude that, if it has that many words in it, it *must* be correct. This isn't so much fluent as effluent.

Meanwhile, in that bastion of 'line noise' that is Perl, Damian Conway's <a href="amazon:0596001738">Perl Best Practices</a> recommends writing all but the most trivial of regular expressions using the a standard set of switches (`msx`) and using whitespace to break the pattern up into logical chunks with comments where necessary. Given that almost everyone uses PCRE nowadays, you can follow the same good practice in your language of choice.

Yes, the regular expression language is terse. Yes it can be opaque until you take the time to learn its grammar. But there aren't that many rules to learn. The language isn't complex, only some of the [things](http://perl.abigail.be/Talks/Sudoku/HTML/title.html) that [people](http://www.flickr.com/photos/pdcawley/54445114/) [use](http://perl.abigail.be/Talks/RE/00_primes.html) it [for](http://search.cpan.org/~abigail/Regexp-Common-2.120/lib/Regexp/Common.pm).

### What is fluency then?

An interface isn't fluent because it's wordy. Fluency is about writing message protocols that make it easy for the user to solve her problem and clearly express her intent *in the same bit of code*. Frankly, plain old regular expressions are a damned sight more fluent in all their terseness than the above bad joke.

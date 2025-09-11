+++
title = "Comprehensible sorting in Ruby"
slug = "comprehensible-sorting-in-ruby"
date = "2007-12-16T03:00:51+00:00"
draft = false

+++

Here's a problem I first came across when I was about 13 and helping do the stock check at the family firm. The parts department kept all their various spare parts racks of parts bins. Each bin was 'numbered' with an alphanumeric id. We had printouts of all the bin numbers along with their expected contents and we'd go along the racks counting the bins' contents and checking them off against the print out. What confused me at the time was the way the printouts were organized. Instead of the obvious ordering, "A1, A2, A3, ..., A99", the lists were ordered like "A1, A10, A11, ..., A2, A20, A21, ...". After a bit of thought I realised that the computer was sorting the numeric bits of the bin numbers as if they were just sequences of strange letters. A bit more thought made me realise why, post computerisation, people were starting to use bin numbers like "A01, A02, ...". Computers were more important than people so, in order to make sorting things easier, just add spurious leading 0s to make the number field a fixed width and Robert's your parent's brother.

27 years later and computers are still crap at sorting things in a sensible fashion. Back before Moore's Law was really kicking in, I suppose it was excusable, but surely we've moved past that now.

Over on the [labnotes](http://blog.labnotes.org/2007/12/13/rounded-corners-173-beautiful-code/) blog, there's an example of some ruby code that attempts to do 'human' sorting:

<code>

<pre>
module Enumerable
def sensible\_sort
sort\_by { |key| key.split(/(\\d+)/).map { |v| v =~ /\\d/ ? v.to\_i : v } }
end
end
</code>

It's okay, as far as it goes. It certainly solves the parts bin problem I outlined above, but it's not ideal. For example, you might expect `['-1', '1', '1.02', '1.1'].sensible_sort` to leave the order unchanged, but what you actually get is '1, 1.02, 1.1, -1'. Not ideal. Let's rewrite sensible sort as

<code>

<pre>
module Enumerable
def sensible\_sort
sort\_by {|k| k.split(/(\[~~*\]?\\d*(?:\\.\\d+)?(?:\[~~*\]?\[eE\]\\d*)?)/).map {|v| Float(v) rescue v}}
end
end
</code>

That ugly regular expression should match a far wider selection of string representations of numbers. Certainly our 'bad' list is now sorted correctly.

But what about "a-1", "a-2". Using the implementation above, they'd get sorted as "a-2, a-1", which can't be right, can it? Let's extend it a bit more and make sure we only worry about the '+' and '-' if they're at the beginning of a line or preceded by whitespace.

<code>

<pre>
module Enumerable
def sensible\_sort
sort\_by {|k| k.to\_s.split(/((?:(?:^|\\s)\[-*\])?\\d*(?:\\.\\d+)?(?:\[eE\]\\d+)?)/ms).map {|v| Float(v) rescue v}}
end
end
</code>

And that works fine, until you find that "B" sorts before "a". Let's catch that as well:

<code>

<pre>
module Enumerable
def sensible\_sort
sort\_by {|k| k.to\_s.split(/((?:(?:^|\\s)\[-*\])?\\d*(?:\\.\\d+)?(?:\[eE\]\\d+)?)/ms).map {|v| Float(v) rescue v.downcase}}
end
end
</code>

Yay!

Oh, wait a minute, what about version numbers? How should we sort, say "perl 5.8.0" and "perl 5.10.0"? The 5.8.0 form should definitely come first... Hmm...

How about

<code>

<pre>
module Enumerable
def sensible\_sort
sort\_by {|k| k.to\_s.split(/((?:(?:^|\\s)\[-*\])?\\d*(?:\\.\\d+?(?:\[eE\]\\d+)?(?:$|(?!\[eE\\.\])))?)/ms).map {|v| Float(v) rescue v.downcase}}
end
end
</code>

### How far down does this thing go?

I just noticed that ".1" sorts after "1". Time for another tweak...

<code>

<pre>
module Enumerable
def sensible\_sort
sort\_by {|k| k.to\_s.split(/((?:(?:^|\\s)\[-*\])?(?:\\.\\d*|\\d+(?:\\.\\d+?(?:\[eE\]\\d+)?(?:$|(?!\[eE\\.\])))?))/ms).map {|v| Float(v) rescue v.downcase}}
end
end
</code>

but that doesn't work with version numbers like ".8.2", ".10.2"...

#### Time passes... Thorin sits down and sings about gold

I was planning on giving an extension of the regex that caught this issue as well, but I'm afraid I've stumped myself - I can't do it with a single regular expression unless I can use a fixed width lookbehind assertion, but they're only available in Perl. Of course, it's still possible to fix it, but doing so will take more thought than I have available to me at this time on a Sunday morning. And all this is before we get onto making sure that "1/2" sorts between "0" and "1". And phone numbers. After all, "01915551238" is 'obviously' the same as "0191 555 1238" and "0191 555-1238", so they should end up next to each other in the sorted list.

It looks like this is a 'three pipe problem' after all. I shall probably return to this...

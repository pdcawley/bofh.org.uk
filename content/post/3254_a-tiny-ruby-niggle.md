+++
title = "A tiny ruby niggle"
slug = "a-tiny-ruby-niggle"
date = "2007-09-09T02:51:00+00:00"
draft = false

+++

You know what? I'm starting to miss compulsory semicolons as statement terminators in Ruby.

"What?" I hear you say. "But not needing semicolons is one of Ruby's cardinal virtues! Are you mad?"

I don't think so, but maybe you'll disagree after I explain further.

Here's a piece of code that I might write if semicolons were the only way of terminating a statement:

<code>

<pre>
Category.should\_receive(:find\_by\_permalink)
.with('foo')
.and\_return(mock\_category);
</code>

Or how about a complex find query

<code>

<pre>
def find\_tags\_for(tag\_maker, order = 'count')
klass = tag\_maker.class
find :all
, :select =&gt; 'tags.\*, count(tags.id) count'
, :group =&gt; Tag.sql\_grouping
, :joins =&gt;
"LEFT JOIN taggings ON "
+ " tags.id = taggings.tag\_id "
+ "LEFT JOIN bookmarks ON "
+ " bookmarks.id = taggings.taggable\_id "
+ " AND taggings.taggable\_type = 'Bookmark' "
+ "LEFT JOIN \#{klass.table\_name} ON "
+ " \#{klass.table\_name}.id = bookmarks.\#{klass.to\_s.underscore}\_id"
, :conditions =&gt; conditions\_for(tag\_holder)
, :order =&gt; (order == 'count')
? 'count(tags.id) desc, tags.name'
: "tags.name"
, :readonly =&gt; true
;
end
</code>

I first came across the idea of the leading comma in Damian Conway's excellent [Perl Best Practices](amazon:0596001738). The idea is that, by leading with the comma it's very easy to add a new argument to an argument list or hash specification without having to remember to stick a comma on the end of the preceding line if it was at the end, and also, the leading comma makes it very plain that the line is a continuation of its predecessor in some way.

To make the examples work in Ruby, you have to add a `\` to the end of each line that has a continuation, so the first example has to be written:

<code>

<pre>
Category.should\_receive(:find\_by\_permalink) \\
.with('foo') \\
.and\_return(mock\_category);
</code>

Lining up the <code>\\</code>s helps to stop them disappearing, but it's an awful faff.

What tends to happen (in the rails source especially) is that ruby programmers simply don't break their lines up. A quick search of the rails source finds plenty of lines more than 160 characters long.

Of course, some will argue that it doesn't matter, that the old 80 column limit is a silly hangover from the days of steam when the only way to interact with your code was through an 80 column, green phosphor terminal. They have a point. An arbitrary line limit *is* silly, and we should get over it, especially in source code. However, unless you're going to go around with every window open to its maximum width, lines *will* wrap, and they won't do it nicely, or respect the indentation conventions of your language. Long lines are murder in diffs too, finding the point of difference is so much easier when your eye doesn't have to scan an epic line.

It's a shame there's no way of forcing ruby's parser to *require* semicolons as statement terminators for those programmers like me who think that the restriction that a statement *must* end with a semicolon is worth the freedom to break lines where we like without needing to escape every line break. It's a shame too that popular tools like Textmate are so clumsy when it comes to dealing with line breaks. I would attempt to hold Emacs up as a paragon in this respect, but its Ruby mode tends to get a wee bit bemused once you start breaking lines, so that's no good.

### Domino theory

It's amazing how far reaching seemingly simple language design decisions can be isn't it? Just getting rid of the need to terminate statements with a semicolon has an enormous effect on they way code in ruby looks. I'm just not sure that they look *better*.

Maybe Smalltalk got it really right - they chose to use the most valuable syntactic character of all, the space, to denote sending a message. That freed up the `.` for use as a statement (sentence?) terminator. Then that freed up `;` for use in one of Smalltalk's most distinctive patterns - the cascade. Where a Rails programmer might write:

<code>

<pre>
form\_for(@comment) do |f|
f.input(:author)
f.input(:title)
f.input(:body)
end
</code>

A Smalltalk programmer might eliminate the need for a temporary variable by doing:

<code>

<pre>
Comment&gt;&gt;printOn: html

(html formFor: self)
input: \#author;
input: \#title;
input: \#body.
</code>

All those `input: ...` messages get sent to the result of `html formFor: self`. Once you get the hang of it, it's a really sweet bit of syntax.

Incidentally, there's been some discussion on the squeak mailing lists of a companion to the cascade, which would use a `;;` as a sort of 'pipe'. The idea is to be able to replace code like:

<code>

<pre>
((self collect: \[:each | each wordCount)
inject: 0 into: \[:total :each| total + each\])
printOn: aStream.
</code>

with

<code>

<pre>
self collect: \[:each | wordCount\]
;; inject: 0 into: \[:total :each | total + each\]
;; printOn: aStream.
</code>

(NB: Please ignore what those code snippets *do*, because that's gruesome. Concentrate on how they do it).

Nobody's quite proposed going as far as Haskell does with its Monads, which can be thought of as a magical land where the meaning of the semicolon changes according to what sort of Monad you're in. (In an IO monad for instance, the semicolon imposes an evaluation order. In some other monad, the semicolon could just as easily denote a backtracking point). Then again, there's nothing to *stop* the dedicated Smalltalker implementing something Monadish - every Smalltalk class can specify how its methods should be compiled after all...

### In conclusion...

I'm not sure I've got a real conclusion for all this. I'm mostly musing. However, I do think it's useful to think carefully about restrictions and what they free us to do as programmers. Lispers will wax lyrical about the way that their language's pared down syntax lets them do amazing things with macros. Smalltalkers will defend to the death the idea that the only way to do anything is to send messages to objects. Pythonistas love their syntactic whitespace. Haskellers love their static typing (admittedly, they have an incredibly flexible notation for expressing type that leaves most other programming languages standing).

And any English speaker with ears will know that a poem like Dylan Thomas's Do Not Go Gentle Into That Good Night gains much of its power from it's form, the villanelle, one of the most restricted forms of poetry there is. Two lines repeating through the poem and a staggering number of rhymes to find:

> Do not go gentle into that good night, <br/>
> Old age should burn and rave at close of day; <br/>
> Rage, rage against the dying of the light. <br/>
> Though wise men at their end know dark is right, <br/>
> Because their words had forked no lightning they <br/>
> Do not go gentle into that good night.<br/><br/>
> Good men, the last wave by, crying how bright <br/>
> Their frail deeds might have danced in a green bay, <br/>
> Rage, rage against the dying of the light.<br/><br/>
> Wild men who caught and sang the sun in flight, <br/>
> And learn, too late, they grieved it on its way, <br/>
> Do not go gentle into that good night.<br/><br/>
> Grave men, near death, who see with blinding sight <br/>
> Blind eyes could blaze like meteors and be gay, <br/>
> Rage, rage against the dying of the light.<br/><br/>
> And you, my father, there on the sad height, <br/>
> Curse, bless me now with your fierce tears, I pray. <br/>
> Do not go gentle into that good night. <br/>
> Rage, rage against the dying of the light.

If that's not making a virtue of a restriction, I don't know what is.

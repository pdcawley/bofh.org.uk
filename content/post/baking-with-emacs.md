+++
title = "Running a bakery on Emacs and PostgreSQL"
author = ["Piers Cawley"]
date = 2019-02-25
lastmod = 2020-05-04T00:15:21+01:00
slug = "baking-with-emacs"
draft = false
series = "Bakehouse diary"
+++

Just over a year ago now, I finally opened the bakery I'd been dreaming of for years. It's been a big change in my life, from spending all my time sat in front of a computer, to spending most of it making actual stuff. And stuff that makes people happy, at that. It's been a huge change, but I can't think of a single job change that's ever made me as happy as this one.

<!--more-->

One of the big changes that came with going pro was that suddenly I was having to work out how much stuff I needed to mix to fill the orders I needed. On the face of it, this is really simple, just work out how much dough you need, then work out what quantities to mix to make that much dough. Easy. You can do it with a pencil and paper. Or, in traditional bakers' fashion, by scrawling with your finger on a floured work bench.

And that's how I coped for a few weeks early on. But I kept making mistakes, which makes for an inconsistent product (bread is very forgiving, you have to work quite hard to make something that isn't bread, but consistency _matters_). I needed to automate.

I'd been on one of Bread Matters' "Baking for a Living" courses and as part of the course materials had received a copy of a spreadsheet that could be used to go from a list of orders to a list of ingredients to mix alongside accurate costings and other useful bits and bobs. It was great and certainly opened my eyes to the possibilities for automation of this part of the job.

And then I tried to add a new recipe.

Spreadsheets aren't my favourite computational model so maybe it was just my lack of experience with them, but adding a new recipe was like pulling teeth; lots of tedious copying, pasting and repetition of formulae. It just seemed wrong, especially as the underlying computations were so straightforward (ish). There had to be a better way.

The key insight is that a bakery formula is so cliched that it can be represented as data. Here's the formula for seedy malt loaves:

{{% table %}}
| recipe           | ingredient       | quantity |
|------------------|------------------|----------|
| Small Seedy Malt | Seedy malt dough | .61 kg   |
| Large Seedy Malt | Seedy malt dough | .92 kg   |
{{% /table %}}

Of course, that's not the full set of formulae, because it doesn't tell you how to make 'Seedy malt dough', but that's just another formula, which consists of flour, water, starter, salt and a multiseed 'soaker', where the starter and the soaker are the results of other formulae, which are (finally) made from basic ingredients{{%marginnote%}}With a certain amount of handwaving to deal with the fact that a starter is strictly made with flour, water and starter.{{%/marginnote%}}. I did consider reaching for the object oriented hammer at this point, but thought that I might be able to do everything I needed without leaving SQL. It was relatively straightforward to move the shape of the calculations in the Bread Matters spreadsheet into my database schema, the only real sticking point being the recursive nature of the formulae, but it turns out that recursive queries are a thing in modern SQL, albeit a little tricky to get absolutely right{{% marginnote %}}A few bakes went a little weird before I finally got things sorted.{{% /marginnote %}} first time.

If you're curious{{% marginnote %}} And several of you seem to be, so I wrote [another post](/2019/03/04/recursive-sql-recipes/) with a bit more detail and some sample code.{{% /marginnote %}} about the details of the schema, you can find it in my [github repo](https://github.com/pdcawley/bakehouse) for the bakery.

So now, a few days before a bake, I'd setup my `production_order` table with the orders for the bake, and run a query on the `production_list` view to find out what I needed to mix when. And all was great. Well, sort of. I had to add a bit extra onto the quantities in the initial starter mix to allow for the bits that get stuck to the bowl and lost to the final dough, and it was all very well until I wanted to bake two days in a row (a bake is a two day process from mixing the starters on a Wednesday evening, through mixing, fermenting and shaping on Thursday to baking the resulting loaves at four on Friday morning). But, vitally, it was much, much easier to add and adjust formulae, and the limitations were no worse than the limitations of the spreadsheet. All was well.

It's the nature of business that you need to keep records. How much got baked? How much sold? Did we clean the floor? Were there any accidents? What sort? How do we prevent them next time? The list is endless. It all needs to be recorded, for both legal and pragmatic reasons. So I started a day book. This is just an .org file{{% marginnote %}} Org-mode is an amazing emacs package that's a sort of outliner/task manager/publishing tool/spreadsheet/diary/literate programming environment. It's bewilderingly capable, and is probably the primary driver of the emacs renaissance as people are coming to the editor for org-mode, and porting the rest of their environment - hence the rise of `evil-mode`, the emacs vim emulation layer.{{% /marginnote %}}. Every day I come into the bakery, I run `org-capture` and I get a template for the day's entry in the daybook, which I fill in as the day goes on.

One of the features of org-mode is `org-babel`, a literate programming environment, which lets me write something like:

```org
#+begin_src sql
SELECT ingredient, quantity
  FROM bakehouse.production_list
 WHERE work_date = 'today';
#+end_src
```

and then, with the cursor somewhere in the code block, hit `C-c C-c` whereupon Emacs will run that SQL against the bakery database and populate a table like:

{{% table %}}
| ingredient  | quantity |
|-------------|----------|
| Old starter | 1.3      |
| Water       | 2.08     |
| White flour | 2.6      |
| ...         | ...      |
{{% /table %}}

If that were all org-mode did to assist, it'd be awesome enough, but the queries I make are a little more complex than that, the current version of the database understands about dates and can cope with overlapping bakes, but all that makes the queries a little more complex. Org-mode helps with that too, because I can file away snippets of code in a 'library of babel' and just reference them from the daybook. And I can set arbitrary variables at any point in the hierarchy of the document.

So I have a bit of code in my emacs config that tweaks the day's entry in a daybook like so:

```emacs-lisp
  (defun pdc//in-bakery-daybook? ()
    "Are we in the bakery daybook?"
    (equal (buffer-name) "CAPTURE-loafery-daybook.org"))

  (defun pdc/set-daybook-entry-properties ()
    "Set the properties we rely on in our boilerplated daybook queries"
    (save-excursion
      (while (not (looking-at "*+ [[:digit:]]\\{4\\}\\(-[[:digit:]]\\{2\\}\\)\\{2\\}"))
        (org-up-element))
      (let ((entry-date (first (s-split " " (org-entry-get (point) "ITEM")))))
        (org-entry-put
         (point)
         "header-args+"
         (format ":var work_date=\"'%s'\"" entry-date)))
      (org-babel-execute-subtree)))

  (defun pdc/org-capture-before-finalize-daybook-entry ()
    (when (pdc//in-bakery-daybook?)
      (pdc/set-daybook-entry-properties)))

  (add-hook 'org-capture-before-finalize-hook
            #'pdc/org-capture-before-finalize-daybook-entry)
```

It won't win any code beauty contests, but it does the job of setting a `work_date` variable for the day's entry and running any code in the subtree as part of the capture process. The capture template has lines like `#+call:mixes()`, which call the stored code snippets, that reference the variable set in the current subtree and so make the query for the right day. This means that all I have to do to know what I should be doing when I get into the bakehouse is to run an org-capture and check the resulting entry in my daybook. Provided, that is, that I've added the appropriate rows to the database.


## Next steps {#next-steps}

The software isn't done, of course, no software ever is. But it's good enough that it's been managing my mixes without a hitch for the last few months, telling me what to pack for which customer and generally removing the need to work anything out with a pencil and paper. It's nowhere near as mature or capable of commercial production management software, but it fits me. I understand what it does and why, how it does it, the limitations it has and how to work around them. When it becomes annoying enough, I might sit down and work out how to fix it, but I'll do that when I'm in the right frame of mind. My current list of niggles looks something like this:

Accounting
: The database already knows how to do costings based on raw ingredient costs etc, but I should probably be able to use it to keep my books as well, using `org-ledger`

Parametric recipes
: At a certain point, it becomes easier to mix a 'stiff starter' in my mixer than it is to just mix the usual wet starter by hand. This breakpoint comes at around 3kg of flour. Right now, I manage this by looking at the mixes for my starters and, if it looks like a lot, changing the order to use 2-stage versions of the formulae and running the query again. I think it should be possible to automate this through a more sophisticated query, but I need to work that out.

Better scheduling
: things get weird if a batch of dough would be more than I can mix in a single go. Right now there are other physical limitations that mean that I simply can't make that much bread anyway, but once I get a few more bannetons and racks, this will become a much more pressing issue.

Order management
: Right now, I manage orders through Postico talking to the database, which is okay, but a little frustrating in places. An autocompleting environment for orders within emacs would be a much neater way to manage things.


## Putting the personal in personal computing {#putting-the-personal-in-personal-computing}

Computers are amazing. They are versatile tools even if you don't know how to program them, because there's almost always an app for what you want, or something close enough that you cant work around its infelicities. It's quite remarkable the things that folks can do with their kit with no programming skill at all.

But... learn to program, and a whole other vista of possibility opens up to you. With good programmable tooling you're only really limited by your skill and understanding. Instead of accommodating yourself to your software, you can accommodate your software to you, and make the right functionality trade-offs for you. There's a brilliant commercial piece of music looping sofware I use that could be massively more brilliant if there were a way of picking up the tempo automatically from the first recorded loop - it would free me from having to sing to a click and generally make the whole process easier. The developers have other (understandable) priorities, like porting the app to windows. And they're not wrong to do so. There were folk clamoring for a windows version, and if a developer isn't making money from a commercial application, then development will stop. I'm definitely not complaining, the feature is not so dramatically necessary that I'm prepared to spend the time learning how to do real time music programming in order to implement it, but if I want software to dance to _my_ tune then doing it myself is the only way.

So... choose tools that let you program them. I choose emacs and PostgreSQL, you might choose vim and SQLite or Atom and a NoSQL database, or you might just live in your Smalltalk image. Once you start to see your computing environment as truly soft and malleable, you can do amazing things, assisted by a computer that is truly _yours_.

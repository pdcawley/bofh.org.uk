+++
date = "2016-07-29T12:29:54+01:00"
description = "Week two – SQL"
draft = false
image = "/img/bakehouse-bg.jpg"
title = "Bakehouse Diary"
series = "Bakehouse Diary"
slug = "bakehouse-diary-2"

+++

Not much progress on the "getting the oven ready front this week". I sheared another machine screw!

{{< embed "https://www.instagram.com/p/BIUhqezAacy/" />}}

At least this one was on the retaining bar that holds the oven soles in place;
I'd rather not have sheared it, but since I'm going to be drilling stuff out
anyway, it's not the end of the world. Basically I'm blocked on doing much
more with the oven 'til I've moved the decks and got them the right way up
rather than on their sides because I don't want to undo the last screw in the
retaining bar and cope with a couple of large lumps of metal and (possibly)
the insulation and heating elements falling out. Once the ovens are the right
way up, gravity will be my friend.

So, I'm going to talk about the software side of things.<!--more-->

## Pricing the product

Last year, I went up to [Bread Matters](http://www.breadmatters.com) in the
Scottish Borders and did their excellent
[Baking for a Living](http://www.breadmatters.com/baking-for-a-living-Bread%20Matters-Andrew-Whitley)
four day course. I can't recommend it highly enough; if you're remotely
interested in taking the next step in your baking and going pro, then this
will give you a solid grounding in stuff you might not have thought about.

One of the exercises was to work out the costings for a product. If you follow
the rule of thumb that's often bandied about for restaurants and catering
(multiply the ingredient costs by 3 and you've got the final price) you are
almost certain to go bust. The working formula is more along the lines of:

<div>[\begin{eqnarray*}
Price_{wholesale} & = & \sum\nolimits_{i \in \lbrace ingredients, labour, package, transport \rbrace}\frac{cost_i}{gross\_margin} \\

Price_{RRP} & = & (1 + retail\_markup) \times Price_{wholesale}
\end{eqnarray*}]</div>

With rules of thumb like `$cost_{transport} = 0.1 \times
Price_{wholesale}$`[^1] and `$cost_{labour} \approx 1.1 \times
cost_{ingredients}$`, which can be replaced with `$cost_{labour} =
 hourly\_rate \div production\_rate $` once you know how many loaves you can
make with an hour of work.[^2]

Working all this stuff out is a complete pain in the arse. It's not the sort
of thing you want to be doing every time you change a product formula, or you
negotiate a wholesale rate with your miller, or the pound goes through the
floor and suddenly you're paying 30% more for your dried fruit.

On the course, after we'd done the calculations by hand, for one product, we
were given a spreadsheet, which was great so long as you were plugging in new
numbers for ingredient costs or your hourly rate. But it's in the nature of
spreadsheets that, once you want to start adding a new product, there was an
awful lot of copying and pasting of formulae and general faffing about.

If there's one thing my time as a programmer has taught me, it's that faffing
about is the sort of thing you should really be leaving to the computer.

So, once I got back from the course (and on and off over the nine months
since), I set about converting the spreadsheets to a relational database. The
great thing about using a database for this sort of thing is that, if you set
up your tables and views correctly, you only have to type formulae like the
one for the wholesale price in once and after that the whole thing becomes
data driven.

If you're happy with absolutely zero documentation and a simple SQL command
line, then you can take a look at my progress by looking at my
[bakehouse](https://github.com/pdcawley/bakehouse) project on Github.

## Ingredients, Recipes and Products

If you have a collection of bread books, you'll find that the ones
aimed at the home baker often have a slightly different dough recipe for every
kind of loaf you might want to make. Which is fine when, in the course of the
day you'll be baking one loaf (or batch of buns or whatever). When you're
baking commercially, this doesn't really fly. You may be making 10 or more
different products and and if you can make three different products (say large
loaves, small loaves, and baps) from a single huge lump of dough, then it
makes sense to do that. And if you can carve off big lump of that dough before
the bulk ferment and add some dried fruit to so you can make teacakes as well,
then that makes sense too. In fact, if you look carefully at the proportions
in the average domestic bread cookery book, you can probably pull out a few
common doughs which are then jazzed up with other ingredients to make most of
the things in the book.

A commercial formula for a product will almost certainly be multi stage - First you take
some starter from yesterday and feed it with flour and water to make a
production leaven, then after four hours you take some of the production
leaven, flour, water and salt and make up your basic doughs (white, wholemeal,
granary) which you'll leave for their bulk ferment before scaling, shaping and
leaving to prove (overnight in the retarder unless you want to be baking at 3
in the morning) so you can come in in the morning, fire up the ovens, bake off
the buns, then the loaves, then the fancy breads and ship everything out to
your customers. Fancier products might involve more fiddling about. Stollen is
a classic 'complicated' multi-stage recipe which involves making up:

- Marzipan - made the day before
- A fruit soaker - dried fruit and booze or apple juice, made the day before to get
  the fruit good and plump.
- A 'flying ferment' of flour, milk, sugar and yeast. Because the final dough
  is quite heavily enriched and full of fruit, you want your yeast to be good
  and active. This gets made a couple of hours before mixing the dough
  properly.

Then you make up your Stollen dough with the ferment, eggs, more flour, more
milk and work in some butter too, which gives you a lovely soft rich dough.
After an hour or so, you mix in the fruit, let it relax, then scale up the
dough and the marzipan and make up your stollen, prove, bake, slather with
melted butter and try to resist scoffing all at once.

If I were simply after working out the ingredient cost per loaf, I could
represent this recipe in the database with a simple table like so:

| product | ingredient         | weight(kg) |
|---------|--------------------|------------|
| Stollen | Ground almonds     |      0.060 |
| Stollen | Caster Sugar       |      0.020 |
| Stollen | Icing Sugar        |      0.020 |
| Stollen | Whole egg          |      0.020 |
| Stollen | Sugar              |      0.005 |
| Stollen | Fresh yeast        |      0.005 |
| Stollen | Milk               |      0.060 |
| Stollen | White flour        |      0.050 |
| Stollen | Sugar              |      0.030 |
| Stollen | White flour        |      0.110 |
| Stollen | Whole egg          |      0.050 |
| Stollen | Salted butter      |      0.050 |
| Stollen | Sultanas           |      0.070 |
| Stollen | Raisins            |      0.060 |
| Stollen | Candied mixed peel |      0.050 |
| Stollen | Rum                |      0.020 |

but that throws away a bunch of information and is a surprising pain in the
arse to type. It's also very hard to look at that table and deduce anything
about what needs doing when in the stollen making process. So we write the
table a little more naturally.

### The `recipe_ingredient` table

| recipe              | ingredient          | weight(kg) |
|---------------------|---------------------|------------|
| Stollen             | Stollen Dough       |      0.560 |
| Stollen             | Marzipan            |      0.120 |
| Stollen             | Beaten Egg          |      0.010 |
| Stollen             | Butter              |      0.050 |
| Stollen             | Icing Sugar         |      0.010 |
| Stollen Dough       | Stollen Fruits      |      0.200 |
| Stollen Dough       | Festive Bread Dough |      0.360 |
| Stollen Fruits      | Sultanas            |      0.070 |
| Stollen Fruits      | Raisins             |      0.060 |
| Stollen Fruits      | Candied Mixed Peel  |      0.050 |
| Stollen Fruits      | Rum                 |      0.020 |
| Marzipan            | Ground Almonds      |      0.060 |
| ...                 | ...                 |        ... |
| Festive Bread Dough | White Flour         |      0.110 |
| Festive Bread Dough | Festive Ferment     |      0.120 |
| ...                 | ...                 |        ... |
| Festive Ferment     | Sugar               |      0.005 |
| Festive Ferment     | Fresh Yeast         |      0.005 |
| Festive Ferment     | Milk                |      0.060 |
| Festive Ferment     | White Flour         |      0.050 |


### The `product` table

If we want to calculate an accurate price, we also need to capture the scale
weight of a 'piece' of product, the number of pieces we can make per hour of
labour, and (think of batched buns, for instance) the number of pieces that go
to make up a product. This gives us a table like:

| product | pieces_per_hour | scale_weight(kg) | pieces |
|---------|-----------------|------------------|--------|
| Stollen |              16 |            0.680 |   1    |

## Breaking down the price

So, now we just need to calculate the cost of all the ingredients in a loaf
which we can use as the inputs to the formulae above. At this point, I'm just
grateful to be a programmer because the SQL query that does that calculation
is... complicated. The spreadsheet version of the calculation copes with the
arbitrarily nested nature of a recipe linking formulae together by hand (which
is part of why the spreadsheet version is hard to extend), the Postgres
version it ends up with a rather icky recursive query which calculates the
per kilo price of each product and intermediate recipe by starting with a
table of just the raw ingredient unit prices and at each iteration calculates
the unit price of all the recipes which can be made using the ingredients and
intermediates we already know the price of. If you want the gory details,
check out
[unit_cost.sql](https://github.com/pdcawley/bakehouse/blob/master/deploy/unit_cost.sql).[^3]

## Next feature

So, that's the price breakdown sorted, so now I have some numbers to plug into
a business plan and, indeed to stick on a pricelist. Huzzah. What's next?

Here's a question I want the software to answer: say I arrive at the bakehouse to
start the day's work. I've got orders for a couple of dozen large sourdough
loaves, a dozen large wholemeal loaves, four dozen burger buns and a pile of
cheese straws. What do I do first?

This is where the `order` and `production_sheet` tables come in. The order
sheet is pretty straightforward. Two columns, product and quantity (I might
get fancy and and add a third column for client, but that's for later). The
production sheet less so.

Right now I know how to make a view which has recipe, ingredient and quantity
columns, the thing that's currently puzzling me is how to add a 'time' column
so I can have a view like:

|time|product|ingredient|quantity|
|---|---|---|---|
|t-15h|100% Sponge| White flour | 5kg|
|t-15h|100% Sponge| Water|5kg|
|t-15h|100% Sponge|Fresh Yeast|25g|
|t-3h|Basic White Dough|100% Sponge|10.025kg|
|t-3h|Basic White Dough|White Flour|5kg|
|t-3h|Basic White Dough|Salt|200g|
|t-3h|Basic White Dough|Water|1.6kg|
|t-1h30m|Scale|18xLarge White|800g|
|t-1h30m|Scale|6xSmall White|400g|
|t|Bake|Large White||
|t|Bake|Small White||
|t+40m|Pull|Small White||
|t+50m|Pull|Large White||

That needs a better representation of a recipe in a database. One which
captures details of how long each step takes and what resources are needed
while it's taking place. Watch this space.

I'd _really_ like to produce a schedule for multiple potentially overlapping
products being prepped at once and constrained by available labour (only one
activity can be carried out at once because I'm my only employee) and retarder
and oven space. Unless I'm missing something that's a proper NP-complete
problem and that'll have me hitting Knuth's preprints on SAT solvers. Which
will be fun.

## Next week

Most of the people who can help me getting the oven on its feet are going to
be away next week, so I doubt there'll be much progress on oven commissioning,
but I hope I'll be able to keep making progress on the bakehouse software.

[^1]: You will note that the transport cost is defined as a fraction of the
    wholesale price, but the wholesale price is defined in terms of the raw
    cost of the product, which includes the transport cost.

    At this point, it's good to remember your algebra. After a certain amount
    of fiddling, it's possible to define the transport cost solely in terms of
    the other input costs and the desired markup. It comes out as
    `$$\frac{\sum\nolimits_{i \in \lbrace ingredients, labour, packaging \rbrace} cost_{i}}{1 - transport\_allowance\_rate -
    gross\_margin}$$` which which can get entertainingly huge as
    `$transport\_allowance\_rate + gross\_margin$` approaches 1. So we make
    sure they don't.

[^2]: Which can be a bit of a pain in the arse to calculate. You should only
    really count the time you spend actively working on the components of a
    product and not the time you spend waiting around for dough to prove or
    bake or whatever. It's worth doing though. Accurate numbers matter.

[^3]: Bear in mind that the table structure in the app isn't quite the same as
    the tables described here.

<script type="text/javascript" async
  src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_SVG">
</script>

<script type="text/x-mathjax-config">
MathJax.Hub.Config({
  SVG: {
    font: "STIX-Web"
  },
  tex2jax: {
    inlineMath: [['$','$'], ['\\(','\\)']],
    displayMath: [['$$','$$'], ['\[','\]']],
    processEscapes: true,
    processEnvironments: true,
    skipTags: ['script', 'noscript', 'style', 'textarea', 'pre'],
}
});
</script>

<script type="text/x-mathjax-config">
  MathJax.Hub.Queue(function() {
    var all = MathJax.Hub.getAllJax(), j;
    for(j = 0; j < all.length; j += 1) {
        all[j].SourceElement().parentNode.className += 'has-jax';
    }
});
</script>

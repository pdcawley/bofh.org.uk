+++
title = "A recipe is just a directed acyclic graph…"
author = ["Piers Cawley"]
date = 2019-03-04
lastmod = 2020-05-04T00:15:22+01:00
slug = "recursive-sql-recipes"
draft = false
series = "Bakehouse diary"
math = true
+++

In [the last post](/2019/02/25/baking-with-emacs) I handwaved the way I represented bakery formulae in the bakery database, so here's a little more detail. It helps to think of a bakery formula as a node on a directed acyclic{{% marginnote %}}If you ignore the fact that a starter is made of flour, water and starter. Which, of course, we're going to.{{% /marginnote %}} graph with weighted edges, where the weights are literally weights. Here's the graph a for a  couple of products

<div class="RESULTS">
  <div></div>

{{< figure src="/ox-hugo/formulae.svg" >}}

</div>

<!--more-->

And here's how we represent that in the database{{% marginnote %}}This table is the result of a query on my real database, where the quantities are in kg, as opposed to the graph representation which was handrolled and adjusted to use bakers' percentages which is how formulae are traditionally written.{{% /marginnote %}}:

{{% table %}}
| name             | ingredient                    | format  |
|------------------|-------------------------------|---------|
| Small Seedy Malt | Seedy Malt Dough              | 0.63 kg |
| Small White Wild | Basic White Sour              | 0.63 kg |
| Basic White Sour | Organic white flour           | 2.00 kg |
| Basic White Sour | Sea salt                      | 0.06 kg |
| Basic White Sour | Water                         | 1.10 kg |
| Basic White Sour | 80% starter                   | 1.80 kg |
| Seedy Malt Dough | 5 Seed Soaker                 | 4.00 kg |
| Seedy Malt Dough | Water                         | 3.80 kg |
| Seedy Malt Dough | Sea salt                      | 0.22 kg |
| Seedy Malt Dough | 80% starter                   | 3.60 kg |
| Seedy Malt Dough | Organic light malthouse flour | 8.00 kg |
| 5 Seed Soaker    | Water                         | 1.20 kg |
| 5 Seed Soaker    | 5 seed mix                    | 1.00 kg |
| Mother           | Water                         | 3.20 kg |
| Mother           | Organic white flour           | 4.00 kg |
{{% /table %}}

Suppose we have an order for 8 Small White loaves. We need to know how much starter to mix tonight. We know that we need 0.63 kg of dough for each loaf, so that's a total of 5.04 kg of Basic White Sour. The formula for Basic White Sour makes a total of \\(1.10 + 1.80 + 0.06 + 2.00 = 4.96 \mathrm{kg}\\) of dough. So we need to multiply each quantity in that formula by the weight of dough we need divided by the total weight of the recipe \\((5.04/4.96 = 1.016)\\). This is straightforward enough for flour, water and salt, which are basic ingredients, but we'll need to do a similar calculation to work out how much flour and water we'll need to make \\(1.016 × 1.8 = 1.829 \mathrm{kg}\\) of starter. You can see how this might become a little tedious.

If I were going to be doing these calculations by hand, it would definitely pay me to normalize my intermediate formulae so they all made a total of 1 kg of stuff. But screw that, we have a computer, so we can make it do the work.

I'm going to simplify things a little (the real database understands about dates, and we need to know a little more about recipes, products and ingredients than will fit in the `recipe_item` table that describes the graph) but this should give you an idea of the recursive queries that drive production planning.

Let's introduct a `production_order` table, where we stash our orders{{% marginnote %}}The real table has extra information about customers and order dates.{{% /marginnote %}}:

{{% table %}}
| product          | quantity |
|------------------|----------|
| Small White Wild | 5        |
| Small Seedy Malt | 5        |
{{% /table %}}

And that's all we need to fire off a recursive query{{% marginnote %}}I'm writing this using the literate programming capabilities of org-mode, so the code you see is being run against my production database, and the results are using my working formulae. Which is why we're not querying the real `production_order` table.{{% /marginnote %}}.

```sql
WITH RECURSIVE po(product, quantity) AS (
    SELECT 'Small White Wild', 5
  UNION
    SELECT 'Large White Wild', 5
), rw(recipe, weight) AS (
    SELECT recipe, sum(amount)
      FROM bakehouse.recipe_item
  GROUP BY recipe
), job(product, ingredient, quantity) AS (
    SELECT po.product,
           ri.ingredient,
           po.quantity * ri.amount
      FROM po
      JOIN bakehouse.recipe_item ri ON po.product = ri.recipe
      JOIN rw ON ri.recipe = rw.recipe
  UNION
    SELECT job.ingredient, ri.ingredient, job.quantity * ri.amount / rw.weight
      FROM job
      join bakehouse.recipe_item ri on job.ingredient = ri.recipe
      join rw on job.ingredient = rw.recipe
)
SELECT product formula, ingredient, ROUND(sum(quantity),2) quantity from job group by job.product, job.ingredient order by formula;
```

Which gives the following result:

{{% table %}}
| formula          | ingredient          | quantity |
|------------------|---------------------|----------|
| Basic White Sour | Sea salt            | 0.09     |
| Basic White Sour | Water               | 1.72     |
| Basic White Sour | Mother              | 2.81     |
| Basic White Sour | Organic white flour | 3.13     |
| Large White Wild | Basic White Sour    | 4.65     |
| Mother           | Organic white flour | 1.56     |
| Mother           | Water               | 1.25     |
| Small White Wild | Basic White Sour    | 3.10     |
{{% /table %}}

A quick sanity check seems to show this is correct (we're making 7.75kg of Basic White Sour, which tallies with the weights needed to make the loaves).
So what's going on in the query? In SQL, `WITH` is a way of giving names to your intermediate results, akin to `let` in a Lisp. We fake up a table to hold our production orders (`po`) and the `rw` clause is totals the weights of all our recipes (in the real database, it's a view). The magic really starts to happen when you use the `WITH RECURSIVE` form. With `RECURSIVE` in play, the last query is treated differently. Instead of being a simple two part `UNION` what happens is that we first run:

```sql
SELECT po.product, ri.ingredient, po.quantity * ri.amount
  FROM po
  JOIN bakehouse.recipe_item ri on po.product = ri.recipe
  JOIN rw on ri.recipe = rw.recipe
```

and call the results `job` and then run the second query, adding any extra rows generated to the results, and repeating that query until the result set stops growing. If we didn't have `WITH RECURSIVE` available, and we knew the maximum depth of recursion we would need, we could fake it by making a bunch of intermediate clauses in our `WITH`. In fact, until I worked out how `WITH RECURSIVE` works, that's exactly what I did.

Have you spotted the mistake? I didn't, until a few bakes when horribly wrong.

Here's what happens when we have an order for 3 small loaves and two large ones

{{% table %}}
| formula          | ingredient          | quantity |
|------------------|---------------------|----------|
| Basic White Sour | Sea salt            | 0.02     |
| Basic White Sour | Water               | 0.41     |
| Basic White Sour | Mother              | 0.68     |
| Basic White Sour | Organic white flour | 0.75     |
| Large White Wild | Basic White Sour    | 1.86     |
| Mother           | Organic white flour | 0.38     |
| Mother           | Water               | 0.30     |
| Small White Wild | Basic White Sour    | 1.86     |
{{% /table %}}

We're only making 1.86 kg of dough? What's going on?

It turns out that the way a `UNION` works is akin to doing `SELECT DISTINCT` on the combined table, so it selects only unique rows. When two orders end up requiring exactly the same amount of the 'same' dough, they get smashed together and we lose half the weight. This is not ideal.{{% marginnote %}}It's _especially_ not ideal when you don't spot there's a problem and end up making far fewer loaves than you expect. Or on one _really_ annoying occasion, making a dough that was far too dry because we lost some water along the way. You can correct this during the mix, but it was a nasty shock.{{% /marginnote %}} I fixed it by adding a 'path' to the query, keeping track of how we arrived at a particular formula. Something like:

```sql
WITH RECURSIVE po(product, quantity) AS (
    SELECT 'Small White Wild', 3
  UNION
    SELECT 'Large White Wild', 2
), rw(recipe, weight) AS (
    SELECT recipe, sum(amount)
      FROM bakehouse.recipe_item
  GROUP BY recipe
), job(path, product, ingredient, quantity) AS (
    SELECT po.product,
           po.product,
           ri.ingredient,
           po.quantity * ri.amount
      FROM po
      JOIN bakehouse.recipe_item ri ON po.product = ri.recipe
      JOIN rw ON ri.recipe = rw.recipe
  UNION
    SELECT job.path || '.' || job.ingredient,
           job.ingredient,
           ri.ingredient,
           job.quantity * ri.amount / rw.weight
      FROM job
      join bakehouse.recipe_item ri on job.ingredient = ri.recipe
      join rw on job.ingredient = rw.recipe
)
SELECT product formula, ingredient, round(sum(quantity),2) weight from job group by formula, ingredient order by formula;
```

This query gives us:

{{% table %}}
| formula          | ingredient          | weight |
|------------------|---------------------|--------|
| Basic White Sour | Sea salt            | 0.05   |
| Basic White Sour | Water               | 0.83   |
| Basic White Sour | Mother              | 1.35   |
| Basic White Sour | Organic white flour | 1.50   |
| Large White Wild | Basic White Sour    | 1.86   |
| Mother           | Organic white flour | 0.75   |
| Mother           | Water               | 0.60   |
| Small White Wild | Basic White Sour    | 1.86   |
{{% /table %}}

This time we're making 3.74 kg of dough, which is right.

In order to see what's going on, we can change the final `SELECT` to `SELECT formula, path, ingredient, round(quantity,2) weight FROM job`, and now we get:

{{% table %}}
| formula          | path                                     | ingredient          | weight |
|------------------|------------------------------------------|---------------------|--------|
| Large White Wild | Large White Wild                         | Basic White Sour    | 1.86   |
| Basic White Sour | Large White Wild.Basic White Sour        | Mother              | 0.68   |
| Basic White Sour | Large White Wild.Basic White Sour        | Organic white flour | 0.75   |
| Basic White Sour | Large White Wild.Basic White Sour        | Water               | 0.41   |
| Basic White Sour | Large White Wild.Basic White Sour        | Sea salt            | 0.02   |
| Mother           | Large White Wild.Basic White Sour.Mother | Water               | 0.30   |
| Mother           | Large White Wild.Basic White Sour.Mother | Organic white flour | 0.38   |
| Small White Wild | Small White Wild                         | Basic White Sour    | 1.86   |
| Basic White Sour | Small White Wild.Basic White Sour        | Organic white flour | 0.75   |
| Basic White Sour | Small White Wild.Basic White Sour        | Sea salt            | 0.02   |
| Basic White Sour | Small White Wild.Basic White Sour        | Water               | 0.41   |
| Basic White Sour | Small White Wild.Basic White Sour        | Mother              | 0.68   |
| Mother           | Small White Wild.Basic White Sour.Mother | Organic white flour | 0.38   |
| Mother           | Small White Wild.Basic White Sour.Mother | Water               | 0.30   |
{{% /table %}}

Which shows that we're considering two lots of Basic White Sour with exactly the same weights, but we (and more importantly, the database engine) know that they're distinct amounts because we get to them through different routes. Hurrah! The problem is solved and we can accurately work out what we should be mixing.


## What's still missing {#what-s-still-missing}

As a baker, I know that, if I've got an order for bread on Friday, then I need to mix the starters on Wednesday night, then spend Tuesday mixing, fermenting and shaping the loaves, which will spend the night in the retarder ready to be baked at 4 on Friday morning. But the schema I've outlined here doesn't. In my full bakehouse schema, I have a few extra tables which hold timing data and such. In particular, I have a `product` table, which knows about everything I sell. This table knows holds info about how many I can make per hour of work and the bake time and temperature. Then there's a `recipe` table which holds information about how long a formula needs to rest{{% marginnote %}}This could be the bulk fermentation time if it's a formula for a dough or a starter, a proof time if it's a loaf, or a soaking time for a soaker (a soaker is usually a mixture of seeds or fruit and a liquid, usually water, but occasionally fruit juice or booze depending on the final product){{% /marginnote %}}. The real queries take this into account to allow us to work back from the `due_date` of a real order to the day we need to do the work. If you want to dig into how I handle dates  you can check out the repository at <https://github.com/pdcawley/bakehouse/>.

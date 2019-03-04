+++
title = "A recipe is just a directed acyclic graph…"
author = ["Piers Cawley"]
date = 2019-02-26
lastmod = 2019-03-04T14:08:58+00:00
slug = "recursive-sql-recipes"
draft = true
series = "Bakehouse diary"
+++

In [the last post](/2019/02/25/baking-with-emacs) I handwaved the way I represented bakery formulae in the bakery database, so here's a little more detail. It helps to think of a bakery formula as a node on a directed acyclic[^fn:1] graph with weighted edges, where the weights are literally weights. Here's the graph a for a couple of products

<div class="RESULTS">
  <div></div>

{{< figure src="/ox-hugo/formulae.svg" >}}

</div>

And here's how we represent that in the database[^fn:2]:

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

Suppose we have an order for 8 Small White loaves. We need to know how much starter to mix tonight. We know that we need 0.63 kg of dough for each loaf, so that's a total of 5.04 kg of Basic White Sour. The formula for Basic White Sour makes a total of \\(1.10 + 1.80 + 0.06 + 2.00 = 4.96 \mathrm{kg}\\) of dough. So we need to multiply each quantity in that formula by the weight of dough we need divided by the total weight of the recipe \\((5.04/4.96 = 1.016)\\). This is straightforward enough for flour, water and salt, which are basic ingredients, but we'll need to do a similar calculation to work out how much flour and water we'll need to make \\(1.016 × 1.8 = 1.829 \mathrm{kg}\\) of starter. You can see how this might become a little tedious.

If I were going to be doing these calculations by hand, it would definitely pay me to normalize my intermediate formulae so they all made a total of 1 kg of stuff. But screw that, we have a computer, so we can make it do the work.

I'm going to simplify things a little (the real database understands about dates, and we need to know a little more about recipes, products and ingredients than will fit in the `recipe_item` table that describes the graph) but this should give you an idea of the recursive queries that drive production planning.

Let's introduct a `production_order` table, where we stash our orders[^fn:3]:

| product          | quantity |
|------------------|----------|
| Small White Wild | 5        |
| Small Seedy Malt | 5        |

And that's all we need to fire off a recursive query[^fn:4].

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

We're only making 1.86 kg of dough? What's going on?

It turns out that the way a `UNION` works is akin to doing `SELECT DISTINCT` on the combined table, so it selects only unique rows. When two orders end up requiring exactly the same amount of the 'same' dough, then they'll get smashed together and we'll lose half the weight. This is not ideal. I fixed it by adding a 'path' to the query, keeping track of how we arrived at a particular formula. Something like:

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

This time we're making 3.74 kg of dough, which is right.

In order to see what's going on, we can change the final `SELECT` to `SELECT formula, path, ingredient, round(quantity,2) weight FROM job`, and now we get:

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

Which shows that we're considering two lots of Basic White Sour with exactly the same weights, but we (and more importantly, the database engine) know that they're distinct amounts because we get to them through different routes.

The thing that's nagging at me now is that I repeat what's essentially the same query structure in a couple of different views and the path through the graph doesn't change unless I adjust a recipe, so I'm wondering if I could make a materialised view that has enough information to shortcut the calculations for both making the production list (what needs to be mixed, when) and for working out my costings (to put a price on a loaf, you need to know how much the raw ingredients cost, and that involves walking the tree again. Maybe a table like:

| product          | sub-formula      | ingredient  | is\_raw | factor | lead time |
|------------------|------------------|-------------|---------|--------|-----------|
| Large White Wild | Basic White Sour | White Flour | TRUE    | 0.403  | 1 day     |
| Large White Wild | Basic White Sour | Salt        | TRUE    | 0.012  | 1 day     |
| Large White Wild | Basic White Sour | Water       | TRUE    | 0.222  | 1 day     |
| Large White Wild | Basic White Sour | 80% Starter | FALSE   | 0.462  | 1 day     |
| Large White Wild | 80% Starter      | White Flour | TRUE    | 0.288  | 2 days    |
| Large White Wild | 80% Starter      | Water       | TRUE    | 0.173  | 2 days    |

If we have that table, then two days before our bread is due, if we have an order for 10 white loaves, we'll need to mix \\(9.3 × .288 \approxeq 2.68\\) kg of flour and \\(9.3 × 0.173 \approxeq 1.61\\) kg of water. And if flour costs a £0.86/kg and salt is £0.41/kg, then the .93 kg of dough we need will cost \\(0.93(0.41 × 0.0121 + 0.86(0.403 + 0.288)) \approxeq £0.56\\).

[^fn:1]: If you ignore the fact that a starter is made of flour, water and starter. Which, of course, we're going to.
[^fn:2]: This table is the result of a query on my real database, where the quantities are in kg, as opposed to the graph representation which was handrolled and adjusted to use bakers' percentages which is how formulae are traditionally written.
[^fn:3]: The real table has extra information about customers and order dates.
[^fn:4]: I'm writing this using the literate programming capabilities of org-mode, so the code you see is being run against my production database, and the results are using my working formulae. Which is why we're not querying the real `production_order` table.

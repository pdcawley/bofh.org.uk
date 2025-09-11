+++
title = "Linguistically motivated refactoring"
slug = "linguistically-motivated-refactoring"
date = "2006-08-25T03:49:00+00:00"
draft = false

+++

When naming a variable, method, parameter or class, give it a name that fits well with the language and concerns of the scope in which you are using it.

Here's some Ruby code:

<code lang='ruby'>

<pre>
def test\_can\_load\_unique\_articles
number\_of\_iterations = 8

articles = create\_articles(number\_of\_iterations) do
create\_valid\_article
end

assert\_equal number\_of\_iterations - 1, articles.first.related\_articles.size
end

...

def create\_articles(number\_of\_iterations)
...
end
</code>

What do you notice about `number_of_iterations` in the definition of `test_can_load_unique_articles`? I notice that it's wrongly named. The name has been chosen to make sense in the context of the loop in the middle of the method, rather than in the assertion at the end - the clue is in the name of `create_articles`'s parameter. In a test method, it seems to me, one of the most important things to do is to ensure that the assertions read as clearly as possible.

Consider:

<code lang='ruby'>

<pre>
def test\_can\_load\_unique\_articles
family\_size = 8

articles = create\_valid\_article\_family(family\_size)

assert\_equal family\_size - 1, articles.first.related\_articles.size
end

def create\_valid\_article\_family(size)
create\_articles(size) {create\_valid\_article}
end

def create\_articles(number\_of\_iterations)
...
end
</code>

Look at how the assertion becomes clearer. In a family with <var>n</var> members then obviously any member of that family will have <var>n</var> - 1 relations. I've also interposed a helper method between the test and `create_articles`. The idea is to make the language used within the test method internally consistent so the test can be read quickly to understand its intent, before chasing down any method definitions whose behaviour isn't immediately apparent.

I've found this sort of linguistically motivated refactoring to be a very handy practice for increasing code clarity. I should do it more often.

With apologies to [Paul Ingles](http://www.oobaloo.co.uk/articles/2006/08/23/the-importance-of-language) - who must be getting sick of me bashing on this particular chunk of code by now. The article I took this code from makes a good point about striving to keep the language within your code relevant to the problem domain, and how different programming languages make that easier or harder.

And now I really am gone fishing.

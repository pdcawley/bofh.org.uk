+++
title = "Cover me; I'm going in!"
slug = "cover-me-im-going-in"
date = "2007-03-15T14:06:00+00:00"
draft = false

+++

Mmm... Rails routes, don't you love them?

Well, not unreservedly, no.

Here's my current problem: Typo articles have a permalink that looks like `/articles/:year/:month/:mday/:permalink`, so the permalink for this article is `/articles/2007/03/15/cover-me-im-going-in` and when someone visits it, the controller gets a params hash which contains keys along the lines of:

{ :year =&gt; '2007', :month =&gt; '03', :mday =&gt; '15',
:permalink =&gt; 'cover-me-im-going-in',
\# the session, controller, etc }

Which is okay as far as it goes[1], but it's a complete pain when it comes to generating the url. I have to do:

url\_for :year =&gt; `article.year, 
          :month     => `article.month,
:mday =&gt; `article.mday, 
          :permalink => `article.permalink

Every value in that hash is dependent on a single object, every key is the same as the method used to find its associated value. I want to be able to write a route that looks like this:

map.article\_url \\
'/articles/:article\[year\]/:article\[month\]/:article\[mday\]/:article\[permalink\]',
:controller =&gt; 'articles', :action =&gt; 'show'

article\_url @article \# generates the route

That'll do for starters; I'll worry about making it Resourceful once I've got the basic recognizer and generator working.

So, that's what I shall be working on within `acts_as_resource` for the next while. Once I've got it working, I'll start using it within Typo and we can get rid of the various model methods we ended up adding to cope with generating our URLs.

### Why bother?

The thing to remember is that, while routes like `/:resources/:id` are okay when you're prototyping, they're problematic for production. Opacity is not a virtue. Exposing internal database keys in the URL is a positive vice.

Meanwhile `/articles/2007/03/15`, `/articles/2007/03` and `/articles/2007` all have obvious meanings and typo supplies the obvious index pages when you go to look at them.

[1] However, it would be better if it were actually more like: `{:article => {:year => '2007', ...}, :controller => ...}` because then I can find the article by a simple `Article.find(:first, :conditions => params[:article])` and let `sanitize_sql` do all the work for me.

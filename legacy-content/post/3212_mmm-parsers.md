+++
title = "Mmm... parsers"
slug = "mmm-parsers"
date = "2007-04-14T14:13:00+00:00"
draft = false

+++

So, in my quest to get Rails routes to accept routes like:

articles/:comment\[article\]/comments/:comment\[id\]

I've been playing with parsers for the first time in my programming career. Quite how I've managed to get this far without them I leave as an exercise for the interested reader.

At the moment I have a parser that will parse

articles/2007/05/12/slug

and give back an `ActionSelector` that yamlizes as

<code>

    --- !ruby/object:ActionSelector
      controller: articles
      action: show
      params:
        :article:
          :year: 2007
          :month: 05
          :mday: 12
          :slug: slug

</code>
Which can be easily turned into the kind of params hash that rails wants from its routing system in order to dispatch the request to the right place.

For my next trick, I need refactor the parser I have so that it's generated via a set of parser builders. Then I need to write a parser for routing specifiers that can use those builders to build a URL parser.

It's a simple matter of programming I tell you.

### Updates

#### RouteBuilder works (2007041608:00ish)

I now have a `RouteBuilder` that generates routes and has the same interface as `ActionController::Routing::RouteBuilder`.

My `Route` doesn't have the same interface as `AC::Routing::Route` yet, but it's not far off. The fun part is working out which bits of the interface are essential and which can be safely ignored. I'm hoping that I won't have to rewrite `AC::Routing::RouteSet` as well, but I'm not sure how complicit it is with the innards of `AC::Route`.

#### Routes can generate paths from options (2007041612:30)

<code>

<pre>
context "Given a default route" do
setup do
@route = RouteBuilder.new.build('/:controller/:action/:id')
end

def gen(hash)
\# Rails interface is a bit surprising
@route.generate(hash.dup, hash.dup, {})
end

specify "generation works" do
gen(:controller =&gt; 'accounts', :action =&gt; 'show', :id =&gt; 1) \\
.should == '/accounts/show/1'

gen(:controller =&gt; 'accounts', :id =&gt; 1) \\
.should == '/accounts/index/1'

gen(:controller =&gt; 'accounts', :action =&gt; ':index') \\
.should == '/accounts'
end
end
</code>

It's nowhere near complete, but I'm being gung ho and starting to integrate my RouteBuilder with Rails. Then I can start pulling the Rails routing tests into my own test suite and get some confidence with its robustness before I start adding the ability to handle routes like `/books/:tune[book][id]/tunes/:tune[id]`, which is why I started this in the first place.

With any luck and some good train hacking time on the way to London I should have something good to show at LRUG this evening.

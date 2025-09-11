+++
title = "How I learned to stop worrying and love aggressive mockery"
slug = "how-i-learned-to-stop-worrying-and-love-aggressive-mockery"
date = "2007-06-06T05:21:00+00:00"
draft = false

+++

There's something enormously liberating about writing an [RSpec](http://rspec.rubyforge.org/) description that starts like:

<code>

<pre>
describe ArticlesController, "feeds:" do
before do
`the_mock = mock('everything', :null_object => true)
    ActiveRecord::Base.stub!(:find).and_return(`the\_mock)
end

it "/articles.atom -&gt; atom feed" do
get :index, :format =&gt; 'atom'
response.should render\_template('\_atom\_feed')
end

...
end
</code>

The `:null_object` flag to rspec's `mock` function is remarkably potent.The resulting mock will return itself from any method call that hasn't got some other expectation set up. When I'm testing that my index methods render the appropriate views for the format, I don't care that all the various variables have been set up correctly - I've already tested that in another description - I just want to get to the point where I'm about to render a template.

What this does is decouple my tests. I can change the way that the index method fetches its stuff from the database and I'm only going to have to change the innards of the specs that test that.

I tend to think of this as being analogous to the object oriented pattern of trying to write your methods at a single level of abstraction. Within a given spec, I should only be setting up expectations that are directly related to what I'm testing.

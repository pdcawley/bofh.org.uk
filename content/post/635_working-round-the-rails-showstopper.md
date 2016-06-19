+++
title = "Working round the Rails showstopper"
slug = "working-round-the-rails-showstopper"
date = "2006-08-10T12:02:00+00:00"
draft = false

+++

So, it turns out that the [rush released](http://weblog.rubyonrails.org/2006/8/9/rails-1-1-5-mandatory-security-patch-and-other-tidbits) Rails 1.1.5 doesn't actually fix the security problem. Worse, it seems that the problem lies somewhere in the nest of serpents that is the routing system. It turns out that some of the magic that lets everything work in nice ways doesn't do enough to make sure that malicious people can make everything work in nasty ways.

The problem lies in the route that everyone has in their `routes.rb`:
<code>

<pre>
map.connect ':controller/:action/:id'
</code>
Which is, frankly, a wide open door.

The fix, which I'm busy applying to Typo, is to get rid of the default route and replace it with an explicit set of routes for your controllers. So at the end of your `routes.rb` you'll have something like:
<code>

<pre>
%w{articles pages xml other controller names}.each do |c|
map.connect "\#{c}/:action/:id", :controller =&gt; c
end
</code>

If you're lucky, that'll be all you need to do. If you're less lucky, you'll have a whole host of calls to `url_for`, `link_to` and their brethren that implicitly rely on the default route. In particular, `url_for :controller => '/'` will stop working. In typo that means grepping through the source and replacing that with `url_for :controller => 'articles'`, but for your particular project you'll need to change that to whatever your default controller is. You'll probably find other painful spots too. Such spots are often your code's way of telling you not to be quite so sloppy, but the pain still exists. For instance, we've had to add a route to a phantom 'files' controller.

With any luck, once I've made the routes explicit the problem of evil URLs will go away. I'll let you know when I reach the point where I can test things, but I have high hopes of this approach. Expect a new Typo release soon.

### Update 1

Yay! It seems to work. Changes have been checked into the typo repository, grab the head if you need it. Scott will be releasing 4.0.2 when he's made sure I've not done anything stupid.

### Update 2

Turns out, I had done something stupid. But I fixed that now.

### Update 3

And, in a single stroke, I fixed things so that themes that use `url_for :controller => '/'` (which is most of 'em) will continue to work. Now I'm off out for a few hours. Don't break the internet while I'm gone.

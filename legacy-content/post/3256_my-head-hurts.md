+++
title = "My head hurts"
slug = "my-head-hurts"
date = "2007-09-23T14:09:24+00:00"
draft = false

+++

During DHH's keynote at RailsConf Europe it was apparent that there's a great deal to like in edge rails, so I thought I'd have a crack at getting Typo up on it.

Ow.

I'd expected the pain points to be related to routing, but it seems that the rails routing system is approaching the level of the Excel calculation engine - nobody dares touch it for fear of breaking things, so typo's custom routes seemed to work quite happily. There were a few things that have been deprecated, pluginized or moved out of the set of modules that's automatically included when you do a `rake rails:freeze:edge`, but they were pretty easy to sort - the deprecation messages are a good deal more informative now than they were last time went deprecation squashing. There's a surprising amount of stuff that's been removed *without* any deprecation warnings though, which isn't very sporting. DHH said there would likely be a 1.2.4 release (possibly a day before 2.0) with a bunch more deprecation warnings covering everything that's *actually* going away, so if you're thinking of moving a maturish app to Rails 2.0 it might make sense to wait for 1.2.4, install that, squash warnings, and move on up to 2.0.

The real pain comes from themes. Typo's themes rely on Rails internals working in a particular way, but they don't work like that any more. In theory, the internals appear to be more theme friendly, related to allowing plugins to include views. The problem is, that it's possible to change Typo's theme without restarting the server, and the new themish internals don't expect anything to change until the server's restarted.

So, I've been playing with plugins. The most promising approach appears to be that of the [themer plugin](http://julik.textdriven.com/svn/tools/rails_plugins/themer/README), which gets pretty close to doing what we need, and does it in a way that seems like it should work with both 1.2.3 and Edge Rails. It does appear to be making some radically different assumptions about the structure of the themes directory, but the basic framework is good and I should be able to make things work by making our current them object conform to Themer::Base's interface and duck type my way to the sunny uplands of Edge Rails compatibility.

Which will be nice.

I like the themer approach a lot. Instead of monkeying about in the guts of rails, it monkeys about in *front* of Rails. It overrides `render` so that you can pass it a theme/lookup object. If it sees a lookup object, it uses that to rewrite the rest of the render arguments into a form that will render the right thing using the standard implementation of render. In a work project I've taken a similar approach to handling polymorphic routes for things like:

<code>

<pre>
map.resources :pictures do |pics|
pics.resources.comments
end

map.resources :users do |users|
users.resources.comments
end
</code>

I ended up with a `to_params` method defined on my `Comment` model, and stuck an extended `url_for` in front of the default Rails version, which looks something like:

<code>

<pre>
def url\_for\_with\_to\_params(\*arguments)
if arguments\[0\].respond\_to?(:to\_params)
with\_options(arguments.shift.to\_params) do |mapper|
mapper.url\_for\_without\_to\_params(\*arguments)
end
else
url\_for\_without\_to\_params(\*arguments)
end
end
alias\_method\_chain :url\_for, :to\_params
</code>

Which is *so* much neater than the last time I attacked this particular problem (see the [acts\_as\_resource](http://www.bofh.org.uk/articles/2007/01/25/initial-release-of-acts_as_resource) plugin).

One of the nice things about Rails is that, although it's opinionated and somewhat liberal with the syntactic vinegar for things the core team don't think is the Right Way, they're pretty good at leaving the door open for people like me who have other opinions. Both the themer plugin and my as yet unpluginized extension of `url_for` work by using existing capabilities in new ways and, because those *capabilities* are documented we can expect them to continue to work over multiple versions of Rails. Plugins that achieve similar effects by monkeying with Rails's *internal* interfaces are hostages to fortune. Internal interfaces are free to change at any time, even between point releases, so a plugin can be left high and dry with surprising rapidity. Just ask the Rails Engines folk.

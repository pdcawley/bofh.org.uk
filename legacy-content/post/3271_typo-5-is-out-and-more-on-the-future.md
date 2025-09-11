+++
title = "Typo 5 is out - and more on the future"
slug = "typo-5-is-out-and-more-on-the-future"
date = "2007-12-30T09:10:18+00:00"
draft = false

+++

Right, we've cut a Typo 5 gem and it's on rubyforge and heading to various mirrors I hope. Frédèric's writing the release notification which will be appearing on [Typosphere](http://blog.typosphere.org/) Real Soon Now.

It's been a surprisingly tricky process - we're now requiring Rails 2.0.2 because the workings of `view_paths` have changed in a way which means we can't quite make themes with Rails 2.0 *and* 2.0.2 and working with the edge seems like the more sensible proposition. If you're on the bleeding edge, you should find that you get the right Rails via svn:externals anyway.

### Typo futures

Meanwhile, I've been playing with stet and I've come to the conclusion that, although there's mileage to be had in a radically slimmed down approach to the way Typo works, I'm better off simply removing the misfeatures from Typo and building from there - there's a surprising amount of *stuff* that needs to be done in a competent blogging engine that Typo gets right - starting again would be throwing the baby out with the bathwater I think.

However, this does mean that if you're following the Typo SVN trunk, you'll be seeing a reduction in features in the short term. We'll be copying the current trunk to a `5-0-stable` branch before we start with the featurectomies though, so if you're just after bugfixes, you'll be better off there.

#### Multiblogging

We're aiming to have multiblogging in the next release, but we're rethinking the *how* of it. Right now, the 'Blog' object adds a bunch of complexity to code that would be much happier simply assuming that it has the database to itself. So we're going to look at switching to a database per blog approach, that way our core code can pretty much forget about the complexities of multiblogging, and (at least initially) anyone who wants multiblogging can get there by monkeying with configuration files - of course, we intend to add a web based admin interface once things settle down and we know how things are going to work.

#### Caching

Caching is always a bugbear in any typo installation. Because we want to be installable on the widest possible range of hosts, we can't rely on the presence of handy tools like 'memcached'. Also, some of our users are operating under some fairly severe memory and process constraints, so it makes sense to have the webserve serve static files as much as possible. Meanwhile, tools like Evan Weaver's [Interlock](http://blog.evanweaver.com/articles/2007/12/13/better-rails-caching/) are pointing the way towards seriously effective fragment caching. I shall be looking into implementing something that conforms to the interlock interface, but which can use an arbitrary cache backing store for fragments *and* maintain a full page cache. It'll be interesting to find out if this is doable...

#### Atom Publishing Protocol

ActionWebService is going to go away - it's already in the `ousted` branch of the rails SVN repository, and including it in Typo to support the various different admin APIs is getting painful. So, we're going to preempt it. We won't be getting rid of the various XMLRPC APIs until the pain becomes too great, but we are going to be concentrating on implementing, and strongly favouring, the Atom Publishing Protocol.

#### Feeds for everything

In particular, we'll be adding atom feeds for all sorts of administrative data as a means of enabling people to write external tools for, say, spam protection, comment moderation and notification tasks. Right now, there's a great deal of computation happening on the server side every time someone, say, comments on a post - in the kind of resource limited environments some people are running Typo in, that's too much work. Switching to a feed + APP approach should help enormously with resource utilization.

#### Speaking of resources...

Using the server to render article previews is... suboptimal. Expect to see a javascript based preview system akin to the one I use for comments here.

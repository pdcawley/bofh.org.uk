+++
title = "Rails 2.0 and the Future of Typo"
slug = "rails-2-0-and-the-future-of-typo"
date = "2007-12-16T10:35:00+00:00"
draft = false

+++

So, if you've been watching the Typo tree, you'll see there's been a fair amount of activity on it since Rails 2.0 got released. There's a new default theme replacing the rather creaky 'azure', and a fair amount of work on getting our code compatible with the current state of Rails. As we work on this, it becomes apparent that Typo's code is getting horribly brittle. I have said before that there's been several places where we've zigged before Rails zagged, and we're paying the price for that. It doesn't help that our test coverage is distinctly ropy either - and I'm probably guiltier than most for letting things get into that state.

So, our goal is to get what we have cleaned up and working with Rails 2 before releasing Typo 5.0. Once that's done, that line of code will go into maintenance mode - there are still plenty of bugs to fix and documentation to write, but I'm afraid that extending that base is becoming too much of a chore.

Which is why I have a new path in my local svk repository, //stet. I'm using this for experimental development of a new, slimmed down blogging engine that will be, first and foremost, a capable Atom Publishing Protocol host. Things like spam processing will be removed from the core of the application, but we'll provide a suite of webservice clients that will consume the 'unmoderated feedback' webfeed and use APP to either approve or delete the feedback as appropriate.

Theming (at least initially) will probably be confined to Javascript and CSS changes, and I'm even thinking of exposing the sidebars as Atom collections - certainly I expect that, in the first cut, sidebars will be static - if you want content that *looks* dynamic you'll have to do it via javascript.

My initial goal is to slim things down as far as I possibly can - I want to build a blogging engine that can cope with the tight memory constraints of shared hosting by off loading much of the heavy lifting to client boxes. After all, I have far more processing capability available to me on the laptop I'm typing this on than the slice of Site5's hosting infrastructure that's actually running the blog. By making things small and static, I also hope to wring good performance numbers out of the tool as well - expect aggressive page caching at the very least.

Another important goal is easy migration of Typo databases. I expect to be writing models and controllers from the ground up, but converting the database should just be a matter of running a migration.

### Experimental

Of course, stet's currently *very* experimental - about the only thing that's actually *written* so far are a couple of routing plugins which should help radically simplify our routes.rb (expect an article's url to change from /articles/2007/12/16/rails-20-and-the-future-of-type to /article/2007/12/16/rails-20-and-the-future-of-typo, but with a redirect in place to cater for the old style urls). I may have grandish plans for the thing, but I could equally discover that I'm off up a blind alley, in which case you can expect me to return to the current typo codebase with a few more lessons learned.

#### ActiveResource?

I remain unconvinced by ActiveResource as a technology. I agree with the authors of [RESTful Webservices](amazon:0596529260) - good webservices are joined up. They take full advantage of what could be described as the defining technology of the world wide web, the URL based hyperlink to knit resources together in a discoverable fashion. An ActiveResource based webservice may well be a good HTTP citizen, but it's still not really 'webby' enough for my taste. Which means the Atom Publishing Protocol will remain my friend for most of the things I hope to do with stet. It may be harder to write a good APP server, but I'm convinced that it's a much better interface for clients, and you should always favour ease of use over ease of implementation. If nothing else, we're aiming to have more users than developers. Many more.

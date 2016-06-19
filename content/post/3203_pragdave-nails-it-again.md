+++
title = "PragDave nails it. Again."
slug = "pragdave-nails-it-again"
date = "2007-03-29T12:39:00+00:00"
draft = false

+++

REST is easy, it's just smart clients using HTTP to the full.

Except, as Dave 'PragDave' Thomas [points out](http://pragdave.pragprog.com/pragdave/2007/03/the_radar_archi.html), browsers aren't smart. They're just dumb terminals with prettier graphics.

And what does that mean? It means that a typical Rails controller is trying to multiplex two protocols in the same chunk of code. There's the pure four and then there's the others: `;new` and `;edit`, offering up forms for clients too dumb to know how to build their own; and RJS helpers like `;preview`, `;autocomplete` and whatever else you need to do server side to make the Browser look smart.

But when you have a class that's trying to be two things at once, you really have two classes, you just don't know it yet.

So, Dave proposes splitting controllers; write a RESTful controller that only has to concern itself with serving up resources that smart clients can reuse and remix as appropriate. Then, you a server side smart client that decorates your resources with all the scaffolding that a browser needs.

Dave calls this RADAR: RESTful Application talking to Dumb Ass Recipient.

My first reaction is that Dave's definitely onto something. If nothing else it deals with one obvious wart involving authentication and REST. The problem is, a good RESTful application doesn't roll its authentication, just uses HTTP Digest authentication or SSL or whatever. With a smart client that's fine; smart clients know how to log off as well as on. Browsers don't. Essentially, once you've logged on using HTTP authentication, you're authenticated until you restart the browser, which is not necessarily what you need a lot of the time. So most applications use cookies for authentication because the application has rather more control over a cookie. But that's not really very RESTful because the connection is now stateful. Disaster!

However, if you decompose your application along RADAR lines, all the conversation with the REST kernel continues on its stateless way, but the combination of your browser presentation layer and the browser share a bit of state through cookies (and possibly a session) in order to do a better imitation of a smart client.

### A thought...

Hmm... suddenly the [Zimki](http://www.zimki.com/) approach of writing everything in JavaScript is looking more attractive. If you write the presentation layer in JavaScript there's nothing to stop it residing either on the server while talking to browsers with JavaScript turned off and running directly on the browser (entirely or partially as the case may be) and talking JSON to the REST kernel.

Or you could pull out all the stops and have a Seaside style front end, doing all that shiny continuation based, rich session magic that makes [DabbleDB](http://www.dabble.com/) such an amazing bit of software; after all, Avi's been busy showing people how to take Seaside's ideas an implement 'em in Ruby (or JavaScript...).

I wonder if Rails is malleable enough to get a proof of concept of this sort of thing up quickly?

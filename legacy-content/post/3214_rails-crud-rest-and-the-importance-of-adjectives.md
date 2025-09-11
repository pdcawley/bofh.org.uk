+++
title = "Rails, CRUD, REST and the importance of adjectives"
slug = "rails-crud-rest-and-the-importance-of-adjectives"
date = "2007-04-18T03:23:00+00:00"
draft = false

+++

Consider a web request:

GET /articles/2007/04/17/adjectives-rule;edit

### "What does it *mean?"*

If we pass it through the prism of Rails routing, it breaks apart as follows:

<dl>
<dt>
`GET`

</dt>
<dd>
The `:method`, sometimes called the verb.

</dd>
<dt>
`/articles`

</dt>
<dd>
The `:controller`, the thing that's going to handle this request

</dd>
<dt>
`/2007/04/17/adjectives-rule`

</dt>
<dd>
Parameters, passed to the controller method that handles this request

</dd>
<dt>
`;edit`

</dt>
<dd>
The `:action`, some way of specifying what this request does

</dd>
</dl>
It's certainly one way of looking at it, but if you do I think you're limiting your understanding of what it means to program in a RESTful fashion. It's an obvious fit for Rails where the dispatcher builds an appropriate controller and then does `controller.send params(:action)`[1] but it's a very 'imperative' view and that's not necessarily a good view. The problem is that `:action` at the end.

What's happening is that Rails's implementation is leaking into the abstraction that is the URL. If you let that happen you end up programming with CRUD (Create, Read, Update, Delete)dy blinkers.

Let's try another way of breaking that request up into 'parts of speech'. Here's what I think the request means: "Get me the editor for the article posted on 2007-04-17 called 'adjectives-rule'". `;edit` isn't an action selector, it's a *view* selector.

### "Isn't this just navel gazing?"

At the London Ruby Users' Group on Monday, there was an occasionally heated discussion of RESTful programming in which several people expressed the view that REST was limiting because they "needed more than 4 verbs" for their application.

Much of the discussion of RESTful programming on Rails has been about how to break things down so that you can think of your application in CRUD terms, and that's a terribly limiting way to view things.

Let's look at another request and see how to interpret it.

<code>

    POST /articles/new;preview
    Content:
    article[title]=Adjectives+Rule;
    article[body]=...

</code>
If you break this request up along the lines suggested by rails, then it seems that this isn't a RESTful request. It's POSTing something, which means its creating a new resource, but that resource isn't going in the datatabase. Wha?

I think this is perfectly RESTful though. What we're saying is "Make me a new preview of the article described in the body of the request". The POST part of the request implies 'make something', the rest of the request describes what to make. Our controller knows that articles created for preview are not persistent, so they don't go in the database. The preview you create and then throw away is just as much a resource as the finished article, the only difference being that every preview gets thrown away.

Looked at like this, it's apparent RESTful programming is much richer than the basic 4 CRUD actions. The important principles to bear in mind are:

<dl>
<dt>
GET must not alter any resource

</dt>
<dd>
This is the really, really, really big one. If you're ever tempted to write a method that interprets a GET request and changes the state of the resource being fetched... well, think again.

</dd>
<dt>
Statelessness

</dt>
<dd>
This is the idea that the request itself contains all the information the server needs to build the response. No session cookies. There are those who argue for no cookies at all, but until browsers handle HTTP authentication better I think we're stuck with cookie based authentication, but the cookie should really be self contained and not an opaque reference into a catch all session table.

</dd>
<dt>
`POST`, `PUT` and `DELETE` should act appropriately

</dt>
<dd>
`POST` makes new stuff, `PUT` changes old stuff, `DELETE` removes old stuff. Not that hard to understand. The only wrinkle is that browsers don't know how to make `PUT` and `DELETE` requests. Your AJAX actions will make them happily, but if use them in a form field, the browser will be confused. Because you can't simply use JavaScript everywhere, you're stuck tunneling those request types through POST. Rails has mechanisms to hide the tunnelling from you.

</dd>
<dt>
Persistent URLs

</dt>
<dd>
If you GET something from `/some/where/or/other` today, you should be able to get it there tomorrow unless it's been deleted. If the resource has been moved, the right thing to do is to respond with a redirect (permanent or temporary as appropriate). Serving up an entirely new resource is right out. Which doesn't mean you can't have a resource like `/articles/at_random` - the resource you're getting isn't the article you receive, but 'a randomly chosen article'. What would be bad would be having `/articles/at_random` not expire for 10 days.

</dd>
</dl>
### "You haven't mentioned adjectives yet..."

This article is a write up of something that occurred to me during a conversation after Monday's LRUG meeting. Someone (whose name I either didn't get or forgot) asked me what was the point of using RESTful principles in the first place. During the course of my answer, it suddenly clicked that what Rails calls actions aren't really actions; they are adjectives. So, when I sat down to write this, I was expecting that that's what I was going to say. But the trouble with late night pub conversation insights is that they aren't always accurate or complete. As I wrote, I realised that my basic insight that an `:action` isn't really an action was okay, but an `:action` is actually a view (I think it might be a controller when you're POSTing though, just not a Rails controller).

### "I'm curious now, what is the point of using RESTful principles?"

RESTful programming is a discipline. It's a set of practices and patterns that help you make good decisions about how to solve problems in your application domain in a consistent way. The hope is that, by being rigorous in your application of the principles, you'll end up with a website that's easy to use, easy to cache and easy to scale. Certainly many RESTful practices are just out and out good practices.

There are other disciplines. [Seaside](http://www.seaside.st) is one. As Avi has [shown](http://www.windley.com/archives/2007/03/applied_web_heresies_etech_2007.shtml) recently, Seaside isn't a smalltalk application framework, it's a state of mind, and a weird one at that.

### A thought

Here's a little something to consider:

#### Thesis

Rails

#### Antithesis

Seaside

#### Synthesis

I dunno, but I'm looking forward to it.

### Coming up

I plan to take a closer look at some of the principles and patterns of RESTful architecture and discuss how to apply them in Rails.

[1] Almost certainly not exactly what happens, but close enough for jazz.

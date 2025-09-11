+++
title = "Things which are fun"
slug = "things-which-are-fun"
date = "2007-05-08T02:12:00+00:00"
draft = false

+++

It's definitely fun to commit a major rework of something that's been bugging you to SVN. It's slightly less fun to check it out in your production server and have it fall over until you remember you to retweak the environment.rb file.

Anyhow, we're now running on Typo 4.1.1+r1438. [Typo 4.1.1](http://blog.typosphere.org/articles/2007/05/08/its-here-its-new-its-improved-its) got released last night and, pretty much as soon as it had cleared the gates I checked in a major rework of the Article and Feedback state mechanisms.

### What state mechanisms?

When you're modelling blog posts, and comments, it's very easy to think of them in terms of simple status flags. An article is published, or its a draft, so we have a `#published` boolean and we're done.

Except, what about an article that you want to publish at a specific time? And how do we work out when to send notifications and trackback pings? Should you send the pings again when an article is first published then withdrawn, then published again?

It's complicated, and you can end up with a rats nest of complex conditional code.

So, you work out that an article can be in one of several states. We went with `:new`, `:draft`, `:just_published`, `:published`, `:publication_pending`, `:just_withdrawn` and `:withdrawn`, and we used the State Pattern to handle this. All our tangled conditionals were replaced with simple delegations to the state object. There's still *some* conditional code, but there's a great deal less of it now.

Until recently, we implemented the states using Rails's `composed_of` helper, but it's not really suited to the task - you know your code isn't happy when you find yourself calling a class `ContentState::Factory` and overriding its `new` method. It was also really hard to divine the workings of the state machine.

So, during a discussion of [Coding without ifs](http://giantrobots.thoughtbot.com/2007/5/1/coding-without-ifs) I found myself sketching a way of setting up a state field declaratively and thought to myself "Hey, I could implement that for Typo and make things a good deal clearer..."

So I did.

Now, if you look at the top of `article.rb`, you'll find:

<code>

<pre>
has\_state(:state,
:valid\_states =&gt; \[:new, :draft,
:publication\_pending, :just\_published, :published,
:just\_withdrawn, :withdrawn\],
:initial\_state =&gt; :new,
:handles =&gt; \[:withdraw,
:post\_trigger,
:after\_save, :send\_pings, :send\_notifications,
:published\_at=, :published=, :just\_published?\])

include States
</code>

And it sets up the delegations (as well as a set of `#new?`, `#draft?`, ... predicates based on the names of the valid states) for you. I pulled the States module out into a separate `article/states.rb` file, but given a simple set of states there's no reason not to declare them inline.

I'm really pleased with it. I'll probably do a bit more work on the interface (I want it to be more language like), add a `has_strategy` declaration (which I intend to use for our feedback spam checking system) and then extract a plugin from it. If you're interested in doing something similar in your own code right now, the thing you need to pinch is `lib/stateful.rb` in the Typo distribution, and the only documentation is the source. Check `app/models/feedback/states.rb` and `app/models/article/states.rb` for examples of states written to this interface.

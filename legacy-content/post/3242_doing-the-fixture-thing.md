+++
title = "Doing the fixture thing"
slug = "doing-the-fixture-thing"
date = "2007-08-05T02:39:00+00:00"
draft = false

+++

Fixtures suck! Mocks rock! Don't you *dare* let your tests touch the database!

Well... yes... I suppose. Except, mocking can be a complete pain in the arse too (made slightly less of a pain in the arse if you use the null object options) - it's awfully easy to end up with huge long setup methods that spend all their time faking out a mock object and about two lines testing what you need.

I'm sure someone'll be along to argue that this is evidence of lazy design on my part. Well, "Yah boo sucks!" to the lot of 'em. Fixtures can be exceedingly useful.

### Spooky action at a distance

The biggest problem I have with Rails' current implementation of fixtures is... oh, where to start... it's probably the action at a distance aspect of them. Your test is here, but your fixture is defined some way over there, in a random collection of yaml files.

Then there's the problem of remembering *which* fixtures you need to have loaded for a particular test, and it all starts getting horrible very quickly.

[One way](http://errtheblog.com/post/7708) of addressing this is to use fixture scenarios and fixture scenario builder, which works around the problem of remembering what to load and means you don't have to write your fixtures in YAML. What it doesn't work around is the action at a distance issue (but I'm betting it wouldn't be too hard to repurpose the fixture scenario builder to let you declare the fixtures you need for a particular test/spec in the same place as you do the testing).

At work though, we came up with another way of making it easy to build your fixtures right in the test file. Our current approach is still a little bit clunky, and it's nowhere near the point where I can turn it into a library, but I think it's worth discussing anyway.

### Exemplars

The question to ask is, what do you use a fixture for? Most of the time, what a test needs is a mostly generic instance of your class which will pass the validations, and which has maybe a couple of attributes set to particular values.

Let's say you have a user class. As is common with such things, your user has a username, an email address and a password. As is so often the case, the usernames and email addresses must be unique, and the password must not be blank. Let's say you're working on a tagging system (isn't everyone). Here's the sort of specification you might write:

<code>

<pre>
context "A taggable object" do
setup do
\# makes a taggable object and a couple of users
end

it "Should aggregate taggings" do
`first_user.tag(`taggable, "tag")
`second_user.tag(`taggable, "tag")

`taggable.save!; `taggable.reload

@taggable.should have(1).tags
end
end
</code>

In this context, the only thing you need from those user objects is that they behave like @users, are distinct and valid. You could simply set up a couple of users your users.yml fixture, but the approach we took at work went something like this:

<code>

<pre>
class User
class &lt;&lt; self
`@exemplar_count = 0
    def exemplar(overrides = {})
      ``exemplar_count += 1
      with_options(:username => "user#{``exemplar_count}", 
                    :email => "user#{`@exemplar\_count}",
:password =&gt; "fredisabadpassword") do |maker|
maker.new(overrides)
end
end

def create\_exemplar(overrides = {})
returning(exemplar(overrides)) {|user| user.save}
end

def create\_exemplar!(overrides = {})
returning(exemplar(overrides)) {|user| user.save!}
end
end
end
</code>

This lets us write setup code like:

<code>

<pre>
setup do
`first_user = User.create_exemplar!
  `second\_user = User.create\_exemplar!
@taggable = ...
end
</code>

In tests where you need an exemplar with a specific property, you can write `Model.exemplar(:tested_attribute => specific_value)` - the way the overrides work means you only have to describe the 'interesting' bits and the obscuring dust involved in simply building a valid object is swept under the carpet.

### Homework

-   If you're familiar with the [Object Mother](http://www.martinfowler.com/bliki/ObjectMother.html) pattern, this might seem a little familiar, with the wrinkle that, instead of having a factory class, we just push the exemplar builder directly onto the model class.

<!-- -->

-   If you start implementing exemplars yourself, you'll probably spot a good deal of repetitive coding. I've not extracted a library yet because I've not quite come up with an interface that I like. Can you come up with a good way of doing it? Can you implement it?

<!-- -->

-   What did I miss?


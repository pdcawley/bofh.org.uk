+++
title = "Maybe time to look at shoulda"
slug = "maybe-time-to-look-at-shoulda"
date = "2008-12-11T06:24:00+00:00"
draft = false

+++

So, because I'm sure that there's a better way of drying up my rails apps, I've been porting [Magritte](http://www.lukas-renggli.ch/smalltalk/magritte) to Ruby (given a good meta-description of your models and judicious application of the visitor and interpreter patterns, it's amazing what you can do). Now, Magritte comes with a decent test suite in its Smalltalk box. However, that test suite makes serious use of inheritance. Several tests of the leaf classes in the Magritte description hierarchy define maybe three helper methods which parameterise the tests they inherit from their parent test suites.

It's all very clever (but in a good way), but it's a bugger to implement in Rspec. I've been reduced to writing a shared behaviours file with lots of blocks that look like:

```ruby
shared_examples_for "MetaDesc::Base" do
  ...
end

shared_examples_for "MetaDesc::Description" do
  ...
  it_should_behave_like "MetaDesc::Base"
end

shared_examples_for "MetaDesc::ElementDescription" do
  ...
  it_should_behave_like "MetaDesc::Description
end
```

Essentially, I'm stitching together an inheritance hierarchy by hand.

Maybe it's time to go back to <code>Test::Unit</code> and maybe to try <code>shoulda</code>

I'll keep you posted on how the Magritte port's going. Next step after the basic port is to write a visitor to generate an ActiveRecord schema I think, but I might end up writing some kind of pidgin for describing objects first---depends how much of pain it is to roll the description objects by hand.

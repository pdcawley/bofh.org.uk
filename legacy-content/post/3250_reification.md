+++
title = "Today's noun is: Reification"
slug = "reification"
date = "2007-08-22T18:22:00+00:00"
draft = false

+++

> Reification: The mental conversion of a person or abstract concept into a
> thing. Also, depersonalization, esp. such as Marx thought was due to capitalist
> industrialization in which the worker is considered as the quantifiable labour
> factor in production or as a commodity. - OED

In the sense that the OED has it, I'm not what you could call a fan of
reification. At work, we have a rule that anybody who starts talking about
'resources' when they mean 'people' gets a (verbal) slap.

However, in OO circles (or maybe just in my head), reification is a
good thing. It's the process of taking something abstract and turning it into
a 'real' object. Usually, the word gets used for big things like turning an
intractable method into an object as a step on the way to refactoring that
method. I tend to use it in a slightly broader sense. For me, reification is
the process of turning something (a method or a data structure usually) into a
full blown object with its own behaviour.

Back when I was working on Pixie (a cunning, but weird, object persistence tool
written in Perl) we had a data structure which was used for keeping track of
managed objects. It started life as a hash. Everything was fine at first, but over
time we ended up with more and more code being repeated across the codebase
that was concerned with manipulating the cache hash. So, we replaced the
hash with a new object and pulled all the repeated code into methods on that
object, which gave us cleaner code to extend, and a strong feeling that we
should have turned the cache into an object much earlier in the game. (By
leaving it so long, we had a lot more code to move about, some of it in fairly
obscure places; tracking down the last bit took a while.)

Data structures like hashes and arrays are really useful in languages that have
them. The catch is, they have this habit of acquiring code. When this
starts to happen, it's time to reify - to replace the hash with a task specific
object. In Ruby, it's easy enough to inherit from `Hash`, but Hash comes with a
pile of methods that probably aren't relevant to your particular
need. Generally it's better to delegate. The first cut doesn't have to be that
complicated, just decorate the hash with a new class and initialize an instance
of the class at the point where you had just made the hash.

Once that's done, you can go through your code and move the bits that treat the
hash as a data structure onto your new class. As you gather all the common
behaviour to the new class, you'll start to see places where you can improve
code quality by merging common behaviours, replacing complex conditionals with
polymorphism (you'll probably have to introduce a factory method if you do
that) and pulling hash keys out into instance variables.

### Stalled reification

Reifying your data structure isn't an end in itself, it's a step along the way
as you refactor your code.

There's an example of a stalled reification to be found in
ActionController::Routing::Resources. Consider the implementations of
`map_resource` and `map_singleton_resource`, which are the worker methods used
whenever you do a `map.resource` or `map.resources` in your `routes.rb`.

<code>

    def map_resource(entities, options = {}, &block)
      resource = Resource.new(entities, options)

      with_options :controller => resource.controller do |map|
        map_collection_actions(map, resource)
        map_default_collection_actions(map, resource)
        map_new_actions(map, resource)
        map_member_actions(map, resource)

        if block_given?
          with_options(:path_prefix => resource.nesting_path_prefix, &block)
        end
      end
    end

    def map_singleton_resource(entities, options = {}, &block)
      resource = SingletonResource.new(entities, options)

      with_options :controller => resource.controller do |map|
        map_collection_actions(map, resource)
        map_default_singleton_actions(map, resource)
        map_new_actions(map, resource)
        map_member_actions(map, resource)

        if block_given?
          with_options(:path_prefix => resource.nesting_path_prefix, &block)
        end
      end
    end

</code>

There's a lot of repetition there. The only differences are the classes of the
resource object, name of the second function called in the `with_options`
block. If we take a look at, `map_collection_actions` things start to look even
fishier. Here's `map_collection_actions`, for example:

<code>

    def map_collection_actions(map, resource)
      resource.collection_methods.each do |method, actions|
        actions.each do |action|
          action_options = action_options_for(action, resource, method)
          map.named_route("#{resource.name_prefix}#{action}_#{resource.plural}", "#{resource.path};#{action}", action_options)
          map.named_route("formatted_#{resource.name_prefix}#{action}_#{resource.plural}", "#{resource.path}.:format;#{action}", action_options)
        end
      end
    end

</code>

`resource.collection_methods.each`? Let's see what happens if we the various
`map_foo_actions` methods into methods on `ActionController::Resources::Resource`. While
we're about it, we can rename `map_default_collection_actions` to
`map_default_actions` on Resource, and `map_default_singleton_actions` to
`map_default_actions` on SingletonResource, which inherits from
Resource. `map_collection_actions` becomes:

<code>

    def map_collection_actions(map)
      collection_methods.each do |method, actions|
        actions.each do |action|
          map.with_options(action_options_for(action, method)) do |m|
            m.named_route("#{name_prefix}#{action}_#{plural}",
                          "#{path};#{action}")
            m.named_route("formatted_#{name_prefix}#{action}_#{plural},
                          "#{path}.:format;#{action}")
          end
        end
      end
    end

</code>

(we move `action_options_for` onto resource as well, of course).

Once we've moved the various mapping helpers onto the resource classes, we can
revisit `map_resource` and `map_singleton_resource`

<code>

    def map_resource(entities, options={}, &block)
      resource = Resource.new(entities, options)

      with_options(:controller => resource_controller) do |map|
        resource.map_collection_actions(map)
        resource.map_default_actions(map)
        resource.map_new_actions(map)
        resource.map_member_actions(map)
      end

      if block_given?
        with_options(:path_prefix => resource.nesting_path_prefix, &block)
      end
    end

    def map_singleton_resource(entities, options={}, &block)
      resource = SingletonResource.new(entities, options)

      with_options(:controller => resource.controller) do |map|
        resource.map_collection_actions(map)
        resource.map_default_actions(map)
        resource.map_new_actions(map)
        resource.map_member_actions(map)
      end

      if block_given?
        with_options(:path_prefix => resource.nesting_path_prefix, &block)
      end
    end

</code>

And now, we no longer have two method bodies that look very similar, apart from
the resource class, we have to methods that look *identical* apart from the
resource class. So, if we pull out the common bits and put them onto Resource,
like so:

<code>

    class ActionController::Resources::Resource
      def install_routes_in(map, &block)
        map.with_options(:controller => controller) do |m|
          map_collection_actions(m)
          map_default_actions(m)
          map_new_actions(m)
          map_member_actions(m)
        end

        if block_given?
          map.with_options(:path_prefix => nesting_path_prefix, &block)
        end
      end
    end

</code>

Then `map_resource` and `map_singleton_resource` become

<code>

    def map_resource(entities, options = {}, &block)
      Resource.new(entities, options).install_routes_in(self)
    end

    def map_singleton_resource(entities, options = {}, &block)
      SingletonResource.new(entities, options).install_routes_in(self)
    end

</code>

### Where's the benefit?

Apart from making the `active_record/lib/resources.rb` a bit shorter (a
laudable result in itself), where's the benefit here?

From my own experience of implementing `datestamped_resource`, a routing
plugin that we use in Typo, it makes the life of anyone writing a resource like
routing helper for Rails a great deal easier. With `datestamped_resource` I
ended up subclassing ActionController::Resources::Resource, doing the
refactoring I've outlined here, but leaving the original Rails methods where
they were and just implementing the 'moved' methods on DatestampedResource
(well, not quite, `map_collection_actions` is pretty different from the
default Resource implementation, but the other actions are pretty much the
same.

In another project I'm working on, I'm trying to retain meaningful urls with
(potentially) deep resource nesting, and it'd be really handy to have an
`inflected_resource` route helper. The problem with using a meaningful
`to_param` on your models is, avoiding permalinks that share a name with your
actions. You could set up validations so that, say, 'new' is an illegal
permalinks, but it's clumsy.

However, if you arrange things so that your URLs are inflected, you can always
tell that a URL that begins `/resource/new` will be a particular resource, with the permalink 'new', and `/resources/new` will be the virtual new resource.

If the
resource system is factored as I outlined, this is almost trivial, you can
introduce a InflectedResource subclass of Resource

<code>

    class InflectedResource < Resource
      def member_path
        @new_path ||= #{path_prefix}/#{singular}/:id
      end
    end

</code>

and you're pretty much done. Admittedly, something like that (plus a small
amount of copy and paste) would work with the current system, but then we're
looking at 3 substantially identical methods in ActionController::Resources and
if it wasn't time to refactor before, it'd definitely be time to refactor
then.

### Conclusions

Reification shouldn't be something you do every day, but nor should it be
something you do once a flood. Take a look at some of your projects and some of
the places where you're using hashes. Are those *really* hashes, or would they
benefit from having some behaviour of their own? You can track down stalled
reification by looking for anaemic classes; classes which have a lot of
accessors but very little behaviour. Once you've found an anaemic class, look
for all the places that instances of it get used. Try moving some of the client
code into methods on your anaemic class. Do that a few times and you'll end up
with a real object.

If you're fussy about never putting HTML in your models, you could end up with
a mediating builder/presenter object as well, but until you start wanting to
render the same structured info in different formats, I'd suggest biting the
bullet and living with HTML in the model as a lesser evil than structural
code. Your mileage may vary.

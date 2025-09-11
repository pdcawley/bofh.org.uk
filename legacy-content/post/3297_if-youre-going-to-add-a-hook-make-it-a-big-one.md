+++
title = "If you're going to add a hook, make it a big one"
slug = "if-youre-going-to-add-a-hook-make-it-a-big-one"
date = "2008-09-08T01:11:00+00:00"
draft = false

+++

Jay Fields [responds to](http://blog.jayfields.com/2008/09/domain-specific-languages-dont-follow.html) on Ola Bini's [Evil Hook Methods?](http://olabini.com/blog/2008/09/evil-hook-methods/) about the common ruby idiom that lets us write:

    class Fruit
      include DataMapper::Resource
      property :id, Integer, :serial => true
      property :name, String
      property :notes, Text, :lazy => false
    end

What Ola and Jay don't like about that is the way that a single `include DataMapper::Resource` actually adds class methods to Fruit because the implementation of `DataMapper::Resource.included` looks like:

    module DataMapper::Resource
      def included(module)
        module.send :include, InstanceMethods
        module.send :extend, ClassMethods
      end
    end

Which is a perfectly common idiom nowadays, but which breaks <code>include</code>'s contract in annoying ways. Jay proposes fixing this by adding a `become` method to Object which would wrap the `include` and `extend` in such away that they'd be called by the including class. Huzzah. And it makes sense... sort of. But it really doesn't go far enough.

Let's take another look at the original code snippet shall we? The thing that I notice is the wide scope of that 'property' method. It really isn't needed anywhere except for defining how a `Fruit` is mapped onto the database. What happens if we take a leaf out of Perl's book:

    class Fruit
      use DataMapper::Resource {
        property :id, Integer, :serial => true
        property :name, String
        property :notes, Text, :lazy => false
      }
      property :foo # => raises an exception
    end

The block gives our extending module somewhere to play, it can introduce a full on domain specific pidgin for the duration of the block with no fear of polluting the including class with anything but the methods its contracted to provide. So, how do we implement <code>use</code>. Something like the following should serve the purpose:

    class Module
      def self.use(mod, *args, &block)
        mod.used_by(self, *args, &block)
      end

      def self.used_by(mod, *args, &block)
        if instance_behaviours || class_behaviours
          mod.become(self)
        else
          mod.send(:include, self)
        end
      end

      def self.become(mod)
        include mod.instance_behaviours) if mod.instance_behaviours
        extend mod.class_behaviours if mod.class_behaviours
      end

      def self.instance_behaviours
        nil
      end

      def self.class_behaviours
        nil
      end
    end

The key idea here is that, in the default case, `use` will ignore all its arguments beyond the first and just include that module (a more robust implementation would probably ensure that an exception was raised if any extra arguments got passed). If the module author had written her module to comply with Jay's proposed `become`, then we simply call `become`.

The interesting stuff happens when a module wants to do something a little more trick. So a version of DataMapper might do something like:

    class DataMapper::Resource
      def self.used_by(mod, &block)
        mod.become build_behaviours(mod, &block)
      end
    end

And `build_behaviours` would `instance_eval` the block with an object that would capture the properties and use them to build a set of class and instance methods appropriate to the description.

Another module might simply take a hash to describe how things should be parameterized. It all depends on the needs of the module being used. The aim being to avoid polluting the caller's namespace any more than necessary. If I use a DataMapper type package, then all I want to end up with in my client classes are appropriate instance accessor methods, I don't need spare class methods like `property` or `storage_names` that are only of any use when I'm describing my class.

### Updates

I edited one of the code snippets to remove a particularly heinous piece of brace matching. Thanks to Giles Bowkett for the catch. Also edited another snippet to make it into real ruby rather than some bastard combination of Ruby and Perl. Thanks to Yossef for that catch.

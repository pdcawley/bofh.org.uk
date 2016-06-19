+++
title = "Get sophisticated"
slug = "get-sophisticated"
date = "2008-08-20T11:25:00+00:00"
draft = false

+++

Ruby's primitives (Strings, Hashes, Arrays, Numbers - anything that has a literal syntax) are fine things. But that doesn't mean you should use them everywhere. You're often much better off wrapping them up in your own Value Objects.

Something I was working on at Railscamp this weekend threw up a great example of why it makes sense to replace primitives with more specific objects as soon as possible. [Tom Morris](http://tommorris.org/) asked me to take a look at [Rena](http://github.com/tommorris/rena), an RDF/Semantic Web thingy.

The RDF spec describes two types of literals: a plain literal, which is a string with an optional language attribute and a typed literal, which is a string and an encoding (so the string might represent an integer, float, or anything else that your schema feels like expressing).

These literals can be output in one of (at least) two formats. We'll start by looking at <code>Literal\#to\_trix</code> and see where that takes us:

``` ruby
def to_trix
  if @lang != nil && @lang != ""
    out = ""
  else
    out = ""
  end
  out += @contents
  out += ""
  return out
end
```

If we look over at <code>TypedLiteral\#to\_trix</code> we see a much more straightforward implementation:

``` ruby
def to_trix
  "#{@contents}"
end
```

How do we eliminate that ugly conditional at the beginning of `Literal#to_trix`, and analogous conditionals in `Literal#to_n3` and `TypedLiteral#to_n3`?

My first thought was that I wanted to be able to write something like:

``` ruby
def to_trix
  "#{@contents}"
end
```

But I didn't want every string in the world suddenly acquiring a `to_trix` method. So, the solution was to intoduce a `Literal::Language` class and coerce our language into it, so `Literal#initialize` became:

``` ruby
def initialize(contents, lang = nil)
  @contents = contents
  @lang = Language.coerce(lang)
end  
```

And Language would look something like:

``` ruby
def self.coerce(lang)
  if lang.is_a?(self)
    return lang
  end

  new(lang.to_s.downcase)
end

def initialize(lang)
  @value = lang
end

def to_trix
  if @value == ''
    ''
  else
    " xml:lang=\"#{@value}\""
  end
end    
```

That ugly conditional's still there though, so we introduced the Null Object pattern, and things started to look a good deal cleaner:

``` ruby
class Language
  class Null
    include Singleton

    def to_trix
      ''
    end
  end

  def self.coerce(lang)
    case lang
    when self
      return lang
    when nil, ''
      return Null.instance
    else
      return new(lang.to_s.downcase)
    end
  end

  ...

  def to_trix
    " xml:lang=\"#{@value}\""
  end
end
```

At this point, we're still just pushing code around. If anything, we've got more lines of code now than when we started, but we're starting to move behaviour nearer to the data it relates to, and our objects are starting to look like objects rather than data structures. So, we press on and make a `TypedLiteral::Encoding` class and, at this point things start to look interesting. TypedLiteral is starting to look almost exactly the same as Literal, but with an Encoding rather than a language.

That strange leading space in `Language#to_trix` is starting bug me. Let's rewrite like so:

``` ruby
class Literal 
  class Language
    def format_as_trix(literal)
      "#{literal}"
    end

    class Null
      def format_as_trix(literal)
        "#{literal}"
      end
    end
  end

  def to_trix
    @lang.format_as_trix(@contents)
  end
end
```

If we make analogous change to `TypedLiteral` and `TypedLiteral::Encoding` it's obvious that TypedLiteral and Literal were essentially the same class. Renaming <code>`lang</code> and <code>`encoding</code> to <code>`language_or_encoding</code> makes this blindingly obvious, so we'll remove all of TypedLiteral's methods except initialize. All that remains is to introduce `Literal.untyped@ and `Literal.typed` factory methods to Literal, and make `Literal.new` into a private method and we can remove TypedLiteral in its entireity. So we change the specs to reflect the new API (wrong way round I know). Now we have a chunk of shorter, clearer code that will hopefully be easier to extend to cope with outputting literals in other formats.

### Retrospective

I realise that patterns aren't the goal of development, but by the end of the process we have a Strategy (Language/Encoding), a couple of Factory Methods (`Literal.typed`, `Literal.untyped`) and a couple of factoryish methods (`Language.coerce`, `Encoding.coerce`).

The most important aspect of the change was the introduction of the two new value object classes. Once they were introduced, they became the obvious places in which to put the varying behaviour and eliminate the repeatition of conditional code from the <code>to\_\*</code> methods. If there were to be a third output style, I would look at introducing classes like `N3Stream`, `TrixStream` and `WhateverStream` and have a scheme like:

``` ruby
def to_n3
  print_on( N3Stream.new )
end

def print_on(stream)
  language_or_encoding.print_on(stream, value)
end
```

but that's almost certainly over complicating things right now.

The other thing I like about this kind of refactoring is that it drives the code towards methods and classes which obey the single responsibility principle and, at the end of the process, not only do we have fewer lines of code in total, but the individual methods involved are *all* substantially shorter and closer to the left hand margin.

I really should start doing this kind of thing more in my Rails practice - I keep being put off by the fact that the `composed_of` helper is so annoyingly not quite right and, rather than submitting a patch or making a plugin I go "Ah well... I can live with a string for a bit longer..." and I *know*. From hard won experience at that, that it's going to come and bite me. It's already bitten Rails recently when Ruby got a new `String#to_chars` which doesn't work like the ActiveSupport version.

### Notes

If you want to see the gory details of how the change got made, Tom has merged this weekends changes into the github repository. It didn't happen in quite the order I've described it in this post, but neither is this post a complete fabrication.

### Changes

Corrected a stupid typo in the first block of code. Ugly condition is actually <code>if `lang != nil && `lang != ''

</pre>
</code>

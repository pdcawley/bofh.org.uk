+++
title = "Today's Noun is: Reticence"
slug = "todays-noun-is-reticence"
date = "2007-09-27T18:30:29+00:00"
draft = false

+++

What does the OED say reticence is?

> *Reticence*: Maintenance of silence; avoidance of saying too much or of speaking freely; disposition to say little.

Pretty straightforward. When I chose reticence as one of my [five nouns for programmers](http://www.bofh.org.uk/articles/2007/08/20/five-nouns-for-programmers) it was another reminder that objects are not the same as datastructures. Well designed objects keep their cards (instance variables) close to their chest. Client code tells objects what to do, it doesn't ask them to kiss and tell. In [Smalltalk Best Practice Patterns](amazon:013476904X), (you don't have a copy yet? Are you mad?) Kent Beck recommends that you put your accessor methods in the `private` protocol unless you have a *very* good reason for indicating that the accessors should be used by clients by putting the accessors in, say, the `accessing` protocol or some other, more suitably named, protocol. (In Smalltalk, all methods are public, but you can and should organize them into protocols/categories, either by choosing some existing protocol, or coming up with a new protocol name). In less flexible OO languages, you should probably at least mark your accessors as protected unless you have the aforementioned very good reason.

### What about parameter objects then?

One obvious 'exception' to the rule of reticence is the parameter object. Say you have an unwieldy method that takes a bunch of arguments and fills the screen with its implementation. Being a conscientious programmer, you want to apply the 'composed method' pattern to the method so you'll still end up with a screen's worth of code, possibly more, but the individual methods will be much more focussed in what they achieve. What stops you is that bunch of parameters which are used through out the method. So, you introduce a parameter object. Bundle up the parameters into a single object, replace each parameter variable in the body of the code with an accessor call on the parameter object, and have at it. You can extract methods easily and you'll only ever have to pass a single object.

It's tempting not to bother creating a 'real' object, it's very easy to just use an options hash, a la Ruby on Rails. And, because the options hash is a common pattern through your code, you can start adding helper methods like `with_options`, and you'll get an awful lot of leverage out of it.

However, what's often missed in discussions of the parameter object pattern is that it's a waystation, not a destination. It may be handy, but if you persist in treating it as a datastructure, you're missing out on the good stuff. Now you have a parameter object, you can start to move *behaviour* onto the parameter object and before you know it you'll end up with a real object doing real, testable (mockable?), work.

### Homework

Find a method where you're using an options hash. Try using that hash to build a parameter object and then apply the principle of reticence. What happens to it? Where does the behaviour go? If, like me, you've come to OO from procedural coding styles, making your objects reticent is not a natural thing to do, it can feel extremely odd. But the more you practice it, the better you'll get at thinking in a truly object oriented fashion. Sometimes an options hash really is the way to go, but not as often as you'd think.

Try it, maybe you'll be converted.

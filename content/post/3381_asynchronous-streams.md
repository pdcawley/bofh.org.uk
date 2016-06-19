+++
title = "Asynchronous Streams"
slug = "asynchronous-streams"
date = "2010-11-24T04:46:00+00:00"
draft = false

+++

<p>
In <a href="http://www.bofh.org.uk/2010/11/18/higher-order-javascript">Higher Order Javascript</a>, I introduced Streams and showed how to use them to implement a
lazy sort. I think that's neat all by itself, but it's not directly useful in
the asynchronous, event driven execution environment that is the average web
page. We'd like a structure where we spend less time twiddling our thumbs as
we wait for <code>force</code> to return something to us.

</p>
<div id="outline-container-1" class="outline-2">
<h3>
Non blocking streams

</h3>
<div class="outline-text-2">
<p>
What if we change the protocol of our stream to something more asynchronous?
Obviously, we'd still have a <code>head</code> element which is immediately available and
some kind of promise to compute the next stream. But rather than promising to
<i>return</i> a new stream, the promise of a non-blocking stream is a promise to
call a function we supply with the value it computes. Here's a
<a href="http://coffeescript.com/">CoffeeScript</a> implementation of what we're
talking about:

</p>

```coffeescript
the_empty_stream =
    is_empty: true

continue_with = (b) -> window.setTimeout((=> b()), 0)

class NonBlockingStream
  constructor: (@head, promise) -> 
    this.force_into = (block) ->
      if promise.length == 0
        continue_with -> block promise()
      else
        continue_with -> promise block
  is_empty: false
```

<p>
I've written <code>NonBlockingStream</code> to work with both 0 argument functions (like
the promise of an ordinary stream) and with 1 argument promises that will be
responsible for calling their block when appropriate. <code>force_into</code> doesn't
block by virtue of the fact that we execute our promise using
<code>setTimeout</code> -- a timeout of 0 milliseconds doesn't mean execute the block
immediately but asks for it to be executed as soon as possible. The fat arrow
(`=>`) used to build the function passed to setTimeout ensures that the
code will be executed with this bound to the stream instead of the global
window object.


This new non blocking stream is a much better citizen on a webpage. It yields
to the event loop at every opportunity and gets out the way of other, possibly
more important events. But so far we know how to use it for things like
finding primes or the top 5 entries of 1000. Not exactly useful in the
browser…

### Streams in the real world 

Listen to any web usability guru for more than about ten minutes and they'll
tell you that pagination is evil. If you have a resource (say a blog index
page) that logically should have 1000 entries on it, then breaking it up into
multiple pages is a sin. The reader should simply be able to scroll through
all 1000 entries. But most users don't scroll through every entry, and
rendering 1000 entries is time consuming. Sites like Google reader and
Twitter solve this by doing 'just in time' fetching. 


Suppose we wanted to re-engineer this blog to use the endless page pattern,
then I might think of using streams. We'd like to serve up an index page that
sets up all the headers and navigational stuff, but which populates its
articles via an unbounded stream of articles. Let's
say that an article looks something like this:

```html
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <title>The current article</title>
    <link rel="next" href="the-next-article.xhtml" />
  </head>
  <body>
    <article>
      <h2>The current article</h2>
      <p>Yada yada yada...</p>
    </article>
  </body>
</html>
```

<p>
You can think of this as a kind of stream. The 'head' of the stream is the
contents of the <code>html</code> body tag, and the promise is the <code>&lt;link rel="next" ... /&gt;</code> tag in the <code>head</code>. Let's write a function which, given a document like
this will make us a stream. We'll use jQuery because, well, why not?

</p>


```coffeescript
$ = jQuery

doc2stream = (data) ->
  new NonBlockingStream $("body", data), (block) ->
    next = $("head link[rel=next]", data).attr('href')
    if next
      $.ajax
        url: next
        success: (data) -> block(doc2stream data)
        dataType: 'xml'
    else
      block the_empty_stream
```

<p>
Note that our promise doesn't call the block it's forced with
immediately. Instead it fires off an asynchronous request for the next
article in the stream with a success callback that (finally) calls the
block.

</p>
</div>
<div class="outline-3">
<h3>
Putting it together

</h3>
<div class="outline-text-3">
<p>
The trick now is to get the stream up and running from our article
index. Let's assume we have an initial page along the lines of:

</p>

```html
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <title>Some blog articles</title>
    <link rel="start" href="the-first-article" />
    <script 
       type="text/javascript"  
       src="http://code.jquery.com/jquery.min.js"></script>
  </head>
  <body>
    <header>
      <h1>Some blog articles</h1>
    </header>
    <section id="articles">
      <footer>
        <p>If you can see this, I'm probably fetching a real article</p>
      </footer>
    </section>
    <script
       type="text/javascript"
       src=".../asynch-fetcher.js"></script>      
    <footer>
      ...
    </footer>

  </body>
</html>
```

<p>
This is where we shove all the navigational bits and pieces of our
blog, the links to atom feeds, sidebars with associate links and
whatever. But it's devoid of content. We need to fix that by finishing off our
asynch-fetcher.coffee<sup><a class="footref" name="asynch-streams-fnr.1" href="#asynch-streams-fn.1">1</a></sup>. First, we need to setup our stream:

</p>

```coffeescript
articles = new NonBlockingStream '', (block) -> 
  $.ajax
    url: $("head link[rel=first]").attr('href')
    success: (data) -> block doc2stream data
    dataType: 'xml'
```

<p>
We can't simply call <code>doc2stream</code> because that expects to find the link to the
next article in a <code>[rel=next]</code> link in the head, but we're using a 'first'
link here. So we make a stream with the empty string a as a dummy head and a
promise to fetch the article linked to by our <code>[rel=first]</code> and turn that into
a stream via `doc2stream`

</p>
<p>
Next we need a function to update the <code>articles</code> variable and extract the
article element from the head of a stream and insert it in our `#articles` section just before the footer. Once this is defined we force the first
article into it.

</p>

```coffeescript
show_next = (stream) ->
  articles = stream
  $("#articles footer").before(
    $("article", stream.head).clone().addClass("last-art")
  )

articles.force_into show_next
```

<p>
We also arrange for <code>show_next</code> to add a <code>last-art</code> class to the newly
inserted article, which we'll use as a target in the watcher function we set
up to handle fetching new articles as they are needed:

</p>

```coffeescript
$.fn.is_in_view = () ->
  $(this).position().top < ($("body").scrollTop() + $(window).height())

watcher = ->
  last_art = $(".last-art")
  if last_art.is_in_view()
    last_art.removeClass('last-art');
    articles.force_into (str) -> 
      unless str.is_empty
        show_next str
        window.setTimeout watcher, 100
  else
    window.setTimeout watcher, 100

window.setTimeout watcher, 100
```

<p>
Our heuristic for judging when to fetch the next article is simple: if the
article tagged with the <code>last-art</code> class is in the viewport, then it's time to
go about fetching the next one. This assumes that our writing is compelling
enough that by the time the reader gets to the bottom of an article, the next
one will have been succesfully fetched. This may be an optimistic heuristic,
but we're all about "for the purposes of illustration" here.

</p>
<p>
To get this to work, we add a simple <code>is_in_view</code> method to <code>jQuery.fn</code>. This
tests if the selected element's top is placed higher than the bottom of the
viewport. With that in place, we can write <code>watcher</code> which checks if the last
article is in view. If it when it is, watcher removes the <code>last-art</code> marker class and
kicks off the process of fetching the next article. We use <code>window.setTimeout</code> to
ensure that we keep fetching next articles as long as their are articles to
fetch and our reader is reading them.<sup><a class="footref" name="asynch-streams-fnr.2" href="#asynch-streams-fn.2">2</a></sup>

</p>
</div>
</div>
</div>
<div class="outline-2">
<h3>
Notes

</h3>
<div class="outline-text-2">
<ul>
<li>
A similar stream could be set up for each article's comments, after which
we might find that we should parameterize <code>show_next</code> and <code>watcher</code> in some
fashion.

</li>
<li>
Real world use would probably involve serving up a few articles in the body
of a blog front page - if only so that Google has something to index. tag
indices or search results could be served up empty and populated on
demand.

</li>
<li>
I've not (yet) redone this blog to use this pattern, but I've tested the
code presented here and it does work.

</li>
<li>
For caching purposes, it may better for searchs and the like to return
a small lump of JSON with a <code>head</code> link to the statically cached article document
and a <code>promise</code> link to the next lump of JSON in the results. If we write
doc2stream right, it should be possible to completely isolate the rest of
the page from this decision, which seems like a win to me.

</li>
</ul>
</div>
</div>
<div id="asynch-streams-footnotes">
<h3 class="footnotes">
Footnotes:

</h3>
<div id="asynch-streams-text-footnotes">
<p class="footnote">
<sup><a class="footnum" name="asynch-streams-fn.1" href="#asynch-streams-fnr.1">1</a></sup> Which our server autogenerates from the coffeescript we're actually writing.

</p>
<p class="footnote">
<sup><a class="footnum" name="asynch-streams-fn.2" href="#asynch-streams-fnr.2">2</a></sup> Or, at least, scrolling past them.

</p>
</div>
</div>


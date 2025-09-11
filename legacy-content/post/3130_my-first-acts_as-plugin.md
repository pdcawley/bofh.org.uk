+++
title = "My first 'acts_as' plugin"
slug = "my-first-acts_as-plugin"
date = "2007-01-23T17:39:00+00:00"
draft = false

+++

So, you've upgraded to Rails 1.2.1 and you're working on a tool to maintain a database of all the tunes you have in your various songbooks and (eventually) your record collection. You start with:

$ ./script/generate rspec\_resource MusicBook title:string author\_id:integer \\
abstract:text
$ ./script/generate rspec\_resource Tune title:string composer\_id:integer \\
abc:text book\_id:integer

You decide to come back to composers and authors later, so you set up your models\[1\]:

MusicBook.has\_many :tunes
Tune.belongs\_to :music\_book

And your routes:

map.resource :music\_books do |book|
book.resource :tunes
end

### Problems start here

Being a cautious sort, before you start adding behaviour, you fire up a development server and go and check things with the browser. The `/music_books/` stuff works fine, but once you start looking at `/music_books/1/tunes` things start to get weird; all of a sudden your links aren't making sense.

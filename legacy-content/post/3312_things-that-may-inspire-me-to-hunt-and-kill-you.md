+++
title = "Things that may inspire me to hunt and kill you"
slug = "things-that-may-inspire-me-to-hunt-and-kill-you"
date = "2009-02-26T09:24:00+00:00"
draft = false

+++

Let's say you're writing a ruby library. Something you want and expect others to use. Here's how to reduce those others to incandescent rage:

<code>

<pre>
require 'rexml'

include REXML

module MyShinyModule
...
end
</code>

No. Not no how. Not never. You just crapped in my namespace and stomped on who knows how many of my own classes. Now, `include` is a fine and dandy thing, and it certainly has its uses, but using it at the top level of your library files is not one of them.

Ahem.

Can anyone guess which library has been wasting my morning?

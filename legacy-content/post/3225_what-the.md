+++
title = "What the?"
slug = "what-the"
date = "2007-05-13T03:42:00+00:00"
draft = false

+++

Wow:

<code>

<pre>
$ rake spec
...

156 examples, no failures

$ ./script/spec spec
...

156 examples, 2 failures

$ rake
\[unit tests, all pass\]
\[functional tests, all pass\]
\[specs...\]

156 examples, 2 failures
</code>

For extra points, the 2 failures from running the plain `rake` are not the same as the failures from running `./script/spec spec`. And if I run `./script/spec spec` after a full `rake` run, I get a host of extra failures.

I wonder what I'm doing to so comprehensively screw up test isolation.

Ho hum.

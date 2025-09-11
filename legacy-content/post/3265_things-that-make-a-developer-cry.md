+++
title = "Things that make a developer cry"
slug = "things-that-make-a-developer-cry"
date = "2007-11-02T09:49:00+00:00"
draft = false

+++

So, we're doing a cobranding exercise at work. The idea being we serve up a branded version of [amazing tunes](http://www.amazingtunes.com/) in a subdomain of our partner, their users get a skinned version of the site that feels like part of the partner's site, we get an influx of new users and everybody is happy. One aspect of this is we're using the partner's site to handle authentication.

Today, we got all our ducks in a row and started authenticating against the partner's SOAP service as part of our user testing. So off I went to the partner site and set up an account...

At amazing tunes, we're pretty scrupulous about password security, we never store plaintext passwords, any request that involves a password being sent is done over an https connection. It's just the right thing to do.

After I'd finished setting up my account on the partner's website I was presented with a screen that looked something like:

<blockquote>
Your username is: pdcawley

Your password is: fucknuckle

</blockquote>
At least it was an https connection, but it doesn't exactly fill me with delight.

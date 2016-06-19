+++
title = "A sketch of declarative ActiveRecord Migrations"
slug = "a-sketch-of-declarative-activerecord-migrations"
date = "2006-11-24T06:18:00+00:00"
draft = false

+++

Writing migrations can get pretty tedious when you're being scrupulous about writing both the up and the down side of the migration. Okay, so the [Textmate](http://macromates.com/) ninjas amongst you can use scarily clever snippets to populate the `down` migration while you write the `up` method, but I can't be the only Mac user who still prefers Emacs. And not everyone gets to run on Macs either.

So, inspired by something Jamis Buck wrote about [designing a DSL](http://weblog.jamisbuck.org/2006/11/13/designing-a-dsl), I've been sketching out a DSL for describing the easy parts of a migration in declarative style. None of this is implemented yet, but I'm pretty sure that it's relatively simple to implement for a decent Ruby metaprogrammer. I'm brain dumping it here so I can come back to it later, or, you never know, someone might have implemented it by the time I revisit...

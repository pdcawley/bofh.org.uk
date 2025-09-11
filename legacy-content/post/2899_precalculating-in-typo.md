+++
title = "Precalculating in Typo"
slug = "precalculating-in-typo"
date = "2006-10-30T02:09:00+00:00"
draft = false

+++

You may or may not know that Typo is multiblog capable under its hood. There is a `blogs` table in the database and every single request ends up fetching the blog object by doing a `Blog.find_by_base_url` query.

We don't have UI support for multiblogging though, and the *vast* majority of Typo installations have exactly one blog object. Even when multiblogging comes along, there won't be that many blogs in any given installation.

The problem is, the instance caching tool we're currently using only caches objects that are fetched using `Model.find(<i>id</i>)`, so we end up with a basic page hit finding the blog by searching for the base url and then, later, fetching it again using the id.

One option is to fix the caching mechanism.

But that's too much like hard work. So, in the latest checking on the typo trunk I've fixed things so that `environment.rb` builds a hash which maps from base urls to blog ids. So now, the first thing we do with any incoming request is use the hash to find the id of our blog object and then do `Blog.find(_id_)` *et voila* we've fetched our blog and populated the instance cache as well. God is in his heaven, all is right with the world, and we're doing one less query per page hit.

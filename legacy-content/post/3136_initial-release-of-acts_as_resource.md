+++
title = "Initial release of acts_as_resource"
slug = "initial-release-of-acts_as_resource"
date = "2007-01-25T12:56:00+00:00"
draft = false

+++

Right. I've bundled `acts_as_resource` up and stuck it on the typosphere SVN server. You can grab it from http://svn.typosphere.org/typo/plugins/acts\_as\_resource if you're interested.

It's currently in what I'd call an all convention, no configuration state - if your resources don't look pretty similar to the kind of things you get from the resource scaffolding, you'll probably have some pain, but I expect to rectify that with coming releases. One thing I want/need to do for instance is to allow for 'relative' ids in your resource url. For instance, if you're looking at `/albums/10/tracks/982`, it's not the most readable of permalinks... next trick is to allow you to have urls like `/albums/because-its-there/tracks/1`, ie: the first track on the album 'Because it's There'. I'm sort of expecting that you'd do that by doing:

class Album
has\_many :tracks
acts\_as\_resource :uri\_field =&gt; :name\_dasherized
end

class Track
belongs\_to :album
acts\_as\_list
acts\_as\_resource :uri\_field =&gt; :position, :parent =&gt; :album
end

However, my first priority is to add some tests (or, more likely,Rspec specifications) so I've got some confidence that I'm not breaking things as I go.

Anyhow, go grab the plugin, have a play, let me know what you think.

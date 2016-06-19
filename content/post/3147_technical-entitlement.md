+++
title = "Technical Entitlement"
slug = "technical-entitlement"
date = "2007-01-30T13:17:00+00:00"
draft = false

+++

At RailsConf Europe this year, [DHH](http://www.loudthinking.com/) went in quite strong on the idea that simply using an opensource framework, like, say, [Rails](http://www.rubyonrails.com) didn't entitle you to much of anything. The precise phrase used was, if memory serves "We don't owe you shit."

Which is why I'm finding a certain irony in this, from the [official Rails weblog](http://weblog.rubyonrails.com/2007/1/29/using-sqlite3-with-rails):

> Versions \[of SQLite\] after 3.3.7 incompatibly changed the way in which default values are stored, making it so that current versions of Rails get into quoting issues and problems with columns with NULL defaults . . . hopefully this will be recognized as an unacceptable change for a point release and will be reverted.

There appears to be a fair amount of confusion going on in the responses to Jamis's [bug report](http://www.sqlite.org/cvstrac/tktview?tn=2203) on the SQLite trac. It seems a Rails user has already reported the bug in the changed behaviour of 3.3.8, but the fix for it seems to be differently incompatible with Rails' expectations...

This one could run for a while I think.

+++
categories = []
date = "2016-09-27T11:40:07+01:00"
description = ""
draft = false
image = "/img/about-bg.jpg"
title = "MacOS Sierra Niggles"

+++

I shouldn't be surprised; every time I upgrade the OS on my Macbook a few
little behaviours I'd taken for granted start breaking. It's just the way of
the world. Here's a couple of things I've fixed already

### Respect my `.zshenv` ###

MacOS comes with a tool called `path_helper`. It's great. It sets up more or
less sane `PATH` and `MANPATH` environment variables based on the contents of
`/etc/paths.d` and `/etc/manpaths.d` respectively. This means that the install
step of an application that includes some command line tools that really
shouldn't be dropped in the existing `/usr/bin` can keep those tools in, say,
`/opt/toolname/bin` and drop a file in `/etc/paths.d` and `/opt/toolname/bin`
will appear on everyone's default path. Very handy.

The default shell rc files that get run when a shell starts up call
`path_helper` and bam, you have a sane path.

Well... yeah... except in `zsh`. There are two global zsh startup files in
`/etc`, `/etc/zshenv` and `/etc/zshrc`. When zsh starts up it sources a bunch
of files to set up the environment. At the very least, it uses `/etc/zshenv`
and `$HOME/.zshenv`. If the shell is interactive, it also uses `/etc/zshrc`
and `$HOME/.zshrc`, but it _always_ loads `zshenv` and `~/.zshenv`.

So... Like a good zsh user, my `PATH` and other environment variables are set
up in my `~/.zshenv`. As well as all the system paths, there's a bunch of
stuff in my home directory that wants to be on the path as well, and `.zshenv`
is the place to do that.

But, everytime I upgrade MacOS I forget that whoever is responsible for the
global zsh startup files at Apple is, not to put too fine a point on it, a
fucking idiot. Because as well as running `path_helper` in `/etc/zshenv`,
where it makes sense, this upstanding member of society sees fit to run it
again in `/etc/zshrc`. Which would be fine if it weren't for the fact that the
very first thing `path_helper` does is nuke the existing path. Which throws
away all my path customisations from `~/.zshenv` and leaves me wondering what
on earth just happened until I remember the zsh startup sequence and fix
`/etc/zshrc` by deleting the `path_helper` stanza.

If you're being driven up the wall by this, then this is what my `/etc/zshrc`
looks like now:

```sh
# Correctly display UTF-8 with combining characters.
if [ "$TERM_PROGRAM" = "Apple_Terminal" ]; then
        setopt combiningchars
fi

disable log
```

I hope you find it useful (and even if you don't, I'm sure I'll find it useful
come the next upgrade).

### Making SSH and the keyring play together again ###

For some reason now, when my laptop wakes from sleep, my `ssh-agent` has
forgotten some of my authentication keys that were added via the system
keychain. After a while, I got bored of typing `ssh-add -A` every time I
failed to ssh into a host and installed `sleepwatcher`
via [Homebrew](http://brew.sh) like so:

```
$ brew install sleepwatcher
$ brew services start sleepwatcher
$ echo <<EOF > ~/.wakeup
#!/bin/bash

ssh-add -A
EOF
$ chmod +x ~/.wakeup
```

which fixed that.

### Further niggles ###

Those are the only two things that I've noticed so far in the Sierra change
that have annoyed me enough to find out how to fix them. If any more crop up,
I'll update this page. If any more command line niggles crop up, I'll update
this page with any fixes I find.

+++
title = "The authentication tarpit"
slug = "the-authentication-tarpit"
date = "2008-01-29T05:19:00+00:00"
draft = false

+++

At work, we're looking at adding the Atom Publishing Protocol in a few places where it makes sense. APP's got a lot going for it - the spec is a great example of how to design a Resourceful API and is worth reading even if it's not an immediately good fit for your application.

### But...

It's one of the givens of good application security that you don't store passwords in clear text and you do your level best not to send them over the wire in cleartext. That way, if someone pinches your user database, they should have their work cut out for them if they want to find out what your password is (because, unless you're very good or are using something like [1password](http://1passwd.com/), you probably use the same password for lots of different websites).

A decent authentication protocol should ensure that the clear text of the password is never sent over the wire and doesn't need to be stored in the clear on the server. Also, it shouldn't be subject to reply attacks. One way to do this is to use SSL for authenticated sessions and rely on that protocol's encryption to solve the problem of the password going over the wire in the clear. Or you could use the standard HTDigest authentication method. The basic trick with systems that don't send the plain password over the wire works a little like this:

Alice and Bob have agreed a secret password and a hashing algorithm.

Alice wants to prove to Bob that a particular request comes from her, so she comes up with a unique 'nonce' string. She then concatenates this string with the agreed secret, and generates a digest string using the agreed hashing algorithm. She attaches the nonce string and the resulting digest to her request. When Bob receives the request, he concatenates the nonce string with the agreed secret, runs it through the hashing algorithm and, if he gets the same digest value as the one attached to the request, then it's very probable that Alice is the real requester.

There are variations with different protocols, of course, but the general rule is to send a set of inputs and a result that can only derived from the results by someone who knows the agreed secret. It's the sort of thing your bank does when you phone them: "Can I have you postcode? Surname and initial? What's the first letter of your password? And the last letter? Your memorable address?" The theory is that only you know how to answer those last 3 questions, but at no point are you required to say "My password is 'flapdoodle'" loudly and clearly in a crowded restaurant. Also, it ensures that the bank employee doesn't get to see your whole password either. Where the banks fall down is when they phone you and immediately try to take you through the security questions without doing anything to prove that they are who they claim to be.

The HTDigest protocol has a neat little wrinkle in its hashing algorithm. Instead of generating the digest directly from a combination of the user identifier[1], password and nonce string, it generates an intermediate digest from the user identifier and password, and then uses the same hashing algorithm to calculate a digest from this intermediate result and the nonce string. This means that the server can store the intermediate result instead of the plaintext password.

The problem with implementing the Atom Publishing Protocol is that one of the client apps that we want to support, Nokia's LifeBlog mobile app, *only* supports the adaptation of the WSSE UserToken authentication protocol [recommended](http://www.xml.com/pub/a/2003/12/17/dive.html) by Marc Pilgrim. There's lots to like about this protocol, especially the way it allows CGI based servers to take control of authentication without needing access to Apache's `.htaccess` or requiring `mod_digest` to be installed. However, the design of the protocol is such that there's no way to avoid storing the user's password in plain text on the server. Which we really, really, really don't want to have to do.

Mutter. Grumble. Chunter. Bloody WS-\* - biting the big one again.

[1] The user identifier is a combination of a username and a 'realm', a little like the way that email addresses often take the form `username`domain@

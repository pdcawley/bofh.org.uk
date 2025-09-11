+++
title = "Domain Specific Pidgin"
slug = "domain-specific-pidgin"
date = "2007-08-08T02:43:00+00:00"
draft = false

+++

So, I'm busily writing an article about implementing an embedded little language in Ruby. It's not something that's going to need an entirely new parser, it borrows Ruby's grammar/syntax but does some pretty language like things to the semantics and ends up feeling far more like at declarative language than the usual Ruby imperative OO style.

Because I tend to [chromatic's view](http://www.oreillynet.com/onlamp/blog/2007/05/the_is_it_a_dsl_or_an_api_ten.html) of many ruby programmers' ability to cry -"Wolf![]("- "DSL)", I don't want to claim that it's a full blown Domain Specific Language, but it's sufficiently language like that 'API' doesn't seem to fit as a description either.

Then it hit me... it's a pidgin.

A pidgin can be thought of as a mashup of two languages, taking vocabulary from both its parents and its grammar (usually simplified) from one parent. Historically, pidgins have arisen to help with trade and colonization; grammars have tended to be lifted and simplified from the 'native' language and then spiked with words from the colonizing language with a leavening of native words where they make sense. All quite politically incorrect nowadays, but they served their purpose. Pidgins are, by their nature, domain specific; fine if you wanted to talk trade or order your coolies about, but not what you'd write poetry in. Poetry tended to get written in the creoles that evolved from some pidgins. A creole is a general purpose language, with a grammar of its own; they seem to evolve from pidgin, getting invented by the kids of parents who speak that pidgin.

In my little language, much of the vocabulary pinched from my problem domain's language and the grammar and some terminology is lifted from ruby. Casting the problem domain as the colonial power and ruby as the native language, it's obvious that I've invented a pidgin language.

Let's embrace that term. We're not writing domain specific languages, we're writing pidgins. ActiveRecord's family of class methods isn't a DSL, it's database pidgin. RSpec is a testing pidgin, Parsec is a parsing pidgin, so are any number of APIs that make their host language feel like a new one.

### Updates

In the comments, Aristotle Pagaltzis points out that Parse::RecDescent isn't a parsing pidgin because it uses a full blown parser (itself, obviously) to parse any grammar declarations.

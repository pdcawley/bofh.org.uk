+++
title = "Things which aren't fun"
slug = "things-which-arent-fun"
date = "2007-05-07T09:13:00+00:00"
draft = false

+++

Let's say you're running tests against your rails application and a test fails.

"Hmm..." you think, "I wonder what could be causing that, let's run that test file by itself."

The test passes.

"Okay... I wonder if it's the rake test loader, let's run that test file using that."

The test passes

"Right, it's only the third test file on the list that's failing, let's try running just the first 3 test files."

The test fails.

"Okay, so it's one of the first two test files that's causing the problem. Let's remove one and see what happens."

The test passes.

"Right, because I'm paranoid, let's try running it with the other test file instead. It should fail."

The test passes.

"Hmm... the failure appears to be fixture related, let's try turning of `use_transactional_fixtures` in `test_helper.rb` and running the failing set of tests again."

The test passes.

### Houston, we have a problem

Part of our problem is that Typo is so huge and old, and its test suite is somewhat spotty. Something we're doing in the workings of a couple of our controllers is confusing transactional fixtures and that can't be good. I suppose my first step is to get all the tests that are still failing with transactional fixtures turned off passing and see if that solves my transactional problem, but I can't pretend it's a fun option.

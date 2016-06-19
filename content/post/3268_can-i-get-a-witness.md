+++
title = "Can I get a witness?"
slug = "can-i-get-a-witness"
date = "2007-11-19T13:17:21+00:00"
draft = false

+++

> Worrying about test coverage when you're doing Test- or Behaviour-driven development is like worrying about the price of fish in Zimbabwe when you're flying a kite.

Your tests are there to help you discover your interface and to provide you with an ongoing stream of small bugs to fix. If you write them cleanly, and keep them well factored (you are refactoring your tests, aren't you?) they will help to document your intent too. Ensuring that every code path is exercised might be intellectually satisfying but that satisfaction costs time, and time is money. And that's before you start worrying about your code's malleability. Cover the happy path and the edge cases you *know* how to deal with and move on. If you've screwed something up, it will get found during acceptance testing (or out in the wild) and you can write a few more tests to isolate the problem, fix it, and move on.

If you deliberately add a test that passes without requiring you to write another line of code, ask yourself why you bothered with it. The test isn't isolating a bug or specifying new behaviour. If you're lucky, it merely confirms something you already know, and if you're unlucky, you just introduced a bug in your test suite. Better to move on and either pull a new bug off the queue, or turn a feature request into a new bug - "Feature X doesn't work!" - fix it, refactor and move on to the next. Further down the road, you may discover that the feature you were about to exhaustively test doesn't even need to be there, or maybe it works differently than you expected. Aren't you glad you didn't worry about 100% coverage then? Maybe you will need to revisit the tests and cover more cases in the future. But future you knows more about the problem domain and can do a better job of working out what the behaviour needs to be. And if future you is likely to do a worse job than you right now, you may have bigger problems than the code to worry about.

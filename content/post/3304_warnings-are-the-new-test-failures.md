+++
title = "Warnings are the new test failures"
slug = "warnings-are-the-new-test-failures"
date = "2008-12-18T10:10:00+00:00"
draft = false

+++

<p>
Have you ever tried to run Rails, Rspec, Rake or, for that matter almost any Ruby library or application that you've ever heard of with the -w flag? How about running Rails with taint checking on?

</p>
<p>
They aren't exactly pleasant experiences.

</p>
<p>
Meanwhile, over in Perl land, it's a very rare module indeed that isn't at least clean under -w and, where appropriate, taint friendly. It would be a very irresponsible Perl Monger indeed who wrote a web framework that didn't assume it was going to be running under the -T flag. Warnings and taint checking are annoyances, and sometimes they're flat out wrong, but more of the time they're useful. Which is why, in Perl, you'll sometimes see blocks of code like:

</p>
<code lang="perl">

    {
      no warnings;
      # Code that does stuff which would trigger a warning
    }

</code>

<p>
If the author is being particularly careful, she will specify which warnings to suppress - after all, there's no need to turn off all the warnings if all you're intending to do is redefine a method. So the prudent Perl programmer would write:

</p>
<code lang="perl">

    {
      no warnings 'redefine';
      # Code that redefines an existing method
    }

</code>

<p>
However, there are often ways of achieving your aim even with all the warnings on.

</p>
<p>
If modules that don't have <code>use warnings</code> are rare on CPAN, modules that don't have <code>use strict</code> will get the unwary programmer laughed at in the street. There are modules that simply won't work under <code>use strict</code>, but they tend to have <code>no strict</code>, either wrapped around the narrowest scope that won't work under strict, or proudly displayed up front. The presence of a <code>no strict</code> implies to the interested reader that the programmer knows (or thinks he knows) what he's doing. Writing code that does even the most implausible metaprogramming things without raising errors from strict or spamming STDERR with warnings is a matter of professional pride. Writing straightforward code that stays silent is the absolute baseline for Perl programming professionalism in my book.

</p>
<p>
Meanwhile, here in Rubyworld, there's no equivalent of <code>strict</code> and it's actively hard to start coding with warnings turned on because important frameworks like rspec and rails aren't <code>-w</code> clean. In Perl, this isn't a problem, <code>use warnings</code> turns warnings on lexically. Your code might well call all sorts of noisy code elsewhere but, unless you're running with <code>~~w</code> as well, you'll only see the warnings for <em>your</em> code. If you set <code>$VERBOSE</code>, you'll get all the warnings. Warnings in the log file should be like red Fs in your test output~~ a sign that all is not as good as it could be in your code. Sure you <em>could</em> just ignore the ones you know are harmless, then you're in danger of losing the <em>real</em> problems in the noise.

</p>
<p>
As a gesture of goodwill, here's <code>alias\_method\_chain</code> written so it should raise no warnings except when the 'without' method already exists.

</p>
<code lang="ruby">

    def alias_method_chain(target, feature)
      aliased_target, punctuation = target.to_s.sub(/([?!=])$/, ''), $1
      yield(aliased_target, punctuation) if block_given?

      with_method, without_method = "#{aliased_target}_with_#{feature}#{punctuation", "#{aliased_target}_without_#{feature}#{punctuation}"

      alias_method without_method, target
      remove_method target # Warning begone!
      alias_method target, with_method

      case
      when public_method_defined?(without_method)
        public target
      when protected_method_defined?(without_method)
        protected target
      when private_method_defined?(without_method)
        private_target
      end
    end

</code>

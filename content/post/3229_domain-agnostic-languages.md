+++
title = "Domain Agnostic Languages"
slug = "domain-agnostic-languages"
date = "2007-05-19T18:52:00+00:00"
draft = false

+++

Windmill tilting time again I'm afraid. Blame [chromatic](http://www.oreillynet.com/onlamp/blog/2007/05/the_is_it_a_dsl_or_an_api_ten.html) and [David A. Black](http://dablog.rubypal.com/2007/4/17/the-l-in-dsl-langue-ou-langage).

What is it that characterizes domain specific languages? Before you trot out something like "Programs written in them read like domain experts talk", take a look at some examples of code written in domain specific languages:

<code>

    /(?ms)"((?>[^\\"]+)?(?>\\.[^\\"]*)*)/

    S3
    R$*                     $: < $1 >
    R$+ < $* >                 < $2 >
    R< $* > $+                 < $1 >
    R<>                     $@ < @ >
    R< $+ >                 $: $1
    R@ $+ , $+        @ $1 : $2
    R@ $+ : $+         $@ <@ $1> : $2
    R$+ @ $+                $: $1 <@ $2>
    R$+ < $+ @ $+ >            $1 $2 <@ $3>
    R$+ <@ $+ >             $@ $1 <@ $2>

    >gi|2501594|sp|Q57997|Y577_METJA PROTEIN MJ0577
    MSVMYKKILYPTDFSETAEIALKHVKAFKTLKAEEVILLHVIDEREIKKRDIFSLLLGVAGLNKSVEEFE
    NELKNKLTEEAKNKMENIKKELEDVGFKVKDIIVVGIPHEEIVKIAEDEGVDIIIMGSHGKTNLKEILLG
    SVTENVIKKSNKPVLVVKRKNS

</code>
If you're reading this on the front page, try and work out what the ones you recognise do before you dip below the fold...

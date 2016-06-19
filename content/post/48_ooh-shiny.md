+++
title = "Ooh, shiny!"
slug = "ooh-shiny"
date = "2004-11-27T11:04:28+00:00"
draft = false

+++

Now I'm a larval maths teacher, I have to set homework, and that means I have choices:

1.  I can use the homework associated with the textbooks we use
2.  I can cut and paste past exam papers and things together
3.  I can write my own homework assignments which I can either
    1.  Write out longhand and use a photocopier
    2.  Generate in electronic form and print out

The first two options aren't exactly bad, and I've used both of 'em before now, but the resulting homework sheets can look scruffy and, well, inconsistent. My gut feeling is that kids respond better if, not only do you have high expectations of them, but you treat them with respect. A mongrel collection of homework assignments with no consistent style to them doesn't say "The teacher has made an effort, so I should make one too".

Ideally, I would set my own homework. I'd write it out longhand in a beautifully legible italic script which survives the rigours of photocopying and teaches by example the correct way to set out mathematical work.

So, I'm using a typesetting system. The geeks among you will be unsurprised to discover I've settled on TeX (actually, LaTeX for now, but I'm giving ConTeXt a serious look) and, after a certain amount of swearing I've got it so that I can use Tekton for my body copy and for numbers within mathematical expressions, which means I'm producing good looking, readable and consistent homework assignments for my pupils.

But that's not what's shiny. What's shiny is that I've just installed the latest version of `auctex` for Emacs and added its sister package, `"preview-latex":http://preview-latex.sourceforge.net/` and it's like magic -- the preview thing detects any expressions in the text, chucks 'em at LaTeX then hides the input text with a rendered image. If you move the cursor back to the expression, it hides the graphics and lets you edit the input again. It's really, really lovely. I recommend it to you wholeheartedly.

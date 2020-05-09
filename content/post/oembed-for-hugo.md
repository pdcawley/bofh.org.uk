+++
title = "Adding a generic oembed handler for Hugo"
author = ["Piers Cawley"]
lastmod = 2020-05-08T22:00:27+01:00
slug = "oembed-for-hugo"
tags = ["hugo"]
draft = true
+++

If you're at all like me, you have content on a bunch of different sites (Instagram, Youtube, Flickr, Soundcloud, Bandcamp...) and, especially for multimedia content, it's great to be able to link to 'live' versions of that content. Of course, all those sites will let you 'share' content and usually have an 'embed' option that hands you a bunch of HTML that you can paste into your blog entry. But screw that! I'm a programmer for whom laziness is one of the cardinal virtues -- if it's at all possible, I prefer to let the computer do the work for me.{{< marginnote >}}If nothing else, once I've got the programming right, it's less likely to screw up than me{{< /marginnote >}}

Hugo[^fn:1] sort of supports this out of the box with its `youtube`, `instagram`, `vimeo` etc. built in shortcodes. The thing is, they're not lazy enough -- you have to dig into each URL to extract a content ID and pass that in to `{{%/* youtube kb-Aq6S4QZQ */%}}` or whatever. Which would be kind of fine, if you weren't used to the way sites like Facebook, Twitter, Tumblr and so on work. With those sites, you enter a URL and they disappear off behind the scenes and fill in a fancy preview of the page you linked to. Why can't Hugo do that?

<!--more-->

Well, it can. It just takes a little work.[^fn:2] The `question` to ask is how do all those user friendly sites do there thing? Twitter and Facebook, being the walled garden behemoths that they are do it by dictating two different microformats{{% marginnote %}}
Of bloody course!
{{%/marginnote%}} that live in a page's `HEAD` section. The microformat approach has a good deal to be said for it: In theory, you can just make a `HEAD` request to the URL you're interested in, parse out the microformat of your choice and build your own media card. I've not worked out how to do this yet though. However, before Twitter and FB started throwing their weight around, there was an open standard that lots of sites support, it's _really_ easy to use. It's called [oembed](https://oembed.com/) and it's great. The idea is that it too is discoverable via a `HEAD` request to your media page. You look for something matching `<link rel="alternate" type="application/json+oembed" href="..." ...>`, make a JSON request to the `href` url and paste in the contents of the `html` key in the object you get back. The catch, of course, is that you still end up having to parse the document's HEAD.

The cool thing about `oembed`, though, is that you _can_ discover its endpoints that way,{{<marginnote>}}Though, I'm seeing fewer and fewer oembed links cropping up in sites that I _know_ support the protocol{{</marginnote>}} but there's also a big list of known endpoints on the [Oembed homepage](https://oembed.com/), which is also available as a big old JSON object if you want to go the full programmatic route. There are JavaScript libraries available that will walk your webpage and the JSON object and replace all your links with chunks of embedded content, that that's what I used to use on this site. But... that's not how I currently roll at Just A Summary. There are currently no `<script>` tags to be found on here and I plan to keep it that way. So I wrote a Hugo shortcode. Here it is:

<a id="code-snippet--Initial embed shortcode"></a>
```go-html-template
{{ $url := (.Get 0) }}
{{- range $.Site.Data.embed }}
  {{- if le 1 ( findRE .pattern $url | len ) }}
    {{- with (getJSON .endpoint "?" (querify "format" "json" "url" $url)) }}
      {{ .html | safeHTML }}
    {{ end }}
  {{ end }}
{{ end }}
```

<div class="src-block-caption">
  <span class="src-block-number"><a href="#code-snippet--Initial embed shortcode">Code Snippet 1</a></span>:
  Initial embed shortcode
</div>

We use it like: `{{</* embed "https://youtub.be/kb-Aq6S4QZQ" */>}}`, which displays like this:

{{< embed "https://youtu.be/kb-Aq6S4QZQ" />}}

{{% newthought %}}"But how does it work?"{{% /newthought %}} I hear you ask? It works in conjunction with some per-site data entries that I've added to the directory `data/embed` in this site's base directory. You might have guessed that the data entries are maps with two entries, a `pattern` and an `endpoint`. If the URL argument matches the `.pattern`, then we make a `getJSON` request to `.endpoint` with a sanitised version of the URL argument tacked on as our query string and inserting the JSON response's `.html` entry. {{< marginnote >}}It's rather tricky to implement oembed for on a strictly static site, but I love the simplicity of it. I have a few thoughts abotu that though. Watch this space.{{</marginnote>}}.

I made the data files by taking the big JSON object from https://oembed.com/providers.json and massaging the supplied patterns into regular expressions. In theory, I could write a script to do the conversion for me, but I'm only really interested in four providers for this site, so I just did it by hand. So the entry for [Instagram](https://instagram.com/):

```json
{
    "provider_name": "Instagram",
    "provider_url": "https:\/\/instagram.com",
    "endpoints": [
        {
            "schemes": [
                "http:\/\/instagram.com\/*\/p\/*,",
                "http:\/\/www.instagram.com\/*\/p\/*,",
                "https:\/\/instagram.com\/*\/p\/*,",
                "https:\/\/www.instagram.com\/*\/p\/*,",
                "http:\/\/instagram.com\/p\/*",
                "http:\/\/instagr.am\/p\/*",
                "http:\/\/www.instagram.com\/p\/*",
                "http:\/\/www.instagr.am\/p\/*",
                "https:\/\/instagram.com\/p\/*",
                "https:\/\/instagr.am\/p\/*",
                "https:\/\/www.instagram.com\/p\/*",
                "https:\/\/www.instagr.am\/p\/*",
                "http:\/\/instagram.com\/tv\/*",
                "http:\/\/instagr.am\/tv\/*",
                "http:\/\/www.instagram.com\/tv\/*",
                "http:\/\/www.instagr.am\/tv\/*",
                "https:\/\/instagram.com\/tv\/*",
                "https:\/\/instagr.am\/tv\/*",
                "https:\/\/www.instagram.com\/tv\/*",
                "https:\/\/www.instagr.am\/tv\/*"
            ],
            "url": "https:\/\/api.instagram.com\/oembed",
            "formats": [
                "json"
            ]
        }
    ]
}
```

<div class="src-block-caption">
  <span class="src-block-number">Code Snippet 2</span>:
  The <a href="https://oembed.com/providers.json">https://oembed.com/providers.json</a> entry for Instagram
</div>

becomes

<a id="code-snippet--instagram.yaml"></a>
```yaml
endpoint: "https://api.instagram.com/oembed/"
pattern: "^https?://(www\\.)?instagr(\\.am|am\\.com)/((.*/)?p/|tv/)"
```

<div class="src-block-caption">
  <span class="src-block-number"><a href="#code-snippet--instagram.yaml">Code Snippet 3</a></span>:
  <code>./data/embed/instagram.yaml</code>
</div>

Collapsing all those `scheme` entries down to a single regular expression was a slight pain to do by hand, and I'm not _entirely_ sure the regular expression will match exactly what the schemes match, but it's not broken on any of the Instagram links I've thrown at it so far, so that's good enough for me.

{{< newthought >}}This isn't the shortcode's final form{{< /newthought >}} -- it's not as robust as I'd like it to be in the face of a missing or temporarily down oembed endpoint, so it would be good to have some kind of fallback in case an endpoint changes or goes away. Also, there are some sites that have their own methods for embedding previews, which don't support oembed {{< marginnote >}}All those IndieWeb sites that use `h-entry` and `h-card` microformats to make the webpage machine parseable, for instance.{{< /marginnote >}} and it would be great to get at those somehow. I suspect I will end up with a shortcode which is essentially a big case statement dispatching to different partials which will handle the real rendering. Again... watch this space

[^fn:1]: [Hugo](https://gohugo.io) is the static site generator I use to build this blog. Another example of letting the computer do all the fiddly repetitive bits. In this case, to handle all the fiddly bits of writing full HTML pages, building index pages and the rest.
[^fn:2]: It's also annoyingly temperamental at the moment; I'm working on that though.

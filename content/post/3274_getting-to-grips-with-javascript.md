+++
title = "Getting to grips with Javascript"
slug = "getting-to-grips-with-javascript"
date = "2008-01-18T02:30:00+00:00"
draft = false

+++

I've been busily adding AJAX features to the work website, and I got bored of writing Form handlers. I got especially bored of attaching similar form handlers to lots of different forms on a page, so I came up with something I could attach to `document.body` and then plug in handlers for different form types as I wrote them.

So, I wrote `FormSender` and set up my event handler like so:

<code>

<pre>
FormSender.onSubmit = function (e) {
if (canDispatch(e)) {
YAHOO.util.Event.stopEvent(e);
YAHOO.util.Connect.setForm(e.target);
YAHOO.util.initHeader('Accept', 'application/javascript, application/xml');
YAHOO.util.Connect(e.target.method.toUpperCase(), e.target.action,
callbackFor(e));
}
};

jQuery(document.body).each(function () {
YAHOO.util.Event.addListener(this, "submit", FormSender.onSubmit);
});
</code>

I decided to mark any Ajax dispatchable forms using a class of 'ajax', the idea being that it would be a simple thing to check
<code>jQuery(e.target).hasClass('ajax')</code>, but there was a snag. We had two sorts of forms on our pages, forms built using <code>form\_for(..., :class =&gt; 'ajax')</code> and forms built using <code>button\_to(..., :class =&gt; 'ajax')</code>, and they attached their classes in different places. In the `form_for` case, the class was on the form tag, but in the `button_to` case, it was on the generated form's submit field. One option would be to monkey patch `button_to`, or roll my own `ajax_button_to`, but I ended up writing `canDispatch` like so:

<code>

<pre>
function canDispatch(e) {
jQuery(e.target).find(':submit').andSelf().hasClass('ajax');
}
</code>

This uses jQuery to build a list of the form, and its submit button, and then checks to see if any member of that list has the class 'ajax'.

So, we can now tell if the source of a `submit` event is a form we should be doing AJAX dispatch with. The next trick is to work out what needs to be done with the results of sending the form. One option is the Prototype trick of simply evaluating the returned javascript, but it often makes sense to keep the behaviour clientside and just have the server return a datastructure. I decided that the way to do this would be by adding a second class to a form which used a none default handler, and then keep a hash of callback constructors keyed by class. This made `callbackFor` look like:

<code>

<pre>
function callbackFor(e) {
var candidates = candidateClasses(e.target);
for (var i = 0; i &lt; candidates.length; i++) {
if (FormSender.callbacks\[candidates\[i\]\]) {
return new FormSender.callbacks\[candidates\[i\]\](e);
}
}
return new FormSender.callbacks.ajax(e);
}
</code>

`candidateClasses` is, again, a little more complex than I'd like, by virtue of the differences between `button_to` and `form_for` differences, but still reasonably straightforward, thanks to jQuery:

<code>

<pre>
function candidateClasses(element) {
return
jQuery(element).find(':submit').andSelf()
.filter('.ajax').attr('className')
.replace(/ajax/, '').trim().split(/ +/);
}
</code>

JQuery gets the form and its submit button, then selects the tag that has the 'ajax' class and pulls out the full `className` string. The `replace` gets rid of 'ajax', `trim` chops any useless whitespace off either end, and <code>split(/ +/)</code> turns it into an array of classnames. The `replace -> trim -> split` pipeline has the feel of something that must already exist in some DOM interface somewhere, but I'm not sure where, so I rolled my own.

Once we have a list of classes it's easy to just cycle through the candidates until we find one that matches a callback constructor, falling back to the default where nothing matches.

For completeness, I'll show you my current default handler, which I expect to be extending to deal with a couple more media types and, in the case of the failure handler, more failure statuses.

<code>

<pre>
FormSender.callbacks.ajax = function (e) {
var form = e.target;
this.scope = form;
};

FormSender.callbacks.ajax.prototype.success = function (o) {
switch (o.getResponseHeader\['Content-Type'\].replace(/;.\*/, '')) {
case 'application/javascript':
case 'application/x-javascript':
case 'text/javascript':
eval(o.responseText);
break;
default:
YAHOO.log("Can't handle AJAX response of type " + o.getResponseHeader\['Content-Type'\]);
}
};

FormSender.callbacks.ajax.prototype.failure = function (o) {
switch (o.status) {
case 401:
Authenticator.loginThenSubmit(this);
break;
default:
switch (o.getResponseHeader\['Content-Type'\].replace(/;.\*/, '')) {
case 'application/javascript':
case 'application/x-javascript':
case 'text/javascript':
eval(o.responseText);
break;
default:
YAHOO.log("Can't handle AJAX failure response of type " + o.getResponseHeader\['Content-Type'\]);
}
}
};
</code>

You'll notice a reference to `Authenticator.loginThenSubmit` in the 401 handler, but that's something I'll save for another day.

### A note on namespacing

Although I've been showing the various FormSender helper functions as if they were in the global namespace, in the real code they're wrapped in a function call:

<code>

<pre>
var FormSender = (function () {
var candidateClasses = function (element) {...};
var callbackFor = function (e) {...};
...
var onSubmit = function (e) {...};
return {onSubmit: onSubmit, callbacks: {}};
})();
</code>

I love the <code>(function () {...})()</code> pattern - it's a great way of keeping your paws out of the global namespace until you really, really need to.

### FormSender Benefits

Aside from the obvious benefit of drastically reducing the number of `onSubmit` event handlers registered with the browser, I found that using `FormSender` has simplified some of my response handlers. For instance, one form would get a chunk of html back from the server and would use that to replace the div that contained the form. But the new div *also* contained a form that needed to have Ajax behaviour, so a chunk of the handler code was concerned with reregistering onSubmit handlers for the new form (or forms). No fun. By switching to a single, body level, form handler, that problem simply disappears - so long as the new forms have the right class, they automatically get the appropriate behaviour. Result.

Obviously, FormSender is unobtrusive javascript, which is nice, and its pluggable nature means it's easy to extend just by writing new response handlers and registering them with the FormSender object.

### Future Directions

One obvious extension to FormSender is to pull out the meat of the `onSubmit` method into the callback object to allow for forms that don't simply send themselves to the server. Another is to wrap my head around the workings of Javascript's object model to make it easy to build handlers that don't duplicate the behaviour of the default handler through the medium of copy and pasting code...

### Your comments please?

I'm still very new to Javascript as a programming language and I'm sure I'm doing plenty of boneheaded things here. Please let me know if there's things I can do to improve this, or point me at any libraries that already cover this ground.

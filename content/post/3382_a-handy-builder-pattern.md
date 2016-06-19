+++
title = "A Handy Builder Pattern"
slug = "a-handy-builder-pattern"
date = "2010-12-31T08:17:00+00:00"
draft = false

+++

I'm working on a web service, and that means that I need to build lots and
lots of mildly different looking HTTP requests with various combinations of
headers and requested URLs. The camel's back got broken this morning when I
realised I didn't want to be writing a method called
<code>ssl_request_from_uk_with_bad_cert</code>, which builds me an HTTP::Request with a
particular combination of headers, that I can use with Plack::Test to
test our webservice. The method name describes what's wanted, but the code is
sopping wet and in desperate need of DRYing up.

My first cut was to write a simple request builder:

```perl
package RequestBuilder;
use Moose;

with 'MooseX::OneArgNew' => {
  type => 'HTTP::Request',
  init_arg => '_req',
}

has _req => (
  is => 'ro',
  isa => 'HTTP::Request',
  handles => [qw(header headers)],
);

sub ssl_request {
  my $class = shift;
  $class->new(HTTP::Request->new(
    GET => 'https://localhost/',
    [
      SSLSessionID => 'deadbeef',
    ],
  );
}

sub from_uk {
  my $self = shift;
  $self->header('X-IP-Is-UK-Combined' => 'yes');
  return $self;
}

sub with_bad_cert {
  my $self = shift;
  $self->header('SSLClientCertStatus' => 'NoClientCert');
  return $self;
}

sub final {
  my $self = shift;
  $self->_req;
}
```

Now I have a builder, I can compose meaningful fragments to build a final request.
So I might write:

```perl
my $r = RequestBuilder->ssl_request
    ->from_uk
    ->with_bad_cert
  ->final;
```

But things got sticky when I wanted to to take a valid
request and bend it out of shape to ensure that it failed correctly. I was
writing things like:

```perl
use RequestBuilder;

sub valid_request {
  my($self, $is_final) = @_;
  my $r = RequestBuilder->ssl_request->from_uk;
  return $is_final ? $r->final : $r;
}

test "we reject requests from outside the UK" => sub {
  my $self = shift;
  my $r = $self->valid_request(0)->from('Turkmenistan')->final;

  ...
};
```

which works, but is uglier than a very ugly thing indeed. What I wanted was
some way of having the finalization magically happen at the point of use but
have some way of extending any intermediate results. The interface I came up with
looks like this:

```perl
use RequestBuilder;

sub valid_request {
  build_request {
    $_->ssl_request->from_uk;
  }
}

test "we reject requests from outside the UK" => sub {
  my $self = shift;
  my $r = build_request {
    $self->valid_request->from('Turkemenistan');
  };

  ...
};
```

When the builder is returned from the outermost <code>build_request</code> block, it gets
finalized.

"How does that work then?" I hear you ask. It's quite simple, once you know
about Moose::Exporter, old fashioned perl prototypes and the Tao of
dynamic scope. We just add something like the following to the end of our
RequestBuilder:

```perl
our $building;

use Moose::Exporter;
Moose::Exporter->setup_import_methods(
  as_is => ['build_request'],
);

sub build_request (&) {
  my $block = shift;
  my $builder = do {
    local $building = 1;
    local $_ = __PACKAGE__;
    $block->();
  };
  return $building ? $builder : $builder->_req;
}
```

So, `build_request` has a `&` prototype, which means it takes a block as its
first argument. Perl treats an initial prototyped block argument as slightly magical and
doesn't require the use of the `sub` keyword (though you can use it if you
want). It then uses a `do` block to dynamically set `$building` to 1 and `$_`
to be the current `*PACKAGE*` name (because `$_` is shorter to type
than `RequestBuilder`) before calling the block to get a builder. Then it
checks whether we're still building. If we are, it returns the builder. If we
aren't, then the block was the outermost block, so `build_request` returns the
built request.

I'm not quite ready to extract this pattern into a parameterized role - I need
to make sure it's robust enough, but it's certainly something to think about
when you next need to make a builder for your tests.


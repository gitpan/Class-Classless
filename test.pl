# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'
# Time-stamp: "1999-09-22 23:50:04 MDT"
######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

BEGIN { $| = 1; print "1..8\n"; }
END {print "not ok 1\n" unless $loaded;}
use Class::Classless 1.1;
$loaded = 1;
print "ok 1\n";

###########################################################################

#$Class::Classless::Debug = 2;
$| = 1;

sub nodelist { join '.', map { "" . $_->{'NAME'} . ""} @_ }
my %nodes;
sub mknew {
  use strict;
  my($sym, @rest) = @_;
  die "Already seen $sym" if ($nodes{$sym} || 0);
  @rest = map { $nodes{$_} || die "$_ not seen" } @rest;

  my $prime = @rest ? shift(@rest) : $Class::Classless::ROOT;
  #print "Cloning $prime\n";
  my $x = $prime->polyclone(@rest);
  #print "Clone: $x\n";
  $x->{'NAME'} = uc($sym);
  $nodes{$sym} = $x;
}

my $root_list = nodelist( $Class::Classless::ROOT->ISA_TREE );
print "root_list: $root_list : ", 'ROOT' eq $root_list ? "ok 2\n" : "FAIL 2\n";

mknew('a');
mknew('b', 'a');
mknew('c', );
mknew('d', );
mknew('e', 'a', 'c' );
mknew('f', 'e');
mknew('h', 'b');
mknew('g', 'f');
mknew('i', 'd');
mknew('j', 'h', 'g', 'i');

###########################################################################

my $j_list = nodelist( $nodes{'j'}->ISA_TREE );
print "j_list: $j_list : ",
 'J.H.B.G.F.E.A.C.I.D.ROOT' eq $j_list ? "ok 3\n" : "FAIL 3\n";

###########################################################################

$nodes{'b'}{'METHODS'}{'zaz'} = sub { print "ok 4\n" };
$nodes{'j'}->zaz;

$nodes{'j'}{'METHODS'}{'const1'} = 'konwun';
$nodes{'h'}{'METHODS'}{'const2'} =
  do {my $x = 'kontoo'; bless \$x, '_deref_scalar'};
$nodes{'b'}{'METHODS'}{'const3'} =
  bless ['foo','bar','baz'], '_deref_array';
$nodes{'g'}{'METHODS'}{'const4'} = undef;

print '',
 ($nodes{'j'}->const1 eq 'konwun')
 ? "ok 5\n" : "FAIL5\n";

print '',
 ($nodes{'j'}->const2 eq 'kontoo')
 ? "ok 6\n" : "FAIL 6\n";

print '',
 (join('~', $nodes{'j'}->const3) eq 'foo~bar~baz')
 ? "ok 7\n" : "FAIL 7\n";

print '',
 (not defined($nodes{'j'}->const4))
 ? "ok 8\n" : "FAIL 8\n";



__END__


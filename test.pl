# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'
# Time-stamp: "1999-05-31 19:25:52 MDT"
######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

BEGIN { $| = 1; print "1..4\n"; }
END {print "not ok 1\n" unless $loaded;}
use Class::Classless;
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

use strict;
use warnings;

use open qw(:utf8);

use IO::File;
use HTML::Entities;

my $in_fh = IO::File->new("G302011.xml", "r");
my $out_fh = IO::File->new("blah.xml", "w");

foreach my $line (<$in_fh>) {
  print $out_fh decode_entities($line);
}


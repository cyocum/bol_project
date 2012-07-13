use strict;
use warnings;

use IO::File;
use HTML::Entities;

my $in_fh = IO::File->new("G800011A.xml", "r");
my $out_fh = IO::File->new("blah.xml", "w");

foreach my $line (<$in_fh>) {
  print $out_fh decode_entities($line);
}


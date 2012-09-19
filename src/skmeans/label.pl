use strict;
use warnings;

use IO::File;

my $fh = IO::File->new("texts.csv", "r");

foreach my $line (<$fh>) {
  my ($title, $csv) = split(" ", $line);
  print "$title\n";
}

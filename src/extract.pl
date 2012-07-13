use strict;
use warnings;
use utf8;

use XML::XPath;
use IO::File;
use Data::Dumper;
use Encode;

my $xp = XML::XPath->new(filename => $ARGV[0]);

my $nodeset = $xp->find("/root/div");

foreach my $node ($nodeset->get_nodelist) {
  my @title_node = $xp->find("title", $node)->get_nodelist();
  my $fh = IO::File->new(("texts/" . $title_node[0]->string_value() . ".txt"), "w");

  my $p_nodes = $xp->find("p", $node);

  foreach my $p_node ($p_nodes->get_nodelist) {
    my $p = $p_node->string_value();
    $p =~ s/\n/ /g;
    print $fh (encode('utf8', $p). "\n");
  }
}

use strict;
use warnings;

my @sf_files = `find . -name "* .txt"`;
my @sl_files = `find . -name " *.txt"`;

my @all_files = (@sf_files, @sl_files);

foreach my $file (@all_files) {
  my $old_file = $file;
  $old_file =~ s/\n//g;

  $file =~ s/(.*)\s+\.+txt|\s+(.*).txt/$1.txt/g;
  $file =~ s/\.\./\./g;
  $file =~ s/\n//g;

  `mv \"$old_file\" \"$file\"`
}

my @dot_files = `find . -name "*..txt"`;

foreach my $file (@dot_files) {
  my $old_file = $file;
  $old_file =~ s/\n//g;

  $file =~ s/\n//;
  $file =~ s/\.\./\./g;

  `mv \"$old_file\" \"$file\"`;
}

@all_files = `find . -name "*.txt"`;

foreach my $file (@all_files) {
  my $old_file = $file;
  $old_file =~ s/\n//g;

  $file =~ s/\n//g;
  $file =~ s/\s+/_/g;

  `mv \"$old_file\" \"$file\"`;
}

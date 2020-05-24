#!/bin/bash
perl -lanE '
BEGIN {
my $mid = false;
 sub printone {
  say join(", ", @s);
  @s = ();
 }
}
if (/^Thread \d/ && $#s) {
  printone();
  say "---" if /^Thread 1 /;
} elsif (/^#\d+\s+([^(]+) \(/) {
  if($1 =~ /::mid$/) {
    next if $mid;
    $mid = true;
  } else {
    $mid = false;
  }
  next if $1 =~ /crossbeam/;
  push(@s, $1);
}
END {
  printone();
}
' "$@"

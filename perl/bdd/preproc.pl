#!/usr/bin/perl

use strict;
use warnings;

while(<>) {
    my $line = $_;
    $line =~ s/FIGURE\((.*),(.*)\)/'''\n<div class="figure">\n\t<img class="picture" src="$2" \/>\n\t<p class="label">$1<\/p>\n<\/div>\n'''/;
    $line =~ s/VIDEO\((.*),(.*)\)/'''\n<div class="figure">\n\t<video class="movie" src="$2" controls preload=metadata \/>\n\t<p class="label">$1<\/p>\n<\/div>\n'''/;
    print $line;
}


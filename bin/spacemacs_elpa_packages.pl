#!/usr/bin/perl

use strict;
use warnings;

use File::Find::Rule;
use Path::Class;
use autodie; # die if problem reading or writing a file

my @files = File::Find::Rule
  ->file()
  ->name("packages.el")
  ->in("layers");

foreach my $path ( @files ) {
    my $file = file("$path");
    my $content = $file->slurp();
    if ( $content =~ /\(setq [a-z-]+-packages '\(([a-z-\s]+)\)\)/agm ) {
        print "$1\n";
    }
}

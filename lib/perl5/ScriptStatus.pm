package ScriptStatus;

use strict;
use warnings;

use parent 'Exporter';
our @EXPORT = qw( status );

use Term::ANSIColor;
use File::Basename;

sub status {
    my $me = basename($0);
    print "[";
    print colored("$me", 'yellow');
    print "] ";
    print colored(@_, 'bright_white'), "\n";
}

1;

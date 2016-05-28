package ShellSequence;

use strict;
use warnings;

use Capture::Tiny 'tee_stdout';
use Array::Iterator;
use Term::UI;
use Term::ReadLine;
use ScriptStatus;

sub new {
    my $class = shift;

    my $self = {
                'commands' => []
               };
    bless $self, $class;

    return $self;
}

sub add_should_zero {
    my $self = shift;
    my @args = @_;

    my $cmd = ['ZERO', @args];

    push @{$self->{'commands'}}, $cmd;
}

sub add_should_succeed {
    my $self = shift;
    my @args = @_;

    my $cmd = ['SUCCEED', @args];

    push @{$self->{'commands'}}, $cmd;
}

sub choice {
    my $i = shift @_;
    my @args = @_;
    my $term = Term::ReadLine->new('brand');


    my $shell = $term->ask_yn(
                              prompt => 'Spawn a shell to investigate?',
                              default => 'n',
                             );
    if ($shell) {
        status "I will try running `@args' again when this shell exits";
        system $ENV{'SHELL'};
        return 1;
    } else {
        if ($i->peek()) {
            my @maybe_next = @{$i->peek()};
            shift @maybe_next;
            my @next = @maybe_next;
            status "info: the next command will be `@next'";
        }
        my $give_up = $term->ask_yn(
                                    prompt => 'Give up and skip this command?',
                                    default => 'n',
                                   );
        return !$give_up;
    }
}

sub run {
    my $self = shift;

    my $i = Array::Iterator->new($self->{'commands'});

    while ( my $cmd = $i->get_next() ) {
        my $require = shift @$cmd;
        my @args = @$cmd;

        while (42) {
            status "running `@args'";
            (my $output, my $exit) = tee_stdout {
                system @args;
            };
            $exit = $exit >> 8;
            if ($require eq 'SUCCEED' && $exit != 0) {
                status "`@args' failed but it was required to succeed";
                choice($i, @args) || last;
            } elsif ($require eq 'ZERO' && length($output)) {
                status "`@args' was required to produce no output";
                choice($i, @args) || last;
            } else {
                last;
            }
        }
    }

    $self->{'commands'} = [];
}

1;

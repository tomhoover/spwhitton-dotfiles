#!/usr/bin/perl -w
# resets window activity status
#  based on act.pl from http://scripts.irssi.org by c0ffee

use strict;
use Irssi 20020120;

Irssi::command_bind('ac', sub { _act(); });

sub _act {
  for (Irssi::windows()) {
      Irssi::signal_emit("window dehilight", $_);
  }
}

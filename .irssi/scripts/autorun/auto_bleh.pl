use Irssi;
use Irssi::Irc;
use strict;
use vars qw($VERSION %IRSSI $DEBUG);

$VERSION = q$Revision: 1.1 $;
%IRSSI = (authors     => 'tomaw, based on auto_bleh.pl by Don Armstrong',
	name        => 'auto_bleh',
	description => 'Provides /ak /aq /ab /abr /abrn /arn /amb /amr /at',
	license     => 'GPL',
	changed     => q$Id: auto_bleh.pl,v 1.1 2005/03/17 08:19:53 don Exp $,
);

#$DEBUG = 1 unless defined $DEBUG;

my ($actions, %defaults);

%defaults = (GET_OP       => 1,    # Should we try to get opped when we auto_bleh?
	USE_CHANSERV => 1,    # Should we use chanserv to get opped?
	EXPIRE       => 6000,   # Do not try to do anything if the action is more than 6000 seconds old.
	TIMEOUT      => 10,   # Timeout /at bans after 10 minutes
);

my %command_bindings = (ak   => 'cmd_ak',
	ab   => 'cmd_ab',
	aq   => 'cmd_aq',
	ar   => 'cmd_ar',
	abr  => 'cmd_abr',
	abk  => 'cmd_abk',
	abrn => 'cmd_abrn',
	abk  => 'cmd_abkn',
	arn  => 'cmd_arn',
	amb  => 'cmd_amb',
	amr  => 'cmd_amr',
	at   => 'cmd_at',
);

my %bans_to_remove;

sub cmd_at {
	my ($data, $server, $witem) = @_;
	return do_auto_bleh('timeout',$data,$server,$witem);
}

sub cmd_ak {
	my ($data, $server, $witem) = @_;
	return do_auto_bleh('kick',$data,$server,$witem);
}

sub cmd_abk {
	my ($data, $server, $witem) = @_;
	return do_auto_bleh('kick,ban',$data,$server,$witem);
}
sub cmd_abkn {
	my ($data, $server, $witem) = @_;
	return do_auto_bleh('kick,ban,notice',$data,$server,$witem);
}

sub cmd_amb{
	my ($data, $server, $witem) = @_;
	my @nicks = split /\s+/, $data;
	for (@nicks) {
		next unless /\w/;
		do_auto_bleh('ban',$_,$server,$witem);
	}
}

sub cmd_ab {
	my ($data, $server, $witem) = @_;
	return do_auto_bleh('ban',$data,$server,$witem);
}

sub cmd_aq {
	my ($data, $server, $witem) = @_;
	return do_auto_bleh('quiet',$data,$server,$witem);
}


sub cmd_amr{
	my ($data, $server, $witem) = @_;
	my @nicks = split /\s+/, $data;
	for (@nicks) {
		next unless /\w/;
		do_auto_bleh('remove',$_,$server,$witem);
	}
}

sub cmd_ar {
	my ($data, $server, $witem) = @_;
	return do_auto_bleh('remove',$data,$server,$witem);
}
sub cmd_abr{
	my ($data, $server, $witem) =@_;
	return do_auto_bleh('remove,ban',$data,$server,$witem);
}
sub cmd_abrn{
	my ($data, $server, $witem) =@_;
	return do_auto_bleh('remove,ban,notice',$data,$server,$witem);
}
sub cmd_arn{
	my ($data, $server, $witem) =@_;
	return do_auto_bleh('remove,notice',$data,$server,$witem);
}


sub do_auto_bleh {
	my ($cmd, $data, $server, $witem, $duration) = @_;

	if (!$server || !$server->{connected}) {
		Irssi::print("Not connected to server");
		return;
	}

	if ($witem->{type} ne 'CHANNEL') {
		Irssi::print("Can't autokick on a non-channel. [$witem->{type}]");
		return;
	}

	# set the network that we're on, the channel and the nick to kick
	# once we've been opped

	$data =~ /^\s*([^\s]+)\s*(\d+)?\s*(.+?|)\s*$/;
	my $nick = $1;
	my $timeout = $2;
	my $reason = $3;
	$timeout = $defaults{TIMEOUT} if not defined $timeout or $timeout eq '';
	$reason = 'you should know better' if not defined $reason or $reason eq '';

	my $nick_rec = $witem->nick_find($nick);
	if (not defined $nick_rec) {
		Irssi::print("Unable to find nick: $nick");
		return;
	}

	my $hostname = $nick_rec->{host} if defined $nick_rec;
	Irssi::print("Unable to find hostname for $nick") if not defined $hostname or $hostname eq '';
	$hostname =~ s/.+\@//;

	Irssi::print("Nick set to '$nick' from '$data', reason set to '$reason'.") if $DEBUG;
	my $action = {type      => $cmd,
		nick      => $nick,
		nick_rec  => $nick_rec,
		network   => $witem->{server}->{chatnet},
		server    => $witem->{server},
		completed => 0,
		inserted  => time,
		channel   => $witem->{name},
		reason    => $reason,
		hostname  => $hostname,
		timeout   => $timeout,
	};
	Irssi::print(i_want($action)) if $DEBUG;
	if ($witem->{chanop}) {
		take_action($action,$server,$witem);
	}
	else {
		$actions->{$data.$action->{inserted}}=$action;
		get_op($server, $action->{channel}) if $defaults{GET_OP};
	}
}

sub get_op {
	my ($server,$channel) = @_;

	Irssi::print("QUOTE CS op $channel") if $DEBUG;
	$server->command("QUOTE CS op $channel") if $defaults{USE_CHANSERV};
}

sub i_want {
	my $action = shift;
	return "I've wanted to $action->{type} $action->{nick} off $action->{channel} on $action->{network} since $action->{inserted}";
}

sub take_action {
	my ($action,$server,$channel) = @_;

	local $_ = $action->{type};
	# Now support multiple actions against a single nick (to FE, kick ban, or remove ban). See /abr foo
	if (/timeout/) {
		Irssi::print("Quieting $action->{nick} on $action->{channel} with hostname $action->{hostname} for $action->{timeout} minutes") if $DEBUG;
		$channel->command("/quote MODE $action->{channel} +q *!*@".$action->{hostname}) if $action->{hostname} ne ''; #quiet hostname
		$bans_to_remove{"$action->{nick}$action->{inserted}"} = $action;
	}
	if (/quiet/) {
		Irssi::print("Quieting $action->{nick} on $action->{channel} with hostname $action->{hostname}") if $DEBUG;
		# Find hostname
		$channel->command("/quote MODE $action->{channel} +q *!*@".$action->{hostname}) if $action->{hostname} ne ''; #quiet hostname
	}
	if (/ban/) {
		Irssi::print("Banning $action->{nick} from $action->{channel} with hostname $action->{hostname}") if $DEBUG;
		$channel->command("/quote MODE $action->{channel} +b *!*@".$action->{hostname}) if $action->{hostname} ne ''; # ban hostname
	}
	if (/kick/) {
		Irssi::print("Kicking $action->{nick} from $action->{channel}") if $DEBUG;
		if ($action->{reason} =~ /\s/) {
			$channel->command("/quote KICK $action->{channel} $action->{nick} :$action->{reason}");
		}
		else {
			$channel->command("/quote REMOVE $action->{channel} $action->{nick} $action->{reason}");
		}
	}
	if (/remove/) {
		Irssi::print("Removing $action->{nick} from $action->{channel}") if $DEBUG;
		if ($action->{reason} =~ /\s/) {
			$channel->command("/quote REMOVE $action->{channel} $action->{nick} :$action->{reason}");
		} else {
			$channel->command("/quote REMOVE $action->{channel} $action->{nick} $action->{reason}");
		}
	}
	if (/notice/) {
		Irssi::print("Noticing $action->{nick} with $action->{reason}") if $DEBUG;
		$channel->command("/NOTICE $action->{nick} $action->{reason}");
	}
	if (/teiuq/) {
		Irssi::print("Unquieting $action->{nick} on $action->{channel} with hostname $action->{hostname}") if $DEBUG;
		$channel->command("/quote MODE $action->{channel} -q *!*@".$action->{hostname});
	}
	return; #for now.
}

sub deop_us {
	my ($rec) = @_;
	my $channel = $rec->{channel};
	my $server = $rec->{server};

	Irssi::print("MODE $channel->{name} -o $channel->{ownnick}->{nick}") if $DEBUG;
	$channel->command("/deop $channel->{ownnick}->{nick}");
}

sub sig_mode_change {
	my ($channel,$nick) = @_;

	# Are there any actions to process?
	# See if we got opped.
	return if scalar(keys %$actions) eq 0;

	my @deop_array;
	if ($channel->{server}->{nick} eq $nick->{nick} and $nick->{op}) {
		Irssi::print("We've been opped") if $DEBUG;
		foreach (keys %$actions) {
			# See if this action is too old
			if (time - $actions->{$_}->{inserted} > $defaults{EXPIRE}) {
				Irssi::print("Expiring action: \"".i_want($actions->{$_})."\" because of time") if $DEBUG;
				delete $actions->{$_};
				next;
			}
			Irssi::print(i_want($actions->{$_})) if $DEBUG;
			# Find the server to take action on
			my $server = $actions->{$_}->{server};
			Irssi::print("Unable to find server for chatnet: $actions->{$_}->{network}") and return if not defined $server;
			Irssi::print("Found server for chatnet: $actions->{$_}->{network}") if $DEBUG;
			# Find the channel to take action on
			my $channel = $server->channel_find($actions->{$_}->{channel});
			Irssi::print("Unable to find channel for channel: $actions->{$_}->{channel}") and return if not defined $channel;
			Irssi::print("Found channel for channel: $actions->{$_}->{channel}") if $DEBUG;
			Irssi::print("We are opped on the channel!") if $DEBUG;
			take_action($actions->{$_},$server,$channel);
			push @deop_array,{server=>$server,channel=>$channel} if Irssi::settings_get_bool('auto_bleh_deop_after_action');
			delete $actions->{$_}; # Do not repeat this action.
		}
		foreach (@deop_array) {
			deop_us($_);
		}
	} else {
		Irssi::print("Fooey. Not opped.") if $DEBUG;
	}
}

sub try_to_remove_bans {
	return unless keys %bans_to_remove;
	for my $key (keys %bans_to_remove) {
		if (($bans_to_remove{$key}{inserted} + $bans_to_remove{$key}{timeout}*60) < time) {
			$bans_to_remove{$key}{type} = 'teiuq'; #unquiet
			$actions->{$key} = $bans_to_remove{$key};
			delete $bans_to_remove{$key};
			get_op($actions->{$key}{server}, $actions->{$key}{channel}) if $defaults{GET_OP};
		}
	}
}

# call the try to remove bans function every minute
Irssi::timeout_add(1000*60,'try_to_remove_bans',undef);
Irssi::signal_add_last('nick mode changed','sig_mode_change');
my ($command,$function);

while (($command,$function) = each %command_bindings) {
	Irssi::command_bind($command,$function, 'auto_bleh');
}

Irssi::settings_add_bool($IRSSI{name}, 'auto_bleh_deop_after_action', 1);

1;


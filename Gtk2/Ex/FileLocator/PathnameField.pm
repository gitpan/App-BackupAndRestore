package Gtk2::Ex::FileLocator::PathnameField;
use strict;
use warnings;

use Gtk2;
use Gtk2::Gdk::Keysyms;
use Glib qw(TRUE FALSE);

use Gtk2::Ex::FileLocator::Helper;
use Gtk2::Ex::FileLocator::FileChooser;

use Glib::Object::Subclass
  Gtk2::Ex::FileLocator::FileChooser::,
  properties => [
	Glib::ParamSpec->object(
		'entry', 'entry', 'The entry to use.',
		'Gtk2::Entry', [qw/readable writable/]
	),
  ],
  signals => {
	scroll_offset_changed => {
		param_types => [qw(Glib::Scalar)],
	},
  },
  ;

sub INIT_INSTANCE {
	my ($this) = @_;
	
	$this->{cursorPosition} = 0;

	my $entry = new Gtk2::Entry;
	$entry->add_events('key-release-mask');

	#$entry->signal_connect( 'key-release-event' => \&on_key_release_event, $this );
	#$entry->signal_connect_after( 'move-cursor' => \&on_move_cursor, $this );
	#$entry->signal_connect( 'event' => \&on_event, $this );
	$entry->show;
	
	$this->set('entry', $entry);
	$this->add($entry);
}

1;
__END__

sub on_key_release_event {
	my ( $entry, $this, $event ) = @_;
	printf "on_key_release_event %s\n", $entry->get_text;
	#printf "%s\n", $event->keyval;

	return if (
		$event->keyval == $Gtk2::Gdk::Keysyms{Insert}   or
		$event->keyval == $Gtk2::Gdk::Keysyms{KP_Enter} or
		$event->keyval == $Gtk2::Gdk::Keysyms{Return} ) and
	  $this->_auto_complete
	  ;

	return if $event->keyval == $Gtk2::Gdk::Keysyms{KP_Enter} and file_open( $this->get_filename );
	return if $event->keyval == $Gtk2::Gdk::Keysyms{Return} and file_open( $this->get_filename );

	$this->signal_emit( 'scroll_offset_changed', $this->get("scroll-offset") );

	return if $this->get_filename eq $entry->get_text;
	$this->set_filename($this->get_text);
	$this->signal_emit( 'file_activated', $this->{filename} );

	Gtk2->main_iteration while Gtk2->events_pending;
	$this->signal_emit( 'scroll_offset_changed', $this->get("scroll-offset") );

	0;
}

sub _auto_complete {
	my $this = shift;
	printf "auto_complete %s\n", $this->get_text;

	my $string = $this->get_text;
	my $match  = string_shell_complete($string);

	if ( $string ne $match ) {
		$this->set_text($match);
		$this->set_position( length $match );
		$this->signal_emit( 'file_activated', $this->get_text );
		return TRUE;
	}

	return;
}

sub on_move_cursor {
	my ($this) = @_;
	$this->{cursorPosition} = $this->get_position;
	Gtk2->main_iteration while Gtk2->events_pending;
	$this->signal_emit( 'scroll_offset_changed', $this->get("scroll-offset") );
	return;
}

sub on_event {
	my ( $this, $event ) = @_;
	return unless $event->type eq 'motion-notify' or $event->type eq 'expose';
	#printf "on_event %s %s\n", $event->type, $this->get("scroll-offset");
	$this->{cursorPosition} = $this->get_position;
	$this->signal_emit( 'scroll_offset_changed', $this->get("scroll-offset") );
	0;
}

sub get_filename {
	my ($this) = @_;
	return $this->get_text;
}

sub set_filename {
	my ( $this, $filename ) = @_;
	$this->set_text( $filename || "" );
	return;
}

sub set_text {
	my ( $this, $filename ) = @_;

	$filename .= "/" if -d $filename;
	$filename =~ s|/+|/|sgo;

	$this->{filename} = $filename;
	$this->Gtk2::Entry::set_text($filename);
	$this->set_position( $this->{cursorPosition} );
	Gtk2->main_iteration while Gtk2->events_pending;
	$this->signal_emit( 'scroll_offset_changed', $this->get("scroll-offset") );
}

1;
__END__
sub on_button_release_event {
	my ($this) = @_;
	$this->set_text( $this->get_text );
	$this->signal_emit( 'file_activated', $this->get_text );
	0;
}

sub on_drag_data_received {
	my ( $this, $context, $x, $y, $data, $flags, $time ) = @_;
	my $type = $data->type->name;

	my $url = $data->data;
	$url = $this->get_text unless $url =~ s|^file://||sgo;
	$url =~ s/[\r\n]+.*//sgo if $type eq "text/uri-list";

	$url = $this->unescape($url);

	$this->set_text($url);
	$this->signal_emit( 'file_activated', $this->get_text );
	0;
}
sub auto_complete {
	my $this = shift;
	printf "auto_complete %s\n", $this->get_text;

	my $original = $this->get_text;

	my $string = $this->get_text;
	my $substr = substr $string, 0, $this->get('cursor-position');

	$string =~ s|/*$||sgo;

	printf "string %s\n", $string;
	printf "substr %s\n", $substr;

	if ( -e $substr ) {
		printf "v1\n";
		if ( -d $string ) {
			$string = "$string/";
			$string = "$ENV{HOME}/$string" unless $string =~ m|^/|o;
		}

		$this->set_text($string);
		$this->set_position( length $string );
	} else {
		printf "v2\n";
		return unless $substr =~ /([^\\]+)$/;

		my $match = $this->get_match($1);
		return unless $match;

		$string =~ s/\Q$substr/$match/;
		$string = "$ENV{HOME}/$string" unless $string =~ m|^/|o;

		$this->set_text($string);
		$this->set_position( length $match );
	}

	if ( $original ne $this->get_text ) {
		$this->signal_emit( 'file_activated', $this->get_text );
		return TRUE;
	}

	return;
}

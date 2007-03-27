package Gtk2::Ex::FileLocator::FileChooser;
use strict;
use warnings;

our $VERSION = 0.01;
our $DEBUG   = 1;

use Gtk2;
use Glib qw(TRUE FALSE);

use Glib::Object::Subclass
  Gtk2::HBox::,
  properties => [
	Glib::ParamSpec->object(
		'chooser', 'chooser', 'The file chooser dialog to use.',
		'Gtk2::FileChooserDialog', [qw/readable writable/]
	),
  ],
  signals => {
	current_folder_changed => {},
	file_activated         => {},
  },
  ;

sub INIT_INSTANCE {
	my $this = shift;

	$this->set_border_width(0);

	$this->set( 'chooser', new Gtk2::FileChooserDialog( '', undef, 'open' ) );
	$this->get('chooser')->set_local_only(FALSE);
	$this->get('chooser')->signal_connect( 'current_folder_changed',
		sub { $this->signal_emit('current_folder_changed') }
	);
	$this->get('chooser')->signal_connect( 'selection_changed',
		sub { $this->signal_emit('file_activated') }
	);

}

sub get_current_folder {
	my ($this) = @_;
	$this->get('chooser')->get_current_folder;
}

sub set_current_folder {
	my ( $this, $folder ) = @_;
	$this->get('chooser')->set_current_folder( $folder || "" );
}

sub get_filename {
	my ($this) = @_;
	$this->get('chooser')->get_filename;
}

sub set_filename {
	my ( $this, $filename ) = @_;
	printf "%s::set_filename %s\n", __PACKAGE__, $filename if $DEBUG > 1;
	$this->get('chooser')->set_filename( $filename || "" );
}

sub get_uri {
	my ($this) = @_;
	$this->get('chooser')->get_uri;
}

sub set_uri {
	my ( $this, $uri ) = @_;
	$this->get('chooser')->set_uri($uri);
	return;
}

1;
__END__

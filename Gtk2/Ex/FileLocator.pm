package Gtk2::Ex::FileLocator;
use strict;
use warnings;

use Gtk2;
use Glib qw(TRUE FALSE);

use Gtk2::Ex::FileLocator::DropPocket;
use Gtk2::Ex::FileLocator::PathBar;
use Gtk2::Ex::FileLocator::PathnameField;
use Gtk2::Ex::FileLocator::RecycleButton;

use Glib::Object::Subclass
  Gtk2::VBox::,
  properties => [
	Glib::ParamSpec->boolean(
		'stdout', 'stdout', 'Output filename to stdout',
		FALSE, [qw/readable writable/]
	),
  ],
  signals => {
	current_folder_changed => {
		param_types => [qw(Glib::Scalar)],
	},
	file_activated => {
		param_types => [qw(Glib::Scalar)],
	},
  },
  ;

sub INIT_INSTANCE {
	my ($this) = @_;

	$this->{filename} = "";

	my $hbox = new Gtk2::HBox;
	$hbox->set_spacing(2);

	$this->{dropPocket} = new Gtk2::Ex::FileLocator::DropPocket;
	$hbox->pack_start( $this->{dropPocket}, FALSE, FALSE, 0 );

	my $vbox = new Gtk2::VBox;
	$vbox->set_spacing(0);

	$this->{pathBar} = new Gtk2::Ex::FileLocator::PathBar;
	$vbox->pack_start( $this->{pathBar}, TRUE, TRUE, 0 );

	$this->{pathnameField} = new Gtk2::Ex::FileLocator::PathnameField;
	$vbox->pack_start( $this->{pathnameField}, FALSE, FALSE, 0 );

	$hbox->pack_start( $vbox, TRUE, TRUE, 0 );

	$this->{recycleButton} = new Gtk2::Ex::FileLocator::RecycleButton;
	$hbox->pack_start( $this->{recycleButton}, FALSE, FALSE, 0 );

	$this->pack_start( $hbox, TRUE, TRUE, 0 );

	$this->{dropPocket}->signal_connect( 'file-activated'    => \&on_file_activated, $this );
	$this->{pathBar}->signal_connect( 'file-activated'       => \&on_file_activated, $this );
	$this->{pathnameField}->signal_connect( 'file-activated' => \&on_file_activated, $this );
	$this->{recycleButton}->signal_connect( 'file-activated' => \&on_file_activated, $this );

	$this->{pathnameField}->signal_connect( 'scroll-offset-changed' => sub { $this->{pathBar}->set_scroll_offset( $_[1] ) } );
	$this->{pathnameField}->signal_connect_after( 'size-request' => sub { $this->{pathBar}->configure_buttons } );
}

sub on_file_activated {
	my ( $widget, $this ) = @_;

	my $uri = $widget->get_uri;
	printf "**** %s\n", $uri;
	
	$this->{dropPocket}->set_uri($uri)    unless $widget == $this->{dropPocket};
	#$this->{pathBar}->set_uri($uri)       unless $widget == $this->{pathBar};
	#$this->{pathnameField}->set_uri($uri) unless $widget == $this->{pathnameField};
	#$this->{recycleButton}->set_uri($uri) unless $widget == $this->{recycleButton};

	#printf "%s\n", $filename if $this->get('stdout');
}

sub set_filename {
	my ( $this, $filename ) = @_;

	$this->{filename} = $filename || "";

	$this->{dropPocket}->set_filename($filename);
	$this->{pathBar}->set_filename($filename);
	$this->{pathnameField}->set_filename($filename);
	$this->{recycleButton}->set_filename($filename);
	return;
}

sub get_filename {
	my ($this) = @_;
	return $this->{filename};
}

sub get_droppocket {
	my ($this) = @_;
	return $this->{dropPocket};
}

sub get_pathbar {
	my ($this) = @_;
	return $this->{pathBar};
}

sub get_pathnamefield {
	my ($this) = @_;
	return $this->{pathnameField};
}

sub get_recyclebutton {
	my ($this) = @_;
	return $this->{recycleButton};
}

1;
__END__

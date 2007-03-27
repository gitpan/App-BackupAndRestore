package Gtk2::Ex::FileQuickFind;
use strict;
use warnings;

use Gtk2;
use Glib qw(TRUE FALSE);
use Gtk2::Ex::FileLocator;

use Glib::Object::Subclass
  Gtk2::Window::,
  ;

sub import {
	my $class = shift;
	my $run   = 0;
	foreach (@_) {
		if (/^-?run$/) {
			$run = 1;
		}
	}
	$class->run(@ARGV) if $run;
}

sub run {
	Gtk2->init;
	my ( $class, $filename ) = @_;
	my $this = $class->new;
	$this->show;
	$this->present;
	$this->set_filename($filename);
	Gtk2->main;
	return;
}

sub INIT_INSTANCE {
	my ( $this, $filename ) = @_;

	$this->set_title('File QuickFind');
	$this->set_position('GTK_WIN_POS_MOUSE');
	$this->set_default_size( 300, -1 );
	#$this->set_size_request( 300, -1 );

	$this->{fileLocator} = new Gtk2::Ex::FileLocator;
	$this->{fileLocator}->show;

	$this->add( $this->{fileLocator} );

	$this->signal_connect( delete_event => sub { Gtk2->main_quit } );
}

sub set_filename {
	my ( $this, $filename ) = @_;
	$this->{fileLocator}->set_filename($filename);
	return;
}

sub show {
	my ( $this, $filename ) = @_;
	$this->show_all;
}

1;
__END__

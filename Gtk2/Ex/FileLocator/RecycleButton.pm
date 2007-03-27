package Gtk2::Ex::FileLocator::RecycleButton;
use strict;
use warnings;

use Gtk2;
use Glib qw(TRUE FALSE);
use Gnome2;

use File::Basename qw(dirname);

use Gtk2::Ex::FileLocator::Helper;
use Gtk2::Ex::FileLocator::FileChooser;

use Glib::Object::Subclass
  Gtk2::Ex::FileLocator::FileChooser::,
  properties => [
	Glib::ParamSpec->boolean(
		'existing_files', 'file_exists', 'Show only existing files',
		TRUE, [qw/readable writable/]
	),
  ],
  ;

sub INIT_INSTANCE {
	my ($this) = @_;

	$this->{iconsize} = 14;

	my $image = Gtk2::Image->new_from_stock( 'gtk-refresh', "GTK_ICON_SIZE_BUTTON" );
	$image->show;

	my $button = new Gtk2::Button;
	$button->add($image);
	$button->show;
	$button->add_events('button-release-mask');
	$button->signal_connect( 'button_release_event' => \&on_button_release_event, $this );

	$this->add($button);

	#popup menu
	$this->{menu} = new Gtk2::Menu;
	$this->{menu}->show;
	
	$this->signal_connect_after( 'map' => \&on_map );
	#$this->signal_connect_after( 'file_activated', \&on_file_activated);
}

sub on_map {
	my $this   = shift;
	my $height = $this->allocation->height + 5;
	$this->set_size_request( $height, 0 );
	0;
}

sub on_button_release_event {
	my ( $button, $event, $this ) = @_;
	#printf "on_button_release_event %s\n", $this->get_filename;
	$this->{menu}->popup( undef, undef, undef, undef, $event->button, $event->time );
	0;
}

sub set_filename {
	my ( $this, $filename ) = @_;
	#printf "set_filename %s\n", $filename;
	$this->add_filename($filename);
	$this->SUPER::set_filename($filename);
	return;
}

sub add_filename {
	my ( $this, $filename ) = @_;
	#printf "add_filename %s\n", $filename;

	return unless $filename;
	return unless -e $filename or not $this->get('existing_files');

	$filename =~ s|\.{1,2}/*$||o if -d $filename;
	$filename .= "/" if -d $filename;
	$filename =~ s|/+|/|sgo if -d $filename;

	my @children = $this->{menu}->get_children;
	return if @children and grep { $_->get_child->get_text eq $filename } @children;

	my $image = Gtk2::Image->new;
	image_set_file_icon( $image, $filename, $this->{iconsize} );

	my $menuItem = Gtk2::ImageMenuItem->new($filename);
	$menuItem->set_image($image);
	$menuItem->signal_connect_after( 'activate' => \&on_menu_activated, $this );
	$menuItem->show;

	$this->{menu}->append($menuItem);
	return;
}

sub on_menu_activated {
	my ( $item, $this ) = @_;
	#printf "on_menu_activated %s\n", $item->get_child->get_text;
	
	my $uri = $item->get_child->get_text;
	$this->set_filename($uri);
	$this->set_current_folder($uri) if -d $uri;

	$this->get_toplevel->present;
	1;
}

1;
__END__
Gtk2->main_iteration while Gtk2->events_pending;

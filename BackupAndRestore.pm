package App::BackupAndRestore;
use strict;
use warnings;

our $VERSION = 0.01;
our $DEBUG   = 1;

#use AutoSplit; autosplit('../App/BackupAndRestore', '../auto/', 0, 1, 1);

use Glib qw(TRUE FALSE);
use Gtk2;
use Gtk2::GladeXML;
use Gtk2::Gdk::Keysyms;
use Gnome2::GConf;

use Cwd qw(abs_path);
use File::Basename qw(basename dirname);
use Number::Bytes::Human qw( format_bytes );
use POSIX qw(strftime);
use Unicode::UTF8simple;

use Gtk2::Ex::FileLocator::RecycleButton;
use App::BackupAndRestore::Helper;

=Globals 		 																						
																											
=cut																										

our $TarOpenCmd = "file-roller";

my $ApplicationName = 'BackupAndRestore';

my $CurrentDat   = "current.dat";
my $ExcludesFile = "excludes.txt";

my @ColumnTypes = qw(
  Glib::String
  Glib::String
  Glib::String
  Glib::String
  Glib::UInt
  Glib::String
  Glib::UInt
  Glib::String
  Glib::String
  Glib::Boolean
  Glib::UInt
);

use enum qw(
  COL_DATE
  COL_HDATE
  COL_TIME
  COL_NAME
  COL_SIZE
  COL_HSIZE
  COL_FILES
  COL_LABEL
  COL_PATH
  COL_CURRENT
  COL_WEIGHT
);
use enum qw(
  EXCLUDE_FOLDER
  EXCLUDE_FILE
  EXCLUDE_PATTERN
);

=AUTOLOAD		 																						
																											
=cut																										

use AutoLoader;
our $AUTOLOAD;

sub AUTOLOAD {
	my $this = shift;
	my $name = substr $AUTOLOAD, rindex( $AUTOLOAD, ':' ) + 1;

	#printf "%s\n", $name if $DEBUG > 3;
	my $widget = $this->{gladexml}->get_widget($name);
	return $widget if ref $widget;
	die "AUTOLOAD: Unknown widget '$name'";
}

=new				 																						
																											
=cut																										

sub new {
	my ($self) = @_;
	my $class = ref($self) || $self;
	my $this = bless {}, $class;

	printf "%s\n", dirname abs_path $0 if $DEBUG > 3;
	chdir dirname abs_path $0;

	$this->{client} = Gnome2::GConf::Client->get_default;

	$this->{gladexml} = Gtk2::GladeXML->new("$ApplicationName/$ApplicationName.glade");
	$this->{gladexml}->signal_autoconnect_from_package($this);

	$this->init;

	return $this;
}

=run						 																				
																											
=cut																										

sub run {
	Gtk2->init;
	my $class = shift;
	my $this  = $class->new(@_);
	$this->window->present;
	Gtk2->main;
	return;
}

=gconf					 																				
																											
=cut																										

sub gconf {
	my ( $this, $key, $value ) = @_;
	my $app_key = "/apps/" . $ApplicationName . "/$key";

	$this->{client}->set( $app_key, { type => 'string', 'value' => $value } )
	  if defined $value;

	return $this->{client}->get_string($app_key);
}

=gui init				 																				
																											
=cut																										

sub init {
	my ($this) = @_;
	print "init $this\n" if $DEBUG > 3;

	# GUI init
	#$this->gconf( 'store-folder', '' );
	#$this->gconf( 'store-folder-name', '' );

	$this->exclude_combo->set_active(0);    # Gtk2::GladeXML macht es nicht

	my $button;
	$this->{folder_recycle_button} = new Gtk2::Ex::FileLocator::RecycleButton;
	$this->{folder_recycle_button}->show;
	$this->folder_box->pack_start( $this->{folder_recycle_button}, FALSE, FALSE, 0 );
	$this->{folder_recycle_button}->signal_connect(
		'current_folder_changed',
		sub { $this->on_folder_recycle_button(@_) }
	);

	#configure;
	$this->store_folder->set_current_folder( $this->gconf("store-folder")    || $ENV{HOME} );
	$this->store_folder_name->set_text( $this->gconf("store-folder-name")    || "Backup" );
	$this->folder->set_current_folder( $this->gconf("current-backup-folder") || $ENV{HOME} );

	$this->configure_expander;
	$this->build_tree;

	$this->log_init;
	$this->log_add_text(
		"*** $ApplicationName\n",
		"Version: $VERSION\n",
		"\n",
	);
	$this->log_add_text( $this->get_tar_version );
}

=build_tree				 																				
																											
=cut																										

sub build_tree {
	my ($this) = @_;
	print "build_tree\n" if $DEBUG > 3;

	#this will create a treeview, specify $tree_store as its model
	my $tree_view = $this->tree_view;

	#

	#create a Gtk2::TreeViewColumn to add
	#to $tree_view
	my $tree_column = Gtk2::TreeViewColumn->new();
	$tree_column->set_title( __ "Time" );
	$tree_column->set_visible(FALSE);

	#create a renderer
	my $renderer = Gtk2::CellRendererText->new;
	$tree_column->pack_start( $renderer, FALSE );
	$tree_column->add_attribute( $renderer, text => COL_TIME );
	#$tree_column->set_sort_column_id(COL_TIME);

	#add $tree_column to the treeview
	$tree_view->append_column($tree_column);

	#

	#create a Gtk2::TreeViewColumn to add
	#to $tree_view
	$tree_column = Gtk2::TreeViewColumn->new();
	$tree_column->set_title( __ "Date" );

	#create a renderer
	$renderer = Gtk2::CellRendererPixbuf->new;
	$renderer->set( 'icon-name' => 'tgz' );
	$tree_column->pack_start( $renderer, FALSE );

	#create a renderer
	$renderer = Gtk2::CellRendererText->new;
	$tree_column->pack_start( $renderer, FALSE );
	$tree_column->add_attribute( $renderer, text       => COL_NAME );
	$tree_column->add_attribute( $renderer, weight_set => COL_CURRENT );
	$tree_column->add_attribute( $renderer, weight     => COL_WEIGHT );
	#$tree_column->set_sort_column_id(COL_TIME);

	#add $tree_column to the treeview
	$tree_view->append_column($tree_column);

	#

	#create a Gtk2::TreeViewColumn to add
	#to $tree_view
	$tree_column = Gtk2::TreeViewColumn->new();
	$tree_column->set_title( __ "Changed files" );
	$tree_column->set_visible(TRUE);

	#create a renderer
	$renderer = Gtk2::CellRendererText->new;
	$tree_column->pack_start( $renderer, FALSE );
	$tree_column->add_attribute( $renderer, text       => COL_FILES );
	$tree_column->add_attribute( $renderer, weight_set => COL_CURRENT );
	$tree_column->add_attribute( $renderer, weight     => COL_WEIGHT );
	#$tree_column->set_sort_column_id(COL_PATH);

	#add $tree_column to the treeviewGtk2::CellRenderer
	$tree_view->append_column($tree_column);

	#

	#create a Gtk2::TreeViewColumn to add
	#to $tree_view
	$tree_column = Gtk2::TreeViewColumn->new();
	$tree_column->set_title( __ "Size" );

	#create a renderer
	$renderer = Gtk2::CellRendererText->new;
	$tree_column->pack_start( $renderer, FALSE );
	$tree_column->add_attribute( $renderer, text       => COL_HSIZE );
	$tree_column->add_attribute( $renderer, weight_set => COL_CURRENT );
	$tree_column->add_attribute( $renderer, weight     => COL_WEIGHT );
	#$tree_column->set_sort_column_id(COL_SIZE);

	#add $tree_column to the treeview
	$tree_view->append_column($tree_column);

	#

	#create a Gtk2::TreeViewColumn to add
	#to $tree_view
	$tree_column = Gtk2::TreeViewColumn->new();
	$tree_column->set_title( __ "Size in bytes" );
	$tree_column->set_visible(FALSE);

	#create a renderer
	$renderer = Gtk2::CellRendererText->new;
	$tree_column->pack_start( $renderer, FALSE );
	$tree_column->add_attribute( $renderer, text => COL_SIZE );
	#$tree_column->set_sort_column_id(COL_SIZE);

	#add $tree_column to the treeviewGtk2::CellRenderer
	$tree_view->append_column($tree_column);

	#

	#create a Gtk2::TreeViewColumn to add
	#to $tree_view
	$tree_column = Gtk2::TreeViewColumn->new();
	$tree_column->set_title( __ "Label" );
	#$tree_column->set_visible(FALSE);

	#create a renderer
	$renderer = Gtk2::CellRendererText->new;
	$tree_column->pack_start( $renderer, FALSE );
	$tree_column->add_attribute( $renderer, text       => COL_LABEL );
	$tree_column->add_attribute( $renderer, weight_set => COL_CURRENT );
	$tree_column->add_attribute( $renderer, weight     => COL_WEIGHT );
	#$tree_column->set_sort_column_id(COL_PATH);

	#add $tree_column to the treeviewGtk2::CellRenderer
	$tree_view->append_column($tree_column);

	#

	#create a Gtk2::TreeViewColumn to add
	#to $tree_view
	$tree_column = Gtk2::TreeViewColumn->new();
	$tree_column->set_title( __ "Path" );
	$tree_column->set_visible(FALSE);

	#create a renderer
	$renderer = Gtk2::CellRendererText->new;
	$tree_column->pack_start( $renderer, FALSE );
	$tree_column->add_attribute( $renderer, text => COL_PATH );
	#$tree_column->set_sort_column_id(COL_PATH);

	#add $tree_column to the treeviewGtk2::CellRenderer
	$tree_view->append_column($tree_column);

}

=backup	 																								
																											
=cut																										

sub on_backup_folder_changed {
	my ($this) = @_;
	print "on_backup_folder_changed\n" if $DEBUG > 0;
	$this->gconf( "current-backup-folder", $this->folder->get_filename );
	$this->restore_folder->set_current_folder( $this->folder->get_filename );
	$this->fill_tree;
	return;
}

sub on_folder_recycle_button {
	my ($this) = @_;
	printf " on_folder_recycle_button %s\n", $this->{folder_recycle_button}->get_current_folder if $DEBUG > 0;
	$this->folder->set_current_folder( $this->{folder_recycle_button}->get_current_folder );

	#$this->folder->set_current_folder( shift(@folders) or $ENV{HOME} );
	return;
}

=tree 	 																								
																											
=cut																										

use Tie::DataDumper;

sub fill_tree {
	my ($this) = @_;
	#$this->window->set_sensitive(FALSE);
	#Gtk2->main_iteration while Gtk2->events_pending;

	$this->restore_button->set_sensitive(FALSE);
	#fill it with arbitry data

	my $folder = $this->get_store_folder;
	printf "fill_tree %s\n", $folder if $DEBUG > 3;

	my $tree_store = Gtk2::TreeStore->new(@ColumnTypes);

	if ( -e $folder ) {
		my ( $day_iter, $day, $day_folder_size, $day_folder_files ) = ( undef, "", 0, 0 );
		my $current_dat = "$folder/$CurrentDat";
		
		my $date_of_last_restore = $this->fetch_restore_date($folder);
		#printf "%s\n", $date_of_last_restore;

		my @filenames = reverse grep { m/\.tar\.bz2$/ } get_files($folder);

		# initialize label of first full backup if {first}.info.txt not exists
		if (@filenames) {
			my $filename = $filenames[ @filenames - 1 ];
			my $infofile = "$folder/" . basename( $filename, ".tar.bz2" ) . ".info.txt";
			my $info     = $this->get_backup_info( $filename, $infofile );
			$info->{label} = 'Full backup';
			tied(%$info)->save;
		}

		foreach my $filename (@filenames) {
			# get basename
			my $basename = basename( $filename, ".tar.bz2" );

			# calculate size
			my $tardat = "$folder/$basename.dat.bz2";
			my $size = ( -s $filename ) + ( -s $tardat );
			$size += -s $current_dat unless $day;

			################################################################
			my $infofile = "$folder/$basename.info.txt";
			my $info = $this->get_backup_info( $filename, $infofile );
			################################################################

			# append day folder
			my ( $date, $time ) = split / /o, $basename;
			if ( $date ne $day ) {
				$tree_store->set( $day_iter,
					COL_SIZE,  $day_folder_size,
					COL_HSIZE, format_bytes($day_folder_size),
					COL_FILES, $day_folder_files,
				) if ref $day_iter;

				$day      = $date;
				$day_iter = $tree_store->append(undef);
				$tree_store->set( $day_iter,
					COL_DATE,    $date,
					COL_HDATE,   format_date($date),
					COL_TIME,    $time,
					COL_NAME,    format_date($date),
					COL_SIZE,    $day_folder_size,
					COL_FILES,   $day_folder_files,
					COL_PATH,    $filename,
					COL_LABEL,   "",
					COL_CURRENT, FALSE,
					COL_WEIGHT,  600,
				);

				$day_folder_size  = 0;
				$day_folder_files = 0;
			}

			#printf "%s\n", $tardat unless -s $tardat if $DEBUG > 3;
			$day_folder_size  += $size;
			$day_folder_files += scalar @{ $info->{files} };

			# day-time column
			my $iter = $tree_store->append($day_iter);
			$tree_store->set( $iter,
				COL_DATE,    $date,
				COL_HDATE,   format_date($date),
				COL_TIME,    $time,
				COL_NAME,    $time,
				COL_SIZE,    $size,
				COL_HSIZE,   format_bytes($size),
				COL_FILES,   scalar @{ $info->{files} },
				COL_LABEL,   __("$info->{label}") . ("$info->{label}" ? ", ":"") . ($date_of_last_restore eq $basename ? __("currently restored") : ""),
				COL_PATH,    $filename,
				COL_CURRENT, $date_of_last_restore eq $basename,
				COL_WEIGHT,  800,
			);
		}

		# append last day folder
		$tree_store->set( $day_iter,
			COL_SIZE,  $day_folder_size,
			COL_HSIZE, format_bytes($day_folder_size),
			COL_FILES, $day_folder_files,
		) if ref $day_iter;
	}

	$this->size_all->set_text( format_bytes( folder_size($folder) ) );

	#this will create a treeview, specify $tree_store as its model
	$this->tree_view->set_model($tree_store);
	$this->exclude_configure;
	#$this->window->set_sensitive(TRUE);
}

sub get_backup_info {
	my ( $this, $filename, $infoname ) = @_;

	tie my %info, 'Tie::DataDumper', $infoname
	  or warn "Problem tying %info: $!";

	$info{files} = [ $this->get_changed_files($filename) ] unless exists $info{files};
	$info{label} = '' unless exists $info{label};

	return \%info;
}

sub get_changed_files {
	my ( $this, $filename ) = @_;
	my $cmd = qq{ env LANG=en_GB.utf8 nice --adjustment=17 \\
			 env LANG=en_GB.utf8 tar --list \\
				 --file "$filename" \\
				 | nice --adjustment=17 grep -E "[^//]\$"
			};
	printf "cmd %s\n", $cmd if $DEBUG > 0;

	my @changed_files = `$cmd`;
	chomp @changed_files;

	printf "changed_files %s\n", scalar @changed_files if $DEBUG > 3;
	return @changed_files;
}

sub get_store_folder {
	my ($this) = @_;
	return sprintf "%s%s", $this->get_main_store_folder || "",
	  $this->folder->get_filename || "";
}

sub on_tree_view_button_press_event {
	my ( $this, $widget, $event ) = @_;
	#print "on_tree_view_button_press_event $this, $widget", $event->type, "\n" if $DEBUG > 3;

	$this->restore_button->set_sensitive(TRUE);
	$this->{tree_view_2button_press} = $event->type eq "2button-press";

	return;
}

sub on_tree_view_button_release_event {
	my ( $this, $widget, $event ) = @_;
#print "on_tree_view_button_release_event $this, $widget", $this->{tree_view_2button_press}, "\n" if $DEBUG > 3;

	my $selected = $this->tree_view->get_selection->get_selected;

	if ( ref $selected ) {
		my ( $hdate, $time ) = $this->tree_view->get_model->get( $selected, COL_HDATE, COL_TIME );
		$this->restore_backup_from_label->set_text("$hdate $time");

		if ( $this->{tree_view_2button_press} ) {
			my $path = $this->tree_view->get_model->get( $selected, COL_PATH );

			printf "*** %s\n", $path if $DEBUG > 3;

			system $TarOpenCmd, $path;
		}
	}

	return;
}

=backup 	 																								
																											
=cut																										

sub on_backup_button_clicked {
	my ($this) = @_;
	print "on_backup_button_clicked $this\n" if $DEBUG > 3;

	$this->window->set_sensitive(FALSE);
	$this->backup_changed_files_label->set_text(0);
	$this->backup_folders_label->set_text(0);
	$this->backup_elapsed_time_label->set_text( sprintf "%s", strtime(0) );
	$this->backup_estimated_time_label->set_text( sprintf "%s / %s",
		map { strtime(0) } ( 0, 0 ) );
	$this->backup_file_label->set_text("");
	$this->backup_progress(0);
	$this->backup_dialog->present;

	$this->backup_folder;

	$this->{folder_recycle_button}->add_filename( $this->folder->get_filename );
	$this->fill_tree;

	$this->backup_dialog->hide;
	$this->window->set_sensitive(TRUE);
	return;
}

sub rmdir_p {
	my ($folder) = @_;
	while ( rmdir $folder ) {
		$folder = dirname $folder;
	}
	return;
}

my @SIGS = qw(KILL HUP TERM INT);

sub backup_folder {
	my ($this) = @_;
	#$this->{backup_folder} = TRUE;

	my $date = strftime( "%F %X", localtime );

	$this->log_add_text( sprintf "\n%s\n", "*" x 42 );
	$this->log_add_text( sprintf __("%s starting backup . . .\n"), $date );

	$this->backup_progress(0);
	$this->backup_dialog->{startTime} = time;

	my $folder    = $this->folder->get_filename;
	my $store     = $this->get_store_folder;
	my $mainstore = $this->get_main_store_folder;

	my $current_dat = "$store/$CurrentDat";
	my $process_dat = "$store/process.dat";
	my $archive     = "$store/$date.tar.bz2";
	my $tardat      = "$store/$date.dat.bz2";
	my $excludes    = "$store/$ExcludesFile";

	$this->{cleanup} = sub {
		print "cleanup @_\n" if $DEBUG > 3;
		unlink $tardat;
		unlink $process_dat;
		unlink $archive;
		unless ( -s $current_dat ) {
			unlink $current_dat;
			unlink $excludes;
			rmdir_p $store;
		}
		exit if @_;
	};

	$SIG{$_} = $this->{cleanup} foreach @SIGS;

	system "mkdir", "-p", $store;
	system "cp", $current_dat, $process_dat if -e $current_dat;

	system "touch", $current_dat;
	$this->save_excludes;

	my $cmd = qq{ env LANG=en_GB.utf8 nice --adjustment=17 \\
	 env LANG=en_GB.utf8 tar --create \\
		 --verbose \\
		 --directory "$folder" \\
		 --file "$archive" \\
		 --listed-incremental "$process_dat" \\
		 --preserve-permissions \\
		 --ignore-failed-read \\
		 --exclude "$mainstore" \\
		 --exclude-from "$excludes" \\
		 --bzip2 \\
		 ./ 2>&1 \\
	};

	printf "cmd %s\n", $cmd if $DEBUG > 3;
	die $! unless $this->{tarpid} = open TAR, "-|", $cmd;

	my $size       = 1;
	my $files      = {};
	my $total_size = 1;
	my $folders    = 0;
	my $utf8       = Unicode::UTF8simple->new;
	while (<TAR>) {
		#print $_ if $DEBUG > 3;
		#last unless $this->{backup_folder};
		chomp;
		$_ = $utf8->fromUTF8( "iso-8859-1", $_ );

		if (s|^\./||o) {
			my $path = "$folder/$_";

			if ( -d $path ) {
				$this->backup_folders_label->set_text( ++$folders );
				Gtk2->main_iteration while Gtk2->events_pending;
			} else {
				unless ( exists $files->{$path} ) {
					$total_size += $files->{$path} = ( -s $path ) || 0;
					$this->backup_changed_files_label->set_text(
						sprintf "%d [%sB]",
						scalar keys %$files,
						format_bytes($total_size)
					);
				}

				$files->{$path} = 0 unless defined $files->{$path};    # wegen: -s link = 0

				my @times =
				  map { strtime($_) }
				  estimated_time( $this->backup_dialog->{startTime}, $size, $total_size );

				$this->backup_elapsed_time_label->set_text( sprintf "%s", $times[0] );
				$this->backup_estimated_time_label->set_text( sprintf "%s / %s",
					@times[ 1, 2 ] );
				$this->backup_file_label->set_text( sprintf "%s [%sB]",
					$path, format_bytes( $files->{$path} ) );

				$this->backup_progress( $size / $total_size );

				$size += $files->{$path};
			}

		} elsif ( m|^tar: \./(.*?): Directory is new$|o
			|| m|^tar: \./(.*?): Directory has been renamed from.*$|o )
		{
			#print "$1\n" if $DEBUG > 3;
			$files->{$_} = -s $_ foreach get_files("$folder/$1");
			$total_size += folder_size("$folder/$1");
			$this->backup_changed_files_label->set_text(
				sprintf "%d [%sB]",
				scalar keys %$files,
				format_bytes($total_size)
			);
			Gtk2->main_iteration while Gtk2->events_pending;
		} elsif (/^(tar: )?(Terminated|Killed|Hangup)$/o) {
			print "$_\n" if $DEBUG > 3;
		} else {
			print "$_\n" if $DEBUG > 3;
		}
	}
	close TAR;

	printf "tar returned %s\n", $? if $DEBUG > 3;

	if ($?) {    # cancel / erroor ...
		&{ $this->{cleanup} }();
	} else {     # everything is fine
		my $retval = system qq{ nice --adjustment=17 \\
			bzip2 -c9 "$process_dat" >  "$tardat" \\
		};
		printf "bzip2 returned %s\n", $retval if $DEBUG > 3;

		$SIG{$_} = 'IGNORE' foreach @SIGS;

		system "cp", $process_dat, $current_dat;
		unlink $process_dat;
	
		#store date
		$this->store_restore_date( $archive );
	}

	delete $SIG{$_} foreach @SIGS;

	#$this->{backup_folder} = FALSE;
	$this->{tarpid} = 0;

	if ( $this->backup_dialog->{startTime} ) {
		$this->log_add_text( sprintf __("Changed files: %d\n"), scalar keys %$files );
		$this->log_add_text( sprintf __("Folders: %d\n"),       $folders );
		$this->log_add_text( sprintf __("Totol size: %s\n"),    format_bytes($total_size) );
		$this->log_add_text( sprintf __("Totol time: %s\n"),
			strtime( localtime( time - $this->backup_dialog->{startTime} ) )
		);
	}

	$this->log_add_text( sprintf __("%s backup done.\n"), strftime( "%F %X", localtime ) )
	  if $this->backup_dialog->{startTime};
}

sub backup_progress {
	my ( $this, $fraction ) = @_;

	$this->backup_progressbar->set_fraction($fraction);
	$this->backup_progressbar->set_text( sprintf "%.2f %%", $fraction * 100 );
	#$this->backup_dialog->set_title( sprintf "Backup in progress %.2f %%", $fraction * 100 );

	Gtk2->main_iteration while Gtk2->events_pending;

	return;
}

sub on_cancel_backup {
	my ($this) = @_;
	printf "on_cancel_backup %s\n", $this->{tarpid} if $DEBUG > 3;
	system "pkill", "-P", $this->{tarpid};
	$this->backup_dialog->{startTime} = 0;
	$this->backup_progressbar->set_fraction(1);
	$this->backup_progressbar->set_text( __ "canceling backup ..." );
	#$this->{backup_folder} = FALSE;
	$this->log_add_text( sprintf __("%s backup canceled.\n"), strftime( "%F %X", localtime ) );
	return 1;
}

=exclude 																								
																											
=cut																										

sub exclude_configure {
	my ($this) = @_;
	#printf "exclude_configure %s\n", $this->get_excludes_filename if $DEBUG > 3;

	$this->exclude_clear;

	my $folder = $this->folder->get_filename || "";

	my $excludes = $this->get_excludes_filename;
	#unlink $excludes;

	if ( -e $excludes ) {
		my @excludes = `cat "$excludes"`;
		foreach (@excludes) {
			chomp;
			next unless $_;
			if ( -f "$folder/$_" ) {
				$this->exclude_add( EXCLUDE_FILE, "$folder/$_" );
			} elsif ( -d "$folder/$_" ) {
				$this->exclude_add( EXCLUDE_FOLDER, "$folder/$_" );
			} else {
				$this->exclude_add( EXCLUDE_PATTERN, $_ );
			}
		}

	} elsif ( $ENV{HOME} =~ /^\Q$folder/ ) {
		$this->exclude_add( EXCLUDE_PATTERN, "*.Trash*" );
		$this->exclude_add( EXCLUDE_FILE,    "$ENV{HOME}/.xsession-errors" );
	}

}

sub exclude_clear {
	my ($this) = @_;
	$this->exclude_box->remove($_) foreach $this->exclude_box->get_children;
	return;
}

sub on_exclude_add {
	my ($this) = @_;
	#printf "on_exclude_add %s\n", $this->exclude_combo->get_active if $DEBUG > 3;
	$this->exclude_add( $this->exclude_combo->get_active );
	return;
}

sub exclude_add {
	my ( $this, $index, @values ) = @_;
	#printf "exclude_add %s\n", $index if $DEBUG > 3;

	my $widget = undef;
	$widget = $this->exclude_folder_add(@values)  if $index == EXCLUDE_FOLDER;
	$widget = $this->exclude_file_add(@values)    if $index == EXCLUDE_FILE;
	$widget = $this->exclude_pattern_add(@values) if $index == EXCLUDE_PATTERN;
	return unless ref $widget;

	$this->exclude_combo->set_active($index);

	my $label = new Gtk2::Label( sprintf "%s:", $this->exclude_combo->get_active_text );
	my $remove_button = Gtk2::Button->new_from_stock('gtk-remove');

	my $hbox = new Gtk2::HBox( 0, 6 );
	$hbox->pack_start( $label,         FALSE, FALSE, 0 );
	$hbox->pack_start( $widget,        TRUE,  TRUE,  0 );
	$hbox->pack_start( $remove_button, FALSE, FALSE, 0 );
	$hbox->show_all;

	$remove_button->signal_connect( 'clicked', sub { $this->exclude_folder_remove($hbox) } );

	$this->exclude_box->add($hbox);

	$this->save_excludes;
	return;
}

sub exclude_folder_remove {
	my ( $this, $widget ) = @_;
	#printf "exclude_folder_remove %d\n", $widget if $DEBUG > 3;
	$this->exclude_box->remove($widget);
	$this->save_excludes;
	return;
}

sub exclude_folder_add {
	my ( $this, $folder ) = @_;
	#printf "exclude_folder_add %s\n", $folder || $this->folder->get_filename if $DEBUG > 3;
	my $widget = new Gtk2::FileChooserButton( __("Select folder"), 'select-folder' );

	$widget->set_current_folder( $folder || $this->folder->get_filename );
	$widget->{pattern} = $folder || $this->folder->get_filename;

	$widget->signal_connect( 'current-folder-changed', sub {
			$widget->{pattern} = $widget->get_filename;
			$this->save_excludes;
	} );

	return $widget;
}

sub exclude_file_add {
	my ( $this, $file ) = @_;
	#printf "** exclude_file_add %s\n", $file || $this->folder->get_filename if $DEBUG > 3;
	my $widget = new Gtk2::FileChooserButton( __("Select file"), 'open' );

	if ( -f $file ) {
		$widget->set_filename($file);
		$widget->{pattern} = $file;
	} else {
		$widget->set_current_folder( $this->folder->get_filename );
	}

	$widget->signal_connect( 'current-folder-changed', sub {
			return unless $widget->get_filename;
			$widget->{pattern} = $widget->get_filename;
			$this->save_excludes;
	} );

	return $widget;
}

sub exclude_pattern_add {
	my ( $this, $pattern ) = @_;
	#printf "exclude_pattern_add %s\n", $pattern || "" if $DEBUG > 3;
	my $widget = new Gtk2::Entry;

	if ($pattern) {
		$widget->set_text($pattern);
		$widget->{pattern} = $pattern;
	}

	$widget->signal_connect( 'changed', sub {
			$widget->{pattern} = $widget->get_text;
			$this->save_excludes;
	} );

	return $widget;
}

sub save_excludes {
	my ($this) = @_;
	return unless -e $this->get_store_folder . "/$CurrentDat";
	my $folder   = $this->folder->get_filename;
	my $excludes = $this->get_excludes_filename;
	#printf "save_excludes\n" if $DEBUG > 3;

	open( EXCLUDES, ">", $excludes ) || die $!;
	printf EXCLUDES "%s\n", join "\n",
	  map { s/^\Q$folder\E\/?//; $_ }
	  grep { $_ }
	  map  { ( $_->get_children )[1]->{pattern} }
	  $this->exclude_box->get_children;
	close EXCLUDES;
	return;
}

sub get_excludes_filename {
	my ($this)   = @_;
	my $store    = $this->get_store_folder;
	my $excludes = "$store/$ExcludesFile";
	return $excludes;
}

=restore 																								
																											
=cut																										

sub on_restore_button_clicked {
	my ($this) = @_;
	print "on_restore_button_clicked $this\n" if $DEBUG > 3;
	$this->window->set_sensitive(FALSE);
	$this->restore_dialog->show;
	return;
}

sub on_restore_dialog_cancel {
	my ( $this, $widget ) = @_;
	print "on_restore_folder_dialog_cancel $this\n" if $DEBUG > 3;
	$this->restore_dialog->hide;
	$this->window->set_sensitive(TRUE);
	return 1;
}

sub on_restore_dialog_ok {
	my ( $this, $widget ) = @_;
	$this->restore_dialog->hide;
	$this->restore_backup;
	$this->fill_tree;
	$this->window->set_sensitive(TRUE);
	return;
}

sub restore_backup {
	my ($this) = @_;

	my $restore_to_folder = $this->restore_folder->get_filename;
	my @files             = $this->get_files_to_restore;

	$this->log_add_text( sprintf "\n%s\n", "*" x 42 );
	$this->log_add_text( sprintf __("%s restore . . .\n"), strftime( "%F %X", localtime ) );

	my $store = $this->get_store_folder;

	# restore process.dat and old $CurrentDat
	#my $date        = basename( $files[0], ".tar.bz2" );
	#my $current_dat = "$store/$CurrentDat";
	#my $process_dat = "$store/process.dat";
	#my $archive_dat = "$store/$date.dat.bz2";
	#system "bzip2 -c -d '$archive_dat' >'$process_dat'";

	#printf "bzip2 -c -d '$archive_dat' >'$current_dat'\n";
	#system "bzip2 -c -d '$archive_dat' >'$current_dat'";
	#unlink $process_dat;

	my $utf8 = Unicode::UTF8simple->new;

	printf "***restore_backup to folder: %s\n", $restore_to_folder if $DEBUG > 3;
	foreach my $file (@files) {
		#printf "file: %s\n", $file if $DEBUG > 3;
		$this->log_add_text( sprintf __("restore backup from %s\n"), basename( $file, ".tar.bz2" ) );

		my $cmd = qq{ env LANG=en_GB.utf8 nice --adjustment=17 \\
		 env LANG=en_GB.utf8 tar --extract \\
			--verbose \\
			--directory "$restore_to_folder" \\
			--file "$file" \\
			--preserve-permissions \\
			--listed-incremental /dev/null \\
			./ 2>&1 \\
		};

		printf "cmd %s\n", $cmd if $DEBUG > 3;
		die $! unless $this->{tarpid} = open TAR, "-|", $cmd;
		while (<TAR>) {
			#print $_ if $DEBUG > 0;
			chomp;
			#last unless $this->{backup_folder};
			#chomp;
			#$_ = $utf8->fromUTF8( "iso-8859-1", $_ );
			#Gtk2->main_iteration while Gtk2->events_pending;
		}
		close TAR;
		printf "tar returned %s\n", $? if $DEBUG > 3;

		if ($?) {    # cancel / erroor ...
		} else {     # everything is fine
		}

	}

	#store date
	$this->store_restore_date( $files[$#files] );

	#finish
	$this->{tarpid} = 0;
	$this->log_add_text( sprintf __("%s restore done . . .\n"), strftime( "%F %X", localtime ) );
}

sub get_files_to_restore {
	my ($this) = @_;

	my @files = ();

	my $selected = $this->tree_view->get_selection->get_selected;
	my $file     = $this->tree_view->get_model->get( $selected, COL_PATH );
	my $folder   = dirname $file;

	#printf "***get_files_to_restore file: %s\n", $file if $DEBUG > 3;
	#printf "***get_files_to_restore folder %s\n", $folder if $DEBUG > 3;

	foreach my $filename ( grep { m/\.tar\.bz2$/ } get_files($folder) ) {
		push @files, $filename;
		last if $filename eq $file;
	}

	return @files;
}

=schedule		 																						
																											
=cut																										

sub on_schedule_enabled_button_toggled {
	my ( $this, $widget ) = @_;
	print "on_schedule_enabled_button_toggled $this\n" if $DEBUG > 3;
	$this->time_hbox->set_sensitive( $widget->get_active );
	$this->wdays_hbox->set_sensitive( $widget->get_active );
	return;
}

=store			 																						
																											
=cut																										

sub on_store_folder_changed {
	my ($this) = @_;
	printf "on_store_folder_changed %s\n", $this->get_main_store_folder if $DEBUG > 3;

	my $store = $this->get_main_store_folder;

	#system "mkdir", "-p", $store;
	$this->gconf( 'store-folder',      $this->store_folder->get_filename );
	$this->gconf( 'store-folder-name', $this->store_folder_name->get_text );

	my @folders = $this->get_store_folders;
	$this->{folder_recycle_button}->add_filename($_)
	  foreach @folders;

	$this->fill_tree;
	return;
}

sub get_main_store_folder {
	my ($this) = @_;
	return sprintf "%s/%s", $this->store_folder->get_filename, $this->store_folder_name->get_text;
}

sub get_store_folders {
	my ($this) = @_;
	my $store = $this->get_main_store_folder;
	return
	  map { s/^$store//; $_; }
	  grep { -e "$_/$CurrentDat" }
	  get_all_sub_folders($store);
}

sub on_store_folder_name_key_release_event {
	my ( $this, $widget, $event ) = @_;
	if ( $event->keyval == $Gtk2::Gdk::Keysyms{KP_Enter} or $event->keyval == $Gtk2::Gdk::Keysyms{Return} ) {
		printf "on_store_folder_name_changed %s\n", $event->keyval if $DEBUG > 3;
		$this->on_store_folder_changed;
	}
}

=expander		 																						
																											
=cut																										

sub configure_expander {
	my ($this) = @_;
	printf "*** configure_expander\n" if $DEBUG > 3;

	$this->exclude_expander->set_expanded( $this->gconf('exclude_expander') )
	  if defined $this->gconf('exclude_expander');

	$this->schedule_expander->set_expanded( $this->gconf('schedule_expander') )
	  if defined $this->gconf('schedule_expander');

	$this->store_expander->set_expanded( $this->gconf('store_expander') )
	  if defined $this->gconf('store_expander');

	$this->log_expander->set_expanded( $this->gconf('log_expander') )
	  if defined $this->gconf('log_expander');

	return;
}

sub on_expander_activate {
	my ( $this, $widget ) = @_;
	printf "%s, %s\n", $widget->get_name, not $widget->get_expanded ? 1 : 0 if $DEBUG > 3;
	$this->gconf( $widget->get_name, not $widget->get_expanded ? 1 : 0 );
	return;
}

=expander nop	 																						
	disables expander																					
=cut																										

sub expander_nop {
	my ( $this, $expander ) = @_;
	$expander->set_expanded(FALSE);
	return;
}

=log	 																									
																											
=cut																										

sub log_init {
	my ($this) = @_;
	my $tview  = $this->log_textview;
	my $buffer = $tview->get_buffer();
	$this->{log_end_mark} = $buffer->create_mark( 'end', $buffer->get_end_iter, FALSE );
	$buffer->signal_connect( insert_text => \&on_log_insert_text, $this );
}

sub log_add_text {
	my ( $this, @text ) = @_;
	my $tview   = $this->log_textview;
	my $content = join "", @text;
	my $buffer  = $tview->get_buffer();
	$buffer->insert( $buffer->get_end_iter, $content );
	Gtk2->main_iteration while Gtk2->events_pending;
}

sub log_clear {
	my ($this) = @_;
	my $tview  = $this->log_textview;
	my $buffer = $tview->get_buffer();
	$buffer->set_text("");
}

sub on_log_insert_text {
	my $this  = pop @_;
	my $tview = $this->log_textview;
	$tview->scroll_mark_onscreen( $this->{log_end_mark} );
}

sub get_tar_version {
	my ($this) = @_;
	my $cmd = qq{ tar --version };
	return `$cmd`;
}

sub store_restore_date {
	my ($this, $file) = @_;
	my $store = dirname($file);
	my $restore_date = basename( $file, ".tar.bz2" );
	my $date_txt = "$store/date.txt";
	system "echo '$restore_date' > '$date_txt'";
	#printf "%s\n", "echo '$restore_date' > '$date_txt'";

}

sub fetch_restore_date {
	my ($this, $store) = @_;
	my $date_txt = "$store/date.txt";
	return "" unless -e $date_txt;
	my $date = `cat '$date_txt'`;
	chomp $date;
	printf "*** %s\n", $date;

	return $date;
}

=gtk_main_quit 																						
																											
	$this->gtk_main_quit;																			
																											
	calls Gtk2->main_quit;																			
																											
=cut																										

sub gtk_main_quit {
	my ($this) = @_;
	print "gtk_main_quit\n" if $DEBUG > 3;
	Gtk2->main_quit;
	return;
}

sub DESTROY {
	my ($this) = @_;
	return;
}

1;
__END__

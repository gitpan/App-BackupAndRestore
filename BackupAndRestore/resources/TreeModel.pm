#!/usr/bin/perl -w
package BackupAndRestore::TreeModel;
use strict;
use warnings;

use Gtk2;
use Glib qw(TRUE FALSE);

use File::Basename qw(basename dirname);

use BackupAndRestore::Helper;

# maybe bad style, but makes life a lot easier
use Exporter 'import';

our @EXPORT = qw(
  COL_TIME
  COL_NAME
  COL_SIZE
  COL_HSIZE
  COL_PATH
  N_COLUMNS
);

# The data columns that we export via the tree model interface

use enum qw(
  COL_TIME
  COL_NAME
  COL_SIZE
  COL_HSIZE
  COL_PATH
  N_COLUMNS
);

#
#  here we register our new type and its interfaces with the type system.
#  If you want to implement additional interfaces like GtkTreeSortable,
#  you will need to do it here.
#

use Glib::Object::Subclass Glib::Object::,
  interfaces => [Gtk2::TreeModel::],
  ;

#
# this is called everytime a new custom list object
# instance is created (we do that in new).
# Initialise the list structure's fields here.
#

sub INIT_INSTANCE {

	my $this = shift;

	$this->{n_columns}    = N_COLUMNS;
	$this->{column_types} = [
		'Glib::String',    # COL_TIME
		'Glib::String',    # COL_NAME
		'Glib::UInt',      # COL_SIZE
		'Glib::String',    # COL_HSIZE
		'Glib::String',    # COL_PATH
	];

	$this->{root} = [];

	# Random int to check whether an iter belongs to our model
	$this->{stamp} = sprintf '%d', rand( 1 << 31 );
}

#
#  this is called just before a custom list is
#  destroyed. Free dynamically allocated memory here.
#

sub FINALIZE_INSTANCE {
	my $this = shift;

	# free all records and free all memory used by the list
	#warning IMPLEMENT
}

#
# tells the rest of the world whether our tree model has any special
# characteristics. In our case, we have a list model (instead of a tree).
# Note that unlike the C version of this custom model, our iters do NOT
# persist.
#

#sub GET_FLAGS {  [qw/list-only iters-persist/] }
sub GET_FLAGS { [] }

#
# tells the rest of the world how many data
# columns we export via the tree model interface
#

sub GET_N_COLUMNS { shift->{n_columns}; }

#
# tells the rest of the world which type of
# data an exported model column contains
#

sub GET_COLUMN_TYPE {
	#
	my ( $this, $index ) = @_;
	# and invalid index will send undef back to the calling XS layer,
	# which will croak.
	return $this->{column_types}[$index];
}

#
# converts a tree path (physical position) into a
# tree iter structure (the content of the iter
# fields will only be used internally by our model).
# We simply store a pointer to our CustomRecord
# structure that represents that row in the tree iter.
#

sub GET_ITER {
	my ( $this, $path ) = @_;
	#printf "%s\n",  $path->to_string;

	my @indices = $path->get_indices;
	my $depth   = $path->get_depth;

	return undef unless $this->{children};

	my $n      = 0;
	my $object = $this->{children};
	my $parent;

	foreach (@indices) {
		$n = $_;
		if ( $object->isa('X3DField') and $object->getType eq "MFNode" ) {
			$parent = $object;
			$object = $object->get1Value($n);
		} elsif ( $object->isa('SFNode') ) {
			my $node      = $object->getValue;
			my $fieldName = $node->getFieldDefinitions->[$n]->getName;
			$parent = $object;
			$object = $node->getField($fieldName);
		}
	}

	return undef unless $parent;

	#X3DError::Debug $n, ref $parent, ref $object->getValue;
	return [ $this->{stamp}, $n, $parent, $object ];
}

#
#  get_path: converts a tree iter into a tree path (ie. the
#                        physical position of that row in the list).
#

sub GET_PATH {
	my ( $this, $iter ) = @_;

	die;

	my $record = $iter->[2];

	my $path = Gtk2::TreePath->new;
	$path->append_index( $record->{pos} );
	return $path;
}

#
# get_value: Returns a row's exported data columns
#                        (_get_value is what gtk_tree_model_get uses)
#

sub GET_VALUE {
	my ( $this, $iter, $column ) = @_;
	#

	return undef unless $column < @{ $this->{column_types} };

	my $parent = $iter->[2];
	my $object = $iter->[3];

	return undef unless ref $object;

	return;
}

#
# iter_next: Takes an iter structure and sets it to point to the next row.
#

sub ITER_NEXT {
	my ( $this, $iter ) = @_;
	#X3DError::Debug $iter->[1];

	return undef unless $iter && $iter->[2];

	my $n      = $iter->[1] + 1;
	my $parent = $iter->[2];
	my $object = $iter->[3];

	if ( $parent->isa("X3DField") and $parent->getType eq "MFNode" ) {
		# Is this the last record in the list?
		return undef if $n > $#{ $parent->getValue };
		$object = $parent->get1Value($n);

	} elsif ( $parent->isa("SFNode") ) {
		my $node             = $parent->getValue;
		my $fieldDefinitions = $node->getFieldDefinitions;
		return undef if $n > $#$fieldDefinitions;

		my $fieldName = $node->getFieldDefinitions->[$n]->getName;
		$object = $node->getField($fieldName);
	}

	#X3DError::Debug $iter->[1];
	return [ $this->{stamp}, $n, $parent, $object ];
}

#
# iter_children: Returns TRUE or FALSE depending on whether the row
#                specified by 'parent' has any children.  If it has
#                children, then 'iter' is set to point to the first
#                child.  Special case: if 'parent' is undef, then the
#                first top-level row should be returned if it exists.
#

sub ITER_CHILDREN { &ITER_NTH_CHILD( @_, 0 ) }

#
# iter_has_child: Returns TRUE or FALSE depending on whether
#                 the row specified by 'iter' has any children.
#                 We only have a list and thus no children.
#

sub ITER_HAS_CHILD { &ITER_N_CHILDREN(@_) }

#
# iter_n_children: Returns the number of children the row specified by
#                  'iter' has. This is usually 0, as we only have a list
#                  and thus do not have any children to any rows.
#                  A special case is when 'iter' is undef, in which case
#                  we need to return the number of top-level nodes, ie.
#                  the number of rows in our list.
#

sub ITER_N_CHILDREN {
	my ( $this, $iter ) = @_;
	#

	# special case: if iter == NULL, return number of top-level rows
	return $this->{children}->getValue->length if !$iter;

	my $n      = $iter->[1];
	my $parent = $iter->[2];
	my $object = $iter->[3];

	if ( $object->isa("SFNode") ) {
		my $node             = $object->getValue;
		my $fieldDefinitions = $node->getFieldDefinitions;
		#X3DError::Debug scalar @$fieldDefinitions;
		return scalar @$fieldDefinitions;
	} elsif ( $object->isa("X3DField") ) {
	}

	return;
}

#
# iter_nth_child: If the row specified by 'parent' has any children,
#                 set 'iter' to the n-th child and return TRUE if it
#                 exists, otherwise FALSE.  A special case is when
#                 'parent' is NULL, in which case we need to set 'iter'
#                 to the n-th row if it exists.
#

sub ITER_NTH_CHILD {
	my ( $this, $iter, $n ) = @_;
	#X3DError::Debug $n;

	# iter == NULL is a special case; we need to return the first top-level row
	unless ($iter) {
		return undef unless $this->{children};
		return undef if $n > $#{ $this->{children} };
		return [ $this->{stamp}, $n, $this->{children}, $this->{children}->[$n] ];
	}

	my $object = $iter->[3];
	my $parent;

	if ( $object->isa("SFNode") ) {
		my $node             = $object->getValue;
		my $fieldDefinitions = $node->getFieldDefinitions;
		return undef if $n > $#$fieldDefinitions;

		my $fieldName = $fieldDefinitions->[$n]->getName;
		$parent = $object;
		$object = $node->getField($fieldName);

	} elsif ( $object->isa("X3DField") ) {
	}

	#X3DError::Debug 0, ref $parent, ref $object;
	return [ $this->{stamp}, $n, $parent, $object ];
}

#
# iter_parent: Point 'iter' to the parent node of 'child'.  As we have a
#              a list and thus no children and no parents of children,
#              we can just return FALSE.
#

sub ITER_PARENT {
	my ( $this, $iter ) = @_;

	my $n;
	my $parent;
	my $object = $iter->[2];

	return undef
	  if ref $object eq ref $this->{children};    ## $parent->getId == $this->{children}->getId

	if ( $object->isa("MFNode") ) {
	} elsif ( $object->isa("SFNode") ) {
		my $node = $object->getValue;
		foreach ( $node->getParents ) {
			printf "%s\n", $_->getType;
		}

		$parent = $this->{children};
		$n      = $parent->index($object);
	}

	return [ $this->{stamp}, $n, $parent, $object ];
}

#
# ref_node and unref_node get called as the model manages the lifetimes
# of nodes in the model.  you normally don't need to do anything for these,
# but may want to if you plan to implement data caching.
#
#sub REF_NODE {  warn "REF_NODE @_\n"; }
#sub UNREF_NODE {  warn "UNREF_NODE @_\n"; }

############################################################################
############################################################################
############################################################################

sub set_folder {
	my ( $this, $folder ) = @_;
	return unless -e $folder;

	$this->{folder} = $folder;

	@{ $this->{root} } = $this->get_days;

	#printf "set_folder %s %d\n", $folder, scalar $this->get_days;

	foreach ( 0 ... $#{ $this->{root} } ) {
		my $path = new Gtk2::TreePath($_);
		my $iter = $this->get_iter($path);
		$this->row_inserted( $path, $iter );
		$this->row_has_child_toggled( $path, $iter );
	}

}

sub get_days {
	my ($this) = @_;

	my $days = {};
	my @days = ();

	foreach my $filename ( reverse grep { m/\.tar\.bz2$/ } get_files( $this->{folder} ) ) {
		my $basename = basename( $filename, ".tar.bz2" );
		my ( $date, $time ) = split / /o, $basename;

		unless ( exists $days->{$date} ) {
			$days->{$date} = 1;
			push @days, $filename;
		}
	}

	return wantarray ? @days : scalar @days;
}

1;
__END__

sub fill_tree {
	my ($this) = @_;
	$this->restore_button->set_sensitive(FALSE);
	#fill it with arbitry data

	my $folder = $this->get_store_folder;
	printf "fill_tree %s\n", $folder;

	my $tree_store =
	  Gtk2::TreeStore->new(qw/Glib::String Glib::String Glib::UInt Glib::String Glib::String/);

	if ( -e $folder ) {
		my ( $day_iter, $day, $day_folder_size ) = ( undef, "", 0 );
		my $current_dat = "$folder/current.dat";

		foreach my $filename ( reverse grep { m/\.tar\.bz2$/ } get_files($folder) ) {
			my $basename = basename( $filename, ".tar.bz2" );
			my $tardat = "$folder/$basename.dat.bz2";

			my $size = ( -s $filename ) + ( -s $tardat );
			$size += -s $current_dat unless $day;

			my ( $date, $time ) = split / /o, $basename;
			if ( $date ne $day ) {
				$tree_store->set( $day_iter,
					COL_SIZE,  $day_folder_size,
					COL_HSIZE, format_bytes($day_folder_size),
				) if ref $day_iter;

				$day      = $date;
				$day_iter = $tree_store->append(undef);
				$tree_store->set( $day_iter,
					COL_TIME, $date,
					COL_NAME, format_date($date),
					COL_SIZE, $day_folder_size,
					COL_PATH, $filename,
				);

				$day_folder_size = 0;
			}

			#printf "%s\n", $tardat unless -s $tardat;
			$day_folder_size += $size;

			my $iter = $tree_store->append($day_iter);
			$tree_store->set( $iter,
				COL_TIME,  "$date $time",
				COL_NAME,  $time,
				COL_SIZE,  $size,
				COL_HSIZE, format_bytes($size),
				COL_PATH,  $filename,
			);
		}

		$tree_store->set( $day_iter,
			COL_SIZE,  $day_folder_size,
			COL_HSIZE, format_bytes($day_folder_size),
		) if ref $day_iter;
	}

	$this->size_all->set_text( format_bytes( folder_size($folder) ) );

	#this will create a treeview, specify $tree_store as its model
	my $tree_model = new BackupAndRestore::TreeModel;
	$tree_model->set_folder( $this->get_store_folder );
	$this->tree_view->set_model($tree_model);
}


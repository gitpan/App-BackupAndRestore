#!/usr/bin/perl -w
use strict;
use warnings;

use lib "$ENV{HOME}/perl";

#printf "*** %s\n", $0;

use App::BackupAndRestore;
run App::BackupAndRestore;

0;
__END__

#!/usr/bin/perl

use warnings;
use strict;
use Encode;
use utf8;
use DBI;

my $db     = 'blog';
my $server = 'localhost';
my $id     = 'luc';
my $pwd    = 'PwD';
my $port   = '';

# Connecting
print "Connecting to database $db.\n";
my $dbh = DBI->connect("DBI:mysql:database=$db;host=$server;port=$port",
    $id, $pwd, { RaiseError => 1, }
) or die "Couldn't connect to database $db : $DBI::errstr";

# Get all articles
my $prep = $dbh->prepare('select * from articles;') or die $dbh->errstr();
$prep->execute() or die 'Request failed !';
while(my @row = $prep->fetchrow_array) {
    print "@row\n";
}
$prep->finish();

# Disconnecting
$dbh->disconnect();


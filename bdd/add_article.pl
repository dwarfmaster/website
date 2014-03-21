#!/usr/bin/perl

use strict;
use warnings;
use Encode;
use utf8;
use DBI;

my $db     = "blog";
my $server = "localhost";
my $id     = "luc";
my $pwd    = "PwD";
my $port   = "";
my $artdir = "~/Log/programmation/server/blog/articles/";

# Connecting to the database.
my $dbh = DBI->connect("DBI:mysql:database=$db;host=$server;port=$port",
    $id, $pwd, { RaiseError => 1, }
) or die "Couldn't connect to database $db : $DBI::errstr";

# Reading the input article.
my ($path,@tags) = @ARGV;
die "You must specifie an article" if not $path;
die "You must specifie at least one tag" if scalar(@tags) == 0;
my $tags_str = '#' . join '#',@tags;
print "Tags used : $tags_str.\n";
`cat $path | ./preproc.pl | txt2tags --encoding=UTF-8 --no-rc --no-toc --no-headers --css-sugar -t html --outfile /tmp/art.html --infile -`;

# Getting the time.
my ($sec,$min,$hour,$day,$month,$year,@rest) = localtime(time);
$year += 1900; ++$month;
my $sqldate = sprintf qq{%04d-%02d-%02d %02d:%02d:%02d}, $year, $month, $day, $hour, $min, $sec;
print "Article added with date $sqldate.\n";

# Saving the article.
$path = "$artdir/$year$month$day-$hour$min$sec.html";
`mv /tmp/art.html $path`;

# Updating the database.
my $prep = $dbh->prepare("insert into articles (path,tags,date) values ($path,$tags_str,$sqldate);") or die $dbh->errstr();
$prep->execute() or die 'Adding to articles table failed';
$prep->finish();

$prep = $dbh->prepare("select id from articles where date=\"$sqldate\";") or die $dbh->errstr();
$prep->execute() or die 'Getting id failed';
my ($id) = $prep->fetchrow_array;
print "Added with id $id.\n";
$prep->finish();

# TODO update tags table.

# Disconnecting.
$dbh->disconnect();


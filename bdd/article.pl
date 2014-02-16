#!/usr/bin/perl

use warnings;
use strict;
use Encode;
use utf8;
use DBI;
do 'common.pl';

my $db     = 'blog';
my $server = 'localhost';
my $id     = 'luc';
my $pwd    = 'PwD';
my $port   = '';

# Connecting
my $dbh = DBI->connect("DBI:mysql:database=$db;host=$server;port=$port",
    $id, $pwd, { RaiseError => 1, }
) or die "Couldn't connect to database $db : $DBI::errstr";


my $query_string = $ENV{'QUERY_STRING'} or die "Argument necessary.";
$query_string =~ m/id=([0-9]+)/ or die "Argument necessary.";
my $sql_command = "select path,tags from articles where id=$1";
my $prep = $dbh->prepare($sql_command) or die $dbh->errstr();
$prep->execute() or die 'Request failed !';

my @row = $prep->fetchrow_array;
my @tags = split('#', $row[1]);

open FD, $row[0] or die "Couldn't open path.";
binmode FD;

$prep->finish();

# HTML
head($dbh);

if(scalar(@tags) > 0) {
    print "<div id=\"tags\">\n";
    print "  <a class=\"tag\" href=\"menu.cgi?tag=$_\">$_</a>\n" while(shift(@tags));
    print "</div>\n";
}

print "<div id=\"article\">\n";
print while(<FD>);
close FD;

$query_string =~ s/id=[0-9]+//g;
my @args = grep $_,split('&', $query_string);
print "<div id=\"navigation\"><a class=\"prevpage\" href=\"menu.cgi";
if(scalar(@args) > 0) {
    print "?" . shift(@args);
    print "&$_" foreach @args;
}
print "\">< Back to menu</a></div>\n";
print "</div>\n";

footing();

# Disconnecting
$dbh->disconnect();

sub head {
    print "Content-type: text/html\n\n";

    print '<!DOCTYPE html>
    <html>
    <head>
    <title>Mon super blog !</title>
    <meta http-equiv="content-type" content="text/html; charset=utf-8" />
    <style type="text/css">
    @import url("../style/article.css") all;
    @import url("../style/menu.css") all;
    @import url("../style/foot.css") all;
    </style>
    <meta name="author" content="Luc Chabassier"/>
    <meta name="publisher" content="Mon super blog !"/>
    <meta name="description" content="Le blog personnele de Luc Chabassier où il parle de ce qui lui passe par la tête et présente ses codes et expérimentations."/>
    <meta name="keywords" content="blog,c++,opencv"/>
    <meta name="identifier-url" content="https://github.com/lucas8"/>
    <meta name="subject" content="Page d\'entrée."/>
    <meta name="distribution" content="global"/>
    <meta name="rating" content="general"/>
    <meta name="language" content="fr"/>
    <meta name="generator" content="super_script.pl"/>
    <meta name="robots" content="all"/>
    <meta name="revisit-after" content="7 days"/>
    </head>
    <body>
    <!-- Menus -->
    ';

    topbar();
    tagmiddlebar(@_);

    print '<div class="layout">
    ';
}

sub footing {
    print '</div>
    <!-- Foot -->
    <div id="footbar">
    <p>This page is part of the Mon Super Blog! website from Luc Chabassier.</p>
    </div>
    </body>
    </html>';
}


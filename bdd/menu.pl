#!/usr/bin/perl

use warnings;
use strict;
use Encode;
use utf8;
use DBI;
do '../utils/common.pl';

my $db     = 'blog';
my $server = 'localhost';
my $id     = 'luc';
my $pwd    = 'PwD';
my $port   = '';

# Connecting
my $dbh = DBI->connect("DBI:mysql:database=$db;host=$server;port=$port",
    $id, $pwd, { RaiseError => 1, }
) or die "Couldn't connect to database $db : $DBI::errstr";

# HTML
head($dbh);

my $sql_command = 'select id,path,date from articles';

# Get max date
my $query_string = $ENV{'QUERY_STRING'};
$query_string = "" if not $query_string;
my $date=undef;
if($query_string =~ m/date=([0-9]{4}-[0-9]{2}-[0-9]{2}%20[0-9]{2}:[0-9]{2}:[0-9]{2})/) {
    $date=$1;
    $date =~ s/%20/ /g;
    $sql_command .= " where date < \"$date\"";
}

my $prev_max=undef;
$prev_max=" " if $query_string =~ m/prev=/;
if($query_string =~ m/prev=([0-9]{4}-[0-9]{2}-[0-9]{2}%20[0-9]{2}:[0-9]{2}:[0-9]{2})/) {
    my $prev_max=$1;
    $prev_max =~ s/%20/ /g if $prev_max;
}

# Get tag
my $tag = undef;
if($query_string =~ m/tag=([a-z]*)/) {
    $sql_command .= " and" if $date;
    $sql_command .= " where" if not $date;
    $sql_command .= " tags regexp \"#$1\"";
    $tag = $1;
}

# Get args
my $args = undef;
$args = $query_string if $query_string;

# Get all articles
$sql_command .= " order by date desc limit 6;";
my $prep = $dbh->prepare($sql_command) or die $dbh->errstr();
$prep->execute() or die 'Request failed !';

my $last_date="";
my $nb=0;
while(my @row = $prep->fetchrow_array) {
    ++$nb;
    article($row[1], $row[0], $args);
    $last_date=$row[2];
}
$prep->finish();

footing($nb, $prev_max, $last_date, $date, $tag);

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
    @import url("../style/common.css") all;
    @import url("../style/articles.css") all;
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
    my ($nb, $prev, $next, $date, $tag) = @_;
    if($prev or $nb == 6) {
        print '<div id="navigation">';
        if($prev) {
            if($prev == " ") {
                print "  <a class=\"prevpage\" href=\"menu.cgi";
                print "?tag=$tag" if $tag;
                print "\"> < Prev page </a>";
            } else {
                print "  <a class=\"prevpage\" href=\"menu.cgi?date=$prev";
                print "&tag=$tag" if $tag;
                print "\"> < Prev page </a>" if $prev != " ";
            }
        }

        if($nb == 6) {
            print "  <a class=\"nextpage\" href=\"menu.cgi?date=$next";
            print "&prev=$date" if $date;
            print "&prev=" if not $date;
            print "&tag=$tag" if $tag;
            print "\"> Next page > </a>";
        }
        print '</div>';
    }

    print '</div>
    <!-- Foot -->
    <div id="footbar">
    <p>This page is part of the Mon Super Blog! website from Luc Chabassier.</p>
    </div>
    </body>
    </html>';
}

sub article {
    my ($path, $id, $args) = @_;
    open FD, $path or die "Couldn't open file $path.";
    binmode FD;
    my $content = "<a class=\"artlink\" href=\"article.cgi?id=$id";
    $content .= "&$args" if $args;
    $content .= "\"></a>\n";
    while(<FD>) {
        $content .= $_;
    }
    print "\n<div class=\"article\">$content</div>\n";
    close FD;
}


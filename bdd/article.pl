#!/usr/bin/perl

use warnings;
use strict;
use Search::Xapian;
do '../utils/common.pl';
my $dbdir  = "/home/luc/Log/programmation/server/blog/xapian";

# Getting th article content.
my $query_string = $ENV{'QUERY_STRING'} or die "Argument necessary.";
$query_string =~ m/id=([0-9]+)/ or die "Argument necessary.";
my $id = $1;
my $db = Search::Xapian::Database->new($dbdir) or die "Couldn't open database";
my $doc = $db->get_document($id) or die "Couldn't get document";

my $path = $doc->get_data();
open FD, $path or die "Couldn't open path.";
binmode FD;

# HTML
head($db);

my @tags; # TODO Handle tags.
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

sub head {
    print "Content-type: text/html\n\n";

    print '<!DOCTYPE html>
    <html>
    <head>
    <title>Mon super blog !</title>
    <meta http-equiv="content-type" content="text/html; charset=utf-8" />
    <style type="text/css">
    @import url("../style/common.css") all;
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


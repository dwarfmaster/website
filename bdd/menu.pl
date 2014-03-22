#!/usr/bin/perl

use warnings;
use strict;
use Search::Xapian;
do '../utils/common.pl';
my $dbdir = "/home/luc/Log/programmation/server/blog/xapian";

# Connecting
my $db = Search::Xapian::Database->new($dbdir) or die "Couldn't open database.";

# HTML
head($db);

# Get arguments
my $query_string = $ENV{'QUERY_STRING'};
$query_string = "" if not $query_string;

my $from = 0;
if($query_string =~ m/from=([0-9]+)/) {
    $from = $1;
}

my $query = "tag:all";
if($query_string =~ m/query=([a-zA-Z(%20):]+)/) {
    $query = $1;
}

# Get all articles
my $enquire = Search::Xapian::Enquire->new($db);
my $qp = Search::Xapian::QueryParser->new();
$qp->set_database($db);
$qp->set_stemmer(Search::Xapian::Stem->new("french"));
$qp->set_stemming_strategy(Search::Xapian::STEM_SOME);
$qp->add_prefix("tag", 'T');
my $vrpdate = new Search::Xapian::DateValueRangeProcessor(0, 1, 1920);
$qp->add_valuerangeprocessor($vrpdate);

my $qr = $qp->parse_query($query,
    Search::Xapian::FLAG_PHRASE | Search::Xapian::FLAG_BOOLEAN | Search::Xapian::FLAG_LOVEHATE | Search::Xapian::FLAG_WILDCARD
);
$enquire->set_query($qr);
$enquire->set_sort_by_value(0, 0); # Sort by date

my $mset = $enquire->get_mset($from, $from + 6);
my $msize = $mset->size();
my $args;
if($query_string =~ m/^.*\?(.*)$/) {
    $args = $1;
} else {
    $args = $query_string;
}

foreach my $m ($mset->items()) {
    article($m->get_document()->get_data(), $m->get_docid(), $args);
}

footing($msize, $query, $from);

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
    my ($nb, $query, $from) = @_;

    print '<div id="navigation">' if $from > 0 or $nb == 6;
    if($from > 0) {
        my $fr = $from - 6;
        print '  <a class="prevpage" href="menu.cgi?';
        print "query=$query&" if $query ne "tag:all";
        print "from=$fr" if $fr > 0;
        print '"> < Prev page </a>';
    }

    if($nb == 6) {
        my $fr = $from + 6;
        print '  <a class="nextpage" href="menu.cgi?';
        print "query=$query&" if $query ne "tag:all";
        print "from=$fr\"> Next page > </a>";
    }
    print '</div>' if $from > 0 or $nb == 6;

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


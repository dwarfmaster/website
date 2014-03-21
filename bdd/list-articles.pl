#!/usr/bin/perl

use warnings;
use strict;
use Search::Xapian;
my $dbdir = "/home/luc/Log/programmation/server/blog/xapian/";

# Opening the database
my $db = Search::Xapian::Database->new($dbdir);
my $sort_by;
if(scalar @ARGV and $ARGV[0] eq "-sdate") {
    $sort_by = 0;
    shift @ARGV;
}

# Opening and enquiring session
my $enquire = Search::Xapian::Enquire->new($db);
my $query_string = join ' ', @ARGV;
my $qp = Search::Xapian::QueryParser->new();
$qp->set_database($db);
$qp->set_stemmer(Search::Xapian::Stem->new("french"));
$qp->set_stemming_strategy(Search::Xapian::STEM_SOME);
$qp->add_prefix("tag", 'T');
my $vrpdate = new Search::Xapian::DateValueRangeProcessor(0, 1, 1920);
$qp->add_valuerangeprocessor($vrpdate);

# Parse the query.
my $query = $qp->parse_query($query_string,
    Search::Xapian::FLAG_PHRASE | Search::Xapian::FLAG_BOOLEAN | Search::Xapian::FLAG_LOVEHATE | Search::Xapian::FLAG_WILDCARD
);
$enquire->set_query($query);
$enquire->set_sort_by_value($sort_by, 0) if defined $sort_by;

# Return the top 10 results
my $mset = $enquire->get_mset(0, 10);
my $msize = $mset->size();
die "No matching results found" if($msize == 0);

# Showing the results
printf "About %u matching documents were found.\n", $mset->get_matches_estimated();
foreach my $m ($mset->items()) {
    printf "#%u: Score %u%%: %s\n",
    $m->get_rank() + 1,
    $m->get_percent(),
    $m->get_document()->get_data();
}


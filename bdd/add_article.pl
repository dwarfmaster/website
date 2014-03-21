#!/usr/bin/perl

use strict;
use warnings;
use Search::Xapian;
my $dbdir  = "/home/luc/Log/programmation/server/blog/xapian";
my $artdir = "/home/luc/Log/programmation/server/blog/articles";

# Connecting to the database.
my ($db, $indexer);
eval {
    $db = Search::Xapian::WritableDatabase->new($dbdir,
        Search::Xapian::DB_CREATE_OR_OPEN);
    $indexer = Search::Xapian::TermGenerator->new();
    my $stemmer = Search::Xapian::Stem->new("french");
    $indexer->set_stemmer($stemmer);
};
die "Couldn't open the xapian database : $@" if $@;

# Reading the input article.
my ($path,@tags) = @ARGV;
die "You must specifie an article" if not $path;
die "You must specifie at least one tag" if scalar(@tags) == 0;
my $tags_str = join ' ',@tags;
print "Tags used : $tags_str.\n";
# Creating the html
`cat $path | ./preproc.pl | txt2tags --encoding=UTF-8 --no-rc --no-toc --no-headers --css-sugar -t html --outfile /tmp/art.html --infile -`;
# Extracting the text to index.
my $content = `txt2tags --encoding=UTF-8 --no-rc --no-toc --no-headers -t txt --outfile - $path`;

# Getting the time.
my ($sec,$min,$hour,$day,$month,$year,@rest) = localtime(time);
$year += 1900; ++$month;
my $sqldate = sprintf qq{%04d-%02d-%02d %02d:%02d:%02d}, $year, $month, $day, $hour, $min, $sec;
print "Article added with date $sqldate.\n";

# Saving the article.
my $id = "$year$month$day$hour$min$sec";
$path = "$artdir/$id.html";
`mv /tmp/art.html $path`;

# Creating the document.
my $doc;
eval {
    $doc = Search::Xapian::Document->new();
    $indexer->set_document($doc);
    $doc->set_data($path);
    # Index tags
    $indexer->index_text($tags_str, 1, "T");
    # Index content
    $indexer->index_text($content);
    # Set unique ID
    $doc->add_term("Q" . $id);
    # To allow sorting by date
    $doc->add_value(0, sprintf(qq{%04d%02d%02d}, $year, $month, $day));
};
die "Couldn't create and configure the xapian document : $@" if $@;

# Adding the document to the db.
$db->add_document($doc) or die "Failed to add the document to the database : $!";


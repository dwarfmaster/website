#!/usr/bin/perl

use strict;
use warnings;
use Search::Xapian;
my $dbdir  = "/home/luc/Log/programmation/server/blog/xapian";
my $artdir = "/home/luc/Log/programmation/server/blog/articles";

# Opening the database
my ($db, $indexer);
eval {
    $db = Search::Xapian::WritableDatabase->new($dbdir,
        Search::Xapian::DB_CREATE_OR_OPEN);
    $indexer = Search::Xapian::TermGenerator->new();
    my $stemmer = Search::Xapian::Stem->new("french");
    $indexer->set_stemmer($stemmer);
};
die "Couldn't open the xapian database : $@" if $@;

# Getting the document ID
my ($path, $file, @tags) = @ARGV;
my $id = undef;
my $date;
if($path =~ m/([0-9]+)\.html$/) {
    $id = "Q" . $1;
    print "ID : $id\n";

    $id =~ m/^Q([0-9]{8})/;
    $date = $1;
} else {
    die "Invalid path.";
}
push @tags, "all";
my $tags_str = join ' ', @tags;

# Parsing file
`cat $file | ./preproc.pl | txt2tags --encoding=UTF-8 --no-rc --no-toc --no-headers --css-sugar -t xhtml --outfile /tmp/art.html --infile -`;
my $content = `txt2tags --encoding=UTF-8 --no-rc --no-toc --no-headers -t txt --outfile - $file`;
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
    $doc->add_term($id);
    # To allow sorting by date
    $doc->add_value(0, $date);
};
die "Couldn't create and configure the xapian document : $@" if $@;

$db->replace_document_by_term($id, $doc);


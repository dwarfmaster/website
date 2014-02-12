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
my $dbh = DBI->connect("DBI:mysql:database=$db;host=$server;port=$port",
    $id, $pwd, { RaiseError => 1, }
) or die "Couldn't connect to database $db : $DBI::errstr";

# HTML
head();

# Get all articles
my $prep = $dbh->prepare('select * from articles;') or die $dbh->errstr();
$prep->execute() or die 'Request failed !';
while(my @row = $prep->fetchrow_array) {
    article($row[1]);
}
$prep->finish();

footing();

# Disconnecting
$dbh->disconnect();

sub head {
    print '<!DOCTYPE html>
    <html>
    <head>
    <title>Mon super blog !</title>
    <meta http-equiv="content-type" content="text/html; charset=utf-8" />
    <style type="text/css">
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
    <div id="topbar">
    <a href="menu.html#INDEX"class="toplink"><span>Index</span></a>
    <a href="menu.html#BlOG" class="toplink_active"><span class="toplink_activespan">Blog</span></a>
    <a href="menu.html#CODES" class="toplink"><span>Codes</span></a>
    <a href="menu.html#PICTURES"class="toplink"><span>Pictures</span></a>
    </div>
    <div id="middlebar">
    <a href="menu.html#SUBALL" class="middlelink"><span class="middlelink_span">All</span></a>
    <a href="menu.html#SUBTAGS" class="middlelink"><span class="middlelink_span">Tags</span></a>
    <a href="menu.html#BUGS" class="middlelink"><span class="middlelink_span">Bugs</span></a>
    </div>
    <div class="layout">
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

sub article {
    (my $path) = @_;
    open FD, $path or die "Couldn't open file $path.";
    binmode FD;
    my $content = "";
    while(<FD>) {
        $content .= $_;
    }
    print "\n<div class=\"article\">$content</div>\n";
    close FD;
}


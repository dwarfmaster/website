
sub topbar {
    print '<div id="topbar">
    <a href="/index.pl"class="toplink"><span>Index</span></a>
    <a href="/blog/menu.pl" class="toplink_active"><span class="toplink_activespan">Blog</span></a>
    <a href="/codes/index.pl" class="toplink"><span>Codes</span></a>
    <a href="/gallery/index.pl"class="toplink"><span>Gallery</span></a>
    </div>
    ';
}

sub tagmiddlebar {
    print '<div id="middlebar">
    <a href="menu.pl" class="middlelink"><span class="middlelink_span">All</span></a>
    ';

    (my $dbh) = @_;
    my $prep = $dbh->prepare('select * from tags;') or die $dbh->errstr();
    $prep->execute() or die 'Request failed !';
    while(my @row = $prep->fetchrow_array) {
        print "<a href=\"menu.pl?tag=$row[0]\" class=\"middlelink\"><span class=\"middlelink_span\">$row[1]</span></a>";
    }
    $prep->finish();

    print '</div
    ';
}


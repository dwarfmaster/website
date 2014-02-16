
sub topbar {
    print '<div id="topbar">
    <a href="/index.cgi"class="toplink"><span>Index</span></a>
    <a href="/blog/menu.cgi" class="toplink_active"><span class="toplink_activespan">Blog</span></a>
    <a href="/codes/index.cgi" class="toplink"><span>Codes</span></a>
    <a href="/gallery/index.cgi"class="toplink"><span>Gallery</span></a>
    </div>
    ';
}

sub tagmiddlebar {
    print '<div id="middlebar">
    <a href="menu.cgi" class="middlelink"><span class="middlelink_span">All</span></a>
    ';

    (my $dbh) = @_;
    my $prep = $dbh->prepare('select name from tags;') or die $dbh->errstr();
    $prep->execute() or die 'Request failed !';
    while(my @row = $prep->fetchrow_array) {
        print "<a href=\"menu.cgi?tag=$row[0]\" class=\"middlelink\"><span class=\"middlelink_span\">$row[0]</span></a>";
    }
    $prep->finish();

    print '</div
    ';
}


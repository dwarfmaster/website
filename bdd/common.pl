
sub topbar {
    print '<div id="topbar">
    <a href="menu.html#INDEX"class="toplink"><span>Index</span></a>
    <a href="menu.html#BlOG" class="toplink_active"><span class="toplink_activespan">Blog</span></a>
    <a href="menu.html#CODES" class="toplink"><span>Codes</span></a>
    <a href="menu.html#PICTURES"class="toplink"><span>Pictures</span></a>
    </div>
    ';
}

sub tagmiddlebar {
    print '<div id="middlebar">
    ';

    (my $dbh) = @_;
    my $prep = $dbh->prepare('select * from tags;') or die $dbh->errstr();
    $prep->execute() or die 'Request failed !';
    while(my @row = $prep->fetchrow_array) {
        print "<a href=\"tag.pl?id=$row[0]\" class=\"middlelink\"><span class=\"middlelink_span\">$row[1]</span></a>";
    }
    $prep->finish();

    print '</div
    ';
}


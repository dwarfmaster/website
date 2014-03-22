
sub topbar {
    print '<div id="topbar">
    <a href="/"class="toplink"><span>Index</span></a>
    <a href="/blog/" class="toplink_active"><span class="toplink_activespan">Blog</span></a>
    <a href="/codes/" class="toplink"><span>Codes</span></a>
    <a href="/gallery/"class="toplink"><span>Gallery</span></a>
    </div>
    ';
}

sub tagmiddlebar {
    print '<div id="middlebar">';

    (my $db) = @_;
    my $it = $db->allterms_begin("T");
    my $end = $db->allterms_end("T");
    while($it ne $end) {
        $it->get_termname =~ m/^T(.*)$/;
        my $tag = $1;
        my $tag_name = ucfirst $tag;
        print "<a href=\"menu.cgi?query=tag:$tag\" class=\"middlelink\"><span class=\"middlelink_span\">$tag_name</span></a>\n";
        ++$it;
    }

    print "</div>\n";
}

sub documenttags {
    my ($db, $docid) = @_;
    my @tags;

    my $it = $db->termlist_begin($docid);
    my $end = $db->termlist_end($docid);
    while($it ne $end) {
        if($it->get_termname() =~ m/^T([a-z]+)/) {
            my $tag = $1;
            push @tags, $tag if $tag ne "all";
        }
        ++$it;
    }

    return @tags;
}


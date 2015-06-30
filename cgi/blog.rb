
module Blog
    def self.head(con, keywords, description)
        # Header
        print <<END_OF_STRING
Content-type: text/html

<!DOCTYPE html>
<html><head>
    <title>Mon super blog !</title>
    <meta http-equiv="content-type" content="text/html; charset=utf-8" />
    <style type="text/css">
        @import url("../style/common.css")   all;
        @import url("../style/articles.css") all;
        @import url("../style/menu.css")     all;
        @import url("../style/foot.css")     all;
    </style>
    <meta name="author"         content="Luc Chabassier" />
    <meta name="publisher"      content="Mon super blog !" />
    <meta name="description"    content="#{description}" />
    <meta name="keywords"       content="#{keywords}" />
    <meta name="identifier-url" content="https://github.com/lucas8" />
    <meta name="subject"        content="Page d'entrée" />
    <meta name="distribution"   content="global" />
    <meta name="rating"         content="general" />
    <meta name="generator"      content="https://github.com/lucas8/website" />
    <meta name="robots"         content="all" />
    <meta name="revisit-after"  content="7 days" />
</head>
<body>
END_OF_STRING
        
        # Top bar
        print <<END_OF_STRING
<div id="topbar">
    <a href="/" class="toplink"><span>About</span></a>
    <a href="/blog/" class="toplink_active"><span class="toplink_activespan">Blog</span></a>
    <a href="/codes/" class="toplink"><span>Codes</span></a>
</div>
END_OF_STRING
        
        # Tag bar
        puts "<div id=\"middlebar\">"
        puts "<a href=\"menu.cgi\" class=\"middlelink\"><span class=\"middlelink_span\">All</span></a>"
        query = con.query("SELECT DISTINCT tag FROM Tags")
        query.num_rows.times do
            tag = query.fetch_row[0]
            puts "<a href=\"menu.cgi?tag=#{tag}\" class=\"middlelink\"><span class=\"middlelink_span\">#{tag.capitalize}</span></a>"
        end
        puts "</div>"
    end

    def self.footer
        print <<END_OF_STRING
<div id="footbar">
    <p>This page is part of the Mon Super Blog! website from Luc Chabassier.</p>
</div>
</body>
</html>
END_OF_STRING
    end

end


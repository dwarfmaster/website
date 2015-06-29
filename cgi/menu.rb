#!/usr/bin/ruby

require 'mysql'
require 'uri'

$user   = "ruby"
$pass   = "ruby"
$dbname = "blog"
$artdir = "/home/luc/Log/programmation/ruby/blog/articles"

def head
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
    <meta name="description"    content="Lo blog personnel de Luc Chabassier où il parle de ce qui lui passe par la tête et présente ses codes et expérimentations." />
    <meta name="keywords"       content="blog,linux,code" />
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
    
    # TODO tag bar
end

def footing(nb, from)
    puts '<div id="navigation">' if from > 0 or nb == 6
    if from > 0
        fr = from - 6
        print "    <a class=\"prevpage\" href=\"menu.cgi?from=#{fr}\"> < Prev page </a>"
    end
    if nb == 6
        fr = from + 6
        print "    <a class=\"nextpage\" href=\"menu.cgi?from=#{fr}\"> Next page > </a>"
    end
    puts '</div>' if from > 0 or nb == 6

    print <<END_OF_STRING
<div id="footbar">
    <p>This page is part of the Mon Super Blog! website from Luc Chabassier.</p>
</div>
</body>
</html>
END_OF_STRING
end

def article(path, id)
    content = "<a class=\"artlink\" href=\"article.cgi?id=#{id}\"></a>\n"
    File.open(path, "r") do |infile|
        while (line = infile.gets)
            content += line
        end
    end
    puts "\n<div class=\"article\">#{content}</div>"
end

def getPath(name)
    return $artdir + "/" + name + "/article"
end

begin
    # Connection to db
    con = Mysql.new 'localhost', $user, $pass, $dbname
    head

    # Parsing query parameters
    query = ENV['QUERY_STRING'].split('&').map do |qr|
        qr.split('=')
    end
    from = 0
    tag = ""
    query.each do |cmd|
        if cmd.length != 2
            return
        end
        if cmd[0] == "from"
            from = cmd[1].to_i
        elsif cmd[0] == "tag"
            tag = cmd[1]
        end
    end

    # Getting the articles
    arts = con.query("SELECT DISTINCT id, path FROM Articles, Tags
                      WHERE id = article #{tag == "" ? "" : "AND tag = \"#{tag}\""}
                      ORDER BY time DESC")
    # TODO use from
    nb = [6, arts.num_rows].min
    nb.times do
        row = arts.fetch_row
        article(getPath(row[1]), row[0])
    end
    footing(nb, 0)
rescue Mysql::Error => e
    puts e.errno
    puts e.error
ensure
    con.close if con
end


#!/usr/bin/ruby

require 'mysql'
require 'uri'

require File.dirname(__FILE__) + '/blog.rb'

$user   = "ruby"
$pass   = "ruby"
$dbname = "blog"
$artdir = "/home/luc/Log/programmation/ruby/blog/articles"

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

    Blog.footer
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
    Blog.head(con, "blog, code", "Le blog personnel de Luc Chabassier où il parle de ce qui lui passe par la tête et présente ses codes et expérimentations.")

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


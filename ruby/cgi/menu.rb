#!/usr/bin/ruby

require 'mysql'
require 'uri'
require File.dirname(__FILE__) + '/blog.rb'
require File.dirname(__FILE__) + '/config.rb'

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

def article(content, id)
    print <<END_OF_STRING
<div class=\"article\">
    <a class=\"artlink\" href=\"article.cgi?id=#{id}\"></a>
    #{content}
</div>
END_OF_STRING
end

begin
    # Connection to db
    con = Mysql.new 'localhost', Config.user, Config.pass, Config.dbname
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
    arts = con.query("SELECT DISTINCT id, content FROM Articles, Tags
                      WHERE id = article #{tag == "" ? "" : "AND tag = \"#{tag}\""}
                      ORDER BY time DESC")
    # TODO use from
    nb = [6, arts.num_rows].min
    nb.times do
        row = arts.fetch_row
        article(row[1], row[0])
    end
    footing(nb, 0)
rescue Mysql::Error => e
    puts e.errno
    puts e.error
ensure
    con.close if con
end


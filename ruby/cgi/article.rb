#!/usr/bin/ruby

require 'mysql'
require File.dirname(__FILE__) + "/blog.rb"
require File.dirname(__FILE__) + "/config.rb"

def query_content(con, id)
    cdoc = con.query("SELECT content FROM Articles WHERE id = #{id}")
    if cdoc.num_rows != 1
        # TODO signal error
        exit 2
    end
    return cdoc.fetch_row[0]
end

def query_tags(con, id)
    query = con.query("SELECT DISTINCT tag FROM Tags WHERE article = #{id}")
    tgs = []
    query.num_rows.times do
        tgs += [query.fetch_row[0]]
    end
    return tgs
end

begin
    con = Mysql.new 'localhost', Config.user, Config.pass, Config.dbname
    id = -1
    ENV['QUERY_STRING'].split('&').each do |query|
        qs = query.split('=')
        if qs.length == 2 && qs[0] == 'id'
            id = qs[1].to_i
        end
    end
    if id < 0
        # TODO Signal error
        exit 2
    end

    content = query_content(con, id)
    tags    = query_tags(con, id)

    Blog.head(con, tags + ['blog'], "Blog article")
    if tags.length != 0
        puts "<div id=\"tags\">"
        tags.each do |tag|
            puts "    <a class=\"tag\" href=\"menu.cgi?tag=#{tag}\">#{tag.capitalize}</a>"
        end
        puts "</div>"
    end

    puts "<div id=\"article\">"
    puts content
    puts "<div id=\"navigation\"><a class=\"prevpage\" href=\"menu.cgi\">< Back to menu</a></div>"
    puts "</div>"
    Blog.footer
rescue Mysql::Error => e
    puts e.errno
    puts e.error
ensure
    con.close if con
end



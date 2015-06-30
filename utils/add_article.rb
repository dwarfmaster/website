#!/usr/bin/ruby -Eutf-8:utf-8
@modules_path = '/home/luc/Log/programmation/ruby/blog/cgi/'

require 'mysql'
require @modules_path + "config.rb"

begin
    con = Mysql.new 'localhost', Config.user, Config.pass, Config.dbname
    if ARGV.length < 3
        puts "Invalid arguments : expect a path to the file, a path to the directory of ressources and tags."
        exit 2
    end
    path, rcs = ARGV
    tags = ARGV
    tags.delete_at(1)
    tags.delete_at(0)

    # Add the row
    time = Time.now.strftime("%Y-%m-%d %H:%M:%S")
    con.query("INSERT INTO Articles (time) VALUES (\"#{time}\")")
    id = con.query("SELECT LAST_INSERT_ID()").fetch_row

    # Set the content
    content = `cat #{path} | ./preproc.rb #{Config.rcs + id} | txt2tags --encoding=UTF-8 --no-rc --no-toc --no-headers --css-sugar -t xhtml --outfile - --infile -`
    con.query("UPDATE Articles SET content = '#{content.gsub("'"){"\\'"}}' WHERE id = #{id}")

    # Add the tags
    values = ""
    tags.each do |tag|
        values += "(#{id}, #{tag}) "
    end
    con.query("INSERT INTO Tags (article,tag) VALUES #{values}")

    # Copy the ressources to their right place
    system "cp -rvf #{rcs} #{Config.rcs + id}"
rescue Mysql::Error => e
    puts e.errno
    puts e.error
ensure
    con.close if con
end



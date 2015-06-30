#!/usr/bin/ruby -Eutf-8:utf-8

dir, _ = ARGV
dir += '/'

$stdin.each_line do |line|
    if (m = /^FIGURE\((.*),(.*)\)/.match(line))
        print <<END_OF_TEXT
'''
<div class="figure">
    <img class="picture" src="#{dir + File.basename(m[2].strip)}" \>
    <p class="label">#{m[1].strip}</p>
</div>
'''
END_OF_TEXT
    elsif (m = /^VIDEO\((.*),(.*)\)/.match(line))
        print <<END_OF_TEXT
'''
<div class="figure">
    <video class="movie" src="#{dir + File.basename(m[2].strip)}" \>
    <p class="label">#{m[1].strip}</p>
</div>
'''
END_OF_TEXT
    else
        print line
    end
end


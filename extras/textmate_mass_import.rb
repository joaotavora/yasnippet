#!/usr/bin/ruby

require 'rubygems'
require 'curb'
require 'hpricot'
require 'plist'

begin
  index = Curl::Easy.http_get("http://svn.textmate.org/trunk/Bundles/").body_str
  parsed = Hpricot.parse(index)
  bundles = (parsed/:a).map {|elem| elem.innerText}
  bundles.select do |bundle|
    bundle =~ /.tmbundle/
  end.each do |bundle|
    50.times do putc "-" end
    puts
    begin
      info = Curl::Easy.http_get("http://svn.textmate.org/trunk/Bundles/#{bundle}/info.plist").body_str
      parsed = Plist::parse_xml(info)
      puts parsed["description"]
    rescue RuntimeError
      puts "Unknown description for #{bundle}"
    end
  end
rescue Exception => e
  $stderr.puts "Oops... #{e.class}:#{e.message}"
end

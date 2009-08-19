#!/usr/bin/env ruby
# This is a quick script to generate YASnippets from TextMate Snippets.
#
# I based the script off of a python script of a similar nature by
# Jeff Wheeler: http://nokrev.com
# http://code.nokrev.com/?p=snippet-copier.git;a=blob_plain;f=snippet_copier.py
#
# Usage
#
# Make sure you have the plist and the choice gem installed
# $ sudo gem install plist
# $ sudo gem install choice
#
# Usage: snippet_copier.rb [-dofp]
#
# Standard Options:
#     -d, --snippet-dir=PATH           Tells the program the directory to find the TextMate Snippets
#     -o, --output-dir=PATH            What directory to write the new YASnippets to
#     -f, --file=SNIPPET FILE NAME     A specific snippet that you want to copy or a glob for various files
#     -p, --pretty-print               Pretty prints multiple snippets when printing to standard out
#
# Common options:
#         --help                       Show this message
require 'rubygems'
require 'plist'
require 'choice'

Choice.options do
  header ''
  header 'Standard Options:'

  option :snippet_dir do
    short '-d'
    long '--snippet-dir=PATH'
    desc 'Tells the program the directory to find the TextMate Snippets'
    default '.'
  end

  option :output_dir do
    short '-o'
    long '--output-dir=PATH'
    desc 'What directory to write the new YASnippets to'
  end

  option :snippet do
    short '-f'
    long '--file=SNIPPET FILE NAME'
    desc 'A specific snippet that you want to copy or a glob for various files'
    default '*.{tmSnippet,plist}'
  end

  option :pretty_print do
    short '-p'
    long '--pretty-print'
    desc 'Pretty prints multiple snippets when printing to standard out'
  end

  separator ''
  separator 'Common options: '

  option :help do
    long '--help'
    desc 'Show this message'
  end
end


class TmSnippet
  @@known_substitutions = {
  "${TM_RAILS_TEMPLATE_START_RUBY_EXPR}"   => "<%= ",
  "${TM_RAILS_TEMPLATE_END_RUBY_EXPR}"     => " %>",
  "${TM_RAILS_TEMPLATE_START_RUBY_INLINE}" => "<% ",
  "${TM_RAILS_TEMPLATE_END_RUBY_INLINE}"   => " -%>",
  "${TM_RAILS_TEMPLATE_END_RUBY_BLOCK}"    => "end" ,
   /$\{TM_SELECTED_TEXT.*\}/               => "`yas/selected-text`" }
  
  def initialize(file)
    @snippet = Plist::parse_xml(file)
  end

  def name
    @snippet["name"]
  end

  def tab_trigger
    @snippet["tabTrigger"]
  end

  def key_equivalent
    @snippet["keyEquivalent"]
  end

  def content
    @snippet["content"]
  end

  def to_yasnippet
    doc = "# -*- mode: snippet -*-\n"
    doc << "#key: #{self.tab_trigger}\n" if self.tab_trigger
    doc << "#contributor : Translated from TextMate Snippet\n"
    doc << "#name : #{self.name}\n"
    doc << "#binding : \"#{self.key_equivalent}\"\n" if self.key_equivalent
    doc << "# --\n"
    @@known_substitutions.each_pair { |k, v| self.content.gsub!(k,v) }
    doc << "#{self.content}"
  end
end

# def yasnippet_file_name(dir, snippet, ext=nil)
#   begin
#     file = ""
#     shortname = (snippet.tab_trigger or
#                  snippet.name.gsub(/[^a-z0-9_-].*/,""))
#     if ext
#       file = File.join(dir, shortname + "." + ext.to_s)
#     else
#       file =  File.join(dir, shortname)
#     end
#     if File.exist?(file)
#       if ext
#         file = yasnippet_file_name(dir,snippet, ext+1)
#       else
#         file = yasnippet_file_name(dir,snippet, 1)
#       end
#     end
#     # puts "returning #{file} since shortname is #{shortname}\n"
#     file
#   rescue TypeError
#     raise "Cannot convert " + snippet.name + " (probably no tab-trigger)"
#   end
# end

def yasnippet_dir_and_name(dir, file)
  dir = File.join(dir,File.dirname(file))
  file = File.join(File.basename(file, File.extname(file))) << ".yasnippet"
  [dir, file]
end

snippet_files_glob = File.join(Choice.choices.snippet_dir, "**", Choice.choices.snippet)
snippet_files = Dir.glob(snippet_files_glob)

puts "Will try to convert #{snippet_files.length} snippets...\n"

snippet_files.each do |file|
  puts "Processing #{file}\n"
  snippet = TmSnippet.new(file)
  if Choice.choices.output_dir
    begin
      ( dir_to_create, file_to_create ) = yasnippet_dir_and_name(Choice.choices.output_dir, file)
      FileUtils.mkdir_p(dir_to_create)
      File.open(File.join(dir_to_create,file_to_create), 'w') do |f|
        f.write(snippet.to_yasnippet)
      end
    rescue RuntimeError => error
      $stderr.print error.message + "\n"
    end
  else
    if Choice.choices.pretty_print
      puts "--------------------------------------------"
    end
    puts snippet.to_yasnippet
    if Choice.choices.pretty_print
      puts "--------------------------------------------"
    end
    puts "\n\n"
  end
end

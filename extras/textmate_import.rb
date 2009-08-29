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
#     -p, --print-pretty               Pretty prints multiple snippets when printing to standard out
#     -b, --convert-bindings           TextMate "keyEquivalent" keys are translated to YASnippet "# binding :" directives
#     -g, --info-plist=INFO            Attempt to derive group information from "info.plist" type-file PLIST
#
# Common options:
#         --help                       Show this message
require 'rubygems'
require 'plist'
require 'choice'
require 'ruby-debug' if $DEBUG

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

  option :print_pretty do
    short '-p'
    long '--pretty-print'
    desc 'Pretty prints multiple snippets when printing to standard out'
  end

  option :convert_bindings do
    short '-b'
    long '--convert-bindings'
    desc "TextMate \"keyEquivalent\" keys are translated to YASnippet \"# binding :\" directives"
  end

  option :info_plist do
    short '-g'
    long '--info-plist'
    desc "Attempt to derive group information from \"info.plist\" type-file PLIST"
  end

  separator ''
  separator 'Common options: '

  option :help do
    long '--help'
    desc 'Show this message'
  end
end


class TmSnippet
  @@known_substitutions=[
                         {
                           "${TM_RAILS_TEMPLATE_START_RUBY_EXPR}"   => "<%= ",
                           "${TM_RAILS_TEMPLATE_END_RUBY_EXPR}"     => " %>",
                           "${TM_RAILS_TEMPLATE_START_RUBY_INLINE}" => "<% ",
                           "${TM_RAILS_TEMPLATE_END_RUBY_INLINE}"   => " -%>",
                           "${TM_RAILS_TEMPLATE_END_RUBY_BLOCK}"    => "end" ,
                           "${0:$TM_SELECTED_TEXT}"                 => "$TM_SELECTED_TEXT$0",
                         },
                         { "$TM_SELECTED_TEXT"                      => "`yas/selected-text`" }
                        ]
  
  attr_reader :file

  # Makes a TmSnippet
  #
  # * file is the .tmsnippet/.plist file path relative to cwd 
  # * optional info is a Plist.parsed info.plist found in the bundle dir
  #
  def initialize(file,info=nil)
    @file    = file
    @info    = info
    @snippet = Plist::parse_xml(file)
  end

  def name
    @snippet["name"]
  end

  def group
    if @info
      submenus = @info["mainMenu"]["submenus"]
      container = submenus.keys.find do |submenu|
        submenus[submenu]["items"].member?(uuid)
      end
      submenus[container]["name"] if container;
    end
  end

  def uuid
    @snippet["uuid"]
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
    doc << "# key: #{self.tab_trigger}\n" if self.tab_trigger
    doc << "# contributor: Translated from TextMate Snippet\n"
    doc << "# name: #{self.name}\n"
    if self.key_equivalent
      doc << "#" unless Choice.choices.convert_bindings
      doc << "# binding: \"#{self.key_equivalent}\"\n"
    end
    doc << "# --\n"
    @@known_substitutions.each {|level| level.each_pair { |k, v| self.content.gsub!(k,v) }}
    doc << "#{self.content}"
  end

  def yasnippet_file(basedir)
    basedir = File.join(basedir,group) if group
    File.join(basedir,@file).gsub(/#{File.extname(@file)}$/,".yasnippet")
  end

end



if $0 == __FILE__ 
  info_plist = Plist::parse_xml(Choice.choices.info_plist) if Choice.choices.info_plist

  if Choice.choices.output_dir
    FileUtils.mkdir_p Choice.choices.output_dir
    FileUtils.touch File.join(Choice.choices.output_dir, ".yas-make-groups")
    FileUtils.touch File.join(Choice.choices.output_dir, ".yas-ignore-filenames-as-triggers")
  end
  
  original_dir = Dir.pwd
  Dir.chdir Choice.choices.snippet_dir
  snippet_files_glob = File.join("**", Choice.choices.snippet)
  snippet_files = Dir.glob(snippet_files_glob)

  puts "Will try to convert #{snippet_files.length} snippets...\n"
  snippet_files.each do |file|
    begin
      puts "Processing \"#{File.join(Choice.choices.snippet_dir,file)}\"\n"
      snippet = TmSnippet.new(file,info_plist)
      if Choice.choices.output_dir
        file_to_create = snippet.yasnippet_file(File.join(original_dir, Choice.choices.output_dir))
        FileUtils.mkdir_p(File.dirname(file_to_create))
        File.open(file_to_create, 'w') do |f|
          f.write(snippet.to_yasnippet)
        end
      else
        if Choice.choices.print_pretty
          puts "--------------------------------------------"
        end
        puts snippet.to_yasnippet
        if Choice.choices.print_pretty
          puts "--------------------------------------------"
        end
        puts "\n\n"
      end
    rescue Exception => e
      $stderr.puts "Oops... #{e.class}:#{e.message}"
    end
  end
end

#!/usr/bin/env ruby
# -*- coding: utf-8 -*-
# textmate_import.rb --- import textmate snippets
# 
# Copyright (C) 2009 Rob Christie, 2010 João Távora
# 
# This is a quick script to generate YASnippets from TextMate Snippets.
#
# I based the script off of a python script of a similar nature by
# Jeff Wheeler: http://nokrev.com
# http://code.nokrev.com/?p=snippet-copier.git;a=blob_plain;f=snippet_copier.py
#
# Use textmate_import.rb --help to get usage information.

require 'rubygems'
require 'plist'
require 'choice'
require 'FileUtils'
require 'Shellwords' # String#shellescape
require 'ruby-debug' if $DEBUG

Choice.options do
  header ''
  header 'Standard Options:'

  option :bundle_dir do
    short '-d'
    long '--bundle-dir=PATH'
    desc 'Tells the program the directory to find the TextMate bundle directory'
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
    default '*.{tmSnippet,plist,tmMacro}'
  end

  option :print_pretty do
    short '-p'
    long '--pretty-print'
    desc 'Pretty prints multiple snippets when printing to standard out'
  end

  option :quiet do
    short '-q'
    long '--quiet'
    desc 'Be quiet.'
  end

  option :convert_bindings do
    short '-b'
    long '--convert-bindings'
    desc "TextMate \"keyEquivalent\" keys are translated to YASnippet \"# binding :\" directives"
  end

  option :info_plist do
    short '-g'
    long '--info-plist=PLIST'
    desc "Specify a plist file derive menu information from defaults to \"bundle-dir\"/info.plist"
  end

  separator ''
  separator 'Common options: '

  option :help do
    long '--help'
    desc 'Show this message'
  end
end

# Represents and is capable of outputting the representation of a
# TextMate menu in terms of `yas/define-menu'
#
class TmSubmenu
  attr_reader :items, :name
  def initialize(name, hash)
    @items = hash["items"]
    @name = name
  end

  def to_lisp(allsubmenus,
              deleteditems,
              indent = 0,
              thingy = ["(", ")"])
    
    first = true;

    string = ""
    items.each do |uuid|
      if deleteditems.index(uuid)
        $stderr.puts "#{uuid} has been deleted!"
        next
      end
      string += "\n"
      string += " " * indent
      string += (first ? thingy[0] : (" " * thingy[0].length))
      
      submenu = allsubmenus[uuid]
      if submenu
        str = "(yas/submenu "
        string += str + "\"" + submenu.name + "\"" 
        string += submenu.to_lisp(allsubmenus, deleteditems,
                                  indent + str.length + thingy[0].length)
      elsif TmSnippet::snippets_by_uid[uuid]
        string += "(yas/item \"" + uuid + "\")"
      elsif (uuid =~ /---------------------/)
        string += "(yas/separator)"
      else
        string += "(yas/external-item \"" + uuid + "\")"
      end
      first = false;
    end
    string += ")"
    string += thingy[1]

    return string
  end

  def self.main_menu_to_lisp (parsed_plist, modename)
    mainmenu = parsed_plist["mainMenu"]
    deleted  = parsed_plist["deleted"]
    
    root = TmSubmenu.new("__main_menu__", mainmenu)
    all = {}
    
    mainmenu["submenus"].each_pair do |k,v|
      all[k] = TmSubmenu.new(v["name"], v)
    end

    closing = "\n                    '("
    closing+= mainmenu["excludedItems"].collect do |uuid|
      snippet = TmSnippet::snippets_by_uid[uuid]
      
      "\"" + (snippet ? snippet.name : uuid) + "\"" 
    end.join(  "\n                       ") + "))"

    str = "(yas/define-menu "
    return str + "'#{modename}" + root.to_lisp(all,
                                               deleted,
                                               str.length,
                                               ["'(" , closing])
  end
end

# Represents a textmate snippet
#
# - @file is the .tmsnippet/.plist file path relative to cwd
# 
# - optional @info is a Plist.parsed info.plist found in the bundle dir
#
# - @@snippets_by_uid is where one can find all the snippets parsed so
#   far.
# 
#
class SkipSnippet < RuntimeError; end
class TmSnippet
  # unix to the rescue
  #
  # ack -aho '\${\d/[^/]*/[^/]*/}' imported/ruby-mode/ | sort | uniq
  @@known_substitutions ={
    "content"   => [
                    {
                      "${TM_RAILS_TEMPLATE_START_RUBY_EXPR}"   => "<%= ",
                      "${TM_RAILS_TEMPLATE_END_RUBY_EXPR}"     => " %>",
                      "${TM_RAILS_TEMPLATE_START_RUBY_INLINE}" => "<% ",
                      "${TM_RAILS_TEMPLATE_END_RUBY_INLINE}"   => " -%>",
                      "${TM_RAILS_TEMPLATE_END_RUBY_BLOCK}"    => "end" ,
                      "${0:$TM_SELECTED_TEXT}"                 => "${0:`yas/selected-text`",
                    }
                   ],
    "condition" => [ {
                      /^source\..*$/ => "" 
                     } ],
    "binding"   => [ {} ]
  }
  # now add some more substitutions
  # TODO: find a better way to add more substitutions
  #
  require 'textmate_import_substitutions.rb'

  @@snippets_by_uid={}
  def self.snippets_by_uid; @@snippets_by_uid; end

  def initialize(file,info=nil)
    @file    = file
    @info    = info
    @snippet = TmSnippet::read_plist(file)
    @@snippets_by_uid[self.uuid] = self;
    raise SkipSnippet.new "not a snippet/command/macro." unless (@snippet["scope"] || @snippet["command"]) 
    raise RuntimeError.new("Cannot convert this snippet #{file}!") unless @snippet;
  end

  def name
    @snippet["name"]
  end

  # def subdir
  #   if @info
  #     submenus = @info["mainMenu"]["submenus"]
  #     container = submenus.keys.find do |submenu|
  #       submenus[submenu]["items"].member?(uuid)
  #     end
  #     submenus[container]["name"] if container;
  #   end
  # end

  def uuid
    @snippet["uuid"]
  end

  def tab_trigger
    @snippet["tabTrigger"]
  end

  def binding
    binding = @snippet["keyEquivalent"]
    if binding
      @@known_substitutions["binding"].each do |level|
        level.each_pair do |k, v|
          binding.gsub!(k,v)
        end
      end
    end
    "## binding: \""+ binding + "\"\n" if binding and not binding.empty?
  end

  def content
    content = @snippet["content"]
    if content
      @@known_substitutions["content"].each do |level|
        level.each_pair do |k, v|
          content.gsub!(k,v)
        end
      end
    end
    content
  end

  def condition
    condition = @snippet["scope"]
    if condition
      @@known_substitutions["condition"].each do |level|
        level.each_pair do |k, v|
          condition.gsub!(k,v)
        end
      end
    end
    "## condition: \""+ condition + "\"\n" if condition and not condition.empty?
  end

  def to_yasnippet
    doc = "# -*- mode: snippet -*-\n"
    doc << "# type: command\n" unless self.content
    doc << "# uuid: #{self.uuid}\n"
    doc << "# key: #{self.tab_trigger}\n" if self.tab_trigger
    doc << "# contributor: Translated from TextMate Snippet\n"
    doc << "# name: #{self.name}\n"
    doc << (self.binding || "")
    doc << (self.condition || "")
    doc << "# --\n"
    doc << (self.content || "")
    doc
  end

  def self.canonicalize(filename)
    invalid_char = /[^ a-z_0-9.+=~(){}\/'`&#,-]/i

    filename.
      gsub(invalid_char, '').  # remove invalid characters
      gsub(/ {2,}/,' ').       # squeeze repeated spaces into a single one
      rstrip                   # remove trailing whitespaces
  end

  def yasnippet_file(basedir)
    # files cannot end with dots (followed by spaces) on windows
    File.join(basedir,TmSnippet::canonicalize(@file[0, @file.length-File.extname(@file).length]) + ".yasnippet")
  end

  def self.read_plist(xml_or_binary)
    begin
      parsed = Plist::parse_xml(xml_or_binary)
      return parsed if parsed
      raise RuntimeError.new "Probably in binary format and parse_xml is very quiet..."
    rescue RuntimeError => e
      if (system "plutil -convert xml1 #{xml_or_binary.shellescape} -o /tmp/textmate_import.tmpxml")
        return Plist::parse_xml("/tmp/textmate_import.tmpxml") 
      else
        raise RuntimeError.new "plutil failed miserably, check if you have it..."
      end
    end
  end

end


if $0 == __FILE__
  # Read the info.plist if we have it
  #
  info_plist_file = Choice.choices.info_plist || File.join(Choice.choices.bundle_dir,"info.plist")
  info_plist = TmSnippet::read_plist(info_plist_file) if info_plist_file and File.readable? info_plist_file;

  # Glob snippets into snippet_files, going into subdirs
  #
  original_dir = Dir.pwd
  Dir.chdir Choice.choices.bundle_dir
  snippet_files_glob = File.join("**", Choice.choices.snippet)
  snippet_files = Dir.glob(snippet_files_glob)

  # Attempt to convert each snippet files in snippet_files
  #  
  puts "Will try to convert #{snippet_files.length} snippets...\n" unless Choice.choices.quiet
  snippet_files.each do |file|
    begin
      puts "Processing \"#{File.join(Choice.choices.bundle_dir,file)}\"\n" unless Choice.choices.quiet
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
        puts snippet.to_yasnippet if Choice.choices.print_pretty or not Choice.choices.info_plist
        if Choice.choices.print_pretty
          puts "--------------------------------------------\n\n"
        end
      end
    rescue SkipSnippet => e
      $stdout.puts "Skipping \"#{file}\": #{e.message}"
    rescue RuntimeError => e
      $stderr.puts "Oops.... \"#{file}\": #{e.message}"
      $strerr.puts "#{e.backtrace.join("\n")}" unless Choice.choices.quiet
    end
  end
  # Attempt to decypher the menu
  #
  modename = File.basename Choice.choices.output_dir || "major-mode-name"  
  menustr = TmSubmenu::main_menu_to_lisp(info_plist, modename) if info_plist
  puts menustr unless !menustr or Choice.choices.quiet

  # Write some basic .yas-* files
  #
  Dir.chdir original_dir
  if Choice.choices.output_dir
    FileUtils.mkdir_p Choice.choices.output_dir
    FileUtils.touch File.join(original_dir, Choice.choices.output_dir, ".yas-make-groups") unless menustr
    FileUtils.touch File.join(original_dir, Choice.choices.output_dir, ".yas-ignore-filenames-as-triggers")
    File.open(File.join(original_dir, Choice.choices.output_dir, ".yas-setup.el"), 'w') do |file|
      file.write ";; .yas-setup.el for #{modename}\n"
      file.write ";;\n"
      file.write ";; Automatically translated menu\n"
      file.write(menustr)
      file.write "\n;;\n"
      file.write ";; .yas-setup.el for #{modename} ends here\n"
    end
  end

end

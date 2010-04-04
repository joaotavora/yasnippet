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
  @@known_substitutions = {
    "content"   => {
      "${TM_RAILS_TEMPLATE_START_RUBY_EXPR}"   => "<%= ",
      "${TM_RAILS_TEMPLATE_END_RUBY_EXPR}"     => " %>",
      "${TM_RAILS_TEMPLATE_START_RUBY_INLINE}" => "<% ",
      "${TM_RAILS_TEMPLATE_END_RUBY_INLINE}"   => " -%>",
      "${TM_RAILS_TEMPLATE_END_RUBY_BLOCK}"    => "end" ,
      "${0:$TM_SELECTED_TEXT}"                 => "${0:`yas/selected-text`" },
    "condition" => {
      /^source\..*$/ => "" },
    "binding"   => {}
  }

  def self.extra_substitutions; @@extra_substitutions; end
  @@extra_substitutions = {
    "content"   => {},
    "condition" => {},
    "binding"   => {}
  }
  
  def self.unknown_substitutions; @@unknown_substitutions; end
  @@unknown_substitutions = {
    "content"   => {},
    "condition" => {},
    "binding"   => {}
  }

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

  def uuid
    @snippet["uuid"]
  end

  def key
    @snippet["tabTrigger"]
  end

  def condition
    yas_directive "condition"
  end

  def binding
    yas_directive "binding"
  end

  def content
    if direct = @@known_substitutions["content"].
        merge(@@extra_substitutions["content"])[uuid]
      return direct
    else
      content = @snippet["content"]
      if content
        @@known_substitutions["content"].
          merge(@@extra_substitutions["content"]).
          each_pair do |k,v|
          if (k != uuid)
            content.gsub!(k,v)
          else
            content = v
          end
        end
        content.scan(%r'\$\{ [^/\}\{:]* /
                                [^/]* /
                                [^/]* /
                                [^\}]*\}'x) do |match|
          @@unknown_substitutions["content"][match] = self
        end
      else
        
      end
    end
  end

  def to_yas
    doc = "# -*- mode: snippet -*-\n"
    doc << "# type: command\n" unless self.content
    doc << "# uuid: #{self.uuid}\n"
    doc << "# key: #{self.key}\n" if self.key
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

  def yas_file()
      File.join(TmSnippet::canonicalize(@file[0, @file.length-File.extname(@file).length]) + ".yasnippet")
  end

  def self.read_plist(xml_or_binary)
    begin
      parsed = Plist::parse_xml(xml_or_binary)
      return parsed if parsed
      raise ArgumentError.new "Probably in binary format and parse_xml is very quiet..."
    rescue StandardError => e
      if (system "plutil -convert xml1 #{xml_or_binary.shellescape} -o /tmp/textmate_import.tmpxml")
        return Plist::parse_xml("/tmp/textmate_import.tmpxml") 
      else
        raise RuntimeError.new "plutil failed miserably, check if you have it..."
      end
    end
  end

  private

  @@yas_to_tm_directives = {"condition" => "scope", "binding" => "keyEquivalent", "key" => "tabTrigger"}
  def yas_directive(yas_directive)
    tm_directive = @@yas_to_tm_directives[yas_directive]
    val = @snippet[tm_directive]
    found = false
    # puts "Looking for a substitution for #{val}"
    if val and !val.delete(" ").empty?
      @@known_substitutions[yas_directive].
        merge(@@extra_substitutions[yas_directive]).
        each_pair do |k, v|
        if (k == uuid)
          val = v
          found = true
          break
        elsif val.gsub!(k,v)
          # puts "#{@snippet[tm_directive]} matched #{k} so replacing with #{v}"
          found = true
          break
        end
      end
      if found
        # puts "found substitution for #{yas_directive} : #{val}"
        "# #{yas_directive}: "+ val + "\n" unless val.empty?
      else
        # puts "found this unknown substitutions for #{yas_directive} : #{val}"
        @@unknown_substitutions[yas_directive][val] = self
        "## #{yas_directive}: \""+ val + "\n"
      end
    end
  end

end


if $0 == __FILE__
  # Read the the bundle's info.plist if can find it/guess it
  #
  info_plist_file = Choice.choices.info_plist || File.join(Choice.choices.bundle_dir,"info.plist")
  info_plist = TmSnippet::read_plist(info_plist_file) if info_plist_file and File.readable? info_plist_file;

  # Calculate the mode name
  # 
  modename = File.basename Choice.choices.output_dir || "major-mode-name"

  # Read in .yas-setup.el looking for the separator between auto-generated
  #
  original_dir = Dir.pwd
  yas_setup_el_file = File.join(original_dir, Choice.choices.output_dir, ".yas-setup.el")
  separator = ";; --**--"
  whole, head , tail = "", "", ""
  if File::exists? yas_setup_el_file
    File.open yas_setup_el_file, 'r' do |file|
      whole = file.read
      head , tail = whole.split(separator)
    end
  else
    head = ";; .yas-setup.el for #{modename}\n" + ";; \n"
  end

  # Now iterate the tail part to find extra substitutions
  #
  tail    ||= ""
  head    ||= ""
  directive = nil
  head.each_line do |line|
    case line
    when /^;; Substitutions for:(.*)$/
      directive = $~[1].strip
      # puts "found the directove #{directive}"
    when /^;;(.*)[ ]+=yyas>(.*)$/
      lookfor, replacewith = $~[1].strip, $~[2].strip
      # puts "found this wonderful substitution for #{directive} which is #{lookfor} => #{replacewith}"
      unless !directive or replacewith =~ /yas\/unknown/ then 
        TmSnippet.extra_substitutions[directive][lookfor] = replacewith
      end
    end
  end

  # Glob snippets into snippet_files, going into subdirs
  #
  Dir.chdir Choice.choices.bundle_dir
  snippet_files_glob = File.join("**", Choice.choices.snippet)
  snippet_files = Dir.glob(snippet_files_glob)

  # Attempt to convert each snippet files in snippet_files
  #  
  puts "Will try to convert #{snippet_files.length} snippets...\n" unless Choice.choices.quiet
  

  # Iterate the globbed files
  #
  snippet_files.each do |file|
    begin
      puts "Processing \"#{File.join(Choice.choices.bundle_dir,file)}\"\n" unless Choice.choices.quiet
      snippet = TmSnippet.new(file,info_plist)

      if 
        file_to_create = File.join(original_dir, Choice.choices.output_dir, snippet.yas_file)
        FileUtils.mkdir_p(File.dirname(file_to_create))
        File.open(file_to_create, 'w') do |f|
          f.write(snippet.to_yas)
        end
      else
        if Choice.choices.print_pretty
          puts "--------------------------------------------"
        end
        puts snippet.to_yas if Choice.choices.print_pretty or not Choice.choices.info_plist
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
  menustr = TmSubmenu::main_menu_to_lisp(info_plist, modename) if info_plist
  puts menustr if $DEBUG

  # Write some basic .yas-* files
  #
  if Choice.choices.output_dir
    FileUtils.mkdir_p Choice.choices.output_dir
    FileUtils.touch File.join(original_dir, Choice.choices.output_dir, ".yas-make-groups") unless menustr
    FileUtils.touch File.join(original_dir, Choice.choices.output_dir, ".yas-ignore-filenames-as-triggers")
    
    # Now, output head + a new tail in (possibly new) .yas-setup.el
    # file
    #
    File.open yas_setup_el_file, 'w' do |file|
      file.puts head
      file.puts separator
      file.puts ";; Automatically generated code, do not edit this part"
      file.puts ";; "
      file.puts ";; Translated menu"
      file.puts ";; "
      file.puts menustr
      file.puts
      file.puts ";; Unknown substitutions"
      file.puts ";; "
      ["content", "condition", "binding"].each do |type|
        file.puts ";; Substitutions for: #{type}"
        file.puts ";; "
        # TmSnippet::extra_substitutions[type].
        #   each_pair do |k,v|
        #   file.puts ";; " + k + "" + (" " * [1, 90-k.length].max) + " =yyas> " + v
        # end
        unknown = TmSnippet::unknown_substitutions[type];
        unknown.keys.uniq.each do |k|
          file.puts ";; # as in " +  unknown[k].yas_file
          file.puts ";; " + k + "" + (" " * [1, 90-k.length].max) + " =yyas> (yas/unknown)"
          file.puts ";; "
        end
        file.puts ";; "
        file.puts
      end
      file.puts ";; .yas-setup.el for #{modename} ends here"
    end
  end
end

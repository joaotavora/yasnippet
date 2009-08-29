#!/usr/bin/ruby

@@mode_mappings = {
  "C.tmbundle"                => "c-mode",
  "CSS.tmbundle"              => "css-mode",
  "Erlang.tmbundle"           => "erlang-mode",
  "Fortran.tmbundle"          => "f90-mode",
  "HTML.tmbundle"             => "html-mode",
  "Haskell.tmbundle"          => "haskell-mode",
  "Java.tmbundle"             => "java-mode",
  "Latex.tmbundle"            => "LaTeX-mode",
  "Lisp.tmbundle"             => "lisp-mode",
  "Make.tmbundle"             => "makefile-mode",
  "Markdown.tmbundle"         => "markdown-mode",
  "Pascal.tmbundle"           => "pascal-mode",
  "Perl.tmbundle"             => "perl-mode",
  "Python.tmbundle"           => "python-mode",
  "Ruby.tmbundle"             => "ruby-mode",
  "SQL.tmbundle"              => "sql-mode",
  "Scheme.tmbundle"           => "scheme-mode",
  "ShellScript.tmbundle"      => "shell-script-mode",
  "Text.tmbundle"             => "text-mode",
  "XML.tmbundle"              => "sgml-mode",
  "YAML.tmbundle"             => "yaml-mode",
  "reStructuredText.tmbundle" => "rst-mode",
  "Ruby on Rails.tmbundle"    => "rails-mode"
}

if __FILE__ == $0
  if ARGV[0]=='--help' or ARGV[0].nil? or ARGV[0]=='-h' or 
    puts "Usage: #{File.basename($0)} <dir-containing-textmate-bundles>"
    puts
    puts "textmate_import.rb must reside in the directory where you call this"
    puts "ouputs to a subdir \"imported\" inside the current directory"
    exit(-1)
  end
  Dir.glob("#{ARGV[0]}/*").each do |indir|
    mapping = @@mode_mappings[File.basename(indir)]
    if mapping
      puts "Importing #{indir} for #{mapping}"
      if File.exists? "#{indir}/Snippets"
        system("ruby textmate_import.rb -d #{indir}/Snippets -o imported/#{mapping} -g #{indir}/info.plist -b")
      end
    end      
  end
end




# -*- Ruby -*-

require 'fileutils'

def find_version
  File.read("yasnippet.el") =~ /;; Package-version: *([0-9.]+[a-z]?) *$/
  $version = $1
end
find_version
FileUtils.mkdir_p('pkg')

desc "generate bundle file for classic snippets."
task :bundle do
  sh 'emacs --batch -l yasnippet.el --eval "(yas/compile-bundle)"'
  sh "tar czf pkg/yasnippet-bundle-#{$version}.el.tgz yasnippet-bundle.el"
end

desc "generate bundle file for textmate snippets."
task :textmate_bundle => [:convert] do
  sh 'emacs --batch -l yasnippet.el --eval "(yas/compile-textmate-bundle)"'
  sh "tar czf pkg/yasnippet-textmate-bundle-#{$version}.el.tgz yasnippet-textmate-bundle.el"
end

desc "convert some textmate bundles to yasnippets"
task :convert_bundles do
  Dir.glob "extras/bundles/*-tmbundle" do |bundle_dir|
    puts "Converting from #{bundle_dir}"
    mode_prefix = File.basename(bundle_dir).match(/[^-]*/)[0]
    raise "Couldn't guess mode name for #{bundle_dir}" unless mode_prefix
    sh "./extras/textmate_import.rb -d #{bundle_dir} -o ./extras/imported/#{mode_prefix}-mode -q" 
  end
end

desc "create a release package"
task :package do
  release_dir = "pkg/yasnippet-#{$version}"
  FileUtils.mkdir_p(release_dir)
  files = ['extras', 'snippets', 'yasnippet.el', 'dropdown-list.el']
  FileUtils.cp_r files, release_dir
  FileUtils.rm_r Dir[release_dir + "/**/.svn"]
  FileUtils.cd 'pkg'
  sh "tar cjf yasnippet-#{$version}.tar.bz2 yasnippet-#{$version}"
  FileUtils.cd ".."
end

desc "create a release package and upload it to google code"
task :release => [:bundle, :package, 'doc:archive'] do
  sh "googlecode_upload.py -s \"YASnippet Release #{$version}\"" +
    " -p yasnippet -l \"Featured,Type-Package,OpSys-All\"" +
    " pkg/yasnippet-#{$version}.tar.bz2"
  sh "googlecode_upload.py -s \"YASnippet Bundle #{$version}\"" +
    " -p yasnippet -l \"Featured,Type-Package,OpSys-All\"" +
    " pkg/yasnippet-bundle-#{$version}.el.tgz"
  sh "googlecode_upload.py -s \"YASnippet Document #{$version}\"" +
    " -p yasnippet -l \"Featured,Type-Docs,OpSys-All\"" +
    " pkg/yasnippet-doc-#{$version}.tar.bz2"
  FileUtils.cp "yasnippet-bundle.el", "pkg/yasnippet-bundle-#{$version}.el"
  sh "echo for ELPA | mutt -a pkg/yasnippet-bundle-#{$version}.el -s " +
     "'YASnippet bundle v#{$version}' elpa@tromey.com"
end

rule '.html' => '.rst' do |t|
  sh "doc/compile-doc.py #{t.source} > #{t.name}"
end
desc "Generate document"
task :doc => FileList['doc/*.rst'].ext('html')

namespace :doc do
  task :archive do
    release_dir = "pkg/yasnippet-#{$version}"
    FileUtils.mkdir_p(release_dir)
    sh "tar cjf pkg/yasnippet-doc-#{$version}.tar.bz2 " +
      "--exclude=doc/.svn --exclude=doc/images/.svn doc/*.html doc/images"
  end

  task :upload do
    if File.exists? 'doc/gh-pages'
      Dir.chdir 'doc/gh-pages' do
        sh "git checkout gh-pages"
      end
      Dir.glob("doc/*.{html,css}").each do |file|
        FileUtils.cp file, 'doc/gh-pages'
      end
      Dir.glob("doc/images/*").each do |file|
        FileUtils.cp file, 'doc/gh-pages/images'
      end
      Dir.chdir 'doc/gh-pages' do
        sh "git commit -a -m 'Automatic documentation update.'"
        sh "git push"
      end
    end
  end
end

desc "Compile yasnippet.el into yasnippet.elc" 

rule '.elc' => '.el' do |t|
  sh "emacs --batch -L . --eval \"(byte-compile-file \\\"#{t.source}\\\")\""
end
task :compile => FileList["yasnippet.el", "dropdown-list.el"].ext('elc')

task :default => :doc

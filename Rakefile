# -*- Ruby -*-

require 'fileutils'

$EMACS = ENV["EMACS"]
if not $EMACS or $EMACS == 't'
  $EMACS = "emacs"
end

def find_version
  File.read("yasnippet.el", :encoding => "UTF-8") =~ /;; Package-version: *([0-9.]+?) *$/
  $version = $1
end
find_version
FileUtils.mkdir_p('pkg')

desc "run tests in batch mode"
task :tests do
  sh "#{$EMACS} -Q -L . -l yasnippet-tests.el" +
    " --batch -f ert-run-tests-batch-and-exit"
end

desc "run test in interactive mode"
task :itests do
  sh "#{$EMACS} -Q -L . -l yasnippet-tests.el" +
     " --eval \"(call-interactively 'ert)\""
end

desc "create a release package"
task :package do
  release_dir = "pkg/yasnippet-#{$version}"
  FileUtils.mkdir_p(release_dir)
  files = ['snippets', 'yasnippet.el']
  FileUtils.cp_r files, release_dir
  File.open(File.join(release_dir,'yasnippet-pkg.el'), 'w') do |file|
    file.puts <<END
(define-package "yasnippet"
                "#{$version}"
                "A template system for Emacs")
END
  end
  sh "git clean -f snippets"
  FileUtils.cd 'pkg' do
    sh "tar cf yasnippet-#{$version}.tar yasnippet-#{$version}"
  end
end

desc "create a release package and upload it to google code"
task :release => [:package, 'doc:archive'] do
  raise "Not implemented for github yet!"
end

desc "Generate document"
task :doc, [:htmlize] do |t, args|
  load_path = '-L .'
  if args[:htmlize]
    load_path += " -L #{args[:htmlize]}"
  end
  sh "#{$EMACS} -Q #{load_path} --batch -l doc/yas-doc-helper.el" +
    " -f yas--generate-html-batch"
end

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
      Dir.glob("doc/stylesheets/*.css").each do |file|
        FileUtils.cp file, 'doc/gh-pages/stylesheets'
      end
      curRev = `git describe`.chomp()
      expRev = IO.read('doc/html-revision').chomp()
      if curRev != expRev
        raise ("The HTML rev: #{expRev},\n" +
               "current  rev: #{curRev}!\n")
      end
      Dir.chdir 'doc/gh-pages' do
        sh "git commit -a -m 'Automatic documentation update.\n\n" +
          "From #{curRev.chomp()}'"
        sh "git push"
      end
    end
  end
end

desc "Compile yasnippet.el into yasnippet.elc"

rule '.elc' => '.el' do |t|
  cmdline = $EMACS + ' --batch -L .'
  if ENV['warnings']
    cmdline += " --eval \"(setq byte-compile-warnings #{ENV['warnings']})\""
  end
  if ENV['Werror']
    cmdline += " --eval \"(setq byte-compile-error-on-warn #{ENV['Werror']})\""
  end
  if ENV['Wlexical']
    cmdline += " --eval \"(setq byte-compile-force-lexical-warnings #{ENV['Wlexical']})\""
  end
  cmdline +=" -f batch-byte-compile #{t.source}"

  sh cmdline
end
task :compile => FileList["yasnippet.el"].ext('elc')
task :compile_all => FileList["*.el"].ext('elc')

task :default => :doc

desc "use yasmate to convert textmate bundles"
task :convert_bundles do
      cd "yasmate"
      sh "rake convert_bundles"
    end

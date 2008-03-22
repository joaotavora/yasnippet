# -*- Ruby -*-

require 'fileutils'

def find_version
  File.read("yasnippet.el") =~ /;; Version: *([0-9.]+) *$/
  $version = $1
end
find_version
FileUtils.mkdir_p('pkg')

desc "generate the bundle file."
task :bundle do
  sh 'emacs --batch -l yasnippet.el --eval "(yas/compile-bundle)"'
  sh "tar czf pkg/yasnippet-bundle-#{$version}.el.tgz yasnippet-bundle.el"
end

desc "create a release package"
task :package do
  release_dir = "pkg/yasnippet-#{$version}"
  FileUtils.mkdir_p(release_dir)
  files = ['snippets', 'yasnippet.el']
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

end

rule '.html' => '.rst' do |t|
  sh "doc/compile-doc.py #{t.source} > #{t.name}"
end
desc "Generate document"
task :doc => FileList['doc/*.rst'].ext('html')

namespace :doc do
  task :upload do
    sh "rsync -avz --exclude '.svn' doc " +
      "pluskid.lifegoo.com:~/public_html/upload/project/yasnippet/"
  end
  
  task :archive do
    release_dir = "pkg/yasnippet-#{$version}"
    FileUtils.mkdir_p(release_dir)
    sh "tar cjf pkg/yasnippet-doc-#{$version}.tar.bz2 " +
      "--exclude=doc/.svn --exclude=doc/images/.svn doc/*.html doc/images"
  end
end

task :default => :doc

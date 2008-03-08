# -*- Ruby -*-

require 'fileutils'

desc "generate the bundle file."
task :bundle do
  sh 'emacs --batch -l yasnippet.el --eval "(yas/compile-bundle ' +
    '\"./yasnippet.el\" \"./yasnippet-bundle.el\" \"./snippets\")"'
end

desc "create a release package"
task :package do
  File.read("yasnippet.el") =~ /;; Version: *([0-9.]+) *$/
  version = $1
  release_dir = "pkg/yasnippet-" + version
  FileUtils.mkdir_p(release_dir)
  files = ['tools', 'snippets', 'yasnippet.el', 'Rakefile']
  FileUtils.cp_r files, release_dir
  FileUtils.rm_r Dir[release_dir + "/**/.svn"]
  FileUtils.cd 'pkg'
  sh "tar cjf yasnippet-" + version + ".tar.bz2 yasnippet-" + version
end

task :default => :bundle

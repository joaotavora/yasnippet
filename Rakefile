# -*- Ruby -*-

require 'fileutils'

desc "generate the bundle file."
task :bundle do
  sh 'emacs --batch -l yasnippet.el --eval "(yas/compile-bundle ' +
    '\"./yasnippet.el\" \"./yasnippet-bundle.el\" \"./snippets\")"'
end

def find_version
  File.read("yasnippet.el") =~ /;; Version: *([0-9.]+) *$/
  version = $1
end

desc "create a release package"
task :package do
  version = find_version
  release_dir = "pkg/yasnippet-" + version
  FileUtils.mkdir_p(release_dir)
  files = ['snippets', 'yasnippet.el', 'Rakefile']
  FileUtils.cp_r files, release_dir
  FileUtils.rm_r Dir[release_dir + "/**/.svn"]
  FileUtils.cd 'pkg'
  sh "tar cjf yasnippet-" + version + ".tar.bz2 yasnippet-" + version
  FileUtils.cd ".."
end

desc "create a release package and upload it to google code"
task :release => :package do
  version = find_version
  sh "googlecode_upload.py -s \"YASnippet Release " + version + "\"" +
    " -p yasnippet --config-dir=none -l \"Featured,Type-Package,OpSys-All\"" +
    " pkg/yasnippet-" + version + ".tar.bz2"
end

task :default => :bundle

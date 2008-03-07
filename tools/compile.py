#!/usr/bin/python

# Author: pluskid <pluskid@gmail.com>
# X-URL: http://code.google.com/p/yasnippet/
# A tool to compile yasnippet.el and the snippets into a single
# yasnippet-bundle.elc file.

import sys
import os
import re
import getopt
import shutil

options = {
    'emacs'        : 'emacs',
    'dest'         : '../yasnippet-bundle.el',
    'byte-compile' : True,
    'src'          : '../yasnippet.el',
    'dirs'         : []
}
dest_file = None

def usage():
    message = sys.argv[0] + """ options directory ...

  Scan directories, load the definitions of snippets
  and compile them with yasnippet.el to a stand-alone
  easy to use bundle.

  Options:
  -i The location of yasnippet.el.
     Default value is '../yasnippet.el'.
  -o The output file.
     Default value is '../yasnippet-bundle.el'.
  -n Do not byte-compile the result.
  -e The location of the emacs executable.
     Default value is 'emacs'.
  -h Print this message."""
    print >> sys.stderr, message

def process_args():
    opts, dirs = getopt.gnu_getopt(sys.argv[1:], "nhi:o:e:")
    for o, v in opts:
        if o == "-h":
            usage()
            return False
        elif o == "-n":
            options['byte-compile'] = False
        elif o == "-i":
            options['src'] = v
        elif o == "-o":
            options['dest'] = v
        elif o == "-e":
            options['emacs'] = v
    options['dirs'] = dirs

    if not os.path.isfile(options['src']):
        print >> sys.stderr, "Can't read " + options['src']
        return False
    for dir in dirs:
        if not os.path.isdir(dir):
            print >> sys.stderr, dir + " is not a existing directory"
            return False
    return True

def prepare():
    global dest_file
    dest_file = open(options['dest'], 'w+')
    dest_file.write(open(options['src']).read())
    dest_file.write("""

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;      Auto-generated code         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(yas/initialize)
""")

def finish():
    dest_file.write("(provide '")
    dest_file.write(re.sub('\.[^.]*$', '', os.path.basename(options['dest'])))
    dest_file.write(")\n")
    dest_file.close()

def parse_snippet(cont):
    rlt = re.search("^# --\n", cont, re.M)
    if rlt:
        header = cont[0:rlt.start()]
        temp = cont[rlt.end():]
        rlt = re.search("^#name *: *(.*)$", header, re.M)
        if rlt:
            return (temp, rlt.group(1))
        return (temp, None)
    return (cont, None)

def quote_string(str):
    return '"' + str.replace("\\", "\\\\").replace("\"", "\\\"") + '"'

def compile_snippet(dir, mode, key):
    cont = open(os.path.join(dir, mode, key)).read()
    template, name = parse_snippet(cont)
    dest_file.write("(yas/define '" + mode + " ")
    dest_file.write(quote_string(key) + "\n  ")
    dest_file.write(quote_string(template) + " ")
    dest_file.write(quote_string(name or key) + ")\n")

def compile_snippets(dir):
    modes = [e for e in os.listdir(dir) if e[0] != '.']
    dest_file.write("\n;;; snippets from " + dir + "\n")
    for mode in modes:
        dest_file.write("\n;; snippets for " + mode + "\n")
        keys = [e for e in os.listdir(os.path.join(dir, mode)) \
                    if os.path.isfile(os.path.join(dir, mode, e))]
        for key in keys:
            compile_snippet(dir, mode, key)

def byte_compile():
    os.system(options['emacs'] + 
              " --batch" +
              " --eval \"(byte-compile-file \\\"" +
              options['dest'] +
              "\\\")'")

if __name__ == '__main__':
    if len(sys.argv) < 2:
        usage()
        sys.exit(1)
    if not process_args():
        sys.exit(2)

    prepare()
    for dir in options['dirs']:
        compile_snippets(dir)
    finish()
    if options['byte-compile']:
        byte_compile()

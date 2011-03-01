#!/usr/bin/python

"""
notmuch2mbox - search notmuch for email and pack them in an mbox.

See notmuch2mbox --help for instructions.

This code is versioned at https://github.com/jvasile/notmuch2mbox

If you want to integrate this into notmuch emacs, add this to your
.emacs, then call it with `M-x notmuch-search-make-mbox`:

(setf notmuch2mbox-dest "~/notmuch.mbox")
(defun notmuch-search-make-mbox (&optional query)
  "Make an mbox of the current search results.  Put mbox in file indicated by notmuch2mbox-dest"
  (interactive)
  (when (null query) (setq query notmuch-search-query-string))
  (let ((dest (expand-file-name notmuch2mbox-dest)))
    (call-process "notmuch2mbox.py" nil 0 nil "-o" dest query)
    ))

Copyright 2011 James Vasile (james@hackervisions.org).  You may
distribute this software under the GNU General Public License, Version
3 or later.  For a copy of this license, see
<http://www.gnu.org/licenses/gpl.html>.
"""

NOTMUCH_BIN = "notmuch-retry"

import os, sys, subprocess
from optparse import OptionParser

def slurp(filespec):
    with open(filespec) as x: f = x.read()
    return f

def parse_options():

    help = """
%prog [options] [search terms]

This program uses notmuch to search for the search terms, then outputs
any found emails in mbox format.

Make sure notmuch-retry is in your path or that you use the -n switch
to specify your usual notmuch binary.
"""

    parser = OptionParser(usage=help)
    parser.add_option("-n", "--notmuch", dest="notmuch", default=NOTMUCH_BIN, help="Path to notmuch binary.")
    parser.add_option("-o", "--outfile", dest="outfile", action="store", 
                      help="Write mbox to specified path instead of stdout")
    (opts, args) = parser.parse_args()

    opts.search = ' '.join(args)
    if not opts.search:
        parser.print_help()
        sys.exit()

    return opts

def make_mbox(search, outfile=None, bin="notmuch"):
    from email.parser import Parser

    cmd = '%(bin)s show %(search)s | grep -o " filename:[^ ]*" | sed "s/ filename://"' % {'bin':bin, 'search':search}
    filespecs = subprocess.Popen(cmd, shell=True, bufsize=1024, stdout=subprocess.PIPE).stdout.read().rstrip().split("\n")

    if outfile:
        FH = open(outfile, 'w')

    for filespec in filespecs:
        try:
            email = slurp(filespec)
        except IOError, e:
            sys.stderr.write("Couldn't open %s: %s\nSkipping." % (filespec, e))
            continue

        headers = Parser().parsestr(email, headersonly=False)

        #TODO: fake envelope-sender by parsing From or Reply-To

        if outfile:
            FH.write(str(headers))
        else:
            print headers

    if outfile:
        FH.close()

def main():
    opts = parse_options()
    make_mbox(opts.search, outfile=opts.outfile, bin=opts.notmuch)

if __name__ == "__main__":
    main()

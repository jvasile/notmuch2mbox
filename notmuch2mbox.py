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

If notmuch-retry is in your path this script will use it to reduce
delays due to database locking.
"""

    parser = OptionParser(usage=help)
    parser.add_option("-n", "--notmuch", dest="notmuch", default=NOTMUCH_BIN, help="path to notmuch binary.  In the case of an error, notmuch2mbox retries using the notmuch found in the path.")
    parser.add_option("-o", "--outfile", dest="outfile", action="store", 
                      help="write mbox to specified path instead of stdout")
    (opts, args) = parser.parse_args()

    opts.search = ' '.join(args)
    if not opts.search:
        parser.print_help()
        sys.exit()

    return opts

def get_filenames(search, bin="notmuch"):
    cmd = '%(bin)s show %(search)s | grep -o " filename:[^ ]*" | sed "s/ filename://"' % {'bin':bin, 'search':search}
    pipe = subprocess.Popen(cmd, shell=True, bufsize=1024, 
                                 stderr=subprocess.PIPE, stdout=subprocess.PIPE)
    filespecs = pipe.stdout.read().rstrip().split("\n")
    err_msg = pipe.stderr.read().rstrip()

    if err_msg:
        if bin != "notmuch":
            filespecs = get_filenames(search)
        else:
            sys.stderr.write(err_msg)
            sys.exit(255)

    return filespecs

def make_mbox(search, outfile=None, bin="notmuch"):
    from email.parser import Parser

    filespecs = get_filenames(search, bin)

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

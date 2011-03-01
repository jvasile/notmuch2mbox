#!/usr/bin/python

"""
notmuch2mbox - search notmuch for email and pack them in an mbox.

See notmuch2mbox --help for instructions.

This code is versioned at 
Copyright 2011 James Vasile (james@hackervisions.org).  You may
distribute this software under the GNU General Public License, Version
3 or later.  For a copy of this license, see
<http://www.gnu.org/licenses/gpl.html>.
"""

NOTMUCH_BIN = "notmuch"

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
        
        print headers

    if outfile:
        FH.close()

def main():
    opts = parse_options()
    make_mbox(opts.search, outfile=opts.outfile, bin=opts.notmuch)

if __name__ == "__main__":
    main()

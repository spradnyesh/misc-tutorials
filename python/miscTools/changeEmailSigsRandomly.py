#!/usr/bin/env python

import os, sys, random

try:
    fp = open(os.environ['HOME'] + '/.sigs', 'r')
    lines = fp.readlines()
    fp.close()
    if len(lines) == 0:
        print '''*** ERROR: File ~/.sigs is empty ***'''
    else:
        sign = '\n'
        while sign == '\n':
            sign = random.choice(lines)
        # print 'Signature: ' + sign
        fp = open(os.environ['HOME'] + '/.signature', 'w')
        fp.write(sign)
        fp.close()
except IOError:
    print '''*** ERROR: File ~/.sigs does not exist ***'''
    sys.exit(1)

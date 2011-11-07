#!/usr/bin/env python

import os, string, sys
#import shutil
import textwrap

os.chdir(os.environ["HOME"])
# os.chdir(os.path.expanduser('~')
try:
    fp = open(".sigs", "r")
except IOError: 
    a = raw_input("*** ERROR: File ~/.sigs does not exist ***")
    sys.exit(1)
lines = fp.readlines()
fp.close()
if len(lines) == 0:
    a = raw_input("*** ERROR: File ~/.sigs is empty ***")
    sys.exit(1)
try:
    n = int(string.split(lines[0])[1])
except ValueError:
    n = 0
    noIndex = 1    #True: Index line is absent in ~/.sigs file
else:
    noIndex = 0    #False: Index line is present in ~/.sigs file
if n == len(lines):
    n = 1
inf = 0
while string.split(lines[n])[0][0] == ';':
    n += 1
    if n == len(lines):
        n = 1
        inf += 1
    if inf == 2:    #check for infinite loop
        raw_input("*** ERROR: All lines in file are commented ***")
        sys.exit(2)
sign = lines[n]
#a = raw_input("*** n: " + str(n) + ", Signature: " + string.rstrip(sign, "\n") + " ***")
fp = open(".signature", "w")
unwrappedSign = textwrap.wrap(sign, 75)
sign = ""
for u in unwrappedSign:
    sign += u + "\n"
fp.write("warm regards,\nPradnyesh Sawant\n--\n" + sign)
fp.close()
fp = open(".sigs.tmp", "w")
fp.write("# " + str(n + 1) + " #Please don't remove this line, it is present for indexing of signature usage. Also, please comment phrases that you don't want to use as signatures by prepending them with \"# \" (including this line)\n")
for i in range(~noIndex + 2, len(lines)):    #"~x + 2" gives it's bitwise complement; eg 0 -> 1, 1 -> 0
                                            #this is b'coz, "~x + 1" gives 2's complement; eg 0 --> 0, 1 --> -1
    fp.write(lines[i])
fp.close()
os.system("mv .sigs.tmp .sigs")
#shutil.move(".sigs.tmp .sigs")

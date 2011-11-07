#!/usr/bin/env python

##########################
# Module: shredder.py
# Author: Pradnyesh Sawant
# Date: 05Jan2008
# Version: 0.2
"""
shred files: basically, replace file contents with random chars and then
delete the file ;)
"""
# Log:
# 24Jan2007: create
# 05Jan2008: traverse directories, and shred all files within it
# 05Jan2008: there exists already a tool/cmd called shred; all I need to do
# is to make it traverse a dir :)
##########################

import random
import sys
import os

def shred(fName):
    cmd = "shred -uz %s" % fName
    print "shredding file " + fName
    os.system(cmd)
def main(path):
    path = os.path.abspath(path)
    if os.path.isfile(path):
        shred(path)
        return
    def rmDir():
        dirList.reverse()
        for d in dirList:
            print 'removing dir ' + d
            os.rmdir(d)
    dirList = []
    for fDir, fDirs, fNames in os.walk(path):
        os.chdir(fDir)
        print "inside " + fDir
        dirList.append(fDir)
        map(shred, fNames)
    rmDir()

if __name__ == "__main__":
    if len(sys.argv) < 2:
        path = os.getcwd()
    else:
        path = sys.argv[1]
    main(path)

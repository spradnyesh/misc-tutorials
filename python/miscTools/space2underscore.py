#!/usr/bin/env python

##########################
# Module: space2underscore.py 
# Author: Pradnyesh Sawant
# Date: 25Apr2007
# Version: 0.
"""
converts space to underscores in filenames in given directory
use os.walk()
"""
# Log:
# 
# ToDo:
# 
##########################
import os
import sys

def main(dir):
    if "~" in dir:
        print "changing ~ to " + os.environ["HOME"]
        dir = dir.replace("~", os.environ["HOME"], 1)
    for fDir, fDirs, fNames in os.walk(dir):
        for fName in fNames:
            if " " in fName:
                print "changing space to underscore for [" + fDir + \
                        "/" + fName + "]"
                fNewName = fName.replace(" ", "_")
                os.rename(fDir + "/" + fName, fDir + "/" + fNewName)

if __name__ == "__main__":
    # main("/home/neil/downloads/entertainment/photos/personalPhotos/2007/new")
    if len(sys.argv) < 2:
        dir = os.getcwd()
    else:
        dir = sys.argv[1]
    main(dir)

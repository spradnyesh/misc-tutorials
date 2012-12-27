#!/usr/bin/env python

##########################
# Module: cmpr.py
# Author: Pradnyesh Sawant
# Date: 09Mar2008
# Version: 0.2
'''
compress images, video and audio from cam
'''
# Log:
# 09Mar2008:
#   command line arg decides whether to call cmpr or to call rmOrig
# ToDo:
##########################
import os
import sys

jpegQuality = 75

def cmpr(fOrig):
    nm, extn = os.path.splitext(fOrig)
    cmd = None
    if extn.lower() == '.jpg':
        fNew = nm + 'Orig' + extn
        os.rename(fOrig, fNew)
        cmd = 'convert -quality ' + str(jpegQuality) + ' ' + fNew + ' ' + fOrig
    elif extn.lower() == '.avi' or extn.lower() == '.mp4':
        #cmd = 'avconv -i ' + fOrig + ' -c:v libx264 -crf 23 -c:a libfaac -b:a 192k ' + nm + '.mpg'
        print "##### sleeping before starting to convert a video"
        os.system("sleep 10")
        cmd = 'ffmpeg -i ' + fOrig + ' -s uxga ' + nm + '.mpg'
    elif extn.lower() == '.wav':
        cmd = 'toolame ' + fOrig + ' ' + nm + '.mp3'
    elif extn.lower() == '.thm':
        cmd = 'rm -f ' + fOrig
    elif extn.lower() == '.mpg' or extn.lower() == '.mp3':
        return
    else:
        return
    print cmd
    os.system(cmd)
def main(dir, boolUndo):
    dir = os.path.abspath(dir)
    for fDir, fDirs, fNames in os.walk(dir):
        os.chdir(fDir)
        print 'inside ' + fDir
        if not boolUndo:
            map(cmpr, fNames)
        else:
            map(rmOrig, fNames)
def rmOrig(fOrig):
    ''' had written this to undo mistake of os.rename in extn == '.jpg'
    above '''
    import re
    nm, extn = os.path.splitext(fOrig)
    indx = nm.find('Orig')
    if indx > 0:
        fNew = nm[:indx] + nm[indx + 4:] + extn
        print fOrig, fNew
        os.rename(fOrig, fNew)


if __name__ == '__main__':
    # main('/mnt/new_d/photos')
    undo = False
    dir = os.getcwd()
    if len(sys.argv) > 1 and sys.argv[1] == '-r':
        undo = True
        if len(sys.argv) < 3:
            dir = os.getcwd()
        else:
            dir = sys.argv[2]
    elif len(sys.argv) > 2 and sys.argv[1] == '-q':
        jpegQuality = sys.argv[2]
    else:
        if len(sys.argv) > 1:
            dir = sys.argv[1]
    main(dir, undo)

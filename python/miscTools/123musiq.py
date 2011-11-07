#!/usr/bin/env python

##########################
# Module: yowallpaper.py
# Author: Pradnyesh Sawant
'''
script to dload mp3s from 123musiq.com
'''
# TODO:
# LOG:
# ##########################
import re
import urllib2
import sys

class Musiq(object):
    def __init__(self):
        pass
    def download(self, uri):
        fp = urllib2.urlopen(uri)
        content = fp.read()
        fp.close()
        ptrnMp3 = re.compile(r'a href=(SOURCE.*?.mp3?)')
        foundMp3 = ptrnMp3.findall(content)
        if foundMp3:
            for m in foundMp3:
                mp3Url = 'http://www.123musiq.com/' + m
                print mp3Url
                fileName = mp3Url[mp3Url.rfind('/') + 1:].replace('%20', '')
                print fileName
                fp = urllib2.urlopen(mp3Url)
                content = fp.read()
                fp.close()
                fp = open(fileName, 'w')
                fp.write(content)
                fp.close()

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print 'usage: 123musiq <url>'
    else:
        mq = Musiq()
        mq.download(sys.argv[1])

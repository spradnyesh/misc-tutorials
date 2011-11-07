#!/usr/bin/env python

##########################
# Module: flickInt.py
# Author: Pradnyesh Sawant
'''
script to get images from flickr interestingness and change kde wallpaper
'''
# TODO:
#   replace os.system by python.dcop
# LOG:
##########################

import urllib2
import glob
import os, os.path
import xml.dom.minidom as minidom
from time import strftime, localtime, sleep

class FlickInt(object):
    ''' script to get images from flickr interestingness and change kde 
    wallpaper '''
    basePath = os.path.expanduser('~') + '/.flickInt'
    api_key = '26d31cf9a6254fc0a65a8ab495d75f8d'
    def __init__(self, sleepTime = 300):
        ''' decide whether to do a firstRun or just runs '''
        self.sleepTime = sleepTime
        os.chdir(FlickInt.basePath)
        self.today = strftime('%Y%m%d', localtime())
        if not glob.glob('*' + self.today + '*'):
            try:
                urllib2.urlopen('http://www.example.org')
                self.firstRun()
            except urllib2.URLError:
                pass
        self.runs()
    def firstRun(self):
        ''' download interestingness photos for today from flickr '''
        url = \
                'http://flickr.com/services/rest?method=flickr.interestingness.getList&api_key=' \
                + FlickInt.api_key
        fp = urllib2.urlopen(url)
        data = fp.read()
        fp.close()
        photoList = minidom.parseString(data).getElementsByTagName('photo')
        for p in photoList:
            server = p.getAttribute('server')
            farm = p.getAttribute('farm')
            photoId = p.getAttribute('id')
            secret = p.getAttribute('secret')
            fName = self.today + '_' + photoId + '_' + secret + '_o.jpg'
            if not glob.glob('*' + photoId + '*'):
                photoUrl = 'http://farm%s.static.flickr.com/%s/%s_%s.jpg' \
                        % (farm, server, photoId, secret)
                print photoUrl
                fp = urllib2.urlopen(photoUrl)
                photo = fp.read()
                fp.close()
                fp = open(fName, 'wb')
                fp.write(photo)
                fp.close()
        self.runs()
    def runs(self):
        ''' change dekstop wallpaper every self.sleepTime seconds '''
        while True:
            photoList = glob.glob('*.jpg')
            fp = open('cnt', 'r')
            cnt = int(fp.read())
            fp.close()
            cnt += 1
            if cnt == len(photoList):
                cnt = 0
            fp = open('cnt', 'w')
            fp.write(str(cnt))
            fp.close()
            fName = photoList[cnt]

            os.system('/usr/bin/dcop' + ' kdesktop KBackgroundIface \
                    setWallpaper 1 ' + os.path.join(FlickInt.basePath, \
                    fName) + ' 1')
            sleep(self.sleepTime)

if __name__ == '__main__':
    fi = FlickInt()

#!/usr/bin/env python

##########################
# Module: yowallpaper.py
# Author: Pradnyesh Sawant
'''
script to d'load naruto wallpapers from yowallpaper.com
'''
# TODO:
# LOG:
# ##########################
import re
import urllib2

class Yowallpaper(object):
    def __init__(self):
        for i in xrange(23):
            uri = 'http://www.yowallpapers.com/v/Anime+Wallpapers/Naruto+wallpaper/' + '?g2_page=' + str((i + 1))
            fp = urllib2.urlopen(uri)
            content = fp.read()
            fp.close()
            print; print; print; print; print; print; print; print; print; print; print; print;
            ptrnPage = re.compile(r'href=\"(.*?html\?)')
            foundPage = ptrnPage.findall(content)
            if foundPage:
                for fPage in foundPage:
                    fp = urllib2.urlopen('http://www.yowallpapers.com' + fPage)
                    content = fp.read()
                    fp.close()
                    ptrnImage = re.compile(r'a href=\"(/d/.*?.jpg?)')
                    foundImage = ptrnImage.findall(content)
                    if foundImage:
                        print foundImage[0]
                        fp = urllib2.urlopen('http://www.yowallpapers.com' + foundImage[0])
                        content = fp.read()
                        fp.close()
                        fName = foundImage[0][foundImage[0].rfind('/') + 1:]
                        # print fName
                        fp = open('/home/pradyus/downloads/wallpapers/naruto/' + fName, 'wb')
                        fp.write(content)
                        fp.close()

if __name__ == '__main__':
    yw = Yowallpaper()

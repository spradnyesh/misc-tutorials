#!/usr/bin/env python

##########################
# Module: getWallpaper.py
# Author: Pradnyesh Sawant
'''
script to d'load naruto wallpapers from yowallpaper.com
'''
# TODO:
#   http://www.spicywallpapers.net/bollywoodactress/v/Bollywood+Actress/Sherlyn+Chopra+Wallpapers/
# LOG:
# ##########################
import re
import urllib2

class GetWallpaper(object):
    def __init__(self):
        for i in xrange(5):
            uri = \
                'http://www.spicywallpapers.net/bollywoodactress/v/Bollywood+Actress/Sherlyn+Chopra+Wallpapers/?g2_page='\
                + str(i + 1)
            print uri
            fp = urllib2.urlopen(uri)
            content = fp.read()
            fp.close()
            ptrnPage = re.compile(r'href="(.*?)\?\r\n" alt="Sherlyn Chopra Wallpapers" title="Sherlyn Chopra Wallpapers"')
            foundPage = ptrnPage.findall(content)
            if foundPage:
                for fPage in foundPage:
                    print fPage
                    fp = urllib2.urlopen('http://www.spicywallpapers.com' + fPage + '?')
                    content = fp.read()
                    fp.close()
                    # print content
                    # ptrnImage = re.compile(r'href="(.*?)\?\r\n" title="Full Size"')
                    ptrnImage = re.compile(r'<a href="(.*?)\?')
                    foundImage = ptrnImage.findall(content)
                    if foundImage:
                        for fImage in foundImage:
                            # fName = fImage.rsplit('/', 1)[1]
                            # fImage = fImage.replace(' ', '%20')
                            print fImage
                            continue
                            fp = urllib2.urlopen(fImage)
                            content = fp.read()
                            fp.close()
                            fp = open('/home/pradyus/downloads/wallpapers/sherlynChopra/' + fName, 'wb')
                            fp.write(content)
                            fp.close()
                    else:
                        print 'not found'
            else:
                print 'not found'

if __name__ == '__main__':
    gw = GetWallpaper()

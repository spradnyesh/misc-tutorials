#!/usr/bin/env python

##########################
# Module: cricinfo.py
# Author: Alagum
'''
http://shout.corp.yahoo.com/hacks/ipl.py
'''
# TODO:
# LOG:
# ##########################

import time
import dbus, gobject
import urllib2
import xml.etree
from xml.etree.ElementTree import fromstring
from dbus.mainloop.glib import DBusGMainLoop
dbus.mainloop.glib.DBusGMainLoop(set_as_default=True)

bus = dbus.SessionBus()
obj = bus.get_object("im.pidgin.purple.PurpleService", "/im/pidgin/purple/PurpleObject")
purple = dbus.Interface(obj, "im.pidgin.purple.PurpleInterface")

while 1:
    current = purple.PurpleSavedstatusGetType(purple.PurpleSavedstatusGetCurrent())
    status = purple.PurpleSavedstatusNew("",current)
    #Get score :)
    cricketurl = 'http://content-ind.cricinfo.com/ipl/engine/current/match/331063.html?view=live;wrappertype=mainframe' 
    data = urllib2.urlopen(cricketurl)
    content = data.readlines()
    score = content[93][:-5]
    purple.PurpleSavedstatusSetMessage(status, score)
    purple.PurpleSavedstatusActivate(status)
    time.sleep(10)

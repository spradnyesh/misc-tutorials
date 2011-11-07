#!/usr/bin/env python

##########################
# Module: changePidginStatus.py
# Author: Pradnyesh Sawant
'''
Change pidgin status automatically from ~/.signature file
'''
# TODO:
# LOG:
# ##########################

import dbus
import os

class ChangePidginStatus(object):
    def __init__(self):
        bus = dbus.SessionBus()
        obj = bus.get_object('im.pidgin.purple.PurpleService',
                '/im/pidgin/purple/PurpleObject')
        self.purple = dbus.Interface(obj, 'im.pidgin.purple.PurpleInterface')
        try:
            fp = open(os.environ['HOME'] + '/.signature', 'r')
            lines = fp.readlines()
            fp.close()
            message = ''
            for line in lines[3:]:
                message += line
            self.setMessage(message)
        except IOError:
            print '''*** ERROR: File ~/.sigs does not exist ***'''
            sys.exit(1)

    def setMessage(self, message):
        current = \
        self.purple.PurpleSavedstatusGetType(self.purple.PurpleSavedstatusGetCurrent())
        status = self.purple.PurpleSavedstatusNew('', current)
        self.purple.PurpleSavedstatusSetMessage(status, message)
        self.purple.PurpleSavedstatusActivate(status)

if __name__ == '__main__':
    cps = ChangePidginStatus()

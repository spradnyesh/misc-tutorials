#!/usr/bin/env python

##########################
# Module: kdeDesktop.py
# Author: Pradnyesh Sawant
'''

'''
# TODO:
# LOG:
# ##########################

import dcop, dcopext, sys
from kdecore import *

if __name__ == '__main__':
    KCmdLineArgs.init (sys.argv,"bla","foo","bar")
    app  = KApplication ()
    dcop = app.dcopClient ()
    
    kwin = dcopext.DCOPApp ("kwin", dcop)
    kdesktop = dcopext.DCOPApp ("kdesktop", dcop)
    
    # Huge list of methods, looks good
    print kwin.KWinInterface.getMethods() 
    
    # Just works
    ok,current_desktop = kwin.KWinInterface.currentDesktop() 
    print "Desktop is : ", current_desktop 
    
    # None, also no Exception
    print kdesktop.KBackgroundIface.getMethods() 
    
    # (False, None), no Exception
    print kdesktop.KBackgroundIface.currentWallpaper(current_desktop) 

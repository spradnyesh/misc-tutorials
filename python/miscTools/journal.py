#!/usr/bin/env python

# ##########################
# Module: journal.py
# Author: Pradnyesh Sawant
# TODO:
# ##########################
from time import sleep, strftime, localtime
import os.path as osp
import sys
from PyQt4 import QtGui, QtCore

class Journal(QtGui.QDialog):
    firstFlag = False
    count = 0
    jrnlFile = open(osp.join(osp.expanduser("~"), ".timeMgtJrnl/" + \
            strftime("%Y%m", localtime())), "a")
    dlgs = []   # this is needed to keep a reference of the newly created dialog
    def __init__(self, parent = None):
        QtGui.QDialog.__init__(self, parent)

        self.curTime = strftime("%d%b%Y, %H:%M", localtime())
        self.createFe()
        self.showFe()
    def createFe(self):
        self.lTime = QtGui.QLabel("Time: " + self.curTime)
        lEntry = QtGui.QLabel("Make entry: ")
        self.teEntry = QtGui.QLineEdit()
        pbSave = QtGui.QPushButton("&Save")
        pbCancel = QtGui.QPushButton("&Cancel")
        loGrd = QtGui.QGridLayout()
        loGrd.addWidget(self.lTime, 0, 0, 1, 2)
        loGrd.addWidget(lEntry, 1, 0)
        loGrd.addWidget(self.teEntry, 1, 1)
        loGrd.addWidget(pbSave, 2, 0)
        loGrd.addWidget(pbCancel, 2, 1)
        self.setLayout(loGrd)
        self.setWindowTitle(self.tr("Journal"))
        QtCore.QObject.connect(pbSave, QtCore.SIGNAL("clicked()"),
                self.makeEntry)
        QtCore.QObject.connect(pbCancel, QtCore.SIGNAL("clicked()"),
                self, QtCore.SLOT("close()"))
    def showFe(self):
        self.show()
        # start a new instance every 15 mins (= 15 * 60 * 1000 = 900000)
        QtCore.QTimer.singleShot(900000, self.newJrnl)
    def newJrnl(self):
        jrnl = Journal()
        Journal.dlgs.append(jrnl)
    def makeEntry(self):
        Journal.jrnlFile.write(self.curTime + "--\t" +
                self.teEntry.text() + "\n")
        Journal.jrnlFile.flush()
        self.close()

if __name__ == "__main__":
    app = QtGui.QApplication(sys.argv)
    jrnl = Journal()
    sys.exit(app.exec_())

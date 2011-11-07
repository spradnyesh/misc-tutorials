#!/usr/bin/env python

##########################
# Module: getDay.py
# Author: Pradnyesh Sawant
# Date: 23Dec2006
# Version: 0.2
"""
Gives day on a given date
"""
# Log: 
# 21Dec2006: nonOO, nonExceptionHandling
# 23Dec2006: OO, EH
##########################

import time

class Date(object):
    mnthDay = {"jan": 31, "feb": 28, "mar": 31, "apr": 30, 
            "may": 31, "jun": 30, "jul": 31, "aug": 31, "sep": 30, 
            "oct": 31, "nov": 30, "dec": 31}
    mnth = ["None", "jan", "feb", "mar", "apr", "may", "jun", 
            "jul", "aug", "sep", "oct", "nov", "dec"]
    days = ["mon", "tue", "wed", "thu", "fri", "sat", "sun"]

    def __init__(self):
        """the above 3 vars are class vars, and not instance vars"""
        pass

    def inputDate(self):
        date = raw_input("Enter dd/mm/yyyy: ")
        date = date.split("/")
        # print date
        self.dt = int(date[0])
        self.mnt = int(date[1])
        self.yr = int(date[2])

   def setDate(self, date, sep = " "):
        date = date.split(sep)
        # print date
        self.dt = int(date[0])
        self.mnt = int(date[1])
        self.yr = int(date[2])

    def checkDate(self):
        try:
            m = self.mnth[self.mnt]
        except IndexError:
            return False
        if self.leap(self.yr) and self.mnt == 2 and self.dt == 29:
            return True
        if self.dt > self.mnthDay[self.mnth[self.mnt]]:
            return False
        return True

     def leap(self, yr):
        """Tell if given year is leap or not"""
        if not yr % 4 and yr % 100 or not yr % 400: return True
        else: return False
    
    def numDays(self, dt, mnt, yr):
        """The given dt is the i'th day of yr (eg. 1Jan is 1st day, 
        whereas 1Feb is 32nd day of yr)"""
        d = 0
        for i in range(1, mnt):
            d += self.mnthDay[str(self.mnth[i])]
        d += dt
        if mnt > 2 and self.leap(yr):
            d += 1
        return d

    def getDay(self):
        """dt falls on xyz day of week"""
        if not self.checkDate():
            raise ValueError
        curT = time.localtime(time.time())
        curYr = curT[0]; curMnt = curT[1]; curDt = curT[2]; curDoW = curT[6]
        # print curT, curYr, curMnt, curDt, curDoW
        curNum = self.numDays(curDt, curMnt, curYr)
        num = self.numDays(self.dt, self.mnt, self.yr)
        # print curNum, num
        diff = (curNum - num) % 7
        dow = (curDoW - diff) % 7
        # print "day on ", self.dt, self.mnth[self.mnt], "is",\
        # self.days[self.dow]
        # found that self.dt/self.mnt/curYr falls on self.days[self.dow]

        numYr = abs(curYr - self.yr)
        if self.yr > curYr:
            for i in range(curYr, self.yr):
                if self.leap(i):
                    numYr += 1
        else:
            for i in range(self.yr, curYr):
                if self.leap(i):
                    numYr += 1
        numYr %= 7
        if self.yr > curYr:
            dow = (dow + numYr) % 7
        else:
            dow = (dow - numYr + 7) % 7
        # print "day on ", self.dt, self.mnth[self.mnt], self.yr, "is", \
        # self.days[self.dow]

        return self.days[dow]
    
if __name__ == "__main__":
    dt = Date()
    for i in range(2006 - 10, 2006 + 10):
        d = "23 12 " + str(i)
        print d, 
        dt.setDate(d)
        try:
            dow = dt.getDay()
            print dow
        except ValueError:
            print "raised Exception"

#!/usr/bin/env python

##########################
# Module: phoneBook.py import 
# Author: Pradnyesh Sawant
# Date: 27Jan2007
# Version: 0.1
"""
create and manage a phonebook (may be enhance later to incorporate b'days,
etc)
"""
# Log:
# 27Jan2007: create
##########################

import string

class NotFound(Exception):
    pass

class PhBk(object):
    dct = {}
    chgd = False
    def add(self, nm, typ, ph):
        nm = string.capwords(nm)
        if self.srch(nm, None, ph):
            return ph + " already present for " + nm
        self.chgd = True
        try:
            lst = self.dct[nm]
            lst.append((typ, ph))
            return ph + " added successfully for " + nm
        except KeyError:
            lst = [(typ, ph)]
            self.dct[nm] = lst
            return ph + " added successfully for " + nm
    def srch(self, nm, typ, ph):
        nm = string.capwords(nm)
        if ph != None:
            try:
                lst = self.dct[nm]
                for t, p in lst:
                    if p == ph:
                        return True
                return False
            except KeyError:
                return False
        elif typ != None:
            try:
                lst = self.dct[nm]
                for t, p in lst:
                    if t == typ:
                        return p
                raise NotFound
            except KeyError:
                raise NotFound
        else:
            pass
    def dlt(self, nm, typ):
        nm = string.capwords(nm)
        try:
            ph = self.srch(nm, typ, None)
            self.chgd = True
            lst = self.dct[nm]
            lst.remove((typ, ph))
            return ph + " deleted from " + nm
        except NotFound:
            return ph + " not found for " + nm
    def saveDB(self, fNm):
        self.chgd = False
        if fNm == "":
            fNm = "phBk"
        print "fNm: " + fNm
        try:
            fp = open(fNm, "r")
            fp.close()
            # create a tmp file if fNm already exists
            fNm = raw_input("file already exists, please specify another \
file name: ")
            self.saveDB(fNm)
        except IOError:
            # open new file if file does not exist already
            print "opening new file"
            fp = open(fNm, "w")
            tmpFlg = False
        import pickle
        try:
            pickle.dump(self.dct, fp)
        except: #dunno which exception
            return "could not save data to file"
        fp.close()
        return "data saved to file"
    def loadDB(self, fNm):
        if fNm == "":
            fNm = "phBk"
        try:
            fp = open(fNm, "r")
        except IOError:
            return "could not open file " + fNm
        import pickle
        try:
            nm, lst = self.dct.popitem() #trying to see if dct is empty
            print "dct is NOT empty"
            self.dct[nm] = lst  #dct is NOT empty, so reinsert popped item
            try:
                dct = pickle.load(fp)
                print dct
                for k in dct.keys():
                    if self.dct.has_key(k):
                        #name already present in self.dct
                        lsd = self.dct[k]
                        ld = dct[k]
                        #handle duplicates
                        #find intersection  #rlst = ld ^ lsd
                        rslt = []
                        for x in lsd:
                            if x in ld and x not in rslt:
                                rslt.append(x)
                        #find diff              #ld -= ld
                        for x in rslt:
                            ld.remove(x)
                        #append diff to lst     #lst += ld
                        for x in ld:
                            lst.append(x)
                    else:
                        #name NOT present in self.dct
                        self.dct[k] = dct[k]
            except: #dunno which exception
                return "could not load data from file"
        except KeyError:    #dct is empty
            try:
                self.dct = pickle.load(fp)
            except: #dunno which exception
                return "could not load data from file"
        fp.close()
        return "data loaded from file"
    def disp(self):
        pass
    def menu(self):
        while 1:
            print "***" + str(self.dct) + "***"
            print "1: add a new entry"
            print "2: search for an entry"
            print "3: delete an entry"
            print "4: save DB to a file"
            print "5: load DB from a file"
            print "0: quit"
            opt = raw_input("Enter a choice: ")
            try:
                opt = int(opt)
            except ValueError:
                continue
            if not 0 <= opt <= 5:
                continue
            elif opt == 0:
                if self.chgd:
                    sv = raw_input("The DB has changed, do you want to \
save it (y/n)? ")
                    if sv.lower() == "y":
                        fnm = raw_input("Enter file name (just press \
\"enter\" for default file name): ")
                        print self.saveDB(fnm)
                break
            elif opt == 1:
                nm = raw_input("Enter name: ")
                typ = raw_input("Enter type of phoneNo: ")
                ph = raw_input("Enter phoneNo: ")
                print self.add(nm, typ, ph)
            elif opt == 2:
                nm = raw_input("Enter name: ")
                typ = raw_input("Enter type of phoneNo: ")
                try:
                    ph = self.srch(nm, typ, None)
                    print "ph is " + ph
                except NotFound:
                    print "ph not found"
            elif opt == 3:
                nm = raw_input("Enter name: ")
                typ = raw_input("Enter type of phoneNo: ")
                print self.dlt(nm, typ)
            elif opt == 4:
                fnm = raw_input("Enter file name (just press \"enter\" for \
default file name): ")
                print self.saveDB(fnm)
            elif opt == 5:
                fnm = raw_input("Enter file name (just press \"enter\" for \
default file name): ")
                print self.loadDB(fnm)

if __name__ == "__main__":
    p = PhBk()
    p.menu()

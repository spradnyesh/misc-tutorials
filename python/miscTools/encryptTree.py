#!/usr/bin/env python

##########################
# Module: encryptTree.py 
# Author: Pradnyesh Sawant
# Date: <2009-07-15 Wed>
# Version: 0.1
'''
walk down a tree
make a encrypted copy of tree
'''
##########################
import os
import sys

def encrypt(fOrig):
    fDir = os.getcwd()
    backupDir = fDir.replace(source, backup)
    fEnc = backupDir + '/' + fOrig + '.gpg'
    if not os.path.islink(fOrig) and \
           (not os.path.exists(fEnc) or \
            os.path.getmtime(fEnc) < os.path.getmtime(fOrig)):
        cmd = 'gpg -e ' + fOrig
        print '**** execute:', cmd
        os.system(cmd)
        os.system('mv -f ' + fOrig + '.gpg ' + fEnc)

def main(source, backup):
    for fDir, fDirs, fNames in os.walk(source):
        os.chdir(fDir)
        backupDir = fDir.replace(source, backup)
        print 'fDir: [' + fDir + '], backupDir: [' + backupDir + ']'
        if not os.path.exists(backupDir):
            print 'created ', backupDir
            os.mkdir(backupDir)
        map(encrypt, fNames)
def convertIntoAbsoluteDirName(path):
    if path[0] == '~':
        path = os.path.expanduser('~') + path[1:]
    if os.path.exists(path) and os.path.isdir(path):
        return os.path.abspath(path)
    return None

if __name__ == '__main__':
    os.chdir(os.environ["HOME"])
    if len(sys.argv) < 2:
        source = os.getcwd() + '/source'
        backup = os.getcwd() + '/backup'
    elif len(sys.argv) < 3:
        print 'usage: encryptTree.py <sourceDir> <backupDir>'
        exit
    else:
        source = sys.argv[1]
        backup = sys.argv[2]
    source = convertIntoAbsoluteDirName(source)
    backup = convertIntoAbsoluteDirName(backup)
    if (source is None) or (backup is None):
        print 'one of source or backup does not exist or is not a directory; please specify existing directories as source and backup'
    main(source, backup)

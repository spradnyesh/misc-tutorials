#!/bin/bash
#set -x

gitPushToBackup() {
cd /media/ssafe/git
for i in *
do
    echo "***** $i *****"
    cd /media/ssafe/git/$i
    remote=`git remote -v show | grep push | awk '{print $2}'`
    cd $remote
    modified=`git status | grep modified | wc -l`
    if [ $modified -gt "0" ]
    then
        git status | grep modified | awk '{print $3}' | xargs git add
        git commit -am "sfrd @ `date`"
    fi
    cd /media/ssafe/git/$i
    git pull $remote master
    #git reset --hard
done
cd
}

gitPullFromBackup() {
cd /media/ssafe/git
for i in *
do
    echo "***** $i *****"
    origin="/media/ssafe/git/$i"
    cd $origin
    cd `git remote -v show | grep push | awk '{print $2}'`
    modified=`git status | grep modified | wc -l`
    if [ $modified -gt "0" ]
    then
        git stash
    fi
    git pull $origin master
    #git reset --hard
    if [ $modified -gt "0" ]
    then
        git stash pop
    fi
done
cd
}

option=$STR$1
if [ "$option" == "pull" ]
then
    gitPullFromBackup
else
    gitPushToBackup
fi

#!/bin/bash

clive http://www.youtube.com/watch?v=$1 --exec 'ffmpeg -i "%n" -f mp3 "%n.mp3"'

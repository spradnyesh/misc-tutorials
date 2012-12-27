#!/bin/bash

ffmpeg -y -i $1 -f mp3 -ab 128k ${1}.mp3

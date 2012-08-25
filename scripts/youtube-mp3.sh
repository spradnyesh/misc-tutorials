#!/bin/bash

cclive -f best ${1} --exec 'avconv -i "%n" "%n.mp3"'

#!/bin/bash

find $1 | while read file
do
    if [[ "$file" =~ ^.*\.clj$ ]]; then
        emacs "$file" \
              --batch \
              --eval="(progn
                        (load \"~/.emacs.d/elpa/clojure-mode-2.0.0/clojure-mode\")
                        (require 'clojure-mode)
                        (clojure-mode)
                        (indent-region (point-min) (point-max))
                        (save-buffer))"
    fi
done

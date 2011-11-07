(defpackage :com.gigamonkeys.id3v2
  (:use :common-lisp
         :com.ipysd.binary-data
         :com.ipysd.pathnames)
  (:export
   :read-id3
   :mp3-p
   :id3-p
   :album
   :composer
   :genre
   :encoding-program
   :artist
   :part-of-set
   :track
   :song
   :year
   :size
   :translated-genre))

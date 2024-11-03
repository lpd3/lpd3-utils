;;;; lpd3-utils/lpd3-utils.asd
                             
(in-package :asdf-user)
                             
(defsystem "lpd3-utils"
  :description "Personal ulities, many
  gleaned from Peter Norvig's On Lisp"
  :author "Larry Devlin"
  :mailto "larrydevlin1770@gmail.com"
  :serial t
  :components ((:file "package")
               (:file "01-evaluation-and-compilation")
               (:file "02-types-and-classes")
               (:file "03-data-and-control-flow")
               (:file "04-iteration")
               (:file "05-objects")
               (:file "06-structures")
               (:file "07-conditions")
               (:file "08-symbols")
               (:file "09-packages")
               (:file "10-numbers")
               (:file "11-characters")
               (:file "12-conses")
               (:file "13-arrays")
               (:file "14-strings")
               (:file "15-sequences")
               (:file "16-hash-tables")
               (:file "17-filenames")
               (:file "18-files")
               (:file "19-streams")
               (:file "20-printer")
               (:file "21-reader")
               (:file "22-system-construction")
               (:file "23-environment")
               (:file "24-miscellany")))
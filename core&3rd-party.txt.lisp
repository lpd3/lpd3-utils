;;;; lpd3-utils/3rd-party.txt.lisp

"
1. Evaluation and Compilation
   
a. Serapeum
   
eval-always
eval-and-compile


2. Types and Classes
   
a. Alexandria

string-designator (type)
coercef
of-type
type=

b. Serapeum
   
->
assure
assuref
supertypep
proper-subtype-p
proper-supertype-p
with-type-dispatch
with-subtype-dispatch
with-string-dispatch
with-vector-dispatch
with-simple-vector-dispatch
with-two-arg-test
with-member-test
with-item-key-function
true
defunit
defunion
match-of

3. Data and Control Flow
   
a. Alexandria

define-constant
destructuring-case
ensure-functionf
multiple-value-prog2
named-lambda
nth-value-or
if-let
when-let
when-let*
switch
cswitch
eswitch
whichever
xor
disjoin
conjoin
compose
ensure-function
multiple-value-compose
curry
rcurry
line-up-first
line-up-last

b. Serapeum
   
callf
callf2
def
define-values
defconst
defsubst
defalias
defplace
defvar-unbound
defparameter-unbound
defloop
lret
lret*
letrec
letrec*
receive
mvlet*
mvlet
and-let*
no
null-if
nor
nand
typecase-of
etypecase-of
case-of
ecase-of
ctypecase-of
ccase-of
destructuring-ecase-of
destructuring-case-of
destructuring-ccase-of
case-using
ecase-using
string-case
string-ecade
eif
eif-let
econd
cond-let
econd-let
cond-every
bcond
case
case-let
comment
example
nix
ensure
ensure2
~>
~>>
nest
select
selector
sort-values
eq*
eql*
equal*
equalp*
without-recursion
op
opf
eqs
eqls
equals
partial
trampoline
define-train
flip
nth-arg
distinct
throttle
once
fuel
juxt
dynamic-closure
hook
fork
hook2
fork2
capped-fork
capped-fork2
fnil
variadic->unary
unary->variadic
mvconstantly
do-nothing
repeat-until-stable
fbind
fbind*
fbindrec
fbindrec*
recklessly-continue
static-binding-flush-error-group
static-binding-flush-error-all-groups-p
static-binding-flush-error
static-binding-active-error
flush-static-bindings-group
flush-all-static-binding-groups
static-let
static-let*
in

c. UIOP
   
call-function
call-functions
nest


4. Iteration
   
a. Serapeum
   
nlet
with-collector
collecting
with-collectors
summing

5. Objects
   
a. Serapeum
   
make
class-name-of
class-name-safe
find-class-safe
slot-value-safe
defmethods

b. UIOP
   
coerce-class


6. Structures
   
a. Serapeum
   
defstruct-read-only
deconstruct
defconstructor


7. Conditions
   
a. Alexandria
   
ignore-some-conditions
   
b. Serapeum
   
case-failure (function)
defcondition
maybe-invoke-restart

c. UIOP
   
call-with-muffled-conditions
match-any-condition-p
match-condition-p
not-implemented-error
parameter-error
with-muffled-conditions

8. Symbols
   
Alexandria

ensure-symbol
format-symbol
make-keyword
make-gensym
make-gensym-list
symbolicate

b. Serapeum
   
find-keyword
bound-value

c. UIOP
   
standard-case-symbol-name

9. Packages
   
a. Serapeum
   
package-exports
package-names
package-name-keyword
find-external-symbol
export-always
export-only
export-only-always

b. UIOP
   
find-package*
find-symbol*
rehome-symbol
symbol-call
define-package
find-standard-case-symbol

10. Numbers
    
a. Alexandria

maxf
minf
binomial-coefficient
count-permutations
clamp
lerp
factorial
subfactorial
gaussian-random
iota
map-iota
mean
median
variance
standard-deviation

b. Serapeum
   
fixnump
null-if-zero
finc
fdec
parse-float
round-to
bits
unbits
shrink
grow
shrinkf
growf
random-in-range
float-precision-contagion

11. Characters
    
a. Serapeum
   
ascii-char-p

b. UIOP

12. Conses
    
Alexandria

proper-list (type)
circular-list (type)
appendf
nconcf
remove-from-plist
delete-from-plist
reversef
nreversef
unionf
nunionf
doplist
circular-list-p
proper-list-p
alist-plist
plist-alist
circular-list (function)
make-circular-list
ensure-car
ensure-cons
ensure-list
flatten
lastcar (setfable)
proper-list-length
mappend
map-product
delete-from-plist*
set-equal
setp

b. Serapeum
   
reuse-cons
car+cdr
walk-tree
map-tree
leaf-walk
leaf-map
occurs-if
prune-if
occurs
prune
filter-map
car-safe
cdr-safe
append1
nconc1
prepend
prependf
push-end
push-end-new
memq
assocdr
assocar
assocadr
rassocar
rassocdr
firstn
powerset
efface
pop-assoc
mapcar-into
nthrest
plist-keys
plist-values
intersectionp
stable-set-difference

c. UIOP
   
lexicographic<=
lexicographic<

13. Arrays
  
a. Alexandria
     
array-index (type)
array-length
copy-array

b. Serapeum
   
vref
array-index-row-major
undisplace-array
octet-vector-p
make-octet-vector
octet-vector
octets
unoctets
octet-vector=
ensure-vector
vect
values-vector
pad-start
pad-end
vector-conc-extend
vector=


14. Strings
    
a. Serapeum
   
whitespacep
trim-whitespace
with-string
blankp
collapse-whitespace
concat
mapconcat
string-join
string-upcase-initials
nstring-upcase-initials
same-case-p
nstring-invert-case
string-invert-case
words
tokens
word-wrap
lines
fmt
escape
ellipsize
string^=
string-prefix-p
string$=
string-suffix-p
string*=
string-contains-p
string~=
string-token-p
string-replace-all
string-replace
chomp
string-count
string+

c. UIOP
   
base-string-p
first-char
frob-substrings
last-char
split-string
string-enclosed-p
strings-common-element-type
stripln


15. Sequences
    
a. Alexandria

proper-sequence (type)
deletef
removef
rotate
shuffle
random-elt
emptyp
sequence-of-length-p
length=
copy-sequence
first-elt
last-elt (setfable)
starts-with
starts-with-subseq
ends-with
ends-with-subseq
map-combinations
map-derangements
map-permutations

b. Serapeum
   
sequencep
null-if-empty
do-each
nsubseq
filter
filterf
keep
single
only-elt
partition
partitions
assort
runs
batches
frequencies
scan
nub
gcp
gcs
of-length
length>
length>=
length<
length<=
longer
shorter
longest
shortest
slice
ordering
take
drop
take-while
take-until
drop-while
drop-until
drop-prefix
drop-suffix
ensure-prefix
ensure-suffix
bisect-left
bisect-right
bestn
nth-best
nth-best!
reshuffle
sort-new
stable-sort-new
extrema
halves
dsu-sort
dsu-sort-new
deltas
inconsistant-graph-constraints
toposort
intersperse
mvfold
mvfoldr
repeat-sequence
seq=
do-splits
collapse-duplicates
same
copy-firstn
splice-seq
nsplice-seq
splice-seqf
nsplice-seqf

c. UIOP
   
access-at-count
access-at

 
16. Hash-Tables
    
a. Alexandria

ensure-gethash
copy-hash-table
maphash-keys
maphash-values
hash-table-keys
hash-table-values
hash-table-alist
hash-table-plist
alist-hash-table
plist-hash-table

b. Serapeum
   
do-hash-table
hash-table-test-p
dict
dict*
dictq
href
href-default
@
pophash
swaphash
hash-fold
maphash-return
maphash-new
maphash-into
merge-tables
flip-hash-table
set-hash-table
hash-table-set
hash-table-predicate
hash-table-function
make-hash-table-function
delete-from-hash-table
pairhash
pretty-print-hash-table
toggle-pretty-print-hash-table

17. Filenames
    
a. Serapeum
   
path-basename
path-join
base-path-join
exe
  
b. UIOP
   
absolute-pathname-p
call-with-enough-pathname
denormalize-pathname-directory-component
directorize-pathname-host-device
directory-pathname-p
directory-separator-for-host
enough-pathname
ensure-absolute-pathname
ensure-directory-pathname
ensure-pathname
file-pathname-p
hidden-pathname-p
logical-pathname-p
make-pathname-component-logical
make-pathname-logical
merge-pathname-directory-components
merge-pathnames*
nil-pathname
normalize-pathname-directory-component
parse-unix-namestring
pathname-directory-pathname
pathname-equal
pathname-host-pathname
pathname-parent-directory-pathname
pathname-root
physical-pathname-p
physicalize-pathname
relative-pathname-p
relativize-directory-component
relativize-pathname-directory
split-name-type
split-unix-namestring-directory-components
subpathname*
subpathname
subpathp
translate-pathname*
unix-namestring
wilden
with-enough-pathname
with-pathname-defaults
*nil-pathname* (variable)
*output-translation-function* (variable)
*unspecific-pathname-type* (variable)
*wild-directory* (variable)
*wild-file-for-directory* (variable)
*wild-file* (variable)
*wild-inferiors* (variable)
*wild-path* (variable)
*wild* (variable)
get-pathname-defaults
split-native-pathnames-string
truename*
truenamize
add-pathname-suffix
call-with-staging-pathname
with-staging-pathname

18. Files
    
a. Alexandria

read-file-into-string
read-file-into-byte-vector

b. Serapeum
   
with-open-files
write-stream-into-file
write-file-into-stream
file-size
resolve-executable
format-file-size-human-readable
file-size-human-readable

c. UIOP
   
chdir
getcwd
hostname
parse-file-location-info
parse-windows-shortcut
call-with-current-directory
collect-sub*directories
delete-directory-tree
delete-empty-directory
delete-file-if-exists
directory-exists-p
directory-files
directory*
ensure-all-directories-exist
file-exists-p
filter-logical-directory-results
probe-file*
rename-file-overwriting-target
resolve-symlinks*
resolve-symlinks
safe-file-write-date
subdirectories
with-current-directory
*resolve-symlinks* (variable)
concatenate-files
copy-file

19. Streams
    
a. Alexandria

read-stream-content-into-string
read-stream-content-into-byte-vector

b. UIOP
   
read-little-endian
read-null-terminated-string
always-default-encoding
call-with-input-file
call-with-null-input
call-with-null-output
call-with-output-file
call-with-temporary-file
copy-stream-to-stream
default-encoding-external-format
default-temporary-directory
detect-encoding
encoding-external-format
eval-input
eval-thunk
finish-outputs
input-string
null-device-pathname
output-string
read-file-form
read-file-forms
read-file-line
read-file-lines
read-file-string
safe-read-file-form
safe-read-file-line
safe-read-from-string
setup-temporary-directory
slurp-stream-form
slurp-stream-forms
slurp-stream-line
slurp-stream-lines
slurp-string
standard-eval-thunk
temporary-directory
tmpize-pathname
with-input
with-null-input
with-null-output
with-output
with-safe-io-syntax
with-temporary-file
*default-encoding* (variable)
*default-stream-element-type* (variable)
*encoding-detection-hook* (variable)
*stderr* (variable)
*stdin* (variable)
*stdout* (variable)
*temporary-directory* (variable)
*utf-8-external-format* (variable)
slurp-input-stream
vomit-output-stream

20. Printer
    
a. UIOP

format!  
println
safe-formst!
writeln


21. Reader
    
a. Serapeum
   
read-eval-prefix
with-standard-input-syntax
                         
22. System Construction
  
a. UIOP
   
next-version
parse-version
unparse-version
version-deprecation
version=
version<=
version<
with-deprecation

23. Environment
    
a. Alexandria

featurep

b. Serapeum

static-load-time-value
universal-to-unix-time
unix-to-universal-time
get-unix-time
date-leap-year-p
time-since
time-until
interval

c. UIOP
   
boolean-to-feature-expression
load-uiop-debug-utility
symbol-test-to-feature-expression
uiop-debug
with-upgradability
*uiop-debug-utility* (variable)
architecture
detect-os
featurep
getenv (setfable)
getenvp
implementation-identifier
implementation-type
lisp-version-string
operating-system
os-genera-p
os-macosx-p
os-unix-p
os-windows-p
*implementation-type* (variable)
getenv-absolute-directories
getenv-absolute-directory
getenv-pathname
getenv-pathnames
inter-directory-separator
lisp-implementation-directory
lisp-implementation-pathname-p
native-namestring
parse-native-namestring


A. Macro Writing
   
a. Alexandria

once-only
with-gensyms
with-unique-names
parse-body
parse-ordinary-lambda-list

b. Serapeum
   
string-gensym
unique-name
unsplice
with-thunk
expand-macro
expand-macro-recursively
partition-declarations
define-do-macro
define-post-modify-macro
parse-leading-keywords
with-read-only-vars
define-case-macro
eval-if-constant
expect-form-list
expect-single-form
unparse-ordinary-lambda-list
with-boolean
boolean-if
boolean-when
boolean-unless

B. Threads
   
a. Serapeum
 
count-cpus
synchronized
monitior 
        
C. Queues
   
a. Serapeum
   
queuep
queue
clear-queue
qlen
qlist
enq
deq
undeq
queue-empty-p
front
qback
qconc
qappend
qpreconc
qprepend
copy-queue

D. Box
   
a. Serapeum
   
box
unbox

E. Hooks
   
a. Serapeum
   
add-hook
remove-hook
with-hook-restart
run-hooks
run-hook
run-hook-until-failure
run-hook-until-success

F. Heaps
   
a. Serapeum
   
make-heap
heap-insert
heap-maximum
heap-extract
heap-extract-maximum
heap-extract-all



"
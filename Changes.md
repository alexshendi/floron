* markdown.scm: remove scone macro from markdown.scm. 
  Iteratively add elements to the front of list, 
  then reverse it.
* sxml.scm: Added from chibi-scheme. 
  Add "buffer" object, which is used to
  implement fake string ports. Done because minischeme
  doesn't support ports, only with-input-from-file, 
  with-output-to-file.
* markdown.scm: all i/o is to stdin/stdout or to "buffer"
  objects
* rename "markdown.scm" to "mkdown.scm"
* macros.scm: Added to support "when", "unless" syntax 
  for miniscm
* macros3.scm: Added to support "unless" syntax for PCS/Geneva 
* floron.scm: all i/o is to stdin/stdout
* floron.scm: Add generation of RSS feed 


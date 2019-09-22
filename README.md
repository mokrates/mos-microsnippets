# mos-microsnippets
a little something like yasnippet but much simpler

## Usage
  * Mark your region you want to use as snippet
  * C-= w to copy the region (C-u C-= w to set new jump marker and
    copy the region)
  * C-= y to yank (insert) the region
  * You have to bind this keys to the corresponding functions
    yourself.  I like this bindings, you can, of course, use your own.
    (see source)
  
### Syntax
  There is something I call jump marker, because the insert function
  jumps there.  The default is '@' but you can customize it to
  something different.  Also: C-u C-= w asks for a new jump marker if
  '@' should not be suitable to your snippet.
  
  If you insert "foo @(bar) baz", you will be asked to fill in the
  "bar" blank.
  
  Inserting "@[2] spam and @[3] bacon" yields for the first insert:
  
	  2 spam and 3 bacon
  and for the second insert:
  
	  3 spam and 4 bacon
  and so on.
  
  Copying a new region resets the incrementation counter to zero.
  
  
  

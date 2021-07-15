# Alphabetical Asdf

Gone are the days of having to edit your asd file every time you want
to add a new file to that quick little utility you're making.

This library defines an ASDF system class that lets the source tree
itself determine the load order. You don't need to worry about keeping
the asd file synchronized with the source files. You can move files
around, add new files, rename or delete files without ever having to
touch the asd file.

Please see the file `alphabetical-asdf.lisp` for more information.

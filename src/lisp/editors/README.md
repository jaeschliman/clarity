editors

porting over the initial minimal implementations of a text editor and a structure editor.



will be stripping down the text editor a bit as I go, to keep it more minimal and focused.
(the initial implementation was very early in the project, and has a lot of 'vestigial' functionality)

the structure editor will follow -- note that the structure editor is still very much WIP, and
does not yet handle all common lisp syntax (e.g. #. #() are not yet supported, there is more...)

following these two, intention is to build a more visually dynamic editor for 'object syntax' rather than
textual syntax using lessons from the structure editor.


idea being object syntax will be experimental, and when that fails, fall back to the structure editor,
and when that fails, fall back to the text editor.

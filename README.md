# Win32Forth6v05H

Win32Forth6v05H 2019 Mar 02 - http://www.inventio.co.uk/LegacyIndex.htm

Win32Forth6v05H, the Win32Forth system used to create win32for.exe, as supplied in the Cweed packages.

The ‘H’ is for Howerd, although I have kept as much of the original V6.05 as possible.

This version of Win32Forth runs under Windows 7 and 10 and does not trigger my anti-virus program.

It also includes bug-fixes from Rod Oakford ( including one in the ExtSources kernel that made a zero sized window when running on a new computer )  - thanks!

I have kept the original help file by Tom Zimmer et al. but moved it to load from the local directory without having to have the source tree present.

This means that it works with the Cweed package. I merged changes in the OS detection words from V6.14 so that it recognises Windows 7, 8 and 10.

I have also added some aliases such as  

loc  for  locate , to locate a word’s source,

\\  for  \S  to end compilation of a file

and some new words :

g  to invoke the editor, and  

tt  that,  if you are in directory Cweed,  will load file Cweed.f

Show{   which compiles the text following it until End} like .” , and

zCompile{   which compiles the text following it until End} as a null terminated string.

Useful for helptext.  Type  loc Show{  to view the source code, and  g  to open the editor.

There is a readme.txt file to get you started

#upload, push to github

echo "# Win32Forth6v05H" >> README.md
git init
git add README.md
git commit -m "first commit"
git remote add origin https://github.com/xxxoxxx/Win32Forth6v05H.git
git push -u origin master

# rawtherapee-convert
Small program which uses rawtherapee to convert raw files to jpgs. This is currently UNDER DEVELOPMENT.

Features (will) include:

1. Process a given directory recursively and mirror it's structure under the given output path. That way you
   get the same directory structure for your converted raw files.
2. Override already converted raw files ONLY if there is a pp3 given which contains different settings than
   the ones the existing jpg has been created with. To make this possible the program always saves the pp3
   file together with the jpg file in the output path.
3. The program will use the pp3 given with the viewed raw file to convert it. 
4. If there's a default pp3 given and the viewed raw file has no pp3 file attached the program will use that
   default one to convert the raw file. 
5. If there's no default pp3 given and the viewed raw file has no pp3 file attached the program will use the one
   that is set as default within the rawtherapee context.
6. Log each decided action to stdout so the user knows what's going on.
7. A dry run is possible, doing nothing but logging what would be done.

## Why does (will) this exist?
This is exactly what the author needs for realising a smooth raw conversion process. The files will get
converted to a raspberry pi and distributed within LAN from there. Maybe it's useful for anyone else.

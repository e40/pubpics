$Id$

Table of Contents:

1. Introduction
2. Binary installation
3. Building from source code

===============================================================================
1. Introduction

`pubpics' is a program that builds web pages from digital photos.
That is, it takes as input a bunch of jpg's from a digital camera and
produces what I think are nice looking web pages, with index pages
containing thumbnails.  A `thumbnail' is a very small version of the
picture, just big enough (hopefully) that you can recognize the
contents of the picture.  The original digital pictures are resized
into three sets of pictures, allowing viewers to sequentially or
randomly browse in any size.  The largest images can optionally have a
copyright notice added to them.

This software relies on other software:

- Allegro CL 7.0 Enterprise.  `pubpics' is written in Allegro Common
  Lisp.  It's less than 1000 source lines of code, and does an amazing
  amount for this few lines of source.

  See http://www.franz.com for more information.

- ImageMagick, a set of image manipulation programs.  `pubpics' has
  been tested with version 5.2.7 on Linux and 5.4.9 on Windows.  Other
  later versions probably work, but I'm pretty sure ones earlier than
  5.2.7 have problems.  It has been tested with 6.0.4 on Windows (with
  Cygwin) and 6.2.4 on Linux.

  See http://www.imagemagick.org for more information.

You use pubpics by starting with a directory contains digital pictures
you want to convert into web pages.  In the simplest form, you call
pubpics this way:

    pubpics source-directory destination-directory

After completion, `destination-directory' will contain an index.htm
that can be viewed with any web browser.

Each index page consists of a set of thumbnails.  You can view the
picture "under" the thumbnail by clicking on it.  The index pages
point to picture pages.

Each index page contains navigation links to previous and subsequent
index pages.  Each picture page contains navigation links to previous
and subsquent pictures pages and to the index page containing the
thumbnail for this picture.  Each picture page also contains a
navigation link to the index page of a different size.

Consider a set of web pages made from three digital pictures, a.jpg,
b.jpg and c.jpg.  Think of the pictures as occupying cells of a
spreadsheet:

   small   a.jpg | b.jpg | c.jpg
           ---------------------
   medium  a.jpg | b.jpg | c.jpg
           ---------------------
   large   a.jpg | b.jpg | c.jpg

The `small' row consists of the smallest images, better viewed on a
laptop, for example.  The `medium' row consists of the middle sized
images, and the `large' row contains the largest images.  Each cell is
linked to the cell to the left and/or right, and top and/or bottom.

There are, of course, lots of options.  Here's a description of them:

-a annotations-file
    A file which contains annotations for pictures.  The format of the
    file is an alist of (base-file-name . "annotation").  For example:

    (
    ("20050807-1241-1600-S1IS.JPG" .
     "Adrian at the Santa Cruz boardwalk beach")
    ("20050807-1241-2600-S1IS.JPG" .
     "Adrian wrestling with the seaweed monster (he won)")
    )

    The generated web pages for the above files will then be
    annotated with the given string.

-B image-magick-root
    The directory where ImageMagick binaries can be found.  This is
    most useful on Windows, where there is a Windows convert.exe in
    the Windows\system32 directory.

-c name
    For the largest image size only, add a copyright notice in a
    border of the image.  It will appear like this:

	Copyright (C) <name>, <year>

-d description
-t title
    The very top of each index page is annotated with the `title' and
    `description'.  The title appears first, then the description.
    The title can be something like "Party at Sherry's House" and the
    description could be the date (i.e., "10/16/02").

    The title will also appear on each picture page, but not the
    description.

-n size
    The maximum number of images on each index page will be set to
    `size'.  It is best if it is a multiple of 3, since there are
    three thumbnails per row on each index page.

-p
    Pause after completion.  This is useful when run from a script and
    you want to make sure there are no error messages in the Window
    before it closes.

-V
    Print version info and exit.

-q
    Enter quiet mode.  That is, do not print informative messages that
    tell the progress of creating the destination directory.  Since it
    can take a long time to process 3 sets of images for each picture,
    it can be handy passing the time by watching `pubpics' in
    non-quiet mode.

-r
    Recurse on the source directory.  Normally, only .jpg's in the
    source directory are considered.

`pubpics' will exit with status 0 if it can successfully make the
files in the destination directory.

===============================================================================
2. Binary installation

Copy the appropriate binary distribution:

ftp://ftp.franz.com/pub/examples/pubpics/pubpics-1.31-linux-glibc-2.2.tar.bz2
ftp://ftp.franz.com/pub/examples/pubpics/pubpics-1.31-linux-glibc-2.2.tar.gz
ftp://ftp.franz.com/pub/examples/pubpics/pubpics-1.31-windows.zip

Linux:

  Unpack the .gz or .bz2 file like this:

  % tar zxf pubpics-1.31-linux-glibc-2.2.tar.gz
  or
  % bunzip2 < pubpics-1.31-linux-glibc-2.2.tar.bz2 | tar xf -

  which will result in a ``pubpics-1.31'' sub-directory in the directory
  in which you execute the tar command above.

  You can then move this directory somewhere, say /usr/local/ and put
  /usr/local/pubpics-1.31 in your PATH so the `pubpics' executable is
  readily available.

Windows:

  Unzip the binary and put the files somewhere in your PATH.  The
  file in the ``system-dlls'' sub-directory, msvcrt.dll, will only be
  needed in rare cases--if you get an error message about this DLL
  being missing, copy it into the same directory as pubpics.exe.

Don't forget the ImageMagick installation required by this software.
See http://www.imagemagick.org for more information.

===============================================================================
3. Building from source code

Start up a fully patched Allegro CL 7.0 Enterprise Edition and
evaluate this:

      (load "buildit.cl")

It will make a pubpics/ directory, which is how the pubpics-<version>
directories above were created.

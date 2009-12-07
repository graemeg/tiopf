This is a RTF base reporting unit.

It allows you to report from Objects, Datasets and Custom data.
You can even extend the available functions used inside the report
and add your own functions in there.

This code is exactly as it was for tiOPF v1. Due to many class renames
etc, this code will not compile as-is with tiOPF v2. But it is
very little effort to get it to compile with tiOPF v2.

I currently have a slightly modified version of this code, that I
use with the fpGUI Toolkit (instead of VCL or LCL). It can be view
via the following link with a web browser.

  http://fpgui.git.sourceforge.net/git/gitweb.cgi?p=fpgui/fpgui;a=tree;f=extras/tiopf/gui;h=520ff2a18f1f8776a9ad938829a7e580339fe650;hb=HEAD
  
or you can checkout the code using Git.

  git clone git://fpgui.git.sourceforge.net/gitroot/fpgui/fpgui
 
and then look in the "extras/tiopf/gui" directory.

tiRtfReport (as I call it) is a very capable reporting tool for tiOPF. It
is currently being used in production software at Master Maths. And it
works very nice when used in combination with OpenOffice. You can preview
your report in read-only mode and OpenOffice hides all toolbars, menu items
etc and it looks like a real reporting preview screen.

Cheers,
  - Graeme -
  

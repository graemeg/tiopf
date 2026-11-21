
GUI support for tiOPF with fpGUI Toolkit 2.x
--------------------------------------------

This is the fpGUI 2.x variant of the GUI/fpGUI layer.

fpGUI 2 replaced the old font API (TfpgFont, fpgGetFont and
fpgStyle.DefaultFont) with a cached font resource manager:

  * TfpgFont            -> TfpgFontResourceBase
  * fpgGetFont(desc)    -> fpgApplication.FontManager.GetFont(desc)
  * fpgStyle.DefaultFont -> fpgStyle.GetDefaultFont
  * FontObj.TextWidth   -> FontObj.GetTextWidth
  * FontObj.Height      -> FontObj.GetHeight

It also dropped the FPGui14 era window-handle API, so WindowAllocated and
WindowAttributes can be used unconditionally.

Because the two APIs are incompatible, this layer is a separate copy of the
units rather than a set of $IFDEFs.  Install tiOPFfpGUI2 (see
Compilers/FPC/tiOPFfpGUI2.lpk) when building against fpGUI 2.x, and the
original tiOPFfpGUI package for fpGUI 1.x.  Both packages expose the same
unit names, so do not install them at the same time.

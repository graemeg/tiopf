unit tiGUIConstants;

{$I tiDefines.inc}

interface

uses
  System.UITypes
 ,System.UIConsts
 ;

const
  // RGB color values
  clPaleBlue    = TColor($FEF5E9);
  clError       = claYellow;

  // Default values
  cuiDefaultLabelWidth    = 80;
  cDefaultHeightSingleRow = 24;
  cDefaultHeightMultiRow  = 41;

  // What fonts are we supposed to use by default?
  {$IFDEF MSWINDOWS}
    {$IFDEF GUI_FIXED_FONT}
    cDefaultFixedFontName   = 'Courier New';
    {$ELSE}
    cDefaultFixedFontName   = 'MS Sans Serif';
    {$ENDIF}
  {$ENDIF}

  {$IFDEF LINUX}
    {$IFDEF LCLGTK1}  // GTK1
      {$IFDEF GUI_FIXED_FONT}
      cDefaultFixedFontName   = '-*-fixed-medium-*-normal-*-*-140-*-*-*-*-iso8859-1';
      {$ELSE}
      cDefaultFixedFontName   = '-adobe-helvetica-medium-r-normal-*-*-120-100-100-*-*-iso8859-1';
      {$ENDIF}
    {$ELSE}   // GTK2
      {$IFDEF GUI_FIXED_FONT}
      cDefaultFixedFontName   = 'Monospace 10';
      {$ELSE}
      cDefaultFixedFontName   = 'Sans';
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}

  cCaption = 'Caption';

implementation

end.


{ pascal translation from htmlhelp.h

  © 2000-2003 EC Software. All rights reserved.

  This product and it's source code is protected by copyright laws and
  international copyright treaties, as well as other intellectual property
  laws and treaties. The product is licensed, not sold.

  The source code and sample programs in this package or parts hereof
  as well as the documentation shall not be copied, modified or redistributed
  without permission, explicit or implied, of the author.


  EMail: info@ec-software.com
  Internet: http://www.ec-software.com

  Disclaimer of Warranty
  ----------------------

  THIS SOFTWARE AND THE ACCOMPANYING FILES ARE PROVIDED "AS IS" AND
  WITHOUT WARRANTIES OF ANY KIND WHETHER EXPRESSED OR IMPLIED.

  In no event shall the author be held liable for any damages whatsoever,
  including without limitation, damages for loss of business profits,
  business interruption, loss of business information, or any other loss
  arising from the use or inability to use the software. }

unit ehshhapi;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics {for TColor};

const
  HH_DISPLAY_TOPIC         = $0000;
  HH_HELP_FINDER           = $0000;  // WinHelp equivalent
  HH_DISPLAY_TOC           = $0001;  // not currently implemented
  HH_DISPLAY_INDEX         = $0002;  // not currently implemented
  HH_DISPLAY_SEARCH        = $0003;  // not currently implemented
  HH_SET_WIN_TYPE          = $0004;
  HH_GET_WIN_TYPE          = $0005;
  HH_GET_WIN_HANDLE        = $0006;
  HH_ENUM_INFO_TYPE        = $0007;  // Get Info type name, call repeatedly to enumerate, -1 at end
  HH_SET_INFO_TYPE         = $0008;  // Add Info type to filter.
  HH_SYNC                  = $0009;
  HH_RESERVED1             = $000A;
  HH_RESERVED2             = $000B;
  HH_RESERVED3             = $000C;
  HH_KEYWORD_LOOKUP        = $000D;
  HH_DISPLAY_TEXT_POPUP    = $000E;  // display string resource id or text in a popup window
  HH_HELP_CONTEXT          = $000F;  // display mapped numeric value in dwData
  HH_TP_HELP_CONTEXTMENU   = $0010;  // text popup help, same as WinHelp HELP_CONTEXTMENU
  HH_TP_HELP_WM_HELP       = $0011;  // text popup help, same as WinHelp HELP_WM_HELP
  HH_CLOSE_ALL             = $0012;  // close all windows opened directly or indirectly by the caller
  HH_ALINK_LOOKUP          = $0013;  // ALink version of HH_KEYWORD_LOOKUP
  HH_GET_LAST_ERROR        = $0014;  // not currently implemented // See HHERROR.h
  HH_ENUM_CATEGORY         = $0015;  // Get category name, call repeatedly to enumerate, -1 at end
  HH_ENUM_CATEGORY_IT      = $0016;  // Get category info type members, call repeatedly to enumerate, -1 at end
  HH_RESET_IT_FILTER       = $0017;  // Clear the info type filter of all info types.
  HH_SET_INCLUSIVE_FILTER  = $0018;  // set inclusive filtering method for untyped topics to be included in display
  HH_SET_EXCLUSIVE_FILTER  = $0019;  // set exclusive filtering method for untyped topics to be excluded from display
  HH_INITIALIZE            = $001C;  // Initializes the help system.
  HH_UNINITIALIZE          = $001D;  // Uninitializes the help system.
  HH_PRETRANSLATEMESSAGE   = $00fd;  // Pumps messages. (NULL, NULL, MSG*).
  HH_SET_GLOBAL_PROPERTY   = $00fc;  // Set a global property. (NULL, NULL, HH_GPROP)

type
  THHWIN_PROP = (
         HHWIN_PROP_TAB_AUTOHIDESHOW,// Automatically hide/show tri-pane window
         HHWIN_PROP_ONTOP,         // Top-most window
         HHWIN_PROP_NOTITLEBAR,    // no title bar
         HHWIN_PROP_NODEF_STYLES,  // no default window styles (only HH_WINTYPE.dwStyles)
         HHWIN_PROP_NODEF_EXSTYLES,// no default extended window styles (only HH_WINTYPE.dwExStyles)
         HHWIN_PROP_TRI_PANE,      // use a tri-pane window
         HHWIN_PROP_NOTB_TEXT,     // no text on toolbar buttons
         HHWIN_PROP_POST_QUIT,     // post WM_QUIT message when window closes
         HHWIN_PROP_AUTO_SYNC,     // automatically ssync contents and index
         HHWIN_PROP_TRACKING,      // send tracking notification messages
         HHWIN_PROP_TAB_SEARCH,    // include search tab in navigation pane
         HHWIN_PROP_TAB_HISTORY,   // include history tab in navigation pane
         HHWIN_PROP_TAB_FAVORITES, // include favorites tab in navigation pane
         HHWIN_PROP_CHANGE_TITLE,  // Put current HTML title in title bar
         HHWIN_PROP_NAV_ONLY_WIN,  // Only display the navigation window
         HHWIN_PROP_NO_TOOLBAR,    // Don't display a toolbar
         HHWIN_PROP_MENU,          // Menu
         HHWIN_PROP_TAB_ADVSEARCH, // Advanced FTS UI.
         HHWIN_PROP_USER_POS,      // After initial creation, user controls window size/position
         HHWIN_PROP_TAB_CUSTOM1,   // Use custom tab #1
         HHWIN_PROP_TAB_CUSTOM2,   // Use custom tab #2
         HHWIN_PROP_TAB_CUSTOM3,   // Use custom tab #3
         HHWIN_PROP_TAB_CUSTOM4,   // Use custom tab #4
         HHWIN_PROP_TAB_CUSTOM5,   // Use custom tab #5
         HHWIN_PROP_TAB_CUSTOM6,   // Use custom tab #6
         HHWIN_PROP_TAB_CUSTOM7,   // Use custom tab #7
         HHWIN_PROP_TAB_CUSTOM8,   // Use custom tab #8
         HHWIN_PROP_TAB_CUSTOM9,   // Use custom tab #9
         HHWIN_TB_MARGIN           // the window type has a margin
         );
  THHWIN_PROPS = set of THHWIN_PROP;

  THHWIN_PARAM = (
         HHWIN_PARAM_NOTUSED,
         HHWIN_PARAM_PROPERTIES,   // valid fsWinProperties
         HHWIN_PARAM_STYLES,	   // valid dwStyles
         HHWIN_PARAM_EXSTYLES,	   // valid dwExStyles
         HHWIN_PARAM_RECT,	   // valid rcWindowPos
         HHWIN_PARAM_NAV_WIDTH,	   // valid iNavWidth
         HHWIN_PARAM_SHOWSTATE,	   // valid nShowState
         HHWIN_PARAM_INFOTYPES,	   // valid ainfoTypes
         HHWIN_PARAM_TB_FLAGS,	   // valid fsToolBarFlags
         HHWIN_PARAM_EXPANSION,	   // valid fNotExpanded
         HHWIN_PARAM_TABPOS,	   // valid tabpos
         HHWIN_PARAM_TABORDER,	   // valid taborder
         HHWIN_PARAM_HISTORY_COUNT,// valid cHistory
         HHWIN_PARAM_CUR_TAB       // valid curNavType
         );
  THHWIN_PARAMS = set of THHWIN_PARAM;

  THHWIN_BUTTON = (
         HHWIN_BUTTON_EXPAND,     // Expand/contract button
         HHWIN_BUTTON_BACK,       // Back button
         HHWIN_BUTTON_FORWARD,    // Forward button
         HHWIN_BUTTON_STOP,       // Stop button
         HHWIN_BUTTON_REFRESH,    // Refresh button
         HHWIN_BUTTON_HOME,       // Home button
         HHWIN_BUTTON_BROWSE_FWD, // not implemented
         HHWIN_BUTTON_BROWSE_BCK, // not implemented
         HHWIN_BUTTON_NOTES,      // not implemented
         HHWIN_BUTTON_CONTENTS,   // not implemented
         HHWIN_BUTTON_SYNC,       // Sync button
         HHWIN_BUTTON_OPTIONS,    // Options button
         HHWIN_BUTTON_PRINT,      // Print button
         HHWIN_BUTTON_INDEX,      // not implemented
         HHWIN_BUTTON_SEARCH,     // not implemented
         HHWIN_BUTTON_HISTORY,    // not implemented
         HHWIN_BUTTON_FAVORITES,  // not implemented
         HHWIN_BUTTON_JUMP1,
         HHWIN_BUTTON_JUMP2,
         HHWIN_BUTTON_ZOOM,
         HHWIN_BUTTON_TOC_NEXT,
         HHWIN_BUTTON_TOC_PREV
         );
  THHWIN_BUTTONS = set of THHWIN_BUTTON;

const
  HHWIN_DEF_BUTTONS = [HHWIN_BUTTON_EXPAND,
                       HHWIN_BUTTON_BACK,
                       HHWIN_BUTTON_OPTIONS,
                       HHWIN_BUTTON_PRINT];

// Button IDs

  IDTB_EXPAND            = 200;
  IDTB_CONTRACT          = 201;
  IDTB_STOP              = 202;
  IDTB_REFRESH           = 203;
  IDTB_BACK              = 204;
  IDTB_HOME              = 205;
  IDTB_SYNC              = 206;
  IDTB_PRINT             = 207;
  IDTB_OPTIONS           = 208;
  IDTB_FORWARD           = 209;
  IDTB_NOTES             = 210; // not implemented
  IDTB_BROWSE_FWD        = 211;
  IDTB_BROWSE_BACK       = 212;
  IDTB_CONTENTS          = 213; // not implemented
  IDTB_INDEX             = 214; // not implemented
  IDTB_SEARCH            = 215; // not implemented
  IDTB_HISTORY           = 216; // not implemented
  IDTB_FAVORITES         = 217; // not implemented
  IDTB_JUMP1             = 218;
  IDTB_JUMP2             = 219;
  IDTB_CUSTOMIZE         = 221;
  IDTB_ZOOM              = 222;
  IDTB_TOC_NEXT          = 223;
  IDTB_TOC_PREV          = 224;

// Notification codes

  HHN_FIRST         = -860;
  HHN_LAST          = -879;
  HHN_NAVCOMPLETE   = HHN_FIRST;
  HHN_TRACK         = HHN_FIRST - 1;
  HHN_WINDOW_CREATE = HHN_FIRST - 2;


type
   PHH_AKLINK = ^THH_AKLINK;
   THH_AKLINK = record
     cbStruct: integer;
     fReserved: boolean;
     pszKeywords: pchar;
     pszUrl: pchar;
     pszMsgText: pchar;
     pszMsgTitle: pchar;
     pszMsgWindow: pchar;
     fIndexOnFail: boolean;
   end;

   PHH_POPUP = ^THH_POPUP;
   THH_POPUP = record
     cbStruct: integer;
     hinst: THandle;
     idString: cardinal;
     pszText: pchar;
     pt: TPoint;
     clrForeground: TColor;
     clrBackground: TColor;
     rcMargins: TRect;
     pszFont: pchar;
   end;

   PHH_FTS_QUERY = ^THH_FTS_QUERY;
   THH_FTS_QUERY = record
     cbStruct: integer;
     fUniCodeStrings: boolean;
     pszSearchQuery: pchar;
     iProximity: longint;
     fStemmedSearch: boolean;
     fTitleOnly: boolean;
     fExecute: boolean;
     pszWindow: pchar;
   end;

   PHH_WINTYPE = ^THH_WINTYPE;
   THH_WINTYPE = record
     cbStruct: integer;
     fUniCodeStrings: boolean;
     pszType: pchar;
     fsValidMembers: THHWIN_PARAMS;
     fsWinProperties: THHWIN_PROPS;
     pszCaption: pchar;
     dwStyles: dword;
     dwExStyles: dword;
     rcWindowPos: TRect;
     nShowState: integer;
     hwndHelp: HWND;
     hwndCaller: HWND;
     hwndToolBar: HWND;
     hwndNavigation: HWND;
     hwndHTML: HWND;
     iNavWidth: integer;
     rcHTML: TRect;
     pszToc: pchar;
     pszIndex: pchar;
     pszFile: pchar;
     pszHome: pchar;
     fsToolBarFlags: THHWIN_BUTTONS;
     fNotExpanded: boolean;
     curNavType: integer;
     idNotify: integer;
     pszJump1: pchar;
     pszJump2: pchar;
     pszUrlJump1: pchar;
     pszUrlJump2: pchar;
   end;

   PHHN_NOTIFY = ^THHN_NOTIFY;
   THHN_NOTIFY = record
     hdr: TNMHDR;
     pszUrl: pchar;
   end;

   PHHNTRACK = ^THHNTRACK;
   THHNTRACK = record
     hdr: TNMHDR;
     pszCurUrl: pchar;
     idAction: integer;
     phhWinType: PHH_WINTYPE;
   end;

   PHH_LAST_ERROR = ^THH_LAST_ERROR;
   THH_LAST_ERROR = record
     cbStruct: integer;
     hr: HRESULT;
     description: PWideChar;
   end;

   THtmlHelpA = function(hwndCaller: THandle; pszFile: pchar; uCommand: cardinal; dwData: longint): THandle; stdCall;

var
   HtmlHelpA: THtmlHelpA;
   HHCTRL: THandle;

function  HHLoaded: boolean;
function  LoadHH: boolean;
procedure UnLoadHH;

implementation

function HHLoaded: boolean;
begin
  result := HHCTRL <> 0;
end;

function LoadHH: boolean;
begin
  if HHCTRL = 0 then
  try
    HtmlHelpA := nil;
    HHCTRL := LoadLibrary('HHCTRL.OCX');
    if (HHCTRL <> 0) then HtmlHelpA := GetProcAddress(HHCTRL, 'HtmlHelpA');
  except
{no error here, HTML HELP is not or not properly installed}
  end;
  result := HHCTRL <> 0;
end;

procedure UnLoadHH;
begin
  if HHLoaded then FreeLibrary(HHCTRL);
end;

initialization
  HHCTRL := 0;

finalization
  UnLoadHH;
end.

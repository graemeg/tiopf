{ THelpRouter

  © 2000-2003 EC Software. All rights reserved.

  This product and it's source code is protected by patents, copyright laws and
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

unit ehshelprouter;

interface

{$I ESHHelpWarn.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus;

type
  THelpType = (htWinhelp, htHTMLhelp, htMixedMode);
  TShowType = (stDefault, stMain, stPopup);

  THtmlOption  = (hoSupportPopups, hoMixedMode);
  THtmlOptions = set of THtmlOption;

  TOnFindHelpFileEvent = function(pData: Longint): string of object ;

  THelpRouter = class(TComponent)
  private
    fHelpType: THelpType;
    fShowType: TShowType;
    fAppOnHelp: THelpEvent;
    fOnHelp: THelpEvent;
    fHelpfile: string;
    fCHMPopupTopics: string;
    fValidateID: boolean;
    fPP: TPoint;
    FOnFindHelpFile: TOnFindHelpFileEvent;
    function  CurrentForm: TCustomForm;
    function  OnRouteHelp(Command: Word; Data: Longint; var CallHelp: Boolean): Boolean;
    function  FindHelpfile: string;
    function  FindHandle: HWND;
    procedure SetHelpType(value: THelpType);
    function  ValidateHTMLID(link: string): string;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    HTMLhelpInstalled: boolean;
    function    HelpContent: boolean;
    function    HelpKeyword(keyword: string): boolean;
    function    HelpKLink(keyword: string): boolean;
    function    HelpALink(keyword: string): boolean;
    function    HelpJump(hfile, topicid: string): boolean; overload ;
    function    HelpJump(topicid: string): boolean; overload ;
    function    HelpPopup(X,Y: integer; HelpContext: integer; text: string): boolean;
  published
    property HelpType: THelpType read fHelpType write SetHelpType;
    property ShowType: TShowType read fShowType write fShowType default stDefault;
    property Helpfile: string read fHelpfile write fHelpfile;
    property CHMPopupTopics: string read fCHMPopupTopics write fCHMPopupTopics;
    property OnHelp: THelpEvent read fOnHelp write fOnHelp;
    property ValidateID: boolean read fValidateID write fValidateID;
    property OnFindHelpFile: TOnFindHelpFileEvent read FOnFindHelpFile Write FOnFindHelpFile;
  end;

var
   GLOBAL_HELPROUTER: THelpRouter;

implementation

uses ehshhapi, types {24/10/2006, PH, Fix BDS2006 Warning};

var
   aHH_AKLINK: THH_AKLINK;
   aHH_POPUP: THH_POPUP;
   aKeyWord, aHelpFile: string;

function CheckRouterInstance: boolean;
begin
  if GLOBAL_HELPROUTER <> nil then raise Exception.Create('Multiple instances of THelpRouter are not allowed');
  result := true;
end;

{ --- THelpRouter --- }

constructor THelpRouter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if CheckRouterInstance and not (csDesigning in Componentstate) then
  begin
       fAppOnHelp := Application.onhelp;
       Application.onhelp := OnRouteHelp;
       GLOBAL_HELPROUTER := Self;
  end;
  fShowType := stDefault;
  fCHMPopupTopics := 'CSHelp.txt';
end;

destructor THelpRouter.Destroy;
begin
  if not (csDesigning in Componentstate) then
  begin
       if assigned(fAppOnHelp) then Application.onhelp := fAppOnHelp else Application.onhelp := nil;
  end;
  GLOBAL_HELPROUTER := nil;
  inherited Destroy;
end;

function THelpRouter.CurrentForm: TCustomForm;
begin
  if Screen.ActiveForm <> nil then result := Screen.ActiveForm
  else begin
       if Screen.ActiveCustomForm <> nil
         then result := Screen.ActiveCustomForm
         else result := Owner as TForm;
  end;
end;

procedure THelpRouter.SetHelpType(value: THelpType);
begin
     if value <> fHelpType then
     begin
          fHelpType := value;
          //if (fHelpType in [htHTMLhelp,htMixedMode])
          //  and (not (csDesigning in ComponentState)) then HTMLHelpInstalled;
     end;
end;

function THelpRouter.HTMLhelpInstalled: boolean;
begin
  if HHCTRL = 0 then LoadHH;
  result := assigned(HtmlHelpA);
end;

function THelpRouter.FindHelpfile: string;
var
  CForm: TCustomForm;
begin
   CForm := CurrentForm;
   if fHelpFile <> '' then result := fHelpFile
   else begin
        result := Application.helpfile;
        if Assigned(CForm) and CForm.HandleAllocated and (CForm.HelpFile <> '') then result := CForm.HelpFile;
   end;
end;

function THelpRouter.FindHandle: HWND;
var
  CForm: TCustomForm;
begin
   CForm := CurrentForm;
   result := Application.handle;

 { if Assigned(CForm) and CForm.HandleAllocated and (CForm.HelpFile <> '') then result := CForm.Handle;

   Above is the old handle return function, changed on 06-27-2002. We always prefer the handle
   of the current form over the application handle, even if the current form does not have it's
   own help file. The handle is required for Winhelp only and for HTML HELP text popups. The
   latter do not appear in front of the form if the current form has FormStyle fsStayOnTop unless
   we explicitely specify the form handle. }

   if Assigned(CForm) and CForm.HandleAllocated then result := CForm.Handle;
end;

function THelpRouter.OnRouteHelp(Command: Word; Data: Longint; var CallHelp: Boolean): Boolean;
const
   HELP_TAB = 15;
var
   showHTML: boolean;
   rHandle: integer;
begin
   result := false;
   if assigned(fOnHelp) then result := fOnHelp(command, data, callhelp);
   if not callHelp then exit;
   if assigned(fAppOnHelp) then result := fAppOnHelp(command, data, callhelp);
   if not callHelp then exit;

   if Assigned(FOnFindHelpFile) then
     aHelpFile := FOnFindHelpFile(Data)
   else
     aHelpFile := FindHelpFile;

   if fShowType = stMain then
     case Command of
     HELP_SETPOPUP_POS: Command := 0;
     HELP_CONTEXTPOPUP: Command := HELP_CONTEXT; //no popup
     end;

   showHTML := false;
   case HelpType of
   htHTMLHelp:  showHTML := true;
   htMixedMode: showHTML := (command <> HELP_CONTEXTPOPUP) and
                            (command <> HELP_SETPOPUP_POS);
   end;

   if showHTML then
   begin
     { 07/2003: Delphi sends a HELP_QUIT call when the application terminates. If HHCTRL.OCX is not
       loaded at this point, the "HTMLHelpInstalled" function attempts to load HHCTRL.OCX which is
       not only unneccessary but results in an access violation because the application is already
       terminating. So, if this is the last command "HELP_QUIT" and THelpRouter is set to HTML Help and
       HHCTRL.OCX is yet not loaded, we simply do nothing, just return OK (which is) and exit. }

     if NOT HHLoaded and (Command = HELP_QUIT) then
     begin
          result := true;
          CallHelp := false;
          exit;
     end
     else if not HTMLhelpInstalled then exit; //attempt to load HH and if it failes, return with result false

     rHandle := 0;
     aHelpFile := changefileext(aHelpFile,'.chm');
     if assigned(HtmlHelpA) then
     case Command of
     HELP_TAB:
       case data of
       0: rHandle := HtmlHelpA(0, pchar(aHelpFile), HH_DISPLAY_TOC, 0);  //show table of contents
       else rHandle := HtmlHelpA(0, pchar(aHelpFile), HH_DISPLAY_INDEX, 0);  //display keyword
       end;
     HELP_FINDER,
     HELP_CONTENTS:
       rHandle := HtmlHelpA(0, pchar(aHelpFile), HH_DISPLAY_TOC, 0);  //show table of contents
     HELP_PARTIALKEY,
     HELP_KEY:
       rHandle := HtmlHelpA(0, pchar(aHelpFile), HH_DISPLAY_INDEX, data);  //display keyword
     HELP_QUIT:
       rHandle := HtmlHelpA(0, nil, HH_CLOSE_ALL, 0);
     HELP_SETPOPUP_POS:
       fPP := SmallPointToPoint(TSmallPoint(Data));
     HELP_CONTEXT:
       rHandle := HtmlHelpA(0, pchar(aHelpFile), HH_HELP_CONTEXT, data);  //display help context
     HELP_CONTEXTPOPUP:
       begin
            if (fPP.x = 0) and (fPP.y = 0) then GetCursorPos(fPP);
            HelpPopup(fPP.x, fPP.y, data, '');
            fPP := point(0,0); //reset;
            CallHelp := false;
            result := true;
            exit;
       end;
     end;
     Result := rHandle <> 0;
   end else
   begin
     if Command <> 0 then
     begin
          { If CHMPopupTopics is set the to HLP file that contains the mixed mode
            HLP popups, change the file name here. This property was previously
            used to specify the internal CHM text popups only but is obviously
            misunderstood by many users. Anyway, let's use it this way... 01/22/2003 }

          if (Command = HELP_CONTEXTPOPUP)
            and (HelpType = htMixedMode)
            and (lowercase(extractfileext(CHMPopupTopics)) = '.hlp')
            then
               if extractFilePath(CHMPopupTopics) = ''
                 then aHelpFile := extractFilePath(aHelpFile) + CHMPopupTopics
                 else aHelpFile := CHMPopupTopics;

          Result := WinHelp(findHandle, PChar(changefileext(aHelpFile,'.hlp')), Command, Data);
     end
     else Result := true;
   end;
   CallHelp := false;
end;

function THelpRouter.HelpContent: boolean;
begin
   result := application.helpcommand(15, 0);
end;

function THelpRouter.ValidateHTMLID(link: string): string;
var
   vli: integer;
begin
      result := '';
      for vli := 1 to length(link) do case link[vli] of
      '\',
      '/',
      '"',
      '|',
      ',',
      '?',
      '¿',
      ':': result := result + '';  //nothing
      ' ',
      '.',
      '%': result := result + '_';
      '>',
      '<': result := result + '~';
      '&',
      '*': result := result + '+';
      '[': result := result + '(';
      ']': result := result + ')';
      'ä',
      'Ä': result := result + 'a';
      'ö',
      'Ö': result := result + 'o';
      'ü',
      'Ü': result := result + 'u';
      'ß': result := result + 's';
      else result := result + link[vli];
      end;
end;


function THelpRouter.HelpJump(hfile, topicid: string): boolean;
var
   Command: array[0..255] of Char;
   HID: string;
begin
   result := false;

   if Hfile <> '' then aHelpFile := HFile else aHelpFile := FindHelpFile;

   if HelpType in [htHTMLhelp,htMixedMode] then
   begin
     if not HTMLhelpInstalled then exit; //result false

     aHelpFile := changefileext(aHelpFile,'.chm');
     HID := TopicID;
     if HID <> '' then
     begin
          if copy(lowercase(extractfileext(HID)),1,4) <> '.htm' then
          begin
               if fValidateID then HID := ValidateHTMLID(HID);
               HID := HID + '.htm';
          end;
     end;
     if HID <> '' then
     begin
          aHelpFile := aHelpFile + '::/' + HID;
          result := HtmlHelpA(0, pchar(aHelpFile), HH_DISPLAY_TOPIC, 0) <> 0; //show topic
     end
     else result := HtmlHelpA(0, pchar(aHelpFile), HH_HELP_FINDER, 0) <> 0;  //show table of contents
   end
   else begin
     aHelpfile := changefileext(aHelpFile,'.hlp');
     StrLFmt(Command, SizeOf(Command) - 1, 'JumpID("","%s")', [TopicID]);
     Result := WinHelp(FindHandle, PChar(aHelpfile), HELP_CONTENTS, 0);
     if Result then Result := WinHelp(FindHandle, PChar(aHelpfile), HELP_COMMAND, Longint(@Command));
   end;
end;

function THelpRouter.HelpPopup(X,Y: integer; HelpContext: integer; Text: string): boolean;
begin
   if HTMLhelpInstalled then
   begin
     with aHH_POPUP do
     begin
          cbStruct := sizeof(aHH_POPUP);
          hInst := 0;
          if Text = '' then idString := HelpContext
                       else idString := 0;
          pszText := PChar(text);
          pt.x := X;
          pt.y := Y;
          clrForeground := -1;
          clrBackground := -1;
          rcMargins.Left := -1;
          rcMargins.Right := -1;
          rcMargins.Top := -1;
          rcMargins.Bottom := -1;
          pszFont := PChar('MS Sans Serif, 8');
     end;

     if aHH_POPUP.idString <> 0 then
     begin
          if fCHMPopupTopics = '' then fCHMPopupTopics := 'CSHelp.txt';
          aHelpFile := changefileext(FindHelpFile,'.chm') + '::/'+fCHMPopupTopics;
          result := HtmlHelpA(FindHandle, pchar(aHelpFile), HH_DISPLAY_TEXT_POPUP, longint(@aHH_POPUP)) <> 0;
     end
     else result := HtmlHelpA(FindHandle, nil, HH_DISPLAY_TEXT_POPUP, longint(@aHH_POPUP)) <> 0;
   end
   else result := false;
end;

function THelpRouter.HelpKeyword(keyword: string): boolean;
var
  Command: array[0..255] of Char;
begin
  StrLcopy(Command, pchar(keyword), SizeOf(Command) - 1);
  result := application.helpcommand(HELP_KEY, Longint(@Command));
end;

function THelpRouter.HelpKLink(keyword: string): boolean;
var
   Command: array[0..255] of Char;
begin
   result := false;

   if HelpType in [htHTMLhelp,htMixedMode] then
   begin
     if not HTMLhelpInstalled then exit; //result false

     aHelpFile := changefileext(FindHelpFile,'.chm');
     aKeyWord := keyword+#0;

     with aHH_AKLINK do
     begin
          cbStruct := sizeof(aHH_AKLINK);
          fReserved := false;
          pszKeywords := pchar(aKeyword);
          pszUrl := '';
          pszMsgText := '';
          pszMsgTitle := '';
          pszMsgWindow := '';
          fIndexOnFail := true;
     end;
     result := HtmlHelpA(0, pchar(aHelpfile), HH_DISPLAY_TOPIC, 0) <> 0;  //create window
     if result then result := HtmlHelpA(0, pchar(aHelpfile), HH_KEYWORD_LOOKUP, longint(@aHH_AKLINK)) <> 0;
   end
   else begin
     aHelpFile := changefileext(FindHelpFile,'.hlp');
     StrLFmt(Command, SizeOf(Command) - 1, 'KL("%s",1)', [keyword]);
     Result := WinHelp(FindHandle, PChar(aHelpFile), HELP_CONTENTS, 0);
     if Result then Result := WinHelp(FindHandle, PChar(aHelpFile), HELP_COMMAND, Longint(@Command));
   end;
end;

function THelpRouter.HelpALink(keyword: string): boolean;
var
   Command: array[0..255] of Char;
begin
   result := false;

   if HelpType in [htHTMLhelp,htMixedMode] then
   begin
     if not HTMLhelpInstalled then exit; //result false

     aHelpFile := changefileext(FindHelpFile,'.chm');
     aKeyWord := keyword+#0;

     with aHH_AKLINK do
     begin
          cbStruct := sizeof(aHH_AKLINK);
          fReserved := false;
          pszKeywords := pchar(akeyword);
          pszUrl := '';
          pszMsgText := '';
          pszMsgTitle := '';
          pszMsgWindow := '';
          fIndexOnFail := true;
     end;
     result := HtmlHelpA(0, pchar(aHelpfile), HH_DISPLAY_TOPIC, 0) <> 0;  //create window
     if result then result := HtmlHelpA(0, pchar(aHelpfile), HH_ALINK_LOOKUP, longint(@aHH_AKLINK)) <> 0;
   end
   else begin
     aHelpFile := changefileext(FindHelpFile,'.hlp');
     StrLFmt(Command, SizeOf(Command) - 1, 'AL("%s",1)', [keyword]);
     result := WinHelp(FindHandle, PChar(aHelpFile), HELP_CONTENTS, 0);
     if result then result := WinHelp(FindHandle, PChar(aHelpFile), HELP_COMMAND, Longint(@Command));
   end;
end;

function THelpRouter.HelpJump(topicid: string): boolean;
begin
  result := HelpJump(HelpFile,topicid);
end;

end.

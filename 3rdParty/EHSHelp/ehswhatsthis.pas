{ TWhatsThis

  © 2000-2003 EC Software. All rights reserved.

  This product and it's source code is protected by copyright patents, laws and
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

unit ehswhatsthis;

{.$DEFINE ehsDEBUG}
{$I EHS.INC}
{$I ESHHelpWarn.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus,
  stdctrls, TypInfo, ehsbase, ehscontextmap;

type
  TF1Action           = (goContext, goDefault, goTOC);
  TWhatsThisMode      = (wmIdle, wmWaitPopup, wmWaitWMHelp, wmPermanent);
  TWhatsThisOption    = (wtDirectHelp, wtMenuRightClick, wtNoContextHelp,
                         wtNoContextMenu, wtPopupOnForm, wtInheritFormContext,
                         wtUseTag, wtUseAppKey);
  TWhatsThisOptions   = set of TWhatsThisOption;

  TF1KeyDownEvent     = procedure(Sender: TObject; var CallHelp: boolean) of object;
  TWTHelpContextEvent = procedure(Sender: TObject; HelpItem: TObject; IsMenu: boolean;
                                  var HContext: THelpcontext; x, y: integer) of object;
  TWTPopupEvent       = procedure(Sender: TObject; HelpItem: TObject;
                                  HContext: THelpcontext; X, Y: integer; var DoPopup: boolean) of object;
  TWTHelpEvent        = procedure(Sender: TObject; HelpItem: TObject; IsMenu: boolean;
                                  HContext: THelpcontext; X, Y: integer; var CallHelp: boolean) of object;

  TWTOnGetActiveWindowEvent = function : TWinControl of object ;

{$IFDEF ehsDEBUG}
  TWTDebugEvent       = procedure(Sender: TObject; Debugstr: string) of object;
{$ENDIF}

  TContextInfo = record
    Control: TControl;
    HelpContext: THelpContext;
    X, Y: Integer;
  end;

  TWhatsThis = class(TehsBase)
  private
    fActive,
    IsActive: boolean;
    fWTPopupMenu: TPopupMenu;
    fPopupCaption: string;
    fPopupHelpContext: THelpContext;
    fOptions: TWhatsThisOptions;
    fF1Action: TF1Action;

    fWTMenuItem: TMenuItem;
    fWTToolbarButton: TControl;
    fWTMenuItemClick: TMethod;
    fWTToolbarButtonClick: TMethod;

    fHelpContextMap: THelpContextMap;

    fOnF1KeyDown: TF1KeyDownEvent;
    fOnGetContext: TWTHelpContextEvent;
    fOnHelp: TWTHelpEvent;
    fOnPopup: TWTPopupEvent;
    fOnEnterContextMode: TNotifyEvent;
    fOnLeaveContextMode: TNotifyEvent;

    fMode: TWhatsThisMode;
    {$IFNDEF EHS_D5_UP}
    fTempMenuList: TList;
    {$ENDIF}

    { Repeated help mode , doesn't switch off until [ESC].
      A very unusual UI behaviour and not fully tested.
      fRepeatPermanent is always false. }
    fRepeatPermanent: boolean;

    fDefaultCursor,
    fHelpCursor: HCursor;

    fShiftDown: boolean;
    fMenuEntered: boolean;
    fIsPopupItem: boolean;
    fActiveControl: TControl;
    fActiveMenuItem: TMenuItem;
    fContextInfo: TContextInfo;

    fWindowHook: HWND;
    fOrgWindowHookProc: TFarProc;

    fPrevInstance: TWhatsThis;
    FOnGetActiveWindow: TWTOnGetActiveWindowEvent;

{$IFDEF ehsDEBUG}
    fOnDebug: TWTDebugEvent;
{$ENDIF}
    procedure SetMode(value: TWhatsThisMode);
    procedure SetActive(value: boolean);
    procedure SetPopupCaption(value: string);
    procedure SetPopupHelpContext(value: THelpContext);
    procedure SetWTMenuItem(item: TMenuItem);
    procedure SetWTToolbarButton(control: TControl);
    procedure SetHelpContextMap(map: THelpContextMap);
{   run time procedures  }
    procedure Activate;
    procedure Deactivate;
{   hook procedures  }
    procedure HookWindow(Handle: HWND);
    procedure UnhookWindow;
    procedure HookCallWnd;
    procedure UnhookCallWnd;
    {$IFNDEF EHS_D5_UP}
    procedure FillMenuList;
    {$ENDIF}
    function  FindMenuItem(Hwnd: HWND; wParam, lParam: longint): TMenuItem;
    procedure WindowHookProc(var Message: TMessage);
{   help related procedures  }
    function  ControlFromHandle(HW: HWND): TWinControl;
    function  ControlFromPoint(HW: HWND; Pos: TPoint): TControl;
    function  InvokeHelp(x, y: integer): boolean;
    procedure WhatsThisMenuClick(Sender: TObject);
    procedure DoOnHelp(HelpItem: TObject; IsMenu: boolean; HContext: THelpcontext; X, Y: integer; var CallHelp: boolean);
    procedure UpdateControls(HelpMode: boolean);
    procedure UpdateControlEvents(Activate: boolean);
    function  GetActiveWindow: TWinControl;
{   debug procedures  }
    property  Mode: TWhatsThisMode read fMode write SetMode;
{$IFDEF ehsDEBUG}
    procedure MonitorMessage(msg, wparam, lparam: integer; proc: string);
    procedure Monitor(str: string);
{$ENDIF}
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure WndProc(var Message: TMessage); override;
    procedure ContextHelp;
    procedure CancelContextHelp;
    property  OnGetActiveWindow : TWTOnGetActiveWindowEvent read FOnGetActiveWindow write FOnGetActiveWindow;
  published
    property Active: boolean read fActive write SetActive;
    property HelpContextMap: THelpContextMap read fHelpContextMap write SetHelpContextMap;
    property F1Action: TF1Action read fF1Action write fF1Action;
    property Options: TWhatsThisOptions read fOptions write fOptions;
    property PopupCaption: string read fPopupCaption write SetPopupCaption;
    property PopupHelpContext: THelpContext read fPopupHelpContext write SetPopupHelpContext;

    property WTMenuItem: TMenuItem read fWTMenuItem write SetWTMenuItem;
    property WTToolbarButton: TControl read fWTToolbarButton write SetWTToolbarButton;

    property OnEnterContextMode: TNotifyEvent read fOnEnterContextMode write fOnEnterContextMode;
    property OnLeaveContextMode: TNotifyEvent read fOnLeaveContextMode write fOnLeaveContextMode;
    property OnGetContext: TWTHelpContextEvent read fOnGetContext write fOnGetContext;
    property OnHelp: TWTHelpEvent read fOnHelp write fOnHelp;
    property OnPopup: TWTPopupEvent read fOnPopup write fOnPopup;
    property OnF1KeyDown: TF1KeyDownEvent read fOnF1KeyDown write fOnF1KeyDown;
{$IFDEF ehsDEBUG}
    property OnDebug: TWTDebugEvent read fOnDebug write fOnDebug;
{$ENDIF}
  end;

  function  Mouse_Proc(nCode: Integer; wP: Longint; lP: Longint): Longint; stdcall;
  function  Keyboard_Proc(nCode: Integer; wP: Longint; lP: Longint): Longint; stdcall;
  procedure HelpItemClick(Sender: TObject);

implementation

uses
  Types
  ;

const
  WM_SHOW_WTPOPUPMENU = WM_USER + 100;
  WM_SHOW_WTHELP = WM_USER + 101;
  WM_WTUNHOOK = WM_USER + 102;

var
  Hook_Keyboard: HHook;    //permanent Keyboard hook
  Hook_Mouse: HHook;       //permanent Mouse hook
  Hook_CallWnd: HHook;     //temporary CallWndProc hook
  WTInstance: TWhatsThis;  //self instance

//-------

{$IFDEF ehsDEBUG}
procedure TWhatsThis.Monitor(str: string);
begin
     if assigned(fOnDebug) then fOnDebug(self, str);
end;

procedure TWhatsThis.MonitorMessage(msg, wparam, lparam: integer; proc: string);
begin
     case msg of
     WM_SHOW_WTPOPUPMENU: monitor('WM_SHOW_WTPOPUPMENU in '+proc);
     WM_DESTROY:  monitor('WM_DESTROY in '+proc);
     WM_ACTIVATE: if loword(wparam) = WA_INACTIVE then monitor('DEACTIVATE in '+proc+ ' - '+inttostr(HIWORD(wParam))+','+inttostr(lparam))
                  else monitor('ACTIVATE in '+proc+ ' - '+inttostr(HIWORD(wParam))+','+inttostr(lparam));
     WM_MOUSEACTIVATE: monitor('WM_MOUSEACTIVATE '+inttostr(wparam) + ' ' + inttostr(lparam) + ' in ' +proc);
     WM_INITMENU: monitor('WM_INITMENU in '+proc);
     WM_INITMENUPOPUP: monitor('WM_INITMENUPOPUP in '+proc);
     WM_ENTERMENULOOP: monitor('WM_ENTERMENULOOP in '+proc);
     WM_EXITMENULOOP: monitor('WM_EXITMENULOOP in '+proc);
     WM_MENUSELECT: monitor('WM_MENUSELECT '+inttostr(wparam) + ' ' + inttostr(lparam) + ' in ' +proc);
     //WM_NCHITTEST: monitor('WM_NCHITTEST in '+proc);
     //WM_MOUSEMOVE: monitor('WM_MOUSEMOVE in '+proc + ' - ' + inttostr(loword(lparam)) +','+ inttostr(hiword(lparam)));
     //WM_SETCURSOR: if HIWORD(lParam) = WM_MOUSEMOVE then monitor('WM_SETCURSOR in '+proc+': '+inttostr(LOWORD(lParam)));
     WM_CONTEXTMENU: monitor('WM_CONTEXTMENU in '+proc + ' ' + inttostr(wparam));
     WM_HELP:        monitor('WM_HELP in '+proc);
     WM_SYSCOMMAND: if wparam = SC_CONTEXTHELP then monitor('WM_SYSCOMMAND (SC_CONTEXTHELP) in '+proc);
     WM_LBUTTONDOWN: monitor('WM_LBUTTONDOWN in '+proc);
     WM_LBUTTONUP:   monitor('WM_LBUTTONUP in '+proc);
     WM_NCLBUTTONDOWN: monitor('WM_NCLBUTTONDOWN in '+proc);
     WM_NCLBUTTONUP:   monitor('WM_NCLBUTTONUP in '+proc);
     WM_RBUTTONUP:  monitor('WM_RBUTTONUP in '+proc + ' - ' + inttostr(loword(lparam)) +','+ inttostr(hiword(lparam)));
     end;
end;
{$ENDIF}

constructor TWhatsThis.Create(AOwner: TComponent);
begin
     inherited;
     fPopupCaption := 'What''s this?';
     fOptions := [wtNoContextHelp, wtNoContextMenu, wtInheritFormContext, wtMenuRightClick];
     fF1Action := goContext;
     fPrevInstance := nil;
     fActive := true;
     IsActive := false;
     fHelpCursor := LoadCursor(0, IDC_HELP);
     fDefaultCursor := LoadCursor(0, IDC_ARROW);
     fMode := wmIdle;
     fRepeatPermanent := false;
end;

destructor TWhatsThis.Destroy;
begin
     if (not (csDesigning in ComponentState)) then
     begin
          if IsActive then Deactivate;
          if fWTPopupMenu <> nil then fWTPopupMenu.free;
          {$IFNDEF EHS_D5_UP}
          if fTempMenuList <> nil then fTempMenuList.free;
          {$ENDIF}

          try
            WTInstance := fPrevInstance;
            if assigned(fPrevInstance) and fPrevInstance.Active then
               fPrevInstance.Activate;
          except
          end;
     end;

     inherited;
end;

procedure TWhatsThis.SetActive(value: boolean);
begin
     if fActive <> value then
     begin
          fActive := value;
          if (fActive <> IsActive) and (not (csDesigning in ComponentState)) then
          begin
               if value then Activate
               else Deactivate;
          end;
     end;
end;

procedure TWhatsThis.SetHelpContextMap(map: THelpContextMap);
begin
     fHelpContextMap := map;
end;

procedure TWhatsThis.SetWTMenuItem(item: TMenuItem);
begin
     if IsActive then raise Exception.create('TWhatsThis must be deactivated to change this property');
     if fWTMenuItem <> nil then fWTMenuItem.shortCut := 0;
     fWTMenuItem := item;
     if (item <> nil) then item.shortcut := 8304;
end;

procedure TWhatsThis.SetWTToolbarButton(control: TControl);
begin
     if IsActive then raise Exception.create('TWhatsThis must be deactivated to change this property');
     fWTToolbarButton := control;
end;

procedure TWhatsThis.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  case Operation of
  opRemove:
    begin
       if (AComponent = fWTMenuItem)      then fWTMenuItem := nil;
       if (AComponent = fWTToolbarButton) then fWTToolbarButton := nil;
       if (AComponent = fHelpContextMap)  then fHelpContextMap := nil;
    end;
  opInsert:
    begin
       if (AComponent is THelpContextMap) and (fHelpContextMap = nil) then
          fHelpContextMap := THelpContextMap(AComponent);
    end;
  end;
end;

procedure TWhatsThis.SetMode(value: TWhatsThisMode);
begin
     fMode := value;
{$IFDEF ehsDEBUG}
     case fmode of
     wmIdle: monitor('Mode = wmIdle');
     wmWaitWMHelp: monitor('Mode = wmWaitWMHelp');
     wmWaitPopup: monitor('Mode = wmWaitPopup');
     wmPermanent: monitor('Mode = wmPermanent');
     end;
{$ENDIF}
end;

procedure TWhatsThis.Loaded;
var
   NewItem: TMenuItem;
begin
     inherited;
     if (not (csDesigning in ComponentState)) then
     try
          {$IFNDEF EHS_D5_UP}
          fTempMenuList := TList.create;
          {$ENDIF}
          fWTPopupMenu := TPopupMenu.create(self);
          NewItem := TMenuItem.create(self);
          NewItem.Caption := fPopupCaption;
          NewItem.HelpContext := fPopupHelpContext;
          NewItem.OnClick := WhatsThisMenuClick;
          fWTPopupMenu.items.add(NewItem);
          if fActive then Activate;
     except
     end;
end;

procedure TWhatsThis.Activate;
begin
     if WTInstance <> nil then
     begin   //we are the second instance and take over
          if WTInstance.IsActive then WTInstance.Deactivate;
          fPrevInstance := WTInstance;
     end;

     if Handle <> 0 then
     try
          fWindowHook := 0;
          Mode := wmIdle;
          fMenuEntered := false;
          WTInstance := self;

          Hook_Mouse := SetWindowsHookEx(WH_MOUSE, TFNHookProc(@Mouse_Proc), 0, GetCurrentThreadID);
          Hook_Keyboard := SetWindowsHookEx(WH_KEYBOARD, TFNHookProc(@Keyboard_Proc), 0, GetCurrentThreadID);

          UpdateControlEvents(true);
          IsActive := true;
     except
     end;
end;

procedure TWhatsThis.Deactivate;
begin
     try
          UnhookCallWnd;
          UnhookWindow;
          if Mode = wmPermanent then CancelContextHelp;
          UpdateControlEvents(false);

          if Hook_Keyboard <> 0 then UnhookWindowsHookEx(Hook_Keyboard);
          Hook_Keyboard := 0;
          if Hook_Mouse <> 0 then UnhookWindowsHookEx(Hook_Mouse);
          Hook_Mouse := 0;
          IsActive := false;
     except
     end;
end;

procedure TWhatsThis.WndProc(var Message: TMessage);
begin
     with Message do case msg of
     WM_WTUNHOOK:
       begin
            UnhookCallWnd;
       end;
     WM_SHOW_WTPOPUPMENU:
       begin
            if Mode = wmWaitPopup then InvokeHelp(wparam, lparam);
            if Mode <> wmPermanent then Mode := wmIdle;
            if not fMenuEntered then UnhookCallWnd;
       end;
     WM_SHOW_WTHELP:
       begin
            InvokeHelp(wparam, lparam);
       end;
     end;
     Inherited WndProc(Message);
end;

procedure HelpItemClick(Sender: TObject);
begin
     if WTInstance <> nil then with WTInstance do
     begin
          if IsActive then
             case Mode of
             wmIdle:      ContextHelp;
             wmPermanent: CancelContextHelp;
             end;
     end;
end;

function Keyboard_Proc(nCode: Integer; wP: Longint; lP: Longint): Longint; stdcall;
const
   HELP_TAB = 15;
var
   P: TPoint;
   Win: TWinControl;
   CallHelp: boolean;
begin
  result := 0;
  if (nCode = HC_ACTION) then with WTInstance do
  try
     if not boolean(lP shr 31) then  //key down
     begin
          if not boolean(lP shr 30) then  //no repeated keypress
          begin
              if Mode = wmPermanent then CancelContextHelp;

              case wP of
              16:  fShiftDown := true;
              VK_F1: begin
                        Mode := wmIdle;
                        Win := GetActiveWindow;

                        if (Win <> nil) or (fF1Action <> goContext) then
                        begin
                           fActiveControl := Screen.activeControl;
                           if not fMenuEntered then HookCallWnd;

                           CallHelp := true;
                           if (not fShiftDown) and assigned(fOnF1KeyDown) then fOnF1KeyDown(WTInstance, CallHelp);

                           if CallHelp then
                           begin
                              if fShiftDown then ContextHelp
                              else begin
                                 if fMenuEntered or (fF1Action = goContext) then
                                 begin
                                      GetCursorPos(P);
                                      if (not fMenuEntered) and (fActiveControl <> nil) then
                                           P := fActiveControl.ClientToScreen(Point(0,0));
                                      InvokeHelp(P.x, P.y);
                                 end
                                 else case fF1Action of
                                 goDefault: Application.HelpCommand(HELP_CONTENTS, 0);
                                 goTOC:     Application.HelpCommand(HELP_TAB, 0);
                                 end;
                              end;
                           end;
                           if not fMenuEntered then PostMessage(Handle, WM_WTUNHOOK, 0, 0);
                           result := 1; //don't process key
                        end;
                        //else ignore key if it isn't a Delphi form
                     end;
              end;
          end; //repeat count
     end
     else begin
         case wP of
         16:  fShiftDown := false;
         {93 = Application key on extended keyboards, changed with v1.10: the
          application key is now trapped, when it is RELEASED (see below), not
          when it is pressed, accordingly to the behaviour of W2000 and Delphi 4
          and 5. Delphi 3, on the contrary, displays a control's context menu,
          when the Application button is pressed.}
         93:  if (not fMenuEntered) and (wtUseAppKey in fOptions) then
              begin
                   Win := GetActiveWindow;
                   if (Win <> nil) then
                   begin
                        fActiveControl := Screen.ActiveControl;
                        with fActiveControl do P := ClientToScreen(Point(width div 2, height div 2));
                        Mode := wmWaitPopup;
                        HookCallWnd;
                        PostMessage(WTInstance.handle, WM_SHOW_WTPOPUPMENU, P.x, P.y);
                   end;
                   //else ignore key if it isn't a Delphi form
              end;
              //else ignore key if we are in a menu
         end;
     end;
  except
  end;
  if (nCode < 0) or (result = 0) then result := CallNextHookEx(Hook_Keyboard, nCode, wP, lP);
end;

function Mouse_Proc(nCode: Integer; wP: Longint; lP: Longint): Longint; stdcall;
var
   Cur: hcursor;
begin
  result := 0;
  if (nCode = HC_ACTION) then with WTInstance, PMOUSEHOOKSTRUCT(lP)^ do
  try
{$IFDEF ehsDEBUG}
      WTInstance.MonitorMessage(wP, 0, 0, 'WT Mouse_Proc');
{$ENDIF}
      case wP of
       WM_RBUTTONDOWN:
         begin
         end;

       WM_RBUTTONUP:
         case Mode of
         wmIdle,
         wmPermanent:
           begin
             if (not fMenuEntered) then
             begin
                 fActiveControl := ControlFromPoint(HWND, pt);
                 if (fActiveControl <> nil) then
                 begin
                   if Mode <> wmPermanent then Mode := wmWaitPopup
                   else if (fActiveControl = fWTToolbarButton) then
                   begin
                        CancelContextHelp;
                        Mode := wmWaitPopup;
                   end;
                   HookCallWnd;
                   PostMessage(Handle, WM_SHOW_WTPOPUPMENU, pt.x, pt.y);
                 end;
             end
             else if ((wtMenuRightClick in fOptions) or (Mode = wmPermanent))
               and (fActiveMenuItem <> nil) then
             begin
                 if Mode = wmPermanent then CancelContextHelp;
                 PostMessage(WTInstance.handle, WM_SHOW_WTHELP, Pt.x, Pt.y);
                 Mode := wmIdle;
                 result := 1; //discard message
                 {Unhook is done when the menu closes due to any reason. It could be that
                  InvokeHelp does nothing and therefore the menu remains active}
             end;
           end;
         end;  //WM_RBUTTONUP

       WM_LBUTTONDOWN:
         if (not fMenuEntered) then
         case Mode of
         wmIdle:
           begin
               HookCallWnd;
           end;
         wmWaitWMHELP:
           if Hook_CallWnd <> 0 then
           begin
               fActiveControl := ControlFromPoint(HWND, pt);
               if (fActiveControl <> nil) then
               begin
                   PostMessage(WTInstance.handle, WM_SHOW_WTHELP, Pt.x, Pt.y);
                   PostMessage(Handle, WM_WTUNHOOK, 0, 0);
               end;
               Mode := wmIdle;
               result := 1; //discard message
           end;
         wmPermanent:
           begin
               fActiveControl := ControlFromPoint(HWND, pt);
               if (fWTToolbarButton = nil) or (fWTToolbarButton <> fActiveControl) then
               begin
                    if (fActiveControl <> nil) then PostMessage(WTInstance.handle, WM_SHOW_WTHELP, Pt.x, Pt.y);
                    if not fRepeatPermanent then CancelContextHelp;
                    result := 1; //discard message
               end;
           end;
         end;  //WM_LBUTTONDOWN

       WM_LBUTTONUP:
         case Mode of
         wmIdle:
           if not fMenuEntered then UnhookCallWnd;
         wmPermanent:
           if fMenuEntered then
           begin
              if (fWTMenuItem = nil) or (fWTMenuItem <> fActiveMenuItem) then
              begin
                   if fIsPopupItem then
                   begin
                        CancelContextHelp;
                        postMessage(WTInstance.handle, WM_SHOW_WTHELP, Pt.x, Pt.y);
                        result := 1;
                   end;
              end;
           end
           else begin
              UnhookCallWnd;
           end;
         end;  //WM_LBUTTONUP

       WM_NCLBUTTONDOWN:
         begin
           case wHitTestCode of
           HTMENU:
             begin
                  if FindControl(HWND) <> nil then HookCallWnd;
             end;
           HTHELP:
             begin
                  if Mode = wmPermanent then CancelContextHelp;
                  if FindControl(HWND) <> nil then HookCallWnd;
                  Mode := wmWaitWMHelp;
             end;
           else
             if Mode = wmPermanent then CancelContextHelp;
           end;
         end;  //WM_NCLBUTTONDOWN

       WM_NCLBUTTONUP:
         begin
              if (not fMenuEntered) then UnhookCallWnd;
         end;

       WM_MOUSEMOVE,
       WM_NCMOUSEMOVE:
         case Mode of
         wmPermanent:
           with PMOUSEHOOKSTRUCT(lP)^ do
           begin
              cur := fDefaultCursor;
              case wHitTestCode of
              HTBORDER,
              HTCLIENT,
              HTMENU:   begin
                           cur := fHelpCursor;
                           if not fMenuEntered then
                           begin
                                if ControlFromHandle(HWND) <> nil then result := 1
                                else result := 0;
                           end;
                        end;
              end;
              SetCursor(cur);
           end;
         end;  //WM_MOUSEMOVE, WM_NCMOUSEMOVE

      end;  //case wP of
  except
  end;
  if (nCode < 0) or (result = 0) then result := CallNextHookEx(Hook_Mouse, nCode, wP, lP);
end;

function CallWnd_Proc(nCode: Integer; wP: longint; lP: longint): Longint; stdcall;
begin
  result := 0;
  if (nCode = HC_ACTION) then with WTInstance, PCWPStruct(LP)^ do
  try
{$IFDEF ehsDEBUG}
     WTInstance.MonitorMessage(message, wparam, lparam, 'WT CallWnd_Proc');
{$ENDIF}
     case message of
     WM_ACTIVATE:      begin
                         if Mode = wmWaitPopup then Mode := wmIdle;
                       end;
     WM_ENTERMENULOOP: begin
                         if Mode = wmWaitPopup then Mode := wmIdle;
                         fMenuEntered := true;
                         fActiveMenuItem := nil;
                         {$IFNDEF EHS_D5_UP}
                         FillMenuList;
                         {$ENDIF}
                       end;
     WM_EXITMENULOOP:  begin
                         fMenuEntered := false;
                         {$IFNDEF EHS_D5_UP}
                         Postmessage(Handle, WM_WTUNHOOK, 0, 0);
                         {$ENDIF}
                       end;
     WM_INITMENUPOPUP: begin
                       end;
     WM_MENUSELECT:    begin
                         fActiveMenuItem := FindMenuItem(HWND, wParam, lParam);
                         if fActiveMenuItem = nil then
                         begin
                              if lParam = abs(fWTPopupMenu.handle) then
                                 fActiveMenuItem := fWTPopupMenu.items[0];
                         end;
{$IFDEF ehsDEBUG}
                         if fActiveMenuitem <> nil then
                         begin
                              if fIsPopupItem then monitor('Selected popup: '+fActivemenuitem.caption)
                              else monitor('Selected: '+fActivemenuitem.caption);
                         end;
{$ENDIF}
                       end;
     WM_HELP:          begin                  // We cannot discard WM_HELP in the hook procedure
                         HookWindow(HWND);    // So we subclass the window it is for and trap it there
                       end;
     end;
  except
  end;
  if (nCode < 0) or (result = 0) then result := CallNextHookEx(Hook_CallWnd, nCode, wP, lP);
end;

function TWhatsThis.FindMenuItem(Hwnd: HWND; wParam, lParam: longint): TMenuItem;
var
   FoundItem: TMenuItem;
   I: Integer;
   FindKind: TFindItemKind;

   MenuFlag, IDItem, HMenu: integer;
   AForm: TWinControl;
begin
     result := nil;    //result is _never_ undefined. This is just to get rid of the annoying warning
     FoundItem := nil;
     IDItem   := loword(dword(wParam));
     MenuFlag := hiword(dword(wParam));
     hMenu    := lParam;
     fIsPopupItem := true;
     if (MenuFlag = $FFFF) and (hMenu = 0) then exit; //menu closed

     if (MenuFlag <> $FFFF) or (IDItem <> 0) then AForm := Findcontrol(hwnd)
     else AForm := nil;

     if (AForm <> nil) and (AForm is TForm) and (TForm(AForm).Menu <> nil) then
     begin
          FindKind := fkCommand;
          if MenuFlag and MF_POPUP <> 0 then
          begin
               FindKind := fkHandle;
               IDItem := GetSubMenu(hMenu, IDItem);
               fIsPopupItem := false;
          end;
          FoundItem := TForm(AForm).Menu.FindItem(IDItem, FindKind);
     end
     else begin
          FindKind := fkCommand;
          if MenuFlag and MF_POPUP <> 0 then FindKind := fkHandle;

          {$IFDEF EHS_D5_UP}
          { D5 and later allows direct access to PopupList in menus.pas!
            We don't need our custom fTempMenuList anymore. }
          for I := 0 to PopupList.Count-1 do
          begin
               FoundItem := TPopupMenu(PopupList[I]).FindItem(IDItem, FindKind);
               if FoundItem <> nil then break;
          end;
          {$ELSE}
          for I := 0 to fTempMenuList.Count-1 do
          begin
               FoundItem := TPopupMenu(fTempMenuList[I]).FindItem(IDItem, FindKind);
               if FoundItem <> nil then break;
          end;
          {$ENDIF}
     end;
     result := FoundItem;
end;

{$IFNDEF EHS_D5_UP}
procedure TWhatsThis.FillMenuList;
var
   I, L: integer;
begin
     fTempMenuList.clear;
     for I := 0 to Screen.CustomFormCount-1 do
       for L := 0 to Screen.CustomForms[i].ComponentCount-1 do
         if Screen.CustomForms[i].Components[l] is TPopupMenu
           then fTempMenuList.add( Screen.CustomForms[i].Components[l] );

     for I := 0 to Screen.DataModuleCount-1 do
       for L := 0 to Screen.DataModules[i].ComponentCount-1 do
         if Screen.DataModules[i].Components[l] is TPopupMenu
           then fTempMenuList.add( Screen.DataModules[i].Components[l] );
end;
{$ENDIF}

procedure TWhatsThis.WindowHookProc(var Message: TMessage);
var
   DoUnhook: boolean;
begin
{$IFDEF ehsDEBUG}
     MonitorMessage(message.msg, message.wparam, message.lparam, 'WT-WindowHookProc');
{$ENDIF}
     DoUnhook := false;

     with message do case msg of
     WM_DESTROY: DoUnhook := true;
     WM_HELP: begin
                   {$IFDEF ehsDEBUG}
                   monitor('WM_HELP in WindowHookProc');
                   {$ENDIF}
                   UnhookWindow; //trap wm_help and exit;
                   exit;   //no inherited
              end;
     end;

     with message do result := CallWindowProc(fOrgWindowHookProc, fWindowHook, Msg, wParam, lParam);
     if DoUnhook then UnhookWindow;
end;

function TWhatsThis.ControlFromHandle(HW: HWND): TWinControl;
var
   W: HWND;
begin
     result := FindControl(HW);
     if (result = nil)
       and (HW <> 0)
       and (GetWindowThreadProcessId(HW, nil) = GetCurrentThreadId)
       and (Screen.Activeform <> nil)
       and (IsChild(Screen.Activeform.handle, HW) or (Screen.Activeform.handle = HW)) then
     begin
          W := HW;
          while (result = nil) and (W <> 0) do
          begin
               result := FindControl(W);
               if (result = nil) then W := GetParent(W);
          end;
     end;
end;

function TWhatsThis.ControlFromPoint(HW: HWND; Pos: TPoint): TControl;
var
   W: HWND;
   Win: TWinControl;
   Ctl: TControl;
   I: Integer;
   P: TPoint;
begin
     result := nil;
     if ControlFromHandle(HW) <> nil then
     begin
       W := WindowFromPoint(Pos);
       if (W <> 0) and (GetWindowThreadProcessId(W, nil) = GetCurrentThreadId) then
       begin
          Win := nil;
          while (Win = nil) and (W <> 0) do
          begin
               Win := FindControl(W);
               if (Win = nil) then W := GetParent(W);
          end;

          Result := Win;
          if assigned(Win) then
          repeat
               P := Win.ScreenToClient(Pos);
               Ctl := nil;
               for I := Win.Controlcount-1 downto 0 do
               begin
                  if Win.Controls[i].Visible and PtInRect(Win.Controls[i].BoundsRect, P) then
                  begin
                       Ctl := Win.Controls[I];
                       break;
                  end;
               end;
               if Ctl <> nil then
               begin
                    Result := Ctl;
                    if Ctl is TWinControl then Win := TWinControl(Ctl)
                    else break;
               end;
          until (Ctl = nil);
       end;
     end;
{$IFDEF ehsDEBUG}
     if result = nil then monitor('ControlFromPoint = NIL')
                     else monitor('ControlFromPoint = '+result.name);
{$ENDIF}
end;

procedure TWhatsThis.HookWindow(Handle: HWND);
begin
     if fWindowHook <> 0 then UnhookWindow;
     if (Handle <> 0) then
     begin
          fWindowHook := Handle;
          {$IFDEF EHS_D6_UP}
          fOrgWindowHookProc:= Pointer(SetWindowLong(fWindowHook, GWL_WNDPROC, Longint( Classes.MakeObjectInstance(WindowHookProc) )));
          {$ELSE}
          fOrgWindowHookProc:= Pointer(SetWindowLong(fWindowHook, GWL_WNDPROC, Longint( MakeObjectInstance(WindowHookProc) )));
          {$ENDIF}

          {$IFDEF ehsDEBUG}
          monitor('Window '+inttostr(fWindowHook)+' hooked');
          {$ENDIF}
     end;
end;

procedure TWhatsThis.UnhookWindow;
var
  CurrentWndProc: Pointer;
begin
     if fWindowHook <> 0 then
     begin
          CurrentWndProc := Pointer(GetWindowLong(fWindowHook, GWL_WNDPROC));
          SetWindowLong(fWindowHook, GWL_WNDPROC, Longint(fOrgWindowHookProc));
          {$IFDEF EHS_D6_UP}
          Classes.FreeObjectInstance(CurrentWndProc);
          {$ELSE}
          FreeObjectInstance(CurrentWndProc);
          {$ENDIF}
          fWindowHook := 0;

          {$IFDEF ehsDEBUG}
          monitor('Window hook removed');
          {$ENDIF}
     end;
end;

procedure TWhatsThis.HookCallWnd;
begin
  if Hook_CallWnd = 0 then Hook_CallWnd := SetWindowsHookEx(WH_CALLWNDPROC, TFNHookProc(@CallWnd_Proc), 0, GetCurrentThreadID);
{$IFDEF ehsDEBUG}
  monitor('CallWnd hook set');
{$ENDIF}
end;

procedure TWhatsThis.UnhookCallWnd;
begin
  if Hook_CallWnd <> 0 then UnhookWindowsHookEx(Hook_CallWnd);
  Hook_CallWnd := 0;
{$IFDEF ehsDEBUG}
  monitor('CallWnd hook removed');
{$ENDIF}
end;

procedure TWhatsThis.ContextHelp;
begin
{$IFDEF ehsDEBUG}
  monitor('Contexthelp mode entered');
{$ENDIF}
  if not IsActive then raise Exception.create(self.name + ' is currently not active.');
  Mode := wmPermanent;
  SetCursor(fHelpCursor);
  UpdateControls(true);
  if assigned(fOnEnterContextMode) then fOnEnterContextMode(self);
end;

procedure TWhatsThis.CancelContextHelp;
begin
{$IFDEF ehsDEBUG}
  monitor('Contexthelp mode left');
{$ENDIF}
  if not IsActive then raise Exception.create(self.name + ' is currently not active.');
  Mode := wmIdle;
  SetCursor(fDefaultCursor);
  screen.cursor := crDefault;
  UpdateControls(false);
  if assigned(fOnLeaveContextMode) then fOnLeaveContextMode(self);
end;

procedure TWhatsThis.UpdateControls(HelpMode: boolean);
var
   PropInfo: PPropInfo;
begin
  if assigned(fWTMenuItem) then fWTMenuItem.checked := HelpMode;

  if assigned(fWTToolbarButton) then
  begin
       PropInfo := GetPropInfo(fWTToolbarButton.classinfo, 'Down');
       if (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkEnumeration) then
       try
          SetOrdProp(fWTToolbarButton, PropInfo, byte(HelpMode));
          fWTToolbarButton.invalidate; 
       except
       end;
  end;
end;

procedure TWhatsThis.UpdateControlEvents(Activate: boolean);

  function UpdateMethod(Control: TObject; var Method: TMethod): boolean;
  var
     PropInfo: PPropInfo;
     AMethod: TMethod;
  begin
       result := true;
       PropInfo := GetPropInfo(Control.classinfo, 'OnClick');
       if (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkMethod) then
       try
          if Activate then
          begin
               Method := GetMethodProp(Control, PropInfo);
               aMethod.code := @HelpItemClick;
               aMethod.Data := Control;
               SetMethodProp(Control, PropInfo, aMethod);
          end
          else begin
               SetMethodProp(Control, PropInfo, Method);
               Method.code := nil;
               Method.data := nil;
          end;
       except
          MessageDlg('Cannot assign onClick events to menu item / toolbar button', mtError, [mbOK], 0);
          result := false;
       end;
  end;
begin
     if not (csDestroying in ComponentState) then
     begin
       if assigned(fWTMenuItem) then
          if not UpdateMethod(fWTMenuItem, fWTMenuItemClick) then fWTMenuItem := nil;
       if assigned(fWTToolbarButton) then
          if not UpdateMethod(fWTToolbarButton, fWTToolbarButtonClick) then fWTToolbarButton := nil;
     end;
end;

function TWhatsThis.InvokeHelp(x, y: integer): boolean;

   function TruncAccelerator(str: string): string;
   var
      I: integer;
   begin
      result := '';
      for I := 1 to length(str) do
        case str[I] of
        '&': if (I < length(str)) and (str[I+1] = '&') then result := result + str[I];
        else result := result + str[I];
        end;
   end;

   function CheckContextMap(Par, Ctl: TComponent): THelpContext;
   var
     TI, Ext: string;
   begin
        result := 0;
        if assigned(fHelpContextMap) and (fHelpContextMap.filename <> '') then
        begin
             Ext := lowercase(extractfileext(fHelpContextMap.filename));
             if Ext = '.hpj' then
             begin
                  TI := '';
                  if (Par <> nil) and (Par <> Ctl) then TI := Par.name;
                  if (Ctl <> nil) and (Par <> Ctl) then TI := TI + '.' + Ctl.name;
             end;
             if Ext = '.hhp' then
             begin
                  TI := '';
                  if (Par <> nil) and (Par <> Ctl) then TI := Par.name;
                  if (Ctl <> nil) and (Par <> Ctl) then TI := TI + '_' + Ctl.name;
             end;
             result := fHelpContextMap.GetContext(TI);
             {$IFDEF ehsDEBUG}
             monitor('Search ContextMap: HC for '+TI + ' = ' + inttostr(result));
             {$ENDIF}
        end;
   end;

 { Procedure DeactivatePopups:
     In wmPermanent help mode, TWhatsThis obviates WM_LBUTTONDOWN, WM_LBUTTONUP
     and WM_RBUTTONUP in popup menus to prevent execution of the menu item's
     OnClick event. For some reasons, popup menus automatically get deactivated
     when the help popup window occours on Win95+98+NT, which is what we want:
     when a help window is displayed - and only then - the popup menu should
     deactivate itself.
     On Win2000+ME they don't, unfortunately. To deactivate an active popup menu
     we create an invisible dummy window and set the focus to it for a moment. }

   procedure DeactivatePopups;
   var
      DummyHwnd: THandle;
   begin
      {$IFDEF EHS_D6_UP}
      DummyHwnd := Classes.AllocateHwnd(nil);
      {$ELSE}
      DummyHwnd := AllocateHwnd(nil);
      {$ENDIF}
      If DummyHwnd <> 0 then
      begin
        SetFocus(DummyHwnd);
        {$IFDEF EHS_D6_UP}
        Classes.DeallocateHwnd(DummyHwnd);
        {$ELSE}
        DeallocateHwnd(DummyHwnd);
        {$ENDIF}
      end;
   end;

var
   HC: THelpContext;
   MenuItem: TMenuItem;
   ParentControl: TComponent;
   Control: TControl;
   FocusControl: TWinControl;
   PropInfo: PPropInfo;
begin
     if fMenuEntered and fIsPopupItem then DeactivatePopups;

     HC := 0;
     result := false;

{$IFDEF ehsDEBUG}
     monitor('Invoke help at mouse position '+inttostr(x)+'/'+inttostr(y));
{$ENDIF}
     if fMenuEntered then
     begin
        if (fActiveMenuItem = nil) then exit;
        MenuItem := fActiveMenuItem;

        ParentControl := fActiveMenuItem.owner;
        if (not (ParentControl is TCustomForm)) and (not (ParentControl is TDataModule)) then ParentControl := nil;

        while (MenuItem <> nil) and (HC = 0) do
        begin
             if MenuItem <> nil then HC := MenuItem.HelpContext;
             if HC = 0 then HC := CheckContextMap(ParentControl, MenuItem);
             MenuItem := MenuItem.Parent;
        end;

     end
     else begin
        if (fActiveControl = nil) then exit;

        if (Mode = wmWaitPopup)
        {  and (not (wtDirectHelp in fOptions))  }
          and (not (wtPopupOnForm in fOptions))
          and (fActiveControl is TForm)
          then exit;  //--> no popup menu when form was clicked directly;

        ParentControl := fActiveControl;

        while (ParentControl <> nil)
          and (not (ParentControl is TCustomForm))
          and (not (ParentControl is TDataModule)) do
          begin
            if (ParentControl is TControl) and (not (ParentControl is TCustomForm))
              then ParentControl := TControl(ParentControl).parent;
          end;

        Control := fActiveControl;
        while (Control <> nil) and (HC = 0) do
        begin
           if (not (Control is TForm)) or (wtInheritFormContext in fOptions) then
           begin
                PropInfo := GetPropInfo(Control.classinfo, 'HelpContext');
                if (PropInfo <> nil)
                  and (PropInfo^.PropType^.Kind = tkInteger) then
                try
                   HC := GetOrdProp(Control, PropInfo);
                except
                end;

                if (HC = 0) then HC := CheckContextMap(ParentControl, Control);

                if HC = 0 then
                begin
                   PropInfo := GetPropInfo(Control.classinfo, 'FocusControl'); //TCustomLabel
                   if (PropInfo <> nil)
                     and (PropInfo^.PropType^.Kind = tkClass)
                     and (GetTypeData(PropInfo^.PropType^).ClassType.Inheritsfrom(TWinControl)) then
                   try
                      FocusControl := TWinControl(GetOrdProp(Control, PropInfo));
                      if FocusControl <> nil then
                        HC := FocusControl.HelpContext;

                      if (HC = 0) then HC := CheckContextMap(ParentControl, FocusControl);
                   except
                   end;
                end;

                if (HC = 0) and (wtUseTag in fOptions) then HC := Control.Tag;
           end;
           Control := Control.Parent;
        end;
     end;

     if assigned(fOnGetContext) then
     begin
          if fMenuEntered then fOnGetContext(self, fActiveMenuItem, true, HC, x, y)
                          else fOnGetContext(self, fActiveControl, false, HC, x, y);
     end;

     result := HC <> 0;
     if not result then
     begin
        if (Mode = wmWaitPopup) and (not (wtDirectHelp in fOptions))
          then result := (wtNoContextMenu in fOptions)
          else result := (wtNoContextHelp in fOptions);
     end;

     if result then
     begin
{$IFDEF ehsDEBUG}
        monitor('now calling help for context '+inttostr(hc));
{$ENDIF}

        if (Mode = wmWaitPopup) and (not (wtDirectHelp in fOptions)) then
        begin
           if (not (fActiveControl is TForm)) or (wtPopupOnForm in fOptions) then
           begin
{$IFDEF ehsDEBUG}
                monitor('>>> Display WhatsThis menu');
{$ENDIF}
                if assigned(fOnPopup) then fOnPopup(self, fActiveControl, HC, X, Y, result);
                if result then
                begin
                     fContextInfo.Control := fActiveControl;
                     fContextInfo.HelpContext := HC;
                     fContextInfo.X := X;
                     fContextInfo.Y := Y;
                     fWTPopupMenu.popup(x, y);
                end;
           end;
        end
        else begin
           if fMenuEntered then DoOnHelp(fActiveMenuItem, true, HC, X, Y, result)
                           else DoOnHelp(fActiveControl, false, HC, X, Y, result);
           if result then
           begin
             if HC < 0 then Application.HelpCommand(HELP_CONTEXT, abs(HC))
             else begin
                if (X <> 0) and (Y <> 0) then Application.HelpCommand(HELP_SETPOPUP_POS, makelong(x,y));
                Application.HelpCommand(HELP_CONTEXTPOPUP, HC);
             end;
           end;
        end;
     end;
     fActiveControl := nil;
{$IFDEF ehsDEBUG}
     monitor('ActiveControl reset to NIL');
{$ENDIF}
end;

procedure TWhatsThis.SetPopupCaption(value: string);
begin
     fPopupCaption := value;
     if (not (csDesigning in ComponentState))
       and (fWTPopupMenu <> nil)
       and (fWTPopupMenu.items.count > 0)
       then fWTPopupMenu.items[0].Caption := value;
end;

procedure TWhatsThis.SetPopupHelpContext(value: THelpContext);
begin
     fPopupHelpContext := value;
     if (not (csDesigning in ComponentState))
       and (fWTPopupMenu <> nil)
       and (fWTPopupMenu.items.count > 0)
       then fWTPopupMenu.items[0].HelpContext := value;
end;

procedure TWhatsThis.WhatsThisMenuClick(Sender: TObject);
var
   CallHelp: Boolean;
begin
    Mode := wmIdle;
    CallHelp := true;
    with fContextInfo do
    begin
       DoOnHelp(Control, false, HelpContext, X, Y, CallHelp);
       if CallHelp then
       begin
          if HelpContext < 0 then Application.HelpCommand(HELP_CONTEXT, abs(HelpContext))
          //else Application.HelpCommand(HELP_CONTEXTPOPUP, HelpContext);
          else begin
               Application.HelpCommand(HELP_SETPOPUP_POS, makelong(x,y));
               Application.HelpCommand(HELP_CONTEXTPOPUP, HelpContext);
          end;
       end;
    end;
end;

procedure TWhatsThis.DoOnHelp(HelpItem: TObject; IsMenu: boolean; HContext: THelpcontext; X, Y: integer; var CallHelp: boolean);
begin
     if assigned(fOnHelp) then fOnHelp(self, HelpItem, IsMenu, HContext, X, Y, CallHelp);
end;

function TWhatsThis.GetActiveWindow: TWinControl;
begin
  if Assigned(FOnGetActiveWindow) then
    result := FOnGetActiveWindow
  else
    result := FindControl(GetForegroundWindow) ;
end;

end.

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  (c) TechInsite Pty. Ltd.
  PO Box 429, Abbotsford, Melbourne. 3067 Australia
  Phone: +61 3 9419 6456
  Fax:   +61 3 9419 1682
  Web:   www.techinsite.com.au
  EMail: peter_hinrichsen@techinsite.com.au

  Created: Mid 1999

  Purpose: Add this component to an application to convert to a tray icon app.
           This code was downloaded form the Delphi Informant web site and was
           the topic for an article in 1999

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit tiTrayIcon;

interface

uses Windows, Messages, SysUtils, Classes, Controls, Forms, Graphics, Menus, Dialogs;

{Event handler procedural types}
type
  TtiTrayClickEvent       = procedure(Sender: TObject; Button: TMouseButton; var DefaultAction: Boolean) of object;
  TtiTrayMouseButtonEvent = procedure(Sender: TObject; Button: TMouseButton) of object;

{Component class types}
type
  TtiTrayIcon = class(TComponent)
  public
    constructor Create(TheOwner: TComponent); override;
    destructor  Destroy;                      override;
  private   {Private data members}
    FHint:             String;
    FIcon:             TIcon;
    FOnClick:          TtiTrayClickEvent;
    FOnDoubleClick:    TtiTrayClickEvent;
    FOnMouseDown:      TtiTrayMouseButtonEvent;
    FOnMouseMove:      TNotifyEvent;
    FOnMouseUp:        TtiTrayMouseButtonEvent;
    FPopupMenu:        TPopupMenu;
    FShowAtDesignTime: Boolean;
    FVisible:          Boolean;
    FWindowHandle:     HWND;
  private   {Private property reader methods}
    function  GetShowApplication:  Boolean;
  private   {Private property writer methods}
    procedure SetShowApplication(NewValue: Boolean);
    procedure SetHint(NewValue: String);
    procedure SetIcon(NewValue: TIcon);
    procedure SetShowAtDesignTime(NewValue: Boolean);
    procedure SetVisible(NewValue: Boolean);
  private   {Private property storage methods}
    function  StoreIcon: Boolean;
  private   {Private static methods}
    procedure IconChanged(Sender: TObject);
    procedure NotifyTrayIcon(Operation: Integer);
    procedure WindowProcedure(var Msg: TMessage);
  protected {Protected virtual methods}
    procedure Loaded;                                                     override;
    procedure Notification(Component: TComponent; Operation: TOperation); override;
  protected {Protected dynamic event handler dispatch methods}
    procedure Click(Button: TMouseButton);       dynamic;
    procedure DoubleClick(Button: TMouseButton); dynamic;
    procedure MouseDown(Button: TMouseButton);   dynamic;
    procedure MouseMove;                         dynamic;
    procedure MouseUp(Button: TMouseButton);     dynamic;
  public    {Public methods}
    procedure Refresh;
    procedure ShowPopupMenu;
  published {Published read/write properties}
    property Hint:             String     read FHint              write SetHint;
    property Icon:             TIcon      read FIcon              write SetIcon             stored StoreIcon;
    property PopupMenu:        TPopupMenu read FPopupMenu         write FPopupMenu;
    property ShowAtDesignTime: Boolean    read FShowAtDesignTime  write SetShowAtDesignTime default False;
    property ShowApplication:  Boolean    read GetShowApplication write SetShowApplication  default False;
    property Visible:          Boolean    read FVisible           write SetVisible          default False;
  published  {Published events}
    property OnClick:       TtiTrayClickEvent       read FOnClick       write FOnClick;
    property OnDoubleClick: TtiTrayClickEvent       read FOnDoubleClick write FOnDoubleClick;
    property OnMouseDown:   TtiTrayMouseButtonEvent read FOnMouseDown   write FOnMouseDown;
    property OnMouseMove:   TNotifyEvent            read FOnMouseMove   write FOnMouseMove;
    property OnMouseUp:     TtiTrayMouseButtonEvent read FOnMouseUp     write FOnMouseUp;
  end;



implementation

uses ShellAPI;


{Unit private constants.}

const
  WM_TRAYNOTIFY = WM_USER;


{Unit private variables.}
var
  FShowApplication:   Boolean;


{TtiTrayIcon implementation.}
constructor TtiTrayIcon.Create(TheOwner: TComponent);
begin
  {Call the ancestor constructor.}
  inherited Create(TheOwner);

  {Initialize simple-type variables.}
  Self.FHint             := 'Tray Icon Application' ;
  Self.FOnClick          := nil;
  Self.FOnDoubleClick    := nil;
  Self.FOnMouseDown      := nil;
  Self.FOnMouseMove      := nil;
  Self.FOnMouseUp        := nil;
  Self.FPopupMenu        := nil;
  Self.FShowAtDesignTime := False;
  Self.FVisible          := False ;

  {Allocate object-type variables.}
  Self.FIcon := TIcon.Create;

  {Set an OnChange event handler for the internal TIcon
   member to ensure that changes to the icon will be
   reflected immediately and automatically.}
  Self.FIcon.OnChange := Self.IconChanged;

  {Allocate invisible window to handle tray notification messages.}
  Self.FWindowHandle := AllocateHWnd(Self.WindowProcedure);
end;

destructor TtiTrayIcon.Destroy;
begin
  {Ensure the icon is removed from the tray.}
  Self.Visible := False;

  {Free allocated windows resources.}
  DeallocateHWnd(FWindowHandle);

  {Free allocated object-type variables.}
  Self.FIcon.Free;

  {Call the ancestor destructor.}
  inherited Destroy;
end;

function TtiTrayIcon.GetShowApplication: Boolean;
begin
  {Get the property value from the unit data member.}
  Result := FShowApplication;
end;

procedure TtiTrayIcon.SetHint(NewValue: String);
begin
  {If the new value for the hint string is not equal to the
   existing value, save the new value in the internal string
   and refresh the shell icon to reflect the change.}
  if (NewValue <> Self.FHint) then begin
    Self.FHint := NewValue;
    Self.Refresh;
  end;
end;

procedure TtiTrayIcon.SetIcon(NewValue: TIcon);
begin
  {Assign the new icon to the internal icon object
   and modify the shell icon to reflect the change.
   The TIcon's OnChange event handler will perform
   the refresh, so no need to call that here.}
  Self.FIcon.Assign(NewValue);
end;

procedure TtiTrayIcon.SetShowApplication(NewValue: Boolean);
begin
  {Don't adjust visibility while designing, because we'll show
   and hide the Delphi app window if we do.}
  if not (csDesigning in Self.ComponentState) then begin
    {Set the Application ShowApplication property.}
    Application.ShowMainForm := NewValue;

    {Show or Hide the Application window depending on the new value.}
    if (Application.ShowMainForm) then begin
      ShowWindow(Application.Handle, SW_SHOW);
    end
    else begin
      ShowWindow(Application.Handle, SW_HIDE);
    end; {else}

    {If the application's main form has initialized, set it's visibility also.}
    if (Application.MainForm <> nil) then begin
      Application.Mainform.Visible := NewValue;
    end; {if}

    {Ensure the application comes to the foreground.}
    SetForegroundWindow(Application.Handle);

  end; {if}

  {Save the property value to the unit data member, causing
   all other TtiTrayIcon instances to reflect the new value
   also.}
  FShowApplication := NewValue;
end;

procedure TtiTrayIcon.SetShowAtDesignTime(NewValue: Boolean);
begin
  {If the new value is different from the existing value...}
  if (NewValue <> Self.FShowAtDesignTime) then begin
    {If we are now designing, and the component is set Visible...}
    if ((csDesigning in Self.ComponentState) and (Self.Visible)) then begin
      {If the new value is True, add the icon to the Tray.}
      if (NewValue) then begin
        Self.NotifyTrayIcon(NIM_ADD);
      end  {if}
      {If the new value is False, delete the icon from the Tray.}
      else begin
        Self.NotifyTrayIcon(NIM_DELETE);
      end; {else}
    end; {if}

    {Update the internal data member.}
    Self.FShowAtDesignTime := NewValue;
  end; {if}
end;

procedure TtiTrayIcon.SetVisible(NewValue: Boolean);
begin
  {If the new value is different from the existing value...}
  if (NewValue <> Self.FVisible) then begin
    {If the new value is True, and we are either Designing and ShowAtDesignTime is True
     or else we are not designing, then add the icon to the tray.}
    if ((NewValue) and
        (((csDesigning in Self.ComponentState) and (Self.ShowAtDesignTime)) or
         (not (csDesigning in Self.ComponentState))))
    then begin
      Self.NotifyTrayIcon(NIM_ADD);
    end  {if}
    {Otherwise, delete the icon from the tray.}
    else begin
      Self.NotifyTrayIcon(NIM_DELETE);
    end; {else}

    {Update the internal data member.}
    Self.FVisible := NewValue;
  end; {if}
end;

function TtiTrayIcon.StoreIcon: Boolean;
begin
  {Store the TIcon's data in the *.frm file only
   if icon data has actually been loaded into it.}
  Result := (not Self.FIcon.Empty);
end;

procedure TtiTrayIcon.IconChanged(Sender: TObject);
begin
  {This function serves as the event handler for the
   internal TIcon data member to ensure that the tray
   icon will reflect any changes to the icon property.}
  Self.Refresh;
end;

procedure TtiTrayIcon.NotifyTrayIcon(Operation: Integer);
var
  IconData: TNotifyIconData;
begin
  {Set up the tray icon data structure.}
  IconData.cbSize           := SizeOf(IconData);
  IconData.Wnd              := Self.FWindowHandle;
  IconData.uID              := 0;
  IconData.uFlags           := NIF_MESSAGE or NIF_ICON or NIF_TIP;
  IconData.uCallbackMessage := WM_TRAYNOTIFY;

  {If this component has an icon assigned, use that
   for the icon shown in the tray.}
  if (Self.FIcon.Handle <> 0) then begin
    IconData.hIcon := Self.FIcon.Handle;
  end  {if}
  {Otherwise, use the application's icon.}
  else begin
    IconData.hIcon := Application.Icon.Handle;
  end; {else}

  {If the hint string is defined, then load it into the icon data
   structure.  Note that the structure can only hold 63 characters,
   so trim it if necessary. Reload the internal data member with
   the actual value stored to reflect any trimming.}
  StrPLCopy(IconData.szTip, Self.FHint, High(IconData.szTip));
  Self.FHint := StrPas(IconData.szTip);

  {Instruct shell to perform operation on tray icon based
   on the Operation value and the IconData structure.}
  Shell_NotifyIcon(Operation, @IconData);
end;

procedure TtiTrayIcon.WindowProcedure(var Msg: TMessage);
begin
  {If the message is a tray notification message...}
  if Msg.Msg = WM_TRAYNOTIFY then begin
    {Check the lParam piece of the message structure to see what
     happened in the tray. Note that since these are just notifications,
     and not the true messages, we can not get the full range of
     information that is normally available in such a message, so
     we have to synthesize it if we want it.}
    case (Msg.lParam) of

      WM_LBUTTONDBLCLK: Self.DoubleClick(mbLeft);
      WM_MBUTTONDBLCLK: Self.DoubleClick(mbMiddle);
      WM_RBUTTONDBLCLK: Self.DoubleClick(mbRight);

      WM_LBUTTONDOWN:   Self.MouseDown(mbLeft);
      WM_MBUTTONDOWN:   Self.MouseDown(mbMiddle);
      WM_RBUTTONDOWN:   Self.MouseDown(mbRight);

      WM_LBUTTONUP:     Self.MouseUp(mbLeft);
      WM_MBUTTONUP:     Self.MouseUp(mbMiddle);
      WM_RBUTTONUP:     Self.MouseUp(mbRight);

      WM_MOUSEMOVE:     Self.MouseMove;
    end; {case}
  end   {if}

  {If the message is not a tray notification message, then send it
   to the default window procedure for handling.}
  else begin
    Msg.Result := DefWindowProc(Self.FWindowHandle, Msg.Msg, Msg.wParam, Msg.lParam);
  end; {else}
end;

procedure TtiTrayIcon.Loaded;
begin
  {Because the Application object is often not initialized
   while the properties are being loaded, it is necessary
   to refresh ShowApplication again after loading is finished to
   ensure the Application property is correctly set. This
   has no effect during design time.}
  Self.SetShowApplication(Self.ShowApplication);
end;

procedure TtiTrayIcon.Notification(Component: TComponent; Operation: TOperation);
begin
  {Since the PopupMenu references a component we do not control,
   we need to deal with the possibility of its being removed.}

  {Call the ancestor method to perform default processing.}
  inherited Notification(Component, Operation);

  {If the component in question is the same as the one pointed to by our
   PopupMenu property, and that component is being removed, then set the
   PopupMenu property's value to nil.}
  if (Component = Self.PopupMenu) and (Operation = opRemove) then begin
    Self.PopupMenu := nil;
  end;
end;

procedure TtiTrayIcon.Click(Button: TMouseButton);
var
  DoDefaultAction: Boolean;
begin
  {Set the DoDefaultAction flag to True by default and call
   the OnClick event handler, if one is assigned.}
  DoDefaultAction := True;
  if Assigned(FOnClick) then begin
    FOnClick(Self, Button, DoDefaultAction);
  end;  {if}

  {If the right button was clicked and the event handler didn't
   turn off the default action, show the popup menu.}
  if (Button = mbRight) and (DoDefaultAction) then begin
    Self.ShowPopupMenu;
  end; {if}
end;

procedure TtiTrayIcon.DoubleClick(Button: TMouseButton);
var
  DoDefaultAction: Boolean;
begin
  {Set the DoDefaultAction flag to True by default and call
   the OnDoubleClick event handler, if one is assigned.}
  DoDefaultAction := True;
  if Assigned(FOnDoubleClick) then begin
    FOnDoubleClick(Self, Button, DoDefaultAction);
  end;  {if}
  {If the left button was double-clicked and the event handler
   didn't turn off the default action, show the main window.}
  if (Button = mbLeft) and (DoDefaultAction) then begin
    Self.ShowApplication := True;
  end; {if}
end;

procedure TtiTrayIcon.MouseDown(Button: TMouseButton);
begin
  {If an OnMouseDown event handler is assigned, call it.}
  if Assigned(FOnMouseDown) then begin
    FOnMouseDown(Self, Button);
  end;
end;

procedure TtiTrayIcon.MouseMove;
begin
  {If an OnMouseMove event handler is assigned, call it.}
  if Assigned(FOnMouseMove) then begin
    FOnMouseMove(Self);
  end;
end;

procedure TtiTrayIcon.MouseUp(Button: TMouseButton);
begin
  {Simulate a mouse click.}
  Self.Click(Button);

  {If an OnMouseUp event handler is assigned, call it.}
  if Assigned(FOnMouseUp) then begin
    FOnMouseUp(Self, Button);
  end;
end;

procedure TtiTrayIcon.Refresh;
begin
  {Do the Windows API chores to make the tray icon reflect
   the latest property values.}
  Self.NotifyTrayIcon(NIM_MODIFY);
end;

procedure TtiTrayIcon.ShowPopupMenu;
var
  MousePos:   TPoint;
begin
  {We want the menu to appear with its lower right corner at the mouse position.
   The use of GetSystemMetrics below is a compensation for TPopupMenu.Alignment's
   failure to implement the TPM_BOTTOMALIGN flag. Its not perfect, but close enough.
   The call to SetForegroundWindow is necessary to ensure the menu disappears when
   the user clicks somewhere else, even if the main window is hidden.  The call to
   PostMessage is suggested by Microsoft as a remedy to an occasional problem with
   TrackPopupMenu() where the menu disappears unexpectedly shortly after showing.}
   if (Self.PopupMenu <> nil) then begin
     GetCursorPos(MousePos);
     FPopupMenu.Alignment := paRight;
     SetForegroundWindow(Self.FWindowHandle);
     FPopupMenu.Popup(MousePos.X, MousePos.Y - (GetSystemMetrics(SM_CYMENUSIZE) * FPopupMenu.Items.Count));
     PostMessage(Self.FWindowHandle, WM_NULL, 0, 0);
   end; {if}
end;



initialization

  {Initialize simple-type variables.}
  FShowApplication  := False;
  //ShowWindow(Application.Handle, SW_HIDE);

end.




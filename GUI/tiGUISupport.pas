{
  Usage is as follows:

    uses
      tiGUISupport;

      TtiGUIPackageCommand<Name> = class(TtiGUIPackageCommand)
      public
        constructor Create; override;
      end;

    constructor TtiGUIPackageCommand<Name>.Create;
    begin
      inherited;
      CommandMenuItems.AddMenuItem(cTIGUICommandNameFile);
      CommandMenuItems.AddMenuItem('Enter Command Name', 100);
      CommandHint     := '';
      CommandShortCut := '';
      CommandBitmap   := '';
      FormClass       := nil;
      FormInstance    := fiSingle;
    end;

    initialization
      gGUIPackageMgr.RegisterCommand(TtiGUIPackageCommand<Name>);

}
unit tiGUISupport;

{$I tiDefines.inc}

interface
uses
  Classes
  ,Forms
  ,SysUtils
  ,Menus
  ,Contnrs
  ,Windows
  ,ActnList
  ,Controls
  ,ComCtrls
  ,tiObject
 ;

type

  TtiGUIPackageMgr = class;
  TtiGUIPackageMenuItem = class;
  TtiGUIPackageMenuItems = class;
  TtiGUIPackageCommand = class;
  TOnRegisterCommand = procedure (const pCommand : TtiGUIPackageCommand;
                                   var pCanRegister : boolean) of Object;
  TtiGUIPackageMenuItem = class(TtiObject)
  private
    FCommandIndex: integer;
    FCommandName: string;
  protected
    function    GetOwner: TtiGUIPackageMenuItems; reintroduce;
    procedure   SetOwner(const AValue: TtiGUIPackageMenuItems); reintroduce;
  public
    property    Owner      : TtiGUIPackageMenuItems             read GetOwner      write SetOwner;
    //function    Clone : TtiObject; reintroduce; // Must override and typecast if to be used
    //procedure   Assign(ASource : TtiObject); reintroduce;
  published
    property CommandName : string read FCommandName write FCommandName;
    property CommandIndex : integer read FCommandIndex write FCommandIndex;
  end;

  TtiGUIPackageMenuItems = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TtiGUIPackageMenuItem; reintroduce;
    procedure   SetItems(i: integer; const AValue: TtiGUIPackageMenuItem); reintroduce;
    function    GetOwner: TtiGUIPackageCommand; reintroduce;
    procedure   SetOwner(const AValue: TtiGUIPackageCommand); reintroduce;
  public
    property    Items[i:integer]: TtiGUIPackageMenuItem read GetItems write SetItems;
    property    Owner      : TtiGUIPackageCommand             read GetOwner      write SetOwner;
    procedure   Add(const AObject : TtiGUIPackageMenuItem); reintroduce;
    procedure   AddMenuItem(const pCaption : string;
                              AIndex : integer = -1);
    function    Last : TtiGUIPackageMenuItem; reintroduce;
  published
  end;


  TtiGUIFormInstance = (fiUnknown, fiSingle, fiMultiple, fiModal);


  TtiGUIPackageCommand = class(TtiObject)
  private
    FCommandMenuItems: TtiGUIPackageMenuItems;
    FCommandHint: string;
    FCommandShortcut: string;
    FCommandBitMap: string;
    FFormClass: TFormClass;
    FFormInstance: TtiGUIFormInstance;
    FMenuItem : TMenuItem;
    FAction : TAction;
    FToolButton : TToolButton;
    FCommandSecurityName: string;
    FPackageName: TFileName;
    function  GetCommandName: string;
    function  GetCommandIndex: integer;
  protected
    function  CreateSingleInstanceForm(pFormClass : TFormClass) :TForm;
    function  CreateMultipleInstanceForm(pFormClass : TFormClass): TForm;
    procedure CreateModalInstanceForm(pFormClass : TFormClass);
    function  CreateForm(pFormClass : TFormClass; pFormInstance : TtiGUIFormInstance): TForm;
    function  FindForm(pFormClass : TFormClass): TForm;
    procedure AfterCreateInGUI; virtual;
  public
    constructor Create; override;
    destructor  Destroy; override;
    property    CommandMenuItems : TtiGUIPackageMenuItems read FCommandMenuItems;
    property    CommandHint  : string read FCommandHint   write FCommandHint;
    property    CommandShortcut : string read FCommandShortcut write FCommandShortcut;
    property    CommandBitMap : string read FCommandBitMap write FCommandBitMap;
    property    CommandSecurityName : string read FCommandSecurityName write FCommandSecurityName;
    property    PackageName : TFileName read FPackageName write FPackageName;
    procedure   ExecuteCommand(Sender : TObject); virtual;
    property    FormClass : TFormClass read FFormClass write FFormClass;
    property    FormInstance : TtiGUIFormInstance read FFormInstance write FFormInstance;
    property    CommandName : string read GetCommandName;
    property    CommandIndex : integer read GetCommandIndex;
    property    MenuItem : TMenuItem read FMenuItem write FMenuItem;
    property    Action  : TAction read FAction write FAction;
    property    ToolButton : TToolButton read FToolButton write FToolButton;
  end;

  TtiGUIPackageCommandClass = class of TtiGUIPackageCommand;

  TtiGUIPackageMgr = class(TObject)
  private
    FMainForm: TForm;
    FActionList : TActionList;
    FImageList : TImageList;
    FMainMenu  : TMainMenu;
    FToolBar   : TToolBar;
    FRegisteredCommands : TObjectList;
    FToBeRegisteredCommands : TList;
    FOnRegisterCommand: TOnRegisterCommand;
    function  CreateMenuItem(pCommand : TtiGUIPackageCommand): TMenuItem;
    procedure CreateCommandInGUI(  pCommand : TtiGUIPackageCommand; pModule : HModule);
    procedure SetMainForm(const AValue: TForm);
  public
    constructor Create;
    destructor  Destroy; override;
    property    MainForm : TForm read FMainForm write SetMainForm;

    procedure   LoadPackage(const pPackageName : TFileName);
    procedure   LoadPackagesByWildCard(const pWildCard : string);
    procedure   LoadPackagesByFileList(const AFileName : string);

    procedure   UnLoadPackage(const pPackageName : TFileName);
    procedure   RegisterCommand(pCommandClass : TtiGUIPackageCommandClass);
    procedure   RegisterCommands;

    function    FindCommandByName(const pCommandName : string): TtiGUIPackageCommand;
    property    OnRegisterCommand : TOnRegisterCommand read FOnRegisterCommand write FOnRegisterCommand;
    procedure   RunCommandByName(const pCommandName : string);

    property    ActionList : TActionList read FActionList;
    property    ImageList : TImageList  read FImageList;

  end;

  // Move the mouse cursor to a button
  procedure tiMouseToButton(pSender: TControl); deprecated 'Use tiMouseToControl() instead';
  procedure tiMouseToControl(pSender: TControl);
  // Return a control's parent form
  function  tiGetParentForm(pControl: TControl): TForm;
  // Find an MDI form if one is already created
  function  tiFindMDIForm(pFormClass: TFormClass): TForm;
  // Create and show a MDI form. If it already exists, then bring it to the frount.
  procedure tiCreateShowMDIForm(pFormClass : TFormClass);
  procedure tiCursorToHourGlass(pOnOrOff : boolean);

function  gGUIPackageMgr : TtiGUIPackageMgr;
procedure FreeAndNilGUIPackageMgr;

const
  cTIGUICommandNameFile    = '&File';
  cTIGUICommandNameWindows = '&Windows';
  cTIGUICommandNameEdit    = '&Edit';
  cTIGUICommandNameUtils   = '&Utils';
  cTIGUICommandNameHelp    = '&Help';

implementation
uses
  tiUtils
  ,tiLog
  ,tiExcept
  ,Graphics
  ,StdCtrls
  ,Types
 ;

var
  uGUIPackageMgr : TtiGUIPackageMgr;
  uHourGlassCount : integer;

function gGUIPackageMgr : TtiGUIPackageMgr;
begin
  if uGUIPackageMgr = nil then
    uGUIPackageMgr := TtiGUIPackageMgr.Create;
  result := uGUIPackageMgr;
end;

procedure FreeAndNilGUIPackageMgr;
begin
  FreeAndNil(uGUIPackageMgr);
end;

procedure tiMouseToButton(pSender: TControl);
begin
  tiMouseToControl(pSender);
end;

procedure tiMouseToControl(pSender: TControl);
var
  Mpt: TPoint;
begin
  Mpt := point(pSender.Width div 2, pSender.Height div 2);
  Mpt := pSender.ClientToScreen(Mpt);
  SetCursorPos(Mpt.x, Mpt.y);
end;

function tiGetParentForm(pControl : TControl): TForm;
var
  lParent : TControl;
begin
  lParent := pControl.parent;
  while not(lParent is TForm) do
    lParent := lParent.parent;
  result := TForm(lParent);
end;

// Find a MDI form if one is already created.
function tiFindMDIForm(pFormClass: TFormClass): TForm;
var
  i : integer;
  lMainForm : TForm;
begin
  result := nil;
  lMainForm := Application.MainForm;
  for i := 0 to lMainForm.MDIChildCount - 1 do
    if lMainForm.MDIChildren[i] is pFormClass then
    begin
      result := lMainForm.MDIChildren[i];
      Break; //==>
    end;
end;

// Create and show a MDI form. If it already exists, then bring it to the frount.
procedure tiCreateShowMDIForm(pFormClass : TFormClass);
var
  lForm : TForm;
begin
  lForm := tiFindMDIForm(pFormClass);
  if lForm <> nil then
    lForm.BringToFront
  else
    pFormClass.Create(Application);
end;



{ TtiGUIPackageMgr }

constructor TtiGUIPackageMgr.Create;
begin
  inherited;
  FToBeRegisteredCommands := TList.Create;
  FRegisteredCommands := TObjectList.Create;
end;

destructor TtiGUIPackageMgr.Destroy;
begin
  FToBeRegisteredCommands.Free;
  FRegisteredCommands.Free;
  inherited;
end;

function TtiGUIPackageMgr.CreateMenuItem(pCommand: TtiGUIPackageCommand): TMenuItem;
  function _CreateSeparator(pMenu : TMenuItem): TMenuItem;
  begin
    result := TMenuItem.Create(pMenu);
    result.Caption := '-';
    result.Tag := pMenu.Tag + 1;
  end;

  function _FindMenuItem(pMenuItem : TMenuItem;
                          pCommandParent : TtiGUIPackageMenuItem): TMenuItem;
  var
    i : integer;
  begin
    result := nil;
    for i := 0 to pMenuItem.Count - 1 do
      if SameText(
           tiStrTran(pMenuItem.Items[i].Caption, '&', ''),
           tiStrTran(pCommandParent.CommandName, '&', '')) then
      begin
        result := pMenuItem.Items[i];
        Exit; //==>
      end;

    if result = nil then
    begin
      result := TMenuItem.Create(pMenuItem);
      result.Caption := pCommandParent.CommandName;
      result.Tag    := pCommandParent.CommandIndex;
      for i := 0 to pMenuItem.Count - 1 do
        if pMenuItem.Items[i].Tag > pCommandParent.CommandIndex then
        begin
//          if pCommandParent.Owner.Owner.SeparatorAfter then
//            pMenuItem.Insert(i, _CreateSeparator(result));
          pMenuItem.Insert(i, result);
          Exit; //==>
        end;
      // You should only get here if the menu item is to be added higher in the
      // in the list than any other menu items.
      pMenuItem.Add(result);
//      if pCommandParent.Owner.Owner.SeparatorAfter then
//        pMenuItem.Add(_CreateSeparator(result));
    end;

  end;

var
  i : integer;
begin
  Assert(pCommand.CommandMenuItems.Count >= 2,
          'There must be at least 2 registered menu items per command.');
  result := FMainMenu.Items;
  for i := 0 to pCommand.CommandMenuItems.Count - 1 do
    result := _FindMenuItem(result,
                             pCommand.CommandMenuItems.Items[i]);
end;

procedure TtiGUIPackageMgr.LoadPackage(const pPackageName: TFileName);
//var
//  lModule : HModule;
begin
  try
    {lModule :=} SysUtils.LoadPackage(pPackageName);
    RegisterCommands;
  except
    on e:exception do
      LogError('Unable to load package <' +
                 pPackageName + '>' + Cr +
                 ' Error message: ' + e.message);
  end;
end;

procedure TtiGUIPackageMgr.RegisterCommands;
var
  i : integer;
  lCommand : TtiGUIPackageCommand;
  lCanRegister : boolean;
begin
  for i := FToBeRegisteredCommands.Count - 1 downto 0 do
  begin
    lCommand := TtiGUIPackageCommand(FToBeRegisteredCommands.Items[i]);
    lCommand.PackageName := ParamStr(0); //pPackageName;
    lCanRegister := true;
    if Assigned(FOnRegisterCommand) then
      FOnRegisterCommand(lCommand, lCanRegister);

    if lCanRegister then
    begin
      CreateCommandInGUI(lCommand, HInstance);
      FToBeRegisteredCommands.Remove(lCommand);
    end else
    begin
      FToBeRegisteredCommands.Remove(lCommand);
      lCommand.Free;
    end;
  end;
end;


procedure TtiGUIPackageMgr.LoadPackagesByWildCard(const pWildCard: string);
var
  lsl : TStringList;
  i : integer;
  lFileName : TFileName;
begin
  lsl := TStringList.Create;
  try
    tiFilesToStringList(tiGetEXEPath,
                         pWildCard,
                         lsl,
                         false);
    for i := 0 to lsl.Count - 1 do
    begin
      lFileName := lsl.Strings[i];
      Self.LoadPackage(lFileName);
//      if i <> lsl.Count - 1 then
//        AddSeperator;
    end;
  finally
    lsl.Free;
  end;

end;

procedure TtiGUIPackageMgr.RegisterCommand(pCommandClass : TtiGUIPackageCommandClass);
var
  lCommand : TtiGUIPackageCommand;
begin
  lCommand := pCommandClass.Create;
  FToBeRegisteredCommands.Add(lCommand);
end;

procedure TtiGUIPackageMgr.CreateCommandInGUI(pCommand : TtiGUIPackageCommand; pModule : HModule);
var
  lBMP : TBitMap;
  i : integer;
  lButtonLeft : integer;
//  lAction : TAction;
//  lMenuItem : TMenuItem;
//  lToolButton : TToolButton;
begin

  // If there is a bitmap name, then try to load it from the resource file
  if pCommand.CommandBitMap <> '' then
  begin
    lBMP := TBitMap.Create;
    lBMP.LoadFromResourceName(pModule, pCommand.CommandBitMap);
    FImageList.AddMasked(lBMP, clSilver);
  end;

  // Create a TAction
  pCommand.Action := TAction.Create(FActionList);
  pCommand.Action.ActionList := FActionList;
  pCommand.Action.Caption   := pCommand.CommandName;
  pCommand.Action.OnExecute := pCommand.ExecuteCommand;
  pCommand.Action.Tag       := 99999; // Just so it's non zero
  if pCommand.CommandShortcut <> '' then
    pCommand.Action.Shortcut  := TextToShortcut(pCommand.CommandShortcut);
  if pCommand.CommandBitMap <> '' then
    pCommand.Action.ImageIndex := FImageList.Count - 1;
  pCommand.Action.Hint      := pCommand.CommandHint;

  // Create the menu item
  pCommand.MenuItem := CreateMenuItem(pCommand);
  pCommand.MenuItem.Action := pCommand.Action;

  // Create a TToolButton
  if pCommand.CommandBitMap <> '' then
  begin
    pCommand.ToolButton := TToolButton.Create(FToolBar);
    pCommand.ToolButton.Action := pCommand.Action;
    lButtonLeft := 0;
    // This logic requires some more work to get the buttons
    // in the correct order.
    for i := 0 to FToolBar.ButtonCount - 1 do
      if FToolBar.Buttons[i].Action.Tag > pCommand.CommandIndex then
        lButtonLeft := lButtonLeft + FToolBar.Buttons[i].Width;
    pCommand.ToolButton.Top   := 9999;
    pCommand.ToolButton.Left  := lButtonLeft ;
    pCommand.ToolButton.Parent := FToolBar;
  end;

  FRegisteredCommands.Add(pCommand);
  pCommand.AfterCreateInGUI;

end;

procedure TtiGUIPackageMgr.UnLoadPackage(const pPackageName: TFileName);
begin

end;

procedure TtiGUIPackageMgr.SetMainForm(const AValue: TForm);
begin

  FMainForm := AValue;
  FMainMenu := TMainMenu(MainForm.FindComponent('MainMenu'));
  Assert(FMainMenu <> nil, 'Unable to find MainMenu on ' + FMainForm.Name);

  FActionList := TActionList(MainForm.FindComponent('ActionList'));
  Assert(FActionList <> nil, 'Unable to find ActionList on ' + FMainForm.Name);

  FImageList := TImageList(MainForm.FindComponent('ImageList'));
  Assert(FImageList <> nil, 'Unable to find ImageList on ' + FMainForm.Name);

  FToolBar := TToolBar(MainForm.FindComponent('ToolBar'));
  Assert(FToolBar <> nil, 'Unable to find ToolBar on ' + FMainForm.Name);

end;

function TtiGUIPackageMgr.FindCommandByName(const pCommandName: string): TtiGUIPackageCommand;
var
  i : integer;
begin
  result := nil;
  for i := 0 to FRegisteredCommands.Count - 1 do
    if SameText(TtiGUIPackageCommand(FRegisteredCommands.Items[i]).CommandName,
                 pCommandName) then
    begin
      result := TtiGUIPackageCommand(FRegisteredCommands.Items[i]);
      Break; //==>
    end;
end;

procedure TtiGUIPackageMgr.RunCommandByName(const pCommandName: string);
var
  lCommand : TtiGUIPackageCommand;
begin
  lCommand := FindCommandByName(pCommandName);
  if lCommand = nil then
    raise EtiOPFProgrammerException.Create('Unable to find command <' + pCommandName + '>');
  lCommand.ExecuteCommand(nil);
end;

procedure TtiGUIPackageMgr.LoadPackagesByFileList(const AFileName: string);
var
  lsl : TStringList;
  i : integer;
  lFileName : string;
begin
  lsl := TStringList.Create;
  try
    lsl.LoadFromFile(tiAddTrailingSlash(tiGetEXEPath) +
                      AFileName);
    for i := 0 to lsl.Count - 1 do
    begin
      lFileName := lsl.Strings[i];
      Self.LoadPackage(lFileName);
    end;
  finally
    lsl.Free;
  end;
end;

{ TtiGUIPackageCommand }

procedure TtiGUIPackageCommand.AfterCreateInGUI;
begin
  // Do nothing. Implement in concrete is frequired.
end;

constructor TtiGUIPackageCommand.Create;
begin
  inherited create;
  FFormInstance := fiUnknown;
  FCommandMenuItems := TtiGUIPackageMenuItems.Create;
  FCommandMenuItems.Owner := Self;
//  FSeparatorAfter := false;
end;

function TtiGUIPackageCommand.CreateForm(pFormClass: TFormClass; pFormInstance: TtiGUIFormInstance): TForm;
begin
  result := nil;
  case pFormInstance of
  fiSingle  : result := CreateSingleInstanceForm(  pFormClass);
  fiMultiple : result := CreateMultipleInstanceForm(pFormClass);
  fiModal   : CreateModalInstanceForm(   pFormClass);
  else
    raise exception.Create('Invalid form instance passed to ' +
                            ClassName + '.CreateForm');
  end;
end;

procedure TtiGUIPackageCommand.CreateModalInstanceForm(pFormClass : TFormClass);
var
  lForm : TForm;
begin
  lForm := pFormClass.Create(nil);
  try
    if lForm.FormStyle <> fsNormal then
      lForm.FormStyle := fsNormal;
    if lForm.Visible then
      lForm.Visible := false;
    if lForm.BorderStyle <> bsDialog then
      lForm.BorderStyle := bsDialog;
    if lForm.Position <> poScreenCenter then
      lForm.Position := poScreenCenter;

    lForm.ShowModal;
  finally
    lForm.Free;
  end;
end;

function TtiGUIPackageCommand.CreateMultipleInstanceForm(pFormClass : TFormClass): TForm;
begin
  Application.CreateForm(pFormClass, Result);
  Result.FormStyle := fsMDIChild;
  Result.WindowState := wsMaximized;
end;

function TtiGUIPackageCommand.CreateSingleInstanceForm(pFormClass : TFormClass): TForm;
begin
  result := FindForm(pFormClass);
  if result <> nil then
    result.BringToFront
  else
  begin
    result            := pFormClass.Create(Application);
    result.FormStyle  := fsMDIChild;
    result.WindowState := wsMaximized;
  end;
end;

destructor TtiGUIPackageCommand.Destroy;
begin
  FCommandMenuItems.Free;
  inherited;
end;

procedure TtiGUIPackageCommand.ExecuteCommand(Sender: TObject);
begin
  Assert(FFormInstance <> fiUnknown, 'FormInstance not assigned.');
  Assert(FFormClass <> nil, 'FormClass not assigned.');
  CreateForm(FFormClass, FFormInstance);
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiGUIPackageMenuItem
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TtiGUIPackageMenuItem.GetOwner: TtiGUIPackageMenuItems;
begin
  result := TtiGUIPackageMenuItems(inherited GetOwner);
end;

procedure TtiGUIPackageMenuItem.SetOwner(const AValue: TtiGUIPackageMenuItems);
begin
  inherited SetOwner(AValue);
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiGUIPackageMenuItems
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiGUIPackageMenuItems.Add(const AObject: TtiGUIPackageMenuItem);
begin
  inherited Add(AObject);
end;

procedure TtiGUIPackageMenuItems.AddMenuItem(
  const pCaption: string; AIndex: integer = -1);
var
  lCommand : TtiGUIPackageMenuItem;
begin
  lCommand := TtiGUIPackageMenuItem.Create;
  lCommand.CommandName := pCaption;
  lCommand.CommandIndex := AIndex;
  Add(lCommand);
end;

function TtiGUIPackageMenuItems.GetItems(
  i: integer): TtiGUIPackageMenuItem;
begin
  result := TtiGUIPackageMenuItem(inherited GetItems(i));
end;

function TtiGUIPackageMenuItems.GetOwner: TtiGUIPackageCommand;
begin
  result := TtiGUIPackageCommand(inherited GetOwner);
end;

function TtiGUIPackageMenuItems.Last: TtiGUIPackageMenuItem;
begin
  result := TtiGUIPackageMenuItem(inherited Last);
end;

procedure TtiGUIPackageMenuItems.SetItems(i: integer;
  const AValue: TtiGUIPackageMenuItem);
begin
  inherited SetItems(i, AValue);
end;

procedure TtiGUIPackageMenuItems.SetOwner(const AValue: TtiGUIPackageCommand);
begin
  inherited SetOwner(AValue);
end;

// TODO: FindForm assumes an MDIForm, which may not be the case. Fix.
function TtiGUIPackageCommand.FindForm(pFormClass: TFormClass): TForm;
var
  i : integer;
  lMainForm : TForm;
begin
  result := nil;
  lMainForm := Application.MainForm;
  for i := 0 to lMainForm.MDIChildCount - 1 do
    if lMainForm.MDIChildren[i] is pFormClass then
    begin
      result := lMainForm.MDIChildren[i];
      Break; //==>
    end;
end;

function TtiGUIPackageCommand.GetCommandIndex: integer;
begin
  result := FCommandMenuItems.Last.CommandIndex;
end;

function TtiGUIPackageCommand.GetCommandName: string;
begin
  result := FCommandMenuItems.Last.CommandName;
end;

procedure tiCursorToHourGlass(pOnOrOff : boolean);
begin
  if GetCurrentThreadID <> MainThreadID then
    Exit; //==>
  if pOnOrOff then
    Inc(uHourGlassCount)
  else
    Dec(uHourGlassCount);
  Assert(uHourGlassCount >= 0, 'HourGlassCount < 0');
  if (uHourGlassCount > 0) and
     (Screen.Cursor <> crHourGlass) then
    Screen.Cursor := crHourGlass
  else if (uHourGlassCount = 0) and
     (Screen.Cursor <> crDefault) then
    Screen.Cursor := crDefault;
end;


initialization
  uHourGlassCount := 0;

finalization
  FreeAndNilGUIPackageMgr;

end.

unit FPickDatabase;

{$I tiDefines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, tiFocusPanel, tiPerAwareCtrls, Buttons,
  tiSpeedButton, ActnList, Menus, tiPersistenceLayers, Contnrs
  {$IFDEF DELPHI6ORABOVE}
  ,Variants
  {$ENDIF}
 ;

const
  cINIIdentLastPerLayer = 'LastPerLayer';

type
  TFormPickDatabase = class(TForm)
    GroupBox1: TGroupBox;
    paePersistenceLayer: TtiPerAwareEdit;
    paeDatabaseName: TtiPerAwareEdit;
    paeUserName: TtiPerAwareEdit;
    paePassword: TtiPerAwareEdit;
    PM: TPopupMenu;
    AL: TActionList;
    sbDefaultToPresetValues: TtiSpeedButton;
    Action1: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sbDefaultToPresetValuesClick(Sender: TObject);
  private
    FDataDirDepth: integer;
    FSingleUserPersistenceLayers: TObjectList;
    function  GetDatabaseName: string;
    function  GetPassword: string;
    function  GetPersistenceLayerName: string;
    function  GetUserName: string;
    procedure SetPersistenceLayer(const APersistenceLayerName: string);
    procedure SetDataDirDepth(const Value: integer);
    procedure RegisterPersistenceLayersAsTests;
    procedure RegisterPersistenceLayerAsTest(
      const APersistenceLayer: TtiPersistenceLayer);
    procedure DoActionExecute(ASender: TObject);
  protected
    function  GetDataDir: string; virtual;
  public
    property  SingleUserPersistenceLayers: TObjectList read FSingleUserPersistenceLayers;
    property  PersistenceLayerName: string  read GetPersistenceLayerName;
    property  DatabaseName        : string  read GetDatabaseName;
    property  UserName            : string  read GetUserName;
    property  Password            : string  read GetPassword;
    property  DataDirDepth        : integer read FDataDirDepth write SetDataDirDepth;
  end;


implementation
uses
   tiConstants
  ,tiUtils
  ,tiOPFManager
  ,tiINI
  ,tiExcept
  ;

{$R *.dfm}

function TFormPickDatabase.GetDataDir: string;
begin
  result:=
    ExpandFileName(
      tiAddTrailingSlash(
        ExtractFilePath(ParamStr(0))) +
      tiReplicate('..\', FDataDirDepth) + '_Data\');
end;

procedure TFormPickDatabase.RegisterPersistenceLayerAsTest(
  const APersistenceLayer: TtiPersistenceLayer);
var
  LDefaults: TtiPersistenceLayerDefaults;
  LAction: TAction;
  LPMI: TMenuItem;
begin
  Assert(APersistenceLayer.TestValid, CTIErrorInvalidObject);
  LDefaults:= TtiPersistenceLayerDefaults.Create;
  try
    APersistenceLayer.AssignPersistenceLayerDefaults(LDefaults);
    LAction:= TAction.Create(AL);
    LAction.ActionList:= AL;
    LAction.Caption:= LDefaults.PersistenceLayerName;
    LAction.Tag:= APersistenceLayer.Index;
    LAction.OnExecute:= DoActionExecute;
    LAction.Enabled:= True;
    LAction.Visible:= True;
    if not LDefaults.CanSupportMultiUser then
      FSingleUserPersistenceLayers.Add(LAction);
    LPMI:= TMenuItem.Create(PM);
    PM.Items.Add(LPMI);
    LPMI.Action:= LAction;
  finally
    LDefaults.Free;
  end;
end;

procedure TFormPickDatabase.RegisterPersistenceLayersAsTests;
var
  i: integer;
begin
  for I := 0 to GTIOPFManager.PersistenceLayers.Count - 1 do
    RegisterPersistenceLayerAsTest(GTIOPFManager.PersistenceLayers.Items[i]);
end;

procedure TFormPickDatabase.sbDefaultToPresetValuesClick(Sender: TObject);
begin
  sbDefaultToPresetValues.ShowPopupMenu(PM);
end;

procedure TFormPickDatabase.DoActionExecute(ASender: TObject);
var
  LPL: TtiPersistenceLayer;
  LIndex: integer;
begin
  LIndex:= (ASender as TAction).Tag;
  LPL:= GTIOPFManager.PersistenceLayers.Items[LIndex];
  SetPersistenceLayer(LPL.PersistenceLayerName);
end;

procedure TFormPickDatabase.FormCreate(Sender: TObject);
var
  lLastPerLayer: string;
begin
  FSingleUserPersistenceLayers:= TObjectList.Create(False);
  RegisterPersistenceLayersAsTests;
  FDataDirDepth:= 1;
  lLastPerLayer:= gINI.ReadString(Name, 'LastPerLayer', '');
  SetPersistenceLayer(lLastPerLayer);
end;

procedure TFormPickDatabase.SetPersistenceLayer(const APersistenceLayerName: string);
var
  LPL: TtiPersistenceLayer;
  LDefaults: TtiPersistenceLayerDefaults;
begin
  LPL:= GTIOPFManager.PersistenceLayers.FindByPerLayerName(APersistenceLayerName);
  if LPL<>nil then
  begin
    LDefaults:= TtiPersistenceLayerDefaults.Create;
    try
      LPL.AssignPersistenceLayerDefaults(LDefaults);
      paePersistenceLayer.Value:= LDefaults.PersistenceLayerName;
      paeDatabaseName.Value:= ExpandFileName(GetDataDir + LDefaults.DatabaseName);
      paeUserName.Value:= LDefaults.UserName;
      paePassword.Value:= LDefaults.Password;
      gINI.WriteString(Name, cINIIdentLastPerLayer, LDefaults.PersistenceLayerName);
    finally
      LDefaults.Free;
    end;
  end else
  begin
    paePersistenceLayer.Value:= '';
    paeDatabaseName.Value:= '';
    paeUserName.Value:= '';
    paePassword.Value:= '';
  end;
end;

function TFormPickDatabase.GetDatabaseName: string;
begin
  result:= paeDatabaseName.Value;
end;

function TFormPickDatabase.GetPassword: string;
begin
  result:= paePassword.Value;
end;

function TFormPickDatabase.GetPersistenceLayerName: string;
begin
  result:= paePersistenceLayer.Value;
end;

function TFormPickDatabase.GetUserName: string;
begin
  result:= paeUserName.Value;
end;

procedure TFormPickDatabase.FormDestroy(Sender: TObject);
begin
  FSingleUserPersistenceLayers.Free;
  gINI.WriteString(Name, cINIIdentLastPerLayer, paePersistenceLayer.Value);
end;

procedure TFormPickDatabase.SetDataDirDepth(const Value: integer);
begin
  FDataDirDepth:= Value;
  SetPersistenceLayer(paePersistenceLayer.Value);
end;

end.


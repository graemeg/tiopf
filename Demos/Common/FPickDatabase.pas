unit FPickDatabase;

{$I tiDefines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, tiFocusPanel, tiPerAwareCtrls, Buttons,
  tiSpeedButton, ActnList, Menus
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
    InterbaseIBX1: TMenuItem;
    XMLMSXMLDOM1: TMenuItem;
    XMLtiOPFXMLLight1: TMenuItem;
    CSV1: TMenuItem;
    MSAccessviaADO1: TMenuItem;
    AL: TActionList;
    aDefaultToIBX: TAction;
    aDefaultToXMLLight: TAction;
    aDefaultToMSXML: TAction;
    aDefaultToCSV: TAction;
    aDefaultToADOAccess: TAction;
    sbDefaultToPresetValues: TtiSpeedButton;
    aDefaultToPreSetValues: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure aDefaultToIBXExecute(Sender: TObject);
    procedure aDefaultToXMLLightExecute(Sender: TObject);
    procedure aDefaultToMSXMLExecute(Sender: TObject);
    procedure aDefaultToCSVExecute(Sender: TObject);
    procedure aDefaultToADOAccessExecute(Sender: TObject);
    procedure aDefaultToPreSetValuesExecute(Sender: TObject);
  private
    FDataDirDepth: integer;
    function  GetDatabaseName: string;
    function  GetPassword: string;
    function  GetPersistenceLayerName: string;
    function  GetUserName: string;
    procedure SetPerLayerData(const APerLayerName: string);
    procedure SetDataDirDepth(const Value: integer);
  protected
    function  GetDataDir: string; virtual;
  public
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

  // Linking these units causes the persistence layers to be available
  ,tiQueryXML
  ,tiQueryIBX
  ,tiQueryXMLLight
  ,tiQueryCSV
  ,tiQueryADOAccess

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

procedure TFormPickDatabase.FormCreate(Sender: TObject);
var
  lLastPerLayer: string;
begin
  aDefaultToIBX.Enabled      := gTIOPFManager.PersistenceLayers.IsLoaded(cTIPersistIBX);
  aDefaultToMSXML.Enabled    := gTIOPFManager.PersistenceLayers.IsLoaded(cTIPersistXML);
  aDefaultToXMLLight.Enabled := gTIOPFManager.PersistenceLayers.IsLoaded(cTIPersistXMLLight);
  aDefaultToCSV.Enabled      := gTIOPFManager.PersistenceLayers.IsLoaded(cTIPersistCSV);
  aDefaultToADOAccess.Enabled:= gTIOPFManager.PersistenceLayers.IsLoaded(cTIPersistADOAccess);

  // Not implemented (yet)
//  cTIPersistBDEParadox
//  cTIPersistTAB
//  cTIPersistIBO
//  cTIPersistADOSQLServer
//  cTIPersistDOA
//  cTIPersistRemote
//  cTIPersistSqldbIB

  FDataDirDepth:= 1;
  lLastPerLayer:= gINI.ReadString(Name, 'LastPerLayer', '');
  SetPerLayerData(lLastPerLayer);
end;

procedure TFormPickDatabase.SetPerLayerData(const APerLayerName: string);
begin
  if APerLayerName = '' then
    // Do nothing
  else if (aDefaultToIBX.Enabled) and (APerLayerName = cTIPersistIBX) then
  begin
    paePersistenceLayer.Value:= cTIPersistIBX;
    paeDatabaseName.Value:= GetDataDir + 'Demo.fdb';
    paeUserName.Value:= 'SYSDBA';
    paePassword.Value:= 'masterkey';
    gINI.WriteString(Name, cINIIdentLastPerLayer, cTIPersistIBX);
  end
  else if (aDefaultToCSV.Enabled) and (APerLayerName = cTIPersistCSV) then
  begin
    paePersistenceLayer.Value:= cTIPersistCSV;
    paeDatabaseName.Value:= GetDataDir + 'DemoCSV\';
    paeUserName.Value:= 'null';
    paePassword.Value:= 'null';
    gINI.WriteString(Name, cINIIdentLastPerLayer, cTIPersistCSV);
  end
  else if (aDefaultToMSXML.Enabled) and (APerLayerName = cTIPersistXML) then
  begin
    paePersistenceLayer.Value:= cTIPersistXML;
    paeDatabaseName.Value:= GetDataDir + 'Demo.xml';
    paeUserName.Value:= 'null';
    paePassword.Value:= 'null';
    gINI.WriteString(Name, cINIIdentLastPerLayer, cTIPersistXML);
  end
  else if (aDefaultToXMLLight.Enabled) and (APerLayerName = cTIPersistXMLLight) then
  begin
    paePersistenceLayer.Value:= cTIPersistXMLLight;
    paeDatabaseName.Value:= GetDataDir + 'Demo1.xml';
    paeUserName.Value:= 'null';
    paePassword.Value:= 'null';
    gINI.WriteString(Name, cINIIdentLastPerLayer, cTIPersistXMLLight);
  end
  else if (aDefaultToADOAccess.Enabled) and (APerLayerName = cTIPersistADOAccess) then
  begin
    paePersistenceLayer.Value:= cTIPersistADOAccess;
    paeDatabaseName.Value:= GetDataDir + 'Demo.mdb';
    paeUserName.Value:= 'null';
    paePassword.Value:= 'null';
    gINI.WriteString(Name, cINIIdentLastPerLayer, cTIPersistADOAccess);
  end
  else
    raise EtiOPFException.CreateFmt('Persistence layer not registered: "%s"', [APerLayerName]);
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
  gINI.WriteString(Name, cINIIdentLastPerLayer, paePersistenceLayer.Value);
end;

procedure TFormPickDatabase.SetDataDirDepth(const Value: integer);
begin
  FDataDirDepth:= Value;
  SetPerLayerData(paePersistenceLayer.Value);
end;

procedure TFormPickDatabase.aDefaultToIBXExecute(Sender: TObject);
begin
  SetPerLayerData(cTIPersistIBX);
end;

procedure TFormPickDatabase.aDefaultToXMLLightExecute(Sender: TObject);
begin
  SetPerLayerData(cTIPersistXMLLight);
end;

procedure TFormPickDatabase.aDefaultToMSXMLExecute(Sender: TObject);
begin
  SetPerLayerData(cTIPersistXML);
end;

procedure TFormPickDatabase.aDefaultToPreSetValuesExecute(Sender: TObject);
begin
  sbDefaultToPresetValues.ShowPopupMenu(PM);
end;

procedure TFormPickDatabase.aDefaultToCSVExecute(Sender: TObject);
begin
  SetPerLayerData(cTIPersistCSV);
end;

procedure TFormPickDatabase.aDefaultToADOAccessExecute(Sender: TObject);
begin
  SetPerLayerData(cTIPersistADOAccess);
end;

end.

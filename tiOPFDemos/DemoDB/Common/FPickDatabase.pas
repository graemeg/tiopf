unit FPickDatabase;

{$I tiDefines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, tiFocusPanel, tiPerAwareCtrls
  {$IFDEF DELPHI6ORABOVE}
  ,Variants
  {$ENDIF}
  ;
                             
type
  TFormPickDatabase = class(TForm)
    paePersistenceLayer: TtiPerAwareEdit;
    paeDatabaseName: TtiPerAwareEdit;
    paeUserName: TtiPerAwareEdit;
    paePassword: TtiPerAwareEdit;
    btnSetupForIBX: TButton;
    btnSetupForXML: TButton;
    btnSetupForCSV: TButton;
    btnSetupForADOAccess: TButton;
    btnSetupForRemote: TButton;
    procedure btnSetupForIBXClick(Sender: TObject);
    procedure btnSetupForXMLClick(Sender: TObject);
    procedure btnSetupForCSVClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSetupForADOAccessClick(Sender: TObject);
    procedure btnSetupForRemoteClick(Sender: TObject);
  private
    FDataDirDepth: integer;
    function  GetDatabaseName: string;
    function  GetPassword: string;
    function  GetPersistenceLayerName: string;
    function  GetUserName: string;
    procedure SetPerLayerData(const pPerLayerName: string);
    procedure SetDataDirDepth(const Value: integer);
  protected
    function  GetDataDir: string; virtual ;
  public
    property  PersistenceLayerName : string  read GetPersistenceLayerName ;
    property  DatabaseName         : string  read GetDatabaseName ;
    property  UserName             : string  read GetUserName ;
    property  Password             : string  read GetPassword ;
    property  DataDirDepth         : integer read FDataDirDepth write SetDataDirDepth ;
  end;


implementation
uses
  cTIPersist
  ,tiUtils
  ,tiPersist
  ,tiRegINI
  ;
  
{$R *.dfm}

procedure TFormPickDatabase.btnSetupForIBXClick(Sender: TObject);
begin
  paePersistenceLayer.Value := cTIPersistIBX ;
  paeDatabaseName.Value := GetDataDir + 'Demo.gdb' ;
  paeUserName.Value := 'SYSDBA' ;
  paePassword.Value := 'masterkey' ;
  gINI.WriteString(Name,'LastPerLayer', cTIPersistIBX);
end;

procedure TFormPickDatabase.btnSetupForXMLClick(Sender: TObject);
begin
  paePersistenceLayer.Value := cTIPersistXML ;
  paeDatabaseName.Value := GetDataDir + 'Demo.xml' ;
  paeUserName.Value := 'null' ;
  paePassword.Value := 'null' ;
  gINI.WriteString(Name,'LastPerLayer', cTIPersistXML);
end;

procedure TFormPickDatabase.btnSetupForCSVClick(Sender: TObject);
begin
  paePersistenceLayer.Value := cTIPersistCSV ;
  paeDatabaseName.Value := GetDataDir + 'DemoCSV\' ;
  paeUserName.Value := 'null' ;
  paePassword.Value := 'null' ;
  gINI.WriteString(Name,'LastPerLayer', cTIPersistCSV);
end;

function TFormPickDatabase.GetDataDir: string;
begin
  result :=
    ExpandFileName(
      tiAddTrailingSlash(
        ExtractFilePath( ParamStr(0) )) +
      tiReplicate( '..\', FDataDirDepth) + 'Data\' ) ;
end;

procedure TFormPickDatabase.FormCreate(Sender: TObject);
var
  lLastPerLayer : string ;
begin
  FDataDirDepth := 3 ;
  lLastPerLayer := gINI.ReadString('PickDatabase','LastPerLayer', '') ;
  SetPerLayerData(lLastPerLayer);
end ;

procedure TFormPickDatabase.SetPerLayerData(const pPerLayerName : string);
begin
  if pPerLayerName = '' then
    // Do nothing
  else if pPerLayerName = cTIPersistIBX then
    btnSetupForIBX.Click
  else if pPerLayerName = cTIPersistCSV then
    btnSetupForCSV.Click
  else if pPerLayerName = cTIPersistXML then
    btnSetupForXML.Click
  else if pPerLayerName = cTIPersistADOAccess then
    btnSetupForADOAccess.Click
  else if pPerLayerName = cTIPersistRemote then
    btnSetupForRemote.Click
  else
    tiFmtException( 'Invalid persistence layer name <' +
                    pPerLayerName + '> read from INI file' ) ;
end;

function TFormPickDatabase.GetDatabaseName: string;
begin
  result := paeDatabaseName.Value ; 
end;

function TFormPickDatabase.GetPassword: string;
begin
  result := paePassword.Value ;
end;

function TFormPickDatabase.GetPersistenceLayerName: string;
begin
  result := paePersistenceLayer.Value ;
end;

function TFormPickDatabase.GetUserName: string;
begin
  result := paeUserName.Value ;
end;

procedure TFormPickDatabase.FormDestroy(Sender: TObject);
begin
  gINI.WriteString('PickDatabase','LastPerLayer', paePersistenceLayer.Value) ;
end;

procedure TFormPickDatabase.btnSetupForADOAccessClick(Sender: TObject);
begin
  paePersistenceLayer.Value := cTIPersistADOAccess ;
  paeDatabaseName.Value := GetDataDir + 'Demo.mdb' ;
  paeUserName.Value := 'null' ;
  paePassword.Value := 'null' ;
  gINI.WriteString(Name,'LastPerLayer', cTIPersistADOAccess);
end;

procedure TFormPickDatabase.btnSetupForRemoteClick(Sender: TObject);
begin
  paePersistenceLayer.Value := cTIPersistRemote ;
  paeDatabaseName.Value := cLocalHost ;
  paeUserName.Value := 'null' ;
  paePassword.Value := 'null' ;
  gINI.WriteString(Name,'LastPerLayer', cTIPersistRemote);
end;

procedure TFormPickDatabase.SetDataDirDepth(const Value: integer);
begin
  FDataDirDepth := Value;
  SetPerLayerData(paePersistenceLayer.Value);
end;

end.

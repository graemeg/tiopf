unit FMainConnectToDBCode;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FPickDatabase, StdCtrls, ExtCtrls, tiFocusPanel, tiPerAwareCtrls,
  tiMemoReadOnly;

type
  TFormMainConnectToDBCode = class(TFormPickDatabase)
    btnLoadPersistenceLayer: TButton;
    btnUnLoadPersistenceLayer: TButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    tiMemoReadOnly1: TtiMemoReadOnly;
    btnLoadDatabase: TButton;
    Button2: TButton;
    tiMemoReadOnly2: TtiMemoReadOnly;
    btnLoadBoth: TButton;
    btnUnLoadBoth: TButton;
    tiMemoReadOnly3: TtiMemoReadOnly;
    btnShowLoaded: TButton;
    procedure btnLoadPersistenceLayerClick(Sender: TObject);
    procedure btnUnLoadPersistenceLayerClick(Sender: TObject);
    procedure btnUnLoadBothClick(Sender: TObject);
    procedure btnLoadDatabaseClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btnLoadBothClick(Sender: TObject);
    procedure btnShowLoadedClick(Sender: TObject);
  private
  protected
    function GetDataDir : string ; override ;
  public
    { Public declarations }
  end;

var
  FormMainConnectToDBCode: TFormMainConnectToDBCode;

implementation
uses
  tiPersist
  ,tiUtils
  ,tiDialogs
  ;

{$R *.DFM}

procedure TFormMainConnectToDBCode.btnLoadPersistenceLayerClick(Sender: TObject);
begin
  if gTIPerMgr.RegPerLayers.IsLoaded(PersistenceLayerName) then
  begin
    tiAppError( 'Persistence layer <' + PersistenceLayerName + '> is already loaded.' );
    Exit ; //==>
  end ;
  gTIPerMgr.LoadPersistenceLayer(PersistenceLayerName);
  tiAppMessage( 'Persistence layer <' + PersistenceLayerName + '> loaded.' );
end;

procedure TFormMainConnectToDBCode.btnUnLoadPersistenceLayerClick(Sender: TObject);
begin
  if not gTIPerMgr.RegPerLayers.IsLoaded(PersistenceLayerName) then
  begin
    tiAppError( 'Persistence layer <' + PersistenceLayerName + '> is not loaded.' );
    Exit ; //==>
  end ;
  gTIPerMgr.UnLoadPersistenceFramework(PersistenceLayerName);
  tiAppMessage( 'Persistence layer <' + PersistenceLayerName + '> unloaded.' );
end;

procedure TFormMainConnectToDBCode.btnUnLoadBothClick(Sender: TObject);
begin
  gTIPerMgr.UnLoadPersistenceFramework(PersistenceLayerName);
  tiAppMessage( 'Persistence layer for <' + PersistenceLayerName + '>' +
               ' and Database connection pool for <' + DatabaseName + '> unloaded.' );
end;

procedure TFormMainConnectToDBCode.btnLoadDatabaseClick(Sender: TObject);
begin
  gTIPerMgr.LoadDatabaseLayer(
    PersistenceLayerName,
    DatabaseName,
    UserName,
    Password ) ;
  tiAppMessage( 'Database connection pool for <' + DatabaseName + '> loaded.' );
end;

procedure TFormMainConnectToDBCode.Button2Click(Sender: TObject);
begin
  gTIPerMgr.UnLoadDatabaseLayer(
    PersistenceLayerName ) ;
  tiAppMessage( 'Database connection pool for <' + DatabaseName + '> unloaded.' );
end;

procedure TFormMainConnectToDBCode.btnLoadBothClick(Sender: TObject);
begin
  gTIPerMgr.LoadPersistenceFramework(
    PersistenceLayerName,
    DatabaseName,
    UserName,
    Password ) ;
  tiAppMessage( 'Persistence layer for <' + PersistenceLayerName + '>' +
               ' and Database connection pool for <' + DatabaseName + '> loaded.' );
end;

function TFormMainConnectToDBCode.GetDataDir: string;
begin
  result :=
    ExpandFileName(
      tiAddTrailingSlash(
        ExtractFilePath( ParamStr(0) )) +
      '..\..\..\..\Data\' ) ;
end;

procedure TFormMainConnectToDBCode.btnShowLoadedClick(Sender: TObject);
var
  i : integer ;
  ls : string ;
begin
  inherited;
  ls := '';
  for i := 0 to gTIPerMgr.RegPerLayers.Count - 1 do
  begin
    if ls <> '' then
      ls := ls + Cr ;
    ls := ls + gTIPerMgr.RegPerLayers.Items[i].PerLayerName ;
  end ;

  if ls <> '' then
    tiAppMessage(ls)
  else
    tiAppMessage('No persistence layers loaded' ) ;
end;

end.

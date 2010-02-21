{

  Purpose:
    Browse the results of a query

  Classes:
    TFormSQLMgrBrowse - The form
    TthrdSQLMgr       - A thread to run the query in

}


unit FSQLMgrBrowse;

{$I tiDefines.inc}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  tiDBConnectionPool, tiSQLMgr_BOM, ActnList, ToolWin, ComCtrls,
  ImgList, Menus, Db
  ,tiThread
  ,tiQuery
  ,tiDataBuffer_BOM
  ,tiDataBuffer_Cli
  ,tiSQLMgrDataSet_Srv // This has to be used somewhere in the ap, and this is a good a place as any
  ,tiSQLMgrDataSet_BOM
  {$IFDEF FPC}
  ,LResources
  ,LCLProc
  {$ENDIF}
  ,Grids
  ;

resourcestring
  crsSaveToCSVDone = 'Finished saving to file <%s>' +
                     #13 + #13 +
                     'Do you want to view this file?' ;

type


  TFormSQLMgrBrowse = class(TForm)
    sb: TStatusBar;
    PopupMenu1: TPopupMenu;
    Structure1: TMenuItem;
    ExporttoCSVfile1: TMenuItem;
    N1: TMenuItem;
    Close1: TMenuItem;
    ImageList1: TImageList;
    ActionList1: TActionList;
    aStructure: TAction;
    aClose: TAction;
    aExport: TAction;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton4: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Close2: TMenuItem;
    SQL1: TMenuItem;
    aSQLCreate: TAction;
    aSQLUpdate: TAction;
    aSQLDelete: TAction;
    aSQLRead: TAction;
    aSQLCreate1: TMenuItem;
    N2: TMenuItem;
    aSQLCreate2: TMenuItem;
    aSQLUpdate1: TMenuItem;
    aSQLDelete1: TMenuItem;
    aSQLAsMapRowToObject: TAction;
    N3: TMenuItem;
    aSQLAsMapRowToObject1: TMenuItem;
    aSQLAsSetupParams: TAction;
    SQLAsSetupParams1: TMenuItem;
    aSQLAsClassInterface: TAction;
    Asclassinterface1: TMenuItem;
    ToolButton5: TToolButton;
    aShowRecord: TAction;
    LV: TListView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure aCloseExecute(Sender: TObject);
    procedure aStructureExecute(Sender: TObject);
    procedure aExportExecute(Sender: TObject);
    procedure aSQLCreateExecute(Sender: TObject);
    procedure aSQLUpdateExecute(Sender: TObject);
    procedure aSQLDeleteExecute(Sender: TObject);
    procedure aSQLReadExecute(Sender: TObject);
    procedure aSQLAsMapRowToObjectExecute(Sender: TObject);
    procedure aSQLAsSetupParamsExecute(Sender: TObject);
    procedure aSQLAsClassInterfaceExecute(Sender: TObject);
    procedure aShowRecordExecute(Sender: TObject);
    procedure LVData(Sender: TObject; Item: TListItem);
  private
    FCols : TStringList ;
    FiColWidth : integer ;
    FtiQueryDataSetMapping: TtiDataBufferQueryMapping;
    function  ColNameText(pIndex: integer): string;
    function  ColNameDelimText(pIndex: integer): string;
    function  ColDataType(pIndex: integer): string;
    function  ColAsProperty(pIndex: integer):string;
    procedure SetTIQueryDataSetMapping( const Value: TtiDataBufferQueryMapping);
  public
    property TIDataSetQueryMapping : TtiDataBufferQueryMapping read FtiQueryDataSetMapping write SetTIQueryDataSetMapping ;
  end;


  // Modify to take a list of SQLMgrQuery objects as well as a single instance
  TthrdSQLMgrAbs = class( TtiThread )
  private
    FtiDataSetQueryMapping : TtiDataBufferQueryMapping ;
    FsErrorText   : string ;
    FForm         : TFormSQLMgrBrowse ;
    procedure   SetSQLMgrQuery(const Value: TSQLMgrQuery);
    function    GetSQLMgrQuery: TSQLMgrQuery;
  protected
    procedure   DoOnTerminate( sender : TObject ) ; override ;
    procedure   DoShowResultSet( sender : TObject ) ; virtual ; abstract ;
    procedure   DoShowNoResultSet( sender : TObject ) ;
    procedure   DoShowException( sender : TObject ) ;
  public
    constructor Create(CreateSuspended: Boolean); override ;
    destructor  Destroy ; override ;
    property    SQLMgrQuery : TSQLMgrQuery read GetSQLMgrQuery write SetSQLMgrQuery ;
    procedure   Execute ; override ;
  end ;

  TthrdSQLMgrBrowse = class( TthrdSQLMgrAbs )
  protected
    procedure   DoShowResultSet( sender : TObject ) ; override ;
  end ;

  TthrdSQLMgrSave = class( TthrdSQLMgrAbs )
  protected
    procedure   DoShowResultSet( sender : TObject ) ; override ;
  end ;

implementation
uses
  tiUtils
  ,ClipBrd
  ,Math
  ,tiLog
  ,tiCommandLineParams
  ,tiOPFManager
  ,tiDialogs
  ,tiINI
  ,tiGUIINI
  ,tiGUIUtils
  ;

//{$IFDEF FPC}
  {$R *.dfm}
//{$ENDIF}
  
var
  uSaveFileName : TFileName ;


function GetSQLSaveFileName( pFileName : TFileName ) : TFileName ;
var
  lSD : TSaveDialog ;
begin
  result := '' ;
  lSD := TSaveDialog.Create( nil ) ;
  try
    lSD.FileName   := pFileName ;
    lSD.Filter     := 'CSV files|*.csv|Text files|*.txt|All files|*.*' ;
    lSD.DefaultExt := '.CSV' ;
    if SameText( tiExtractExtension( pFileName ), 'TXT' ) then
      lSD.FilterIndex := 1 ;
    if lSD.Execute then
    begin
      result := lSD.FileName ;
    end;
  finally
    lSD.Free ;
  end ;
end ;

procedure TFormSQLMgrBrowse.FormCreate(Sender: TObject);
begin
  LV.Align := alClient ;
//  DBGrid.Align := alClient;
//  DBGrid.DataSource := DS;
  GGUIINI.ReadFormState( self ) ;
  FCols := TStringList.Create ;
  FiColWidth := 0 ;
end;

procedure TFormSQLMgrBrowse.FormDestroy(Sender: TObject);
begin
  GGUIINI.WriteFormState( self ) ;
  FCols.Free ;
  FtiQueryDataSetMapping.Free ;
end;

procedure TFormSQLMgrBrowse.LVData(Sender: TObject; Item: TListItem);
begin
  tiDataSetToListItem(FtiQueryDataSetMapping.TIDataSet, Item);
end;

procedure TFormSQLMgrBrowse.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFormSQLMgrBrowse.aCloseExecute(Sender: TObject);
begin
  Close ;
end;

procedure TFormSQLMgrBrowse.aStructureExecute(Sender: TObject);
var
  i : integer ;
  ls : string ;
begin
  ls := '' ;
  for i := 0 to LV.Columns.Count - 1 do
    ls := ls + LV.Columns[i].Caption + CrLf ;
  tiShowString( ls ) ;
end;

procedure TFormSQLMgrBrowse.aExportExecute(Sender: TObject);
var
  lFileName : TFileName ;
begin
  lFileName := GGUIINI.ReadString( Name, 'FileName', '' ) ;
  lFileName := GetSQLSaveFileName( lFileName ) ;
  tiDataSetToTextFile( FtiQueryDataSetMapping.TIDataSet, lFileName ) ;
  if lFileName <> '' then
  begin
    GGUIINI.WriteString( Name, 'FileName', lFileName ) ;
    if tiAppConfirmation( Format( crsSaveToCSVDone, [lFileName] )) then
      tiEditFile( lFileName ) ;
  end ;
end ;

{ TthrdSQLMgrAbs }

constructor TthrdSQLMgrAbs.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := true ;
  FsErrorText := '' ;
  FtiDataSetQueryMapping := TtiDataBufferQueryMapping.Create ;
end;

procedure TthrdSQLMgrAbs.DoOnTerminate(sender: TObject);
begin
  inherited DoOnTerminate( sender ) ;

  // An error
  if FsErrorText <> '' then
    DoShowException( Sender )
  // An empty result set
  else if FtiDataSetQueryMapping.TIDataSet.Count = 0 then
    DoShowNoResultSet( Sender )
  // A valid result set
  else
    DoShowResultSet( Sender ) ;

end;

procedure TthrdSQLMgrAbs.DoShowException( Sender : TObject ) ;
begin
  tiMessageTextDlg( FsErrorText, ['Return to SQL Editor'], mtError, 'Error running query' ) ;
end ;

procedure TthrdSQLMgrAbs.Execute;
begin
  try
    gTIOPFManager.Read( FtiDataSetQueryMapping ) ;
  except
    on e:exception do
      FsErrorText:= e.message;
  end;
end;

procedure TthrdSQLMgrAbs.SetSQLMgrQuery(const Value: TSQLMgrQuery);
begin
  FtiDataSetQueryMapping.SQLMgrQuery := Value;
  Text := 'Running: ' + FtiDataSetQueryMapping.SQLMgrQuery.Caption ;
end;

destructor TthrdSQLMgrAbs.destroy;
begin
  inherited ;
end;

procedure TFormSQLMgrBrowse.aSQLCreateExecute(Sender: TObject);
var
  ls : string ;
  i  : integer ;
begin

  ls := 'insert into ENTER_TABLE_NAME' + CrLf +
        '(' + CrLf ;

  for i := 0 to FCols.Count - 1 do
  begin
    if i = 0 then
      ls := ls + '     '
    else
      ls := ls + '    ,' ;
    ls := ls + ColNameText(i) + CrLf ;
  end ;

  ls := ls + ')' + CrLf + 'VALUES' + CrLf + '(' + CrLf ;

  for i := 0 to FCols.Count - 1 do
  begin
    if i = 0 then
      ls := ls + '     :'
    else
      ls := ls + '    ,:' ;
    ls := ls + ColNameText(i) + CrLf ;
  end ;

  ls := ls + ')' ;

  Clipboard.AsText := ls ;

end;

procedure TFormSQLMgrBrowse.aSQLUpdateExecute(Sender: TObject);
var
  ls : string ;
  i  : integer ;
begin

  ls := 'UPDATE ENTER_TABLE_NAME' + CrLf +
        'SET' + CrLf ;

  for i := 0 to FCols.Count - 1 do
  begin
    if i = 0 then
      ls := ls + '     '
    else
      ls := ls + '    ,' ;
    ls := ls + ColNameText(i) + ' = :' + ColNameText(i) + CrLf ;
  end ;

  ls := ls + 'WHERE' + CrLf ;

  for i := 0 to FCols.Count - 1 do
  begin
    if i = 0 then
      ls := ls + '     '
    else
      ls := ls + 'AND  ' ;
    ls := ls + ColNameText(i) + ' = :OLD_' + ColNameText(i) + CrLf ;
  end ;

  Clipboard.AsText := ls ;

end;

procedure TFormSQLMgrBrowse.aSQLDeleteExecute(Sender: TObject);
var
  ls : string ;
  i  : integer ;
begin

  ls := 'DELETE FROM ENTER_TABLE_NAME' + CrLf +
        'WHERE' + CrLf ;

  for i := 0 to FCols.Count - 1 do
  begin
    if i = 0 then
      ls := ls + '     '
    else
      ls := ls + 'AND  ' ;
    ls := ls +
          ColNameText( i ) + ' = :' + ColNameText(i) + CrLf ;
  end ;

  Clipboard.AsText := ls ;

end;

procedure TFormSQLMgrBrowse.aSQLReadExecute(Sender: TObject);
var
  ls : string ;
  i  : integer ;
begin
  ls := 'SELECT' + CrLf ;

  for i := 0 to FCols.Count - 1 do
  begin
    if i = 0 then
      ls := ls + '     '
    else
      ls := ls + '    ,' ;
    ls := ls + ColNameText(i) + CrLf ;
  end ;

  ls := ls + 'FROM' + CrLf +
        'ENTER_TABLE_NAME' + CrLf ;

  ls := ls + 'WHERE' + CrLf ;

  for i := 0 to FCols.Count - 1 do
  begin
    if i = 0 then
      ls := ls + '     '
    else
      ls := ls + 'AND  ' ;
    ls := ls + ColNameText(i) + ' = :' + ColNameText(i) + CrLf ;
  end ;


  Clipboard.AsText := ls ;

end;

function TFormSQLMgrBrowse.ColNameText( pIndex : integer ) : string ;
begin
  result := tiPadR( FCols.Strings[pIndex], FiColWidth ) ;
end ;

function TFormSQLMgrBrowse.ColNameDelimText( pIndex : integer ) : string ;
begin
  result := tiPadR( QuotedStr( FCols.Strings[pIndex]), FiColWidth+2 ) ;
end ;

procedure TFormSQLMgrBrowse.aSQLAsMapRowToObjectExecute(Sender: TObject);
var
  ls : string ;
  i  : integer ;
begin
  ls :=
    'var'                               + CrLf +
    '  lData: EnterClassName;'          + CrLf +
    'begin'                             + CrLf +
    '  lData := EnterClassName.Create;' + CrLf +
    '  lData.OID.AssignFromTIQuery(Query);' + CrLf;

  for i := 0 to FCols.Count - 1 do
  begin
    ls := ls +
          '  ' +
          'lData.' + ColAsProperty(i) + ' := Query.FieldAs' +
          tiPadR( ColDataType(i) + '[ ', 10 ) +
          ColNameDelimText(i) + ' ];' + CrLf;
  end ;

  ls := ls + '  lData.ObjectState := posClean;' + CrLf;
  ls := ls + '  (Visited as TtiObjectList).Add(lData);' + CrLf;

  Clipboard.AsText := ls;
end;

procedure TFormSQLMgrBrowse.aSQLAsSetupParamsExecute(Sender: TObject);
var
  ls: string;
  i: integer;
begin
  ls :=
    'var'                            + CrLf +
    '  lData: EnterClassType;'         + CrLf +
    'begin'                          + CrLf +
    '  lData := (Visited as EnterClassType);' + CrLf +
    '  lData.OID.AssignToTIQuery(Query);' + CrLf;

  for i := 0 to FCols.Count - 1 do
  begin
    ls := ls + '  Query.ParamAs[ ' +
        ColNameDelimText(i) + ' ] := lData.' + ColAsProperty(i) + ';' + CrLf;
  end ;

  Clipboard.AsText := ls;
end;

procedure TFormSQLMgrBrowse.aSQLAsClassInterfaceExecute(Sender: TObject);
var
  ls : string ;
  i  : integer ;
begin
  ls := '  EnterClassType = class(TtiObject)' + CrLf +
        '  private' + CrLf +
        '  protected' + CrLf +
        '  public' + CrLf +
        '  published' + CrLf ;

  for i := 0 to FCols.Count - 1 do
  begin
    ls := ls +
          '    property ' + ColAsProperty(i) +
          ' : ' + ColDataType(i) + ' read F' +
          ColAsProperty(i) +
          ' write F' +
          ColAsProperty(i) +
          ';' + CrLf ;
  end ;

  ls := ls +
        '  end;' + CrLf( 2 ) ;

  Clipboard.AsText := ls ;
end;

procedure TthrdSQLMgrAbs.DoShowNoResultSet(sender: TObject);
begin
  tiAppMessage( 'Query executed. There is no result set.' ) ;
end;

procedure TFormSQLMgrBrowse.SetTIQueryDataSetMapping( const Value: TtiDataBufferQueryMapping);
var
  i : integer ;
begin
  FtiQueryDataSetMapping := Value;
  Caption := ' Results for query: ' + FtiQueryDataSetMapping.SQLMgrQuery.Caption ;
  
  // Create the Columns in the Listview
  tiDataSetToListView( FtiQueryDataSetMapping.TIDataSet, LV ) ;
  LV.OnData:= LVData;
  SB.Panels[0].Text := 'Record count: ' + intToStr( FtiQueryDataSetMapping.TIDataSet.Count ) ;
  SB.Panels[1].Text := 'Time to execute on server: '  + IntToStr( FtiQueryDataSetMapping.TimeToRun ) + 'ms' ;
  SB.Panels[2].Text := 'Time to download dara: '  + IntToStr( FtiQueryDataSetMapping.TimeToScan ) + 'ms' ;
  FCols.Clear ;
  for i := 0 to FtiQueryDataSetMapping.TIDataSet.Fields.Count - 1 do
    FCols.Add(FtiQueryDataSetMapping.TIDataSet.Fields.Items[i].Name);
  FiColWidth := 0 ;
  for i := 0 to FCols.Count - 1 do
    FiColWidth := Max( FiColWidth, Length( FCols.Strings[i] )) ;
    
  // Populate the ListView with data (items)
//  for i := 0 to FtiQueryDataSetMapping.TIDataSet.Count - 1 do
//  begin
//    LItem := LV.Items[i];
//    tiDataSetToListItem(FtiQueryDataSetMapping.TIDataSet, LItem);
//  end;
end;

function TthrdSQLMgrAbs.GetSQLMgrQuery: TSQLMgrQuery;
begin
  result := FtiDataSetQueryMapping.SQLMgrQuery ;
end;

function TFormSQLMgrBrowse.ColDataType(pIndex: integer): string;
begin
  result :=
    QueryFieldKindToString(
      FtiQueryDataSetMapping.TIDataSet.Fields.Items[pIndex].Kind ) ;
  if SameText( result, 'date' ) then
    result := result + 'Time' ;
end;

{ TthrdSQLMgrBrowse }

procedure TthrdSQLMgrBrowse.DoShowResultSet(sender: TObject);
begin
  FForm := TFormSQLMgrBrowse.Create( nil ) ;
  FForm.TIDataSetQueryMapping := FtiDataSetQueryMapping ;
  FForm.Visible := true ;
end;

{ TthrdSQLMgrSave }

procedure TthrdSQLMgrSave.DoShowResultSet(sender: TObject);
var
  lFileName : TFileName ;
begin
  lFileName := uSaveFileName ;
  if lFileName = '' then
    lFileName := gINI.ReadString( 'SQLResultAsFile', 'FileName', '' ) ;
  lFileName := GetSQLSaveFileName( lFileName ) ;
  if lFileName <> '' then
  begin
    tiDataSetToTextFile( FtiDataSetQueryMapping.TIDataSet, lFileName ) ;
    gINI.ReadString( 'SQLResultAsFile', 'FileName', lFileName ) ;
    uSaveFileName := lFileName ;
  end ;
end;

procedure TFormSQLMgrBrowse.aShowRecordExecute(Sender: TObject);
var
  lRow : TtiDataBufferRow ;
  ls : string ;
  i : integer ;
  lNameWidth : integer ;
begin
  lRow :=
    TtiDataBufferRow(
      FtiQueryDataSetMapping.TIDataSet.Items[ LV.Selected.Index ] ) ;
  ls := '' ;
  lNameWidth := 0 ;
  for i := 0 to lRow.Count - 1 do
    lNameWidth := Max( lNameWidth,
                       Length( lRow.Owner.Fields.Items[i].Name )) ;
  Inc( lNameWidth, 2 ) ;
  ls := '' ;
  for i := 0 to lRow.Count - 1 do
  begin
    ls := tiAddTrailingValue( ls, CrLf ) ;
    ls := ls +
          tiPadR( lRow.Owner.Fields.Items[i].Name + ': ', lNameWidth ) +
          ': ' + lRow.Items[i].ValueAsString ;
  end ;
  {$IFDEF FPC}
  // For some reason any popup dialog freezes the user interface. I think it
  // is a threading issue.
  Debugln(ls);
  {$ELSE}
  tiShowString( ls ) ;
  {$ENDIF}
end;

function TFormSQLMgrBrowse.ColAsProperty(pIndex: integer): string;
begin
  result := SQLParamAsProp(FCols.Strings[pIndex], FiColWidth)
end;

initialization
  {$IFDEF FPC}
  {$i FSQLMgrBrowse.lrs}
  {$ENDIF}
  uSaveFileName := '' ;

end.




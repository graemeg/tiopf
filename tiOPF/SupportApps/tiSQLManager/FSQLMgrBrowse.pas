{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  Originally developed by TechInsite Pty. Ltd.
  23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
  PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
  Phone: +61 3 9419 6456
  Fax:   +61 3 9419 1682
  Web:   www.techinsite.com.au

  This code is made available on the TechInsite web site as open source.
  You may use this code in any way you like, except to sell it to other
  developers. Please be sure to leave the file header and list of
  contributors unchanged.

  If you make any changes or enhancements, which you think will benefit other
  developers and will not break any existing code, please forward your changes
  (well commented) to TechInsite and I will make them available in the next
  version.

  Revision history:
    November 2000, Peter Hinrichsen, Made open source

  Purpose:
    Browse the results of a query

  Classes:
    TFormSQLMgrBrowse - The form
    TthrdSQLMgr       - A thread to run the query in

  ToDo:
    This form uses a TListView do display the data - which is good because
    we don't want to go near TDataSets, which would be necessary for a TDBGrid.
    But - it is very slow to load with a big result set so must come up with some
    way of using the TtiListView's Data property, but without browsing
    TPersistent(s) with published properties.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit FSQLMgrBrowse;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  tiDBConnectionPool, tiSQLMgr_BOM, ActnList, ToolWin, ComCtrls,
  ImgList, Menus, Db
  ,tiThreadProgress
  ,tiQuery
  ,tiDataSet_BOM
  ,tiDataSet_Cli
  ,tiSQLMgrDataSet_Srv // This has to be used somewhere in the ap, and this is a good a place as any
  ,tiSQLMgrDataSet_BOM
  ;

resourcestring
  crsSaveToCSVDone = 'Finished saving to file <%s>' +
                     #13 + #13 +
                     'Do you want to view this file?' ;

type


  TFormSQLMgrBrowse = class(TForm)
    sb: TStatusBar;
    DS: TDataSource;
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
    LV: TListView;
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
    procedure LVData(Sender: TObject; Item: TListItem);
    procedure aShowRecordExecute(Sender: TObject);
  private
    FCols : TStringList ;
    FiColWidth : integer ;
    FtiQueryDataSetMapping: TtiDataSetQueryMapping;
    function  ColNameText(pIndex: integer): string;
    function  ColNameDelimText(pIndex: integer): string;
    function  ColDataType(pIndex: integer): string;
    function  ColAsProperty(pIndex: integer):string;
    procedure SetTIQueryDataSetMapping( const Value: TtiDataSetQueryMapping);
  public
    property TIDataSetQueryMapping : TtiDataSetQueryMapping read FtiQueryDataSetMapping write SetTIQueryDataSetMapping ;
  end;

  // Modify to take a list of SQLMgrQuery objects as well as a single instance
  TthrdSQLMgrAbs = class( TtiThreadProgress )
  private
    FtiDataSetQueryMapping : TtiDataSetQueryMapping ;
    FsErrorText   : string ;
    FForm         : TFormSQLMgrBrowse ;

    procedure SetSQLMgrQuery(const Value: TSQLMgrQuery);
    function  GetSQLMgrQuery: TSQLMgrQuery;
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
  ,tiPersist
  ,tiDialogs
  ,tiRegINI
  ;

var
  uSaveFileName : TFileName ;

{$R *.DFM}

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

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TFormSQLMgrBrowse
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
{
procedure TFormSQLMgrBrowse.AssignQuery( Value: TtiQuery );
var
  i : integer ;
  lCol : TListColumn ;
  lListItem : TListItem ;
  liCount : integer ;
  liStart : DWord ;
  ls : string ;
begin

  LV.Items.BeginUpdate ;
  try
    for i := 0 to Value.FieldCount - 1 do begin
      lCol := LV.Columns.Add ;
      lCol.Caption := Value.FieldName( i ) ;
      FCols.Add( Value.FieldName( i )) ;

      lCol.Width   := Trunc( LV.Canvas.TextWidth( Value.FieldName( i )) * 1.2 ) ;
      FiColWidth := Max( FiColWidth, Length( Value.FieldName( i ))) ;
    end ;

    liStart := GetTickCount ;
    liCount := 0 ;
    while not Value.eof do begin
      lListItem := LV.Items.Add ;
      for i := 0 to Value.FieldCount - 1 do begin
        ls := Value.FieldAsString[ Value.FieldName( i ) ] ;
        if i = 0 then
          lListItem.Caption := ls
        else
          lListItem.SubItems.Add( ls ) ;
        LV.Columns[i].Width := Max( LV.Columns[i].Width,
                                    Trunc( LV.Canvas.TextWidth( ls ) * 1.2 ))
      end ;
      inc( liCount ) ;
      Value.Next ;
      Application.ProcessMessages ; // We are now in the main thread
    end ;

    TimeToScan    := GetTickCount - liStart ;
    RecordCount   := liCount ;
  finally
    LV.Items.EndUpdate ;
  end ;
end;
}
procedure TFormSQLMgrBrowse.FormCreate(Sender: TObject);
begin
  LV.Align := alClient ;
  gINI.ReadFormState( self ) ;
  FCols := TStringList.Create ;
  FiColWidth := 0 ;
end;

procedure TFormSQLMgrBrowse.FormDestroy(Sender: TObject);
begin
  gINI.WriteFormState( self ) ;
  FCols.Free ;
  FtiQueryDataSetMapping.Free ;
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
  lFileName := gINI.ReadString( Name, 'FileName', '' ) ;
  lFileName := GetSQLSaveFileName( lFileName ) ;
  tiDataSetToTextFile( FtiQueryDataSetMapping.TIDataSet, lFileName ) ;
  if lFileName <> '' then
  begin
    gINI.WriteString( Name, 'FileName', lFileName ) ;
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
  FtiDataSetQueryMapping := TtiDataSetQueryMapping.Create ;
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
  tiMessageDlg( FsErrorText,
                ['Continue'],
                mtError,
                'Error running query' ) ;
end ;

procedure TthrdSQLMgrAbs.Execute;
begin
//  if gTIPerMgr.VisMgr.Execute( cgsPopulateTIDataSet, FtiDataSetQueryMapping ) <> '' then
  FsErrorText := gTIPerMgr.Read( FtiDataSetQueryMapping ) ;
  if FtiDataSetQueryMapping.ErrorMessage <> '' then
    FsErrorText := 
      FsErrorText + #13 +
      FtiDataSetQueryMapping.ErrorMessage ;
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

  ls := 'insert into <Table_Name>' + CrLf +
        '(' + CrLf ;

  for i := 0 to FCols.Count - 1 do
  begin
    if i = 0 then
      ls := ls + '     '
    else
      ls := ls + '    ,' ;
    ls := ls + ColNameText(i) + CrLf ;
  end ;

  ls := ls + ')' + CrLf + 'Values' + CrLf + '(' + CrLf ;

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
  tiAppMessage( 'SQL copied to clipboard' ) ;

end;

procedure TFormSQLMgrBrowse.aSQLUpdateExecute(Sender: TObject);
var
  ls : string ;
  i  : integer ;
begin

  ls := 'update <Table_Name>' + CrLf +
        'Set' + CrLf ;

  for i := 0 to FCols.Count - 1 do
  begin
    if i = 0 then
      ls := ls + '     '
    else
      ls := ls + '    ,' ;
    ls := ls + ColNameText(i) + ' = :' + ColNameText(i) + CrLf ;
  end ;

  ls := ls + 'where' + CrLf ;

  for i := 0 to FCols.Count - 1 do
  begin
    if i = 0 then
      ls := ls + '     '
    else
      ls := ls + 'and  ' ;
    ls := ls + ColNameText(i) + ' = :Old_' + ColNameText(i) + CrLf ;
  end ;

  Clipboard.AsText := ls ;
  tiAppMessage( 'SQL copied to clipboard' ) ;

end;

procedure TFormSQLMgrBrowse.aSQLDeleteExecute(Sender: TObject);
var
  ls : string ;
  i  : integer ;
begin

  ls := 'delete from <Table_Name>' + CrLf +
        'where' + CrLf ;

  for i := 0 to FCols.Count - 1 do
  begin
    if i = 0 then
      ls := ls + '     '
    else
      ls := ls + 'and  ' ;
    ls := ls +
          ColNameText( i ) + ' = :' + ColNameText(i) + CrLf ;
  end ;

  Clipboard.AsText := ls ;
  tiAppMessage( 'SQL copied to clipboard' ) ;

end;

procedure TFormSQLMgrBrowse.aSQLReadExecute(Sender: TObject);
var
  ls : string ;
  i  : integer ;
begin
  ls := 'select' + CrLf ;

  for i := 0 to FCols.Count - 1 do
  begin
    if i = 0 then
      ls := ls + '     '
    else
      ls := ls + '    ,' ;
    ls := ls + ColNameText(i) + CrLf ;
  end ;

  ls := ls + 'from' + CrLf +
        '<Table_Name>' + CrLf ;

  ls := ls + 'where' + CrLf ;

  for i := 0 to FCols.Count - 1 do
  begin
    if i = 0 then
      ls := ls + '     '
    else
      ls := ls + 'and  ' ;
    ls := ls + ColNameText(i) + ' = :' + ColNameText(i) + CrLf ;
  end ;


  Clipboard.AsText := ls ;
  tiAppMessage( 'SQL copied to clipboard' ) ;

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
    '  lData : T<Class type> ;'         + CrLf +
    'begin'                             + CrLf +
    '  lData := T<Class type>.Create ;' + CrLf ;

  for i := 0 to FCols.Count - 1 do
  begin
    ls := ls +
          '  ' +
          'lData.' + ColAsProperty(i) + ' := Query.FieldAs' +
          tiPadR( ColDataType(i) + '[ ', 10 ) +
          ColNameDelimText(i) + ' ] ;' + Cr ;
  end ;

  ls := ls + '  lData.ObjectState := posClean ;' + Cr ;
  ls := ls + '  TPerVisList( Visited ).Add( lData ) ;' + Cr ;

  Clipboard.AsText := ls ;
  tiAppMessage( 'Delphi code copied to clipboard' ) ;
  
end;

procedure TFormSQLMgrBrowse.aSQLAsSetupParamsExecute(Sender: TObject);
var
  ls : string ;
  i  : integer ;
begin
  ls :=
    'var'                            + Cr +
    '  lData : T<Class type> ;'         + Cr +
    'begin'                          + Cr +
    '  lData := T<Class type>( Visited ) ;' + Cr ;

  for i := 0 to FCols.Count - 1 do
  begin
    ls := ls +
          '  Query.ParamAs[ ' +
          ColNameDelimText(i) + ' ] := lData.' + ColAsProperty(i) + ' ;' + Cr ;
  end ;

  Clipboard.AsText := ls ;
  tiAppMessage( 'SQL copied to clipboard' ) ;
end;

procedure TFormSQLMgrBrowse.aSQLAsClassInterfaceExecute(Sender: TObject);
var
  ls : string ;
  i  : integer ;
begin
  ls := '  T<ClassName> = class( TPerObjAbs )' + CrLf +
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
          ' ;' + CrLf ;
  end ;

  ls := ls +
        '  end ;' + CrLf( 2 ) ;

  Clipboard.AsText := ls ;
  tiAppMessage( 'Class stub copied to clipboard' ) ;
end;

procedure TthrdSQLMgrAbs.DoShowNoResultSet(sender: TObject);
begin
  tiAppMessage( 'Query executed. There is no result set.' ) ;
end;

procedure TFormSQLMgrBrowse.SetTIQueryDataSetMapping( const Value: TtiDataSetQueryMapping);
var
  i : integer ;
begin
  FtiQueryDataSetMapping := Value;
  Caption := ' Results for query: ' + FtiQueryDataSetMapping.SQLMgrQuery.Caption ;
  tiDataSetToListView( FtiQueryDataSetMapping.TIDataSet, LV ) ;
  SB.Panels[0].Text := 'Record count: ' + intToStr( FtiQueryDataSetMapping.TIDataSet.Count ) ;
  SB.Panels[1].Text := 'Time to execute on server: '  + IntToStr( FtiQueryDataSetMapping.TimeToRun ) + 'ms' ;
  SB.Panels[2].Text := 'Time to download dara: '  + IntToStr( FtiQueryDataSetMapping.TimeToScan ) + 'ms' ;
  FCols.Clear ;
  for i := 0 to FtiQueryDataSetMapping.TIDataSet.Fields.Count - 1 do
    FCols.Add(FtiQueryDataSetMapping.TIDataSet.Fields.Items[i].Name);
  FiColWidth := 0 ;
  for i := 0 to FCols.Count - 1 do
    FiColWidth := Max( FiColWidth, Length( FCols.Strings[i] )) ;
end;

procedure TFormSQLMgrBrowse.LVData(Sender: TObject; Item: TListItem);
begin
  tiDataSetToListItem(FtiQueryDataSetMapping.TIDataSet, Item);
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
  lRow : TtiDataSetRow ;
  ls : string ;
  i : integer ;
  lNameWidth : integer ;
begin
  lRow :=
    TtiDataSetRow(
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
    ls := tiAddTrailingValue( ls, Cr ) ;
    ls := ls +
          tiPadR( lRow.Owner.Fields.Items[i].Name + ': ', lNameWidth ) +
          ': ' + lRow.Items[i].ValueAsString ;
  end ;
  tiShowString( ls ) ;
end;

function TFormSQLMgrBrowse.ColAsProperty(pIndex: integer): string;
begin
  result := SQLParamAsProp(FCols.Strings[pIndex], FiColWidth)
end;

initialization
  uSaveFileName := '' ;

end.




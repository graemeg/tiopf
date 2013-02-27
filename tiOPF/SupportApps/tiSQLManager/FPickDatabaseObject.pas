{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Peter Hinrichsen, TechInsite Pty. Ltd.
  as the tiOPF (TechInsite Object Persistence Framework)

    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm

  Please submit changes to tiOPF@techinsite.com.au

  Revision history:

  Purpose:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit FPickDatabaseObject;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, tiTreeView, ComCtrls, ActnList, ToolWin, tiButtons, ImgList,
  Buttons, tiPerAwareCtrls, tiQuery, tiPtnVisPerObj, tiFocusPanel ;

type

  TSQLFormatters   = class ;
  TSQLFormatterAbs = class ;

  //----------------------------------------------------------------------------
  TSQLFormatters = class( TPerObjList )
  protected
    function    GetItems(i: integer): TSQLFormatterAbs ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TSQLFormatterAbs); reintroduce ;
  public
    constructor Create ; override ;
    property    Items[i:integer] : TSQLFormatterAbs read GetItems write SetItems ;
    procedure   Add( pObject : TSQLFormatterAbs   ; pDefDispOrdr : boolean = true ) ; reintroduce ;
  end ;

  //----------------------------------------------------------------------------
  TSQLFormatterAbs = class( TPerObjAbs )
  private
    FData : TPerObjAbs ;
  protected
    function    GetOwner: TSQLFormatters; reintroduce ;
    procedure   SetOwner(const Value: TSQLFormatters); reintroduce ;
    property    Data : TPerObjAbs read FData write FData ;
    function    GetCurrentTable : TtiDBMetaDataTable ;
    function    GetCurrentField : TtiDBMetaDataField ;
    function    GetCurrentTableName : string ;
    function    GetCurrentFieldName : string ;
    function    GetAllFields: string;
  public
    function    GetSQL( pData : TPerObjAbs ) : String ; virtual ;
    property    Owner       : TSQLFormatters             read GetOwner      write SetOwner ;
  end ;

  TSQLFormatterCurrentObject = class( TSQLFormatterAbs )
  protected
    function GetCaption: string; override ;
  public
    function    GetSQL( pData : TPerObjAbs ) : String ; override ;
  end ;

  TSQLFormatterAllFields = class( TSQLFormatterAbs )
  protected
    function GetCaption: string; override ;
  public
    function    GetSQL( pData : TPerObjAbs ) : String ; override ;
  end ;

  TSQLFormatterSelectSQL = class( TSQLFormatterAbs )
  protected
    function GetCaption: string; override ;
  public
    function    GetSQL( pData : TPerObjAbs ) : String ; override ;
  end ;

  TSQLFormatterCreateSQL = class( TSQLFormatterAbs )
  protected
    function GetCaption: string; override ;
  public
    function    GetSQL( pData : TPerObjAbs ) : String ; override ;
  end ;

  TSQLFormatterUpdateSQL = class( TSQLFormatterAbs )
  protected
    function GetCaption: string; override ;
  public
    function    GetSQL( pData : TPerObjAbs ) : String ; override ;
  end ;

  TSQLFormatterDeleteSQL = class( TSQLFormatterAbs )
  protected
    function GetCaption: string; override ;
  public
    function    GetSQL( pData : TPerObjAbs ) : String ; override ;
  end ;

  TSQLFormatterDropTable = class( TSQLFormatterAbs )
  protected
    function GetCaption: string; override ;
  public
    function    GetSQL( pData : TPerObjAbs ) : String ; override ;
  end ;

  TSQLFormatterCreateTableSQL = class( TSQLFormatterAbs )
  protected
    function    GetCaption: string; override ;
  public
    function    GetSQL( pData : TPerObjAbs ) : String ; override ;
  end ;

  TFormPickDatabaseObject = class(TForm)
    TV: TtiTreeView;
    AL: TActionList;
    aInsCurrentObject: TAction;
    aClose: TAction;
    ilButtons: TImageList;
    ilTV: TImageList;
    aRefresh: TAction;
    Panel1: TPanel;
    sbClose: TSpeedButton;
    sbCurrentObject: TSpeedButton;
    sbRefresh: TSpeedButton;
    SpeedButton1: TSpeedButton;
    aInsertSQL: TAction;
    paeAction: TtiPerAwareComboBoxDynamic;
    aInsertStatement: TAction;
    procedure FormCreate(Sender: TObject);
    procedure TVSelectNode(ptiTreeView: TtiTreeView; pNode: TTreeNode;
      pData: TObject; pParentNode: TTreeNode; pParentData: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure aInsCurrentObjectExecute(Sender: TObject);
    procedure ALUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure aCloseExecute(Sender: TObject);
    procedure aRefreshExecute(Sender: TObject);
    procedure aInsertStatementExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FText : string ;
    FData : TtiDBMetaData ;
    FSQLFormatters : TSQLFormatters ;
  public
    function Execute( pPoint : TPoint ) : String ;
  end;


implementation
uses
  tiPtnVisMgr
  ,tiUtils
  ,tiPersist
  ,Math
  ,tiRegINI
  ;

{$R *.DFM}

const
  cusPad = '    ' ;
  cusCR  = #13 + #10 ;

function TFormPickDatabaseObject.Execute( pPoint : TPoint ): String;
begin
  result := '' ;
  if pPoint.y < Screen.Height - Height then
    Top  := pPoint.y
  else
    Top := Screen.Height - Height ;

  if pPoint.x < Screen.Width - Width then
    Left := pPoint.x
  else
    Left := Screen.Width - Width ;
  ShowModal ;
  result := FText ;
end;

procedure TFormPickDatabaseObject.FormCreate(Sender: TObject);
var
  i : integer ;
begin
  TV.Align := alClient ;
  FData := gtiPerMgr.DefaultPerLayer.DBConnectionPools.Find( gTIPerMgr.DefaultDBConnectionName ).MetaData ;
  TV.Data := FData ;
  TV.SelectedAddress := gINI.ReadString( Name, 'SelectedAddress', '' ) ;
  FSQLFormatters := TSQLFormatters.Create ;
  paeAction.List := FSQLFormatters.List ;
  paeAction.FieldNameDisplay := 'Caption' ;
  i := Min( FSQLFormatters.Count-1,
            gINI.ReadInteger( Name, 'Action', -1 )) ;
  if i <> -1 then
    paeAction.Value :=
      FSQLFormatters.Items[i] ;
end;

procedure TFormPickDatabaseObject.FormDestroy(Sender: TObject);
begin
  gINI.WriteInteger( name, 'width', width ) ;
  gINI.WriteInteger( name, 'height', height ) ;
  gINI.WriteString( Name, 'SelectedAddress', TV.SelectedAddress ) ;
  gINI.WriteInteger( Name, 'Action',
    FSQLFormatters.IndexOf( paeAction.Value )) ;
  FSQLFormatters.Free ;
end;
{
  if ( Key = VK_RETURN ) or
     ( Key = VK_SPACE ) then
  begin
    aInsCurrentObjectExecute( nil ) ;
    Key := 0 ;
  end ;
}

procedure TFormPickDatabaseObject.TVSelectNode(ptiTreeView: TtiTreeView;
  pNode: TTreeNode; pData: TObject; pParentNode: TTreeNode;
  pParentData: TObject);
begin
  if not( pData is TtiDBMetaDataTable ) then
    Exit ; //==>
  TtiDBMetaDataTable( pData ).Read ;
  pNode.HasChildren := TtiDBMetaDataTable( pData ).Count > 0 ;
end;

procedure TFormPickDatabaseObject.aInsCurrentObjectExecute( Sender: TObject);
var
  lSQLFormatter : TSQLFormatterAbs ;
begin
  lSQLFormatter := TSQLFormatterCurrentObject.Create ;
  try
    FText := lSQLFormatter.GetSQL(TPerObjAbs(TV.SelectedData )) ;
  finally
    lSQLFormatter.Free ;
  end ;
  Close ;
end;

function TSQLFormatterAbs.GetAllFields : string ;
var
  lTable : TtiDBMetaDataTable ;
  i : integer ;
begin

  lTable := GetCurrentTable ;

  if lTable = nil then
    Exit ; //==>

  for i := 0 to lTable.Count - 1 do
  begin
    result := tiAddTrailingValue( result, cusCR + '    ,' ) ;
    result := result + lTable.Name + '.' + lTable.Items[i].Name ;
  end ;

  if result <> '' then
    result := cusPad + result ;

end;

procedure TFormPickDatabaseObject.ALUpdate(Action: TBasicAction; var Handled: Boolean);
var
  lSelectedData : boolean ;
begin
  lSelectedData := TV.SelectedData <> nil ;
  aInsCurrentObject.Enabled := lSelectedData ;
  aInsertStatement.Enabled  :=
    ( paeAction.Value <> nil ) and
    ( TV.SelectedData <> nil );
  Handled := true ;
end;

function TSQLFormatterAbs.GetCurrentField: TtiDBMetaDataField;
begin
  if ( FData is TtiDBMetaDataField ) then
    result := TtiDBMetaDataField( FData )
  else
    result := nil ;
end;

function TSQLFormatterAbs.GetCurrentFieldName: string;
var
  lField : TtiDBMetaDataField ;
begin
  lField := GetCurrentField ;
  if lField <> nil then
    result := lField.Name
  else
    result := '' ;
end;

function TSQLFormatterAbs.GetCurrentTable: TtiDBMetaDataTable;
begin
  if ( FData is TtiDBMetaDataTable ) then
    result := TtiDBMetaDataTable( FData )
  else
  if ( FData is TtiDBMetaDataField ) then
    result := TtiDBMetaDataField( FData ).Owner
  else
    result := nil ;
end;

function TSQLFormatterAbs.GetCurrentTableName: string;
var
  lTable : TtiDBMetaDataTable ;
begin
  lTable := GetCurrentTable ;
  if lTable <> nil then
    result := lTable.Name
  else
    result := '' ;
end;

procedure TFormPickDatabaseObject.aCloseExecute(Sender: TObject);
begin
  FText := '' ;
  Close ;
end;

procedure TFormPickDatabaseObject.aRefreshExecute(Sender: TObject);
begin
  TV.Data := nil ;
  FData.Clear ;
  FData.Read ;
  TV.Data := FData ;
end;

{ TSQLFormatterInsertAllFields }

function TSQLFormatterAllFields.GetSQL( pData : TPerObjAbs ) : String ;
begin
  inherited GetSQL( pData ) ;
  result := GetAllFields ;
end;

function TSQLFormatterAllFields.GetCaption: string;
begin
  result := 'Insert all fields' ;
end;

{ TSQLFormatterSelectSQL }

function TSQLFormatterSelectSQL.GetSQL(pData: TPerObjAbs):String;
begin
  inherited GetSQL( pData ) ;
  result := GetAllFields ;
  if Result <> '' then
    result :=
      'select' + cusCR +
      result + cusCR +
      'from' + cusCR +
      cusPad +
      GetCurrentTableName ;
end;

function TSQLFormatterSelectSQL.GetCaption: string;
begin
  result := 'Insert SELECT SQL' ;
end;

{ TSQLFormatters }

procedure TSQLFormatters.Add(pObject: TSQLFormatterAbs; pDefDispOrdr: boolean);
begin
  inherited Add( pObject, pDefDispOrdr ) ;
end;

constructor TSQLFormatters.Create;
begin
  inherited;
  Add( TSQLFormatterAllFields.Create ) ;
  Add( TSQLFormatterSelectSQL.Create ) ;
  Add( TSQLFormatterCreateSQL.Create ) ;
  Add( TSQLFormatterUpdateSQL.Create ) ;
  Add( TSQLFormatterDeleteSQL.Create ) ;
  Add( TSQLFormatterDropTable.Create ) ;
  Add( TSQLFormatterCreateTableSQL.Create ) ;
end;

function TSQLFormatters.GetItems(i: integer): TSQLFormatterAbs;
begin
  result := TSQLFormatterAbs( inherited GetItems( i )) ;
end;

procedure TSQLFormatters.SetItems(i: integer; const Value: TSQLFormatterAbs);
begin
  inherited SetItems( i, Value ) ;
end;

{ TSQLFormatterAbs }

function TSQLFormatterAbs.GetOwner: TSQLFormatters;
begin
  result := TSQLFormatters( inherited GetOwner ) ;
end;

function TSQLFormatterAbs.GetSQL(pData: TPerObjAbs): String;
begin
  FData := pData ;
end;

procedure TSQLFormatterAbs.SetOwner(const Value: TSQLFormatters);
begin
  inherited SetOwner( Value ) ;
end;

procedure TFormPickDatabaseObject.aInsertStatementExecute(Sender: TObject);
begin
  FText := TSQLFormatterAbs( paeAction.Value ).GetSQL(TPerObjAbs(TV.SelectedData )) ;
  Close ;
end;

{ TSQLFormatterCurrentObject }

function TSQLFormatterCurrentObject.GetCaption: string;
begin
  result := 'Insert current object' ;
end;

function TSQLFormatterCurrentObject.GetSQL(pData: TPerObjAbs): String;
begin
  inherited GetSQL( pData ) ;
  if GetCurrentFieldName <> '' then
    result := GetCurrentTableName + '.' +
              GetCurrentFieldName
  else
  if GetCurrentTableName <> '' then
    result := GetCurrentTableName
  else
    result := '' ;
end;

{ TSQLFormatterCreateSQL }

function TSQLFormatterCreateSQL.GetCaption: string;
begin
  result := 'Insert INSERT SQL' ;
end;

function TSQLFormatterCreateSQL.GetSQL(pData: TPerObjAbs): String;
var
  lTable : TtiDBMetaDataTable ;
  i : integer ;
  lFields : string ;
  lParams : string ;
begin
  inherited GetSQL( pData ) ;

  lTable := GetCurrentTable ;

  if lTable = nil then
    Exit ; //==>

  lFields := '' ;
  lParams := '' ;
  for i := 0 to lTable.Count - 1 do
  begin
    lFields := tiAddTrailingValue( lFields, cusCR + '    ,' ) ;
    lFields := lFields + lTable.Items[i].Name ;

    lParams := tiAddTrailingValue( lParams, cusCR + '    ,' ) ;
    lParams := lParams + ':' + lTable.Items[i].Name ;

  end ;

  if ( lFields <> '' ) and
     ( lParams <> '' ) then
    result := 'insert into ' + lTable.Name + cusCR +
              '(' + cusCR +
              '     ' + lFields + cusCR +
              ')' + cusCR +
              'values' + cusCR +
              '(' + cusCR +
              '     ' + lParams +
              ')' ;
end;

{ TSQLFormatterUpdateSQL }

function TSQLFormatterUpdateSQL.GetCaption: string;
begin
  result := 'Insert UPDATE SQL' ;
end;

function TSQLFormatterUpdateSQL.GetSQL(pData: TPerObjAbs): String;
var
  lTable : TtiDBMetaDataTable ;
  i : integer ;
  lFields : string ;
  lParams : string ;
begin
  inherited GetSQL( pData ) ;

  lTable := GetCurrentTable ;

  if lTable = nil then
    Exit ; //==>

  lFields := '' ;
  lParams := '' ;
  for i := 0 to lTable.Count - 1 do
  begin
    lFields := tiAddTrailingValue( lFields, cusCR + '    ,' ) ;
    lFields := lFields +
               lTable.Items[i].RPadName + ' = :' +
               lTable.Items[i].Name ;

    lParams := tiAddTrailingValue( lParams, cusCR + '    ,' ) ;
    lParams := lParams +
               lTable.Items[i].RPadName + ' = :Old_' +
               lTable.Items[i].Name ;

  end ;

  if ( lFields <> '' ) and
     ( lParams <> '' ) then
    result := 'update ' + lTable.Name + cusCR +
              'set' + cusCR +
              '     ' + lFields + cusCR +
              'where' + cusCR +
              '     ' + lParams ;
end;

{ TSQLFormatterDeleteSQL }

function TSQLFormatterDeleteSQL.GetCaption: string;
begin
  result := 'Insert DELETE SQL' ;
end;

function TSQLFormatterDeleteSQL.GetSQL(pData: TPerObjAbs): String;
var
  lTable : TtiDBMetaDataTable ;
  i : integer ;
  lParams : string ;
begin
  inherited GetSQL( pData ) ;

  lTable := GetCurrentTable ;

  if lTable = nil then
    Exit ; //==>

  lParams := '' ;
  for i := 0 to lTable.Count - 1 do
  begin
    lParams := tiAddTrailingValue( lParams, cusCR + 'and ' ) ;
    lParams := lParams +
               lTable.Items[i].RPadName + ' = :Old_' +
               lTable.Items[i].RPadName ;
  end ;

  if ( lParams <> '' ) then
    result := 'delete from ' + lTable.Name + cusCR +
              'where' + cusCR +
              '    ' + lParams ;
end;

procedure TFormPickDatabaseObject.FormShow(Sender: TObject);
begin
  Width  := gINI.ReadInteger( name, 'width', width ) ;
  Height := gINI.ReadInteger( name, 'height', height ) ;
end;

{ TSQLFormatterDropTable }

function TSQLFormatterDropTable.GetCaption: string;
begin
  result := 'Drop table' ;
end;

function TSQLFormatterDropTable.GetSQL(pData: TPerObjAbs): String;
var
  lTable : TtiDBMetaDataTable ;
begin
  inherited GetSQL( pData ) ;

  lTable := GetCurrentTable ;

  if lTable = nil then
    Exit ; //==>

  result := 'drop table ' + lTable.Name ;

end;

{ TSQLFormatterCreateTableSQL }

function TSQLFormatterCreateTableSQL.GetCaption: string;
begin
  result := 'Create table SQL' ;
end;

function TSQLFormatterCreateTableSQL.GetSQL(pData: TPerObjAbs): String;
var
  lTable : TtiDBMetaDataTable ;
  i : integer ;
  lFields : string ;
begin
  inherited GetSQL( pData ) ;

  lTable := GetCurrentTable ;

  if lTable = nil then
    Exit ; //==>

  lFields := '' ;
  for i := 0 to lTable.Count - 1 do
  begin
    lFields := tiAddTrailingValue( lFields, cusCR + '    ,' ) ;
    lFields := lFields +
               lTable.Items[i].RPadName + ' ' +
               'DataTypeUnderConstruction' ;
  end ;

  if ( lFields <> '' ) then
    result := 'create table ' + lTable.Name + cusCR +
              '(' +
              '    ' + lFields + cusCR +
              ')' ;

end;

end.

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

unit tiSQLMgrDataSet_Srv;

interface
uses
  tiPtnVis
  ,tiPtnVisSQL
  ,tiPtnVisPerObj
  ,tiDataSet_BOM
  ,tiSQLMgrDataSet_BOM
  ,tiSQLMgr_BOM
  ;

type

  //----------------------------------------------------------------------------
  TVisTIDataSetRead = class( TtiPerObjVisitor )
  private
    FSQLMgrQuery : TSQLMgrQuery ;
    FtiDataSet   : TtiDataSet ;
    procedure   SetupQuery;
    procedure   ReadMetaData;
    procedure   MapRowToObject;
  protected
    function    AcceptVisitor  : boolean ; override ;
  public
    procedure   Execute( const pData : TVisitedAbs ) ; override ;
  end ;

implementation
uses
  SysUtils
  ,tiUtils
  ,tiQuery
  ,tiPtnVisPerObj_Cli
  ,Windows
  ,tiPersist
  ;

{ TVisTIDataSetRead }

function TVisTIDataSetRead.AcceptVisitor: boolean;
begin
  result := ( Visited is TtiDataSetQueryMapping ) and
            (( Visited.ObjectState = posEmpty ) or
             ( TtiDataSetQueryMapping( Visited ).SQLMgrQuery.Dirty )) ;
end;

procedure TVisTIDataSetRead.Execute(const pData: TVisitedAbs);
var
  liTimeToRun  : DWord ;
  liTimeToScan : DWord ;
begin
  inherited Execute( pData ) ;

  if not AcceptVisitor then
    Exit ; //==>

  FSQLMgrQuery := TtiDataSetQueryMapping( Visited ).SQLMgrQuery ;
  FtiDataSet   := TtiDataSetQueryMapping( Visited ).TIDataSet ;

  // 1. Copy the SQL and Parameters to the TtiQuery
  SetupQuery ;

  // 2. Empty the TtiDataSet
  FtiDataSet.Clear ;

  liTimeToRun := GetTickCount ;
  // 3. Execute the TtiQuery
  try
    // mmm, not sure what to do about this. Some sql engines will allow
    // either ExecSQL or Open to be used, others will require the correct
    // call. (For example, the BDE requires open to be called for SELECT sql,
    // and ExecSQL for UPDATE.
    // Will have to:
    //   a) Parse the SQL, and decide if it's a select or create; or
    //   b) try select, then if it fails, try ExecSQL
    if Query.QueryType = qtSelect then
      Query.Open
    else
      Query.ExecSQL ;
  except
    on e:exception do
    begin
      TtiDataSetQueryMapping( Visited ).ErrorMessage :=
        'Error: ' + e.message ;
      tiFmtException( e, className, 'Execute' ) ;
    end ;
  end ;

  liTimeToRun := GetTickCount - liTimeToRun ;

  if Query.QueryType = qtSelect then
  begin
    // 4. Read in the meta data
    ReadMetaData ;

    liTimeToScan := GetTickCount ;
    // 5. Call MapRowToObject
    while not Query.EOF do
    begin
      MapRowToObject ;
      Query.Next ;
    end ;
    // It would be nice if this was inside a finally block
    // but this is a bit tricky to do because of all the
    // other things going on.
    Query.Close ;
    liTimeToScan := GetTickCount - liTimeToScan ;
  end
  else
    liTimeToScan := 0 ;

  Final ;

  TtiDataSetQueryMapping( Visited ).TimeToRun  := liTimeToRun ;
  TtiDataSetQueryMapping( Visited ).TimeToScan := liTimeToScan ;

end;

procedure TVisTIDataSetRead.SetupQuery ;
var
  lParam : TSQLMgrParam ;
  i : integer;
begin

  // This should be made thread safe...
  FSQLMgrQuery := TtiDataSetQueryMapping( Visited ).SQLMgrQuery ;

  Query.SQLText := FSQLMgrQuery.SQL ;

  for i := 0 to FSQLMgrQuery.Params.Count - 1 do
  begin
    lParam := TSQLMgrParam( FSQLMgrQuery.Params.Items[i] ) ;

    if lParam.Deleted then
      System.Continue ; //==>

    if lParam.IsNull then
      Query.ParamIsNull[ lParam.ParamName ] := True
    else
    begin
      case lParam.ParamType of
        qfkString   : Query.ParamAsString[   lParam.ParamName ] := lParam.ParamValue;
        qfkInteger  : Query.ParamAsInteger[  lParam.ParamName ] := StrToInt64( lParam.ParamValue );
        qfkFloat    : Query.ParamAsFloat[    lParam.ParamName ] := StrToFloat( lParam.ParamValue );
        qfkDateTime : Query.ParamAsDateTime[ lParam.ParamName ] := StrToDateTime( lParam.ParamValue );
        qfkMacro    : Query.ParamAsMacro[    lParam.ParamName ] := lParam.ParamValue;
        else
          raise exception.Create( 'Invalid param type passed to TthrdSQLMgr.Execute' ) ;
      end;
    end;
  end ;
end ;

procedure TVisTIDataSetRead.ReadMetaData ;
var
  i : integer ;
  lField : TtiDBMetaDataField ;
begin
  for i := 0 to Query.FieldCount - 1 do
  begin
    lField := TtiDBMetaDataField.Create ;
    lField.ObjectState := posClean ;
    lField.Name      := Query.FieldName( i ) ;
    lField.Kind := Query.FieldKind( i ) ;
    FtiDataSet.Fields.Add( lField ) ;
  end ;
end ;

procedure TVisTIDataSetRead.MapRowToObject ;
var
  i     : integer ;
  lRow  : TtiDataSetRow ;
  lCell : TtiDataSetCell ;
begin
  lRow := TtiDataSetRow.Create ;
  FtiDataSet.Add( lRow ) ;
  for i := 0 to Query.FieldCount - 1 do
  begin
    lCell := TtiDataSetCell.Create ;
    lCell.ValueAsString := Query.FieldAsString[ Query.FieldName( i )] ;
    lRow.Add( lCell ) ;
  end ;
end ;


{
procedure TVisTIDataSetRead.Final;
begin
  Visited.ObjectState := posClean ;
end;
}

initialization

  gTIPerMgr.RegReadVisitor( TVisTIDataSetRead ) ;
  //gVisMgr.RegisterVisitor( cgsPopulateTIDataSet, TVisTIDataSetRead ) ;

end.

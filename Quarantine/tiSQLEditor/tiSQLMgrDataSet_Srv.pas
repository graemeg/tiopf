unit tiSQLMgrDataSet_Srv;

{$I tiDefines.inc}

interface
uses
  tiVisitor
  ,tiVisitorDB
  ,tiObject
  ,tiDataBuffer_BOM
  ,tiSQLMgrDataSet_BOM
  ,tiSQLMgr_BOM
  ;

type

  TVisTIDataSetRead = class( TtiObjectVisitor )
  private
    FSQLMgrQuery : TSQLMgrQuery ;
    FtiDataSet   : TtiDataBuffer ;
    procedure   SetupQuery;
    procedure   ReadMetaData;
    procedure   MapRowToObject;
  protected
    function    AcceptVisitor  : boolean ; override ;
  public
    procedure   Execute( const pData : TtiVisited ) ; override ;
  end ;

implementation
uses
   tiUtils
  ,tiQuery
  ,tiGUIUtils
  ,tiOPFManager
  ,tiExcept
  ,SysUtils
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF}
  ;

{ TVisTIDataSetRead }

function TVisTIDataSetRead.AcceptVisitor: boolean;
begin
  result := ( Visited is TtiDataBufferQueryMapping ) and
            (( Visited.ObjectState = posEmpty ) or
             ( TtiDataBufferQueryMapping( Visited ).SQLMgrQuery.Dirty )) ;
end;

procedure TVisTIDataSetRead.Execute(const pData: TtiVisited);
var
  liTimeToRun  : DWord ;
  liTimeToScan : DWord ;
begin
  inherited Execute( pData ) ;

  if not AcceptVisitor then
    Exit ; //==>

  FSQLMgrQuery := TtiDataBufferQueryMapping( Visited ).SQLMgrQuery ;
  FtiDataSet   := TtiDataBufferQueryMapping( Visited ).TIDataSet ;

  // 1. Copy the SQL and Parameters to the TtiQuery
  SetupQuery ;

  // 2. Empty the TtiDataBuffer
  FtiDataSet.Clear ;

  liTimeToRun := tiGetTickCount;
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
      TtiDataBufferQueryMapping( Visited ).ErrorMessage :=
        'Error: ' + e.message ;
      raise ;
    end ;
  end ;

  liTimeToRun := tiGetTickCount - liTimeToRun;

  if Query.QueryType = qtSelect then
  begin
    // 4. Read in the meta data
    ReadMetaData ;

    liTimeToScan := tiGetTickCount;
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
    liTimeToScan := tiGetTickCount - liTimeToScan;
  end
  else
    liTimeToScan := 0 ;

//  Final ;

  TtiDataBufferQueryMapping( Visited ).TimeToRun  := liTimeToRun ;
  TtiDataBufferQueryMapping( Visited ).TimeToScan := liTimeToScan ;

end;

procedure TVisTIDataSetRead.SetupQuery ;
var
  lParam : TSQLMgrParam ;
  i : integer;
begin

  // This should be made thread safe...
  FSQLMgrQuery := TtiDataBufferQueryMapping( Visited ).SQLMgrQuery ;

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
  lRow  : TtiDataBufferRow ;
  lCell : TtiDataBufferCell ;
begin
  lRow := TtiDataBufferRow.Create(Query.FieldCount) ;
  FtiDataSet.Add( lRow ) ;
  for i := 0 to Query.FieldCount - 1 do
  begin
    lCell := TtiDataBufferCell.Create ;
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

  gTIOPFManager.RegReadVisitor( TVisTIDataSetRead ) ;
  //gVisMgr.RegisterVisitor( cgsPopulateTIDataSet, TVisTIDataSetRead ) ;

end.

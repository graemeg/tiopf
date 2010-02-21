{

  Purpose:

  ToDo:

}


unit tiSQLMgrDataSet_BOM;

{$I tiDefines.inc}

interface
uses
   Classes
  ,tiObject
//  ,tiVisitor
  ,tiQuery
  ,tiSQLMgr_BOM
  ,tiDataBuffer_BOM
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF}
  {$IFNDEF VER130}
   ,Variants
  {$ENDIF}  
  ;


type

  TtiSQLMgrDataSet = class( TtiDataBuffer )
  private
    FtiQuery: TSQLMgrQuery;
  public
    property    TIQuery : TSQLMgrQuery read FtiQuery write FtiQuery ;
  end;


  TtiDataBufferQueryMapping  = class;


  TtiDataBufferQueryMappings = class( TtiObjectList )
  private
  protected
    function  GetItems(pIndex: integer): TtiDataBufferQueryMapping;reintroduce ;
    procedure SetItems(pIndex: integer;  const Value: TtiDataBufferQueryMapping);reintroduce ;
  public
    property Items[ pIndex : integer ] : TtiDataBufferQueryMapping read GetItems write SetItems ;
    function FindBySQLMgrQuery( pSQLMgrQuery : TSQLMgrQuery ) : TtiDataBufferQueryMapping ;
  end;


  TtiDataBufferQueryMapping = class( TtiObject )
  private
    FTIDataSet: TtiDataBuffer;
    FSQLMgrQuery: TSQLMgrQuery;
    FsErrorMessage: string;
    FiTimeToRun: DWord;
    FiTimeToScan: DWord;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    property    SQLMgrQuery : TSQLMgrQuery read FSQLMgrQuery write FSQLMgrQuery ;
    property    TIDataSet   : TtiDataBuffer     read FTIDataSet   write FTIDataSet ;
    property    ErrorMessage : string read FsErrorMessage write FsErrorMessage ;
    property    TimeToRun    : DWord read FiTimeToRun  write FiTimeToRun ;
    property    TimeToScan   : DWord read FiTimeToScan write FiTimeToScan ;
  end;


implementation
uses
  SysUtils
//  ,tiUtils
  ;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiDataBufferQueryMappings
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TtiDataBufferQueryMappings.FindBySQLMgrQuery( pSQLMgrQuery: TSQLMgrQuery): TtiDataBufferQueryMapping;
var
  i : integer ;
begin
  result := nil ;
  for i := 0 to Count-1 do
    if Items[i].SQLMgrQuery = pSQLMgrQuery then
    begin
      result := Items[i] ;
      Break ; //==>
    end ;
  if result = nil then
  begin
    result := TtiDataBufferQueryMapping.Create ;
//    result.TIDataSet := TtiDataBuffer.Create ;
    result.SQLMgrQuery := pSQLMgrQuery ;
    Add( Result ) ;
  end ;
end;

function TtiDataBufferQueryMappings.GetItems( pIndex: integer): TtiDataBufferQueryMapping;
begin
  result := TtiDataBufferQueryMapping( Inherited GetItems( pIndex )) ;
end;

procedure TtiDataBufferQueryMappings.SetItems(pIndex: integer; const Value: TtiDataBufferQueryMapping);
begin
  inherited SetItems( pIndex, Value ) ;
end;

{ TtiDataBufferQueryMapping }

constructor TtiDataBufferQueryMapping.Create;
begin
  inherited;
  FtiDataSet := TtiDataBuffer.Create ;
end;

destructor TtiDataBufferQueryMapping.Destroy;
begin
  FtiDataSet.Free ;
  inherited;
end;

end.


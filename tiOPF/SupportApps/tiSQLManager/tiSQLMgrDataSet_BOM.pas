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

unit tiSQLMgrDataSet_BOM;

interface
uses
   Classes
  ,tiPtnVisPerObj
  ,tiPtnVis
  ,tiQuery
  ,tiSQLMgr_BOM
  ,tiDataSet_BOM
  ,Windows
  {$IFNDEF VER130}
   ,Variants
  {$ENDIF}  
  ;


type

  TtiSQLMgrDataSet = class( TtiDataSet )
  private
    FtiQuery: TSQLMgrQuery;
  public
    property    TIQuery : TSQLMgrQuery read FtiQuery write FtiQuery ;
  end ;

  TtiDataSetQueryMapping  = class ;

  TtiDataSetQueryMappings = class( TPerObjList )
  private
  protected
    function  GetItems(pIndex: integer): TtiDataSetQueryMapping;reintroduce ;
    procedure SetItems(pIndex: integer;  const Value: TtiDataSetQueryMapping);reintroduce ;
  public
    property Items[ pIndex : integer ] : TtiDataSetQueryMapping read GetItems write SetItems ;
    function FindBySQLMgrQuery( pSQLMgrQuery : TSQLMgrQuery ) : TtiDataSetQueryMapping ;
  end ;

  TtiDataSetQueryMapping = class( TPerObjAbs )
  private
    FTIDataSet: TtiDataSet;
    FSQLMgrQuery: TSQLMgrQuery;
    FsErrorMessage: string;
    FiTimeToRun: DWord;
    FiTimeToScan: DWord;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    property    SQLMgrQuery : TSQLMgrQuery read FSQLMgrQuery write FSQLMgrQuery ;
    property    TIDataSet   : TtiDataSet     read FTIDataSet   write FTIDataSet ;
    property    ErrorMessage : string read FsErrorMessage write FsErrorMessage ;
    property    TimeToRun    : DWord read FiTimeToRun  write FiTimeToRun ;
    property    TimeToScan   : DWord read FiTimeToScan write FiTimeToScan ;
  end ;

implementation
uses
  SysUtils
  ,tiUtils
  ;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiDataSetQueryMappings
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TtiDataSetQueryMappings.FindBySQLMgrQuery( pSQLMgrQuery: TSQLMgrQuery): TtiDataSetQueryMapping;
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
    result := TtiDataSetQueryMapping.Create ;
//    result.TIDataSet := TtiDataSet.Create ;
    result.SQLMgrQuery := pSQLMgrQuery ;
    Add( Result ) ;
  end ;
end;

function TtiDataSetQueryMappings.GetItems( pIndex: integer): TtiDataSetQueryMapping;
begin
  result := TtiDataSetQueryMapping( Inherited GetItems( pIndex )) ;
end;

procedure TtiDataSetQueryMappings.SetItems(pIndex: integer; const Value: TtiDataSetQueryMapping);
begin
  inherited SetItems( pIndex, Value ) ;
end;

{ TtiDataSetQueryMapping }

constructor TtiDataSetQueryMapping.Create;
begin
  inherited;
  FtiDataSet := TtiDataSet.Create ;
end;

destructor TtiDataSetQueryMapping.Destroy;
begin
  FtiDataSet.Free ;
  inherited;
end;

end.


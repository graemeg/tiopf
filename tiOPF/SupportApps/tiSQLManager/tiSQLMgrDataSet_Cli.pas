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

unit tiSQLMgrDataSet_Cli;

interface
uses
  tiPtnVisPerObj
  ,tiDataSet_BOM
  ,Contnrs
  ,Classes
  ,tiQuery
  ,tiSQLMgr_BOM
  ,tiSQLMgrDataSet_BOM
  ,ComCtrls
  ,tiPtnVis
  ,SysUtils
  ;

type

  TtiDataSetMgr = class( TObject )
  private
    FMappings : TtiDataSetQueryMappings ;
  public
    constructor create ;
    destructor  destroy ; override ;
    function    FindBySQLMgrQuery( const pTIQuery : TSQLMgrQuery ) : TtiDataSet ;
  end ;

function  gTIDataSetMgr : TtiDataSetMgr ;

implementation
uses
  tiPersist
  ,Math
  ,tiUtils
  ;

var
  uTIDataSetMgr : TtiDataSetMgr ;

function gTIDataSetMgr : TtiDataSetMgr ;
begin
  if uTIDataSetMgr = nil then
    uTIDataSetMgr := TtiDataSetMgr.Create;
  result := uTIDataSetMgr ;
end ;

{ TtiDataSetMgr }

constructor TtiDataSetMgr.create;
begin
  inherited ;
  FMappings := TtiDataSetQueryMappings.Create ;
end;

destructor TtiDataSetMgr.destroy;
begin
  FMappings.Free ;
  inherited;
end;

function TtiDataSetMgr.FindBySQLMgrQuery(const pTIQuery: TSQLMgrQuery): TtiDataSet;
var
  lDataSetQueryMapping : TtiDataSetQueryMapping ;
begin
  lDataSetQueryMapping := FMappings.FindBySQLMgrQuery( pTIQuery ) ;
  gTIPerMgr.Read( lDataSetQueryMapping ) ;
  result := lDataSetQueryMapping.TIDataSet ;
end;

initialization

finalization
  uTIDataSetMgr.Free ;

end.

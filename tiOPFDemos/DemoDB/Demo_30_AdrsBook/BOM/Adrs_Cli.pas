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
    April 2002, Peter Hinrichsen, Created

  Purpose:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit Adrs_Cli;

interface
uses
  tiPtnVis
  ,tiPtnVisPerObj
  ;

type

  TVisAdrsBookSetAllToCreate = class( TVisitorAbs )
  protected
    function AcceptVisitor : boolean ; override ;
  public
    procedure Execute( const pVisited : TVisitedAbs ) ; override ;
  end ;

procedure SetAllToCreate( const pData : TPerObjAbs ) ;


implementation
uses
  Adrs_BOM
  ;

procedure SetAllToCreate( const pData : TPerObjAbs ) ;
var
  lVis : TVisAdrsBookSetAllToCreate ;
begin
  lVis := TVisAdrsBookSetAllToCreate.Create ;
  try
    pData.Iterate( lVis ) ;
  finally
    lVis.Free ;
  end ;
end ;


{ TVisAdrsBookSetAllToCreate }

function TVisAdrsBookSetAllToCreate.AcceptVisitor: boolean;
begin
  result :=
    (( Visited is TPerson ) or
     ( Visited is TCompany ) or
     ( Visited is TAdrsAbs )) and
     ( TPerObjAbs( Visited ).ObjectState in [ posCreate, posUpdate, posClean ]);

end;

procedure TVisAdrsBookSetAllToCreate.Execute(const pVisited: TVisitedAbs);
begin
  Inherited Execute( pVisited ) ;

  if not AcceptVisitor then
    exit ; //==>

  TPerObjAbs( Visited ).ObjectState := posCreate ;

end;

end.

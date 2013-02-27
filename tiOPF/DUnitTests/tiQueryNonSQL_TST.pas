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

unit tiQueryNonSQL_TST;

interface
uses
   tiQueryAbs_TST
  ;
type

  // For testing Non SQL databases: XML, CSV
  TTestTIQueryNonSQL = class( TTestTIQueryAbs )
  private
  protected
    // These don't get run as they only have meaning for SQL databases
    procedure GetSetSQL ;       override ;
    procedure QueryType ;       override ;
    procedure OpenCloseActive ; override ;
    procedure ExecSQL ;         override ;
  published

    // SQL management
    procedure ParamName ; override ;
    procedure ParamCount ; override ;
    procedure ParamsAsString ; override ;
    procedure ParamAsString ; override ;
    procedure ParamAsInteger ; override ;
    procedure ParamAsFloat ; override ;
    procedure ParamAsBoolean ; override ;
    procedure ParamAsDateTime ; override ;
    procedure ParamAsStream ; override ;
    procedure ParamAsMacro ; override ;
    procedure ParamIsNull ; override ;
  protected
    procedure Setup ; override ;
  end ;

implementation
uses
   tiPersist
  ,Windows
  ,Contnrs
  ,tiLog
  ,tiPtnVisPerObj
  ,tiPtnVisPerObj_Cli
  ,tiUtils
  ,Forms
  ,cTIPersist
  ,TypInfo
  {$IFNDEF DELPHI6ORABOVE}
  ,FileCtrl
  {$ENDIF}
  ,tiDUnitUtils
  ,tiDialogs // for debugging
  ,tiDUnitDependencies
  ,SysUtils
  ,Classes
  ,tiPersistAbs_TST
  ;

procedure TTestTIQueryNonSQL.QueryType;
begin
  Assert(false, 'Test not implemented') ;
end;

procedure TTestTIQueryNonSQL.ExecSQL;
begin
  LogWarning(  'Test not implemented' ) ;
end;

procedure TTestTIQueryNonSQL.OpenCloseActive;
begin
  Assert( false, 'Test not implemented' ) ;
end;

procedure TTestTIQueryNonSQL.ParamAsBoolean;
begin
  Query.ParamAsBoolean[ cTIQueryColName ] := True ;
  CheckEquals( True, Query.ParamAsBoolean[ cTIQueryColName ], 'True' ) ;
  Query.ParamAsBoolean[ cTIQueryColName ] := False ;
  CheckEquals( False, Query.ParamAsBoolean[ cTIQueryColName ], 'False' ) ;
end;

procedure TTestTIQueryNonSQL.ParamAsDateTime;
var
  lDate : TDateTime ;
begin
  lDate := now ;
  Query.ParamAsDateTime[ cTIQueryColName ] := lDate ;
  CheckEquals( lDate, Query.ParamAsDateTime[ cTIQueryColName ], 0.00001) ;
end;

procedure TTestTIQueryNonSQL.ParamAsFloat;
const
  cValue = 12345.6789 ;
begin
  Query.ParamAsFloat[ cTIQueryColName ] := cValue ;
  CheckEquals( cValue, Query.ParamAsFloat[ cTIQueryColName ], 0.00001 ) ;
end;

procedure TTestTIQueryNonSQL.ParamAsInteger;
begin
  Query.ParamAsInteger[ cTIQueryColName ] := 123456 ;
  CheckEquals( 123456, Query.ParamAsInteger[ cTIQueryColName ] ) ;
end;

procedure TTestTIQueryNonSQL.ParamAsMacro;
begin
  LogWarning(  'Test not implemented' ) ;
end;

procedure TTestTIQueryNonSQL.ParamAsString;
begin
  Query.ParamAsString[ cTIQueryColName ] := 'mickymouse' ;
  CheckEquals( 'mickymouse', Query.ParamAsString[ cTIQueryColName ] ) ;
end;

procedure TTestTIQueryNonSQL.ParamCount;
begin

  Check( Query.ParamCount = 0, 'ParamCount failed on 0' ) ;
  Query.ParamAsString[ 'Item_Str_Field' ] := 'test' ;
  Check( Query.ParamCount = 1, 'ParamCount failed on 1' ) ;

  Query.ParamAsInteger[ 'Item_Int_Field' ] := 123 ;
  Check( Query.ParamCount = 2, 'ParamCount failed on 2' ) ;

  Query.ParamAsFloat[   'Item_Float_Field' ] := 3.3 ;
  Check( Query.ParamCount = 3, 'ParamCount failed on 3' ) ;

end;

procedure TTestTIQueryNonSQL.ParamIsNull;
begin
  Query.ParamAsString[ 'Item_Str_Field' ] := 'micky mouse' ;
  Check( Query.ParamIsNull[ 'Item_Str_Field' ] = false, 'Error checking ParamIsNull (false)' ) ;
  Query.ParamIsNull[ 'Item_Str_Field' ] := true ;
  Check( Query.ParamIsNull[ 'Item_Str_Field' ] = true, 'Error checking ParamIsNull (true)' ) ;
end;

procedure TTestTIQueryNonSQL.ParamName;
begin
  Query.ParamAsString[ 'Item_Str_Field' ] := 'test' ;
  Query.ParamAsInteger[ 'Item_Int_Field' ] := 123 ;
  Query.ParamAsFloat[   'Item_Float_Field' ] := 3.3 ;
  Check( SameText( Query.ParamName( 0 ), 'Item_Str_Field' ),   'ParamName failed on 0' ) ;
  Check( SameText( Query.ParamName( 1 ), 'Item_Int_Field' ),   'ParamName failed on 1' ) ;
  Check( SameText( Query.ParamName( 2 ), 'Item_Float_Field' ), 'ParamName failed on 2' ) ;
end;

procedure TTestTIQueryNonSQL.ParamsAsString;
var
  ls : string ;
begin
  ls := Query.ParamsAsString ;
  Check( SameText(
         ls,
         '' ),
         'ParamsAsStr failed with 0 params. Returned values was:' + CrLf +
         ls ) ;

  Query.ParamAsString[ 'Item_Str_Field' ] := 'test' ;
  ls := Query.ParamsAsString ;
  Check( SameText(
         ls,
         'ITEM_STR_FIELD := test' ),
         'ParamsAsStr failed with 1 param. Returned values was:' + CrLf +
         ls ) ;

  Query.ParamAsInteger[ 'Item_Int_Field' ] := 123 ;
  ls := Query.ParamsAsString ;
  Check( SameText(
         ls,
         'ITEM_STR_FIELD := test' + CrLf +
         'ITEM_INT_FIELD := 123' ),
         'ParamsAsStr failed with 2 params. Returned values was:' + CrLf +
         ls ) ;

  Query.ParamAsFloat[   'Item_Float_Field' ] := 3.3 ;
  ls := Query.ParamsAsString ;
  Check( SameText(
         ls,
         'ITEM_STR_FIELD := test' + CrLf +
         'ITEM_INT_FIELD := 123' + CrLf +
         'ITEM_FLOAT_FIELD := 3.3' ),
         'ParamsAsStr failed with 3 params. Returned values was:' + CrLf +
         ls ) ;

end;

procedure TTestTIQueryNonSQL.Setup;
begin
  inherited;
end;

{ EMethodNotImplemented }

procedure TTestTIQueryNonSQL.GetSetSQL;
begin
  Assert(false, 'Test not implemented') ;
end;

procedure TTestTIQueryNonSQL.ParamAsStream;
var
  lStreamFrom : TStringStream ;
  lStreamTo   : TMemoryStream ;
begin
  lStreamFrom := TStringStream.Create(LongString);
  try
    lStreamTo   := TMemoryStream.Create ;
    try
      Query.AssignParamFromStream('Item_Binary_Field', lStreamFrom);
      Query.AssignParamToStream('Item_Binary_Field', lStreamTo ) ;
      CheckStreamContentsSame( lStreamFrom, lStreamTo ) ;
    finally
      lStreamTo.Free;
    end;
  finally
    lStreamFrom.Free;
  end;
end;

end.

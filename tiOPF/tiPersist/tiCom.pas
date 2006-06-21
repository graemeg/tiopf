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

unit tiCOM;

interface

procedure tiCoInitialize ;
procedure tiCoUnInitialize ;
procedure ShowCoInitializeThreadIDs ;


implementation
uses
  Classes
  ,Windows
  ,ActiveX
  ,tiUtils
  ,SysUtils
  ,tiDialogs
  ;

var
  uCoInitializeList : TList ;
// Not thread safe. Should be a TThreadList.


procedure ShowCoInitializeThreadIDs ;
var
  i : integer ;
  ls : string ;
begin
  for i := 0 to uCoInitializeList.Count - 1 do
    ls := ls +
      IntToStr( Integer( TObject( uCoInitializeList.Items[i] ))) + CrLf ;

  tiShowString( ls ) ;
end ;

procedure tiCoInitialize ;
var
  i : integer ;
  liThreadID : integer ;
begin
  if uCoInitializeList = nil then
    uCoInitializeList := TList.Create ;
  liThreadID := GetCurrentThreadID ;
  for i := 0 to uCoInitializeList.Count - 1 do
  begin
    if Integer( TObject( uCoInitializeList.Items[i] )) = liThreadID then
      Exit ; //==>
  end ;
  // If you get here, then CoInitialize for this thread has not been called
  CoInitialize( nil ) ;
  uCoInitializeList.Add( TObject( liThreadID )) ;
end ;

procedure tiCoUnInitialize ;
var
  i : integer ;
  liThreadID : integer ;
begin
  liThreadID := GetCurrentThreadID ;
  for i := 0 to uCoInitializeList.Count - 1 do
    if Integer( TObject( uCoInitializeList.Items[i] )) = liThreadID then
    begin
      CoUnInitialize ;
      uCoInitializeList.Delete(i) ;
      Exit ; //==>
    end ;
end ;


initialization
  tiCoInitialize ;

finalization
  tiCoUnInitialize ;
  uCoInitializeList.Free ;

end.

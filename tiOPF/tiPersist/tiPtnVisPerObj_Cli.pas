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

unit tiPtnVisPerObj_Cli;

interface
uses
  tiPtnVisPerObj
  {$IFDEF MSWINDOWS}
  ,Graphics      // Canvas
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  ,QGraphics
  {$ENDIF LINUX}
  ;

const
  cgsSaveAndClose = 'Do you want to save your changes before exiting?' ;

function  tiPerObjAbsAsString( pVisited : TPerObjAbs ; pbIncludeDeleted : boolean = false ) : string ;
procedure tiShowPerObjAbs( pVisited : TPerObjAbs ; pbIncludeDeleted : boolean = false ) ;

// Set canvas.font.color to crRed if pData.Dirty, crBlank if not
procedure tiPerObjAbsFormatCanvas( pCanvas : TCanvas ; pData : TPerObjAbs ) ;
// If pData.Dirty, then prompt user to save
function  tiPerObjAbsSaveAndClose( const pData      : TPerObjAbs ;
                                   var   pbCanClose : boolean ;
                                   const psMessage  : string = cgsSaveAndClose ) : boolean ;
function  tiPerObjAbsConfirmAndDelete( const pData : TPerObjAbs ;
                                       const pMessage : string = '' ) : boolean ;

implementation
uses
  tiUtils
  ,tiDialogs
  {$IFDEF MSWINDOWS}
  ,Controls // mrYes
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  ,QControls   // mrYes
  {$ENDIF LINUX}
  ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// *  Global funcs and procs
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function tiPerObjAbsAsString( pVisited : TPerObjAbs ; pbIncludeDeleted : boolean = false ) : string ;
var
  lVisitor : TVisPerObjToText ;
begin
  lVisitor := TVisPerObjToText.Create ;
  try
    lVisitor.IncludeDeleted := pbIncludeDeleted ;
    pVisited.Iterate( lVisitor ) ;
    result := lVisitor.Text ;
  finally
    lVisitor.Free ;
  end ;
end ;

// -----------------------------------------------------------------------------
procedure tiShowPerObjAbs( pVisited : TPerObjAbs ; pbIncludeDeleted : boolean = false );
var
  ls : string ;
begin
  ls := tiPerObjAbsAsString( pVisited, pbIncludeDeleted ) ;
  tiShowString( ls ) ;
end ;

// -----------------------------------------------------------------------------
procedure tiPerObjAbsFormatCanvas( pCanvas : TCanvas ; pData : TPerObjAbs ) ;
begin
  if pData.Dirty then
    pCanvas.Font.Color := clRed
  else
    pCanvas.Font.Color := clBlack ;
end ;

// -----------------------------------------------------------------------------
function  tiPerObjAbsSaveAndClose( const pData      : TPerObjAbs ;
                                   var   pbCanClose : boolean ;
                                   const psMessage  : string = cgsSaveAndClose ) : boolean ;
var
  lResult : Word ;
begin

  result     := false ;
  pbCanClose := true ;

  if not pData.Dirty then
    exit ; //==>

  lResult := tiYesNoCancel( psMessage ) ;
  case lResult of
    mrYes    : result := true ;
    mrNo     : ; // Do nothing
    mrCancel : pbCanClose := false ;
  end ;

end ;

// -----------------------------------------------------------------------------
function  tiPerObjAbsConfirmAndDelete( const pData : TPerObjAbs ;
                                       const pMessage : string = '' ) : boolean ;
var
  lMessage : string ;
begin
  result := false ;

  if pData = nil then
    Exit ; //==>

  if pMessage = '' then
    lMessage := 'Are you sure you want to delete <' +
                pData.Caption + '>?'
  else
    lMessage := pMessage ;

  result := tiAppConfirmation( lMessage ) ;

  if result then
    pData.Deleted := true ;

end ;

end.

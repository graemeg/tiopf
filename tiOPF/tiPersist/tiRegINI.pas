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
    June 2002, Peter Hinrichsen, Moved from tiUtils

  Purpose:
    Wrappers for TRegINIFile and TINIFile

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiRegINI;

interface
uses
   Classes
  ,IniFiles
  {$IFDEF MSWINDOWS}
  ,registry
  ,Forms
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  ,QForms
  {$ENDIF LINUX}
  ,tiUtils
  ;

type
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // * Registry and INI file manipulation
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // Registry manipulation - Registry key with the same name as the application
  {$IFDEF MSWINDOWS}
  TtiRegINIFile = class( TRegINIFile )
  public
    constructor CreateExt ;
    procedure   ReadFormState(  pForm : TForm ) ;
    procedure   WriteFormState( pForm : TForm ) ;
    function    ReadDate(       const pStrSection : string ; pStrIndent : string ; pDTDefault : TDateTime ) : TDateTime ;
    procedure   WriteDate(      const pStrSection : string ; pStrIndent : string ; pDTValue : TDateTime ) ;
    procedure   ReadStrings(    const pStrSection : string ; pStrings : TStrings ) ;
    procedure   WriteStrings(   const pStrSection : string ; pStrings : TStrings ) ;
  end ;

  // Don't use this class, use TtiRegINIFile
  TUserRegistry = class(TtiRegINIFile);
  {$ENDIF MSWINDOWS}

  // INI file manipulation - INI file name the same as the application
  // or in the same directory with a file name you specify
  // If a value is not in the INI file and ReadXXX is called, the default
  // value will be written to the file
  //----------------------------------------------------------------------------
  TtiINIFile = class( TINIFile )
  private
    FReadOnly : Boolean ;
  public
    constructor CreateExt( const pFileName : string = ''; pReadOnly: Boolean = false ) ;

    function    ReadString(const Section, Ident, Default: string): string; override;
    function    ReadInteger(const Section, Ident: string; Default: Longint): Longint; override;
    function    ReadBool(const Section, Ident: string; Default: Boolean): Boolean; override;
    function    ReadDate(const Section, Name: string; Default: TDateTime): TDateTime; override;
    function    ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime; override;
    function    ReadFloat(const Section, Name: string; Default: Double): Double; override;
    function    ReadTime(const Section, Name: string; Default: TDateTime): TDateTime; override;

    procedure   ReadFormState(  const pForm : TForm ; pHeight : integer = -1 ; pWidth : integer = -1 ) ;
    procedure   WriteFormState( pForm : TForm ) ;

  end ;

  // Don't use TUserINIFile use TtiINIFile
  TUserINIFile = class(TtiINIFile) ;

// These are both singletons
{$IFDEF MSWINDOWS}
function gReg : TtiRegINIFile ;
{$ENDIF MSWINDOWS}
function gINI(const pFileName: string = '') : TtiINIFile ;

implementation
uses
  SysUtils
  ;

var
  {$IFDEF MSWINDOWS}
  uReg : TtiRegINIFile ;
  {$ENDIF MSWINDOWS}
  uINI : TtiINIFile ;

// -----------------------------------------------------------------------------
{$IFDEF MSWINDOWS}
function gReg : TtiRegINIFile ;
begin
  if uReg = nil then
    uReg := TtiRegINIFile.CreateExt ;
    result := uReg ;
end ;
{$ENDIF MSWINDOWS}

// -----------------------------------------------------------------------------
function gINI(const pFileName: string = '') : TtiINIFile ;
begin
  if uINI = nil then
    uINI := TUserINIFile.CreateExt(pFileName) ;
  result := uINI ;
end ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiRegINIFile
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
{$IFDEF MSWINDOWS}
procedure TtiRegINIFile.ReadFormState( pForm : TForm ) ;
var
  sRegKey : string ;
  liTop  : integer ;
  liLeft : integer ;
  lHeight : integer ;
  lWidth  : integer ;
begin
  sRegKey := pForm.name + 'State' ;
  with pForm do begin

    // Only set the form size if a bsSizable window
    if borderStyle = bsSizeable then
    begin
      lHeight := readInteger( sRegKey, 'Height', -1 ) ;
      lWidth  := readInteger( sRegKey, 'Width',  -1 ) ;
      if ( lHeight = -1 ) or ( lWidth = -1 ) then
      begin
        lHeight := Screen.Height div 10 * 9 ;
        lWidth  := Screen.Width  div 10 * 9 ;
      end ;
      Height := lHeight ;
      Width  := lWidth ;
    end ;

    // Do not read position if an MDIChild form
    if formStyle <> fsMDIChild then
    begin
      // Read form position, -1 if not stored in registry
      liTop  := readInteger( sRegKey, 'Top',    -1 ) ;
      liLeft := readInteger( sRegKey, 'Left',   -1 ) ;
      // The form pos was found in the registr
      if (liTop <> -1) and ( liLeft <> -1 ) then begin
        Top    := readInteger( sRegKey, 'Top',    Top ) ;
        Left   := readInteger( sRegKey, 'Left',   Left ) ;
      // No form pos in the registry, so default to screen center
      end else
      begin
        Position := poScreenCenter ;
      end ;
    end ;
    WindowState := TWindowState( ReadInteger( sRegKey, 'WindowState', ord( wsNormal ))) ;
  end ;
end ;
{$ENDIF MSWINDOWS}

//------------------------------------------------------------------------------
{$IFDEF MSWINDOWS}
procedure TtiRegINIFile.WriteFormState( pForm : TForm ) ;
var sRegKey : string ;
begin ;
  sRegKey := pForm.name + 'State' ;
  writeInteger( sRegKey, 'WindowState', ord( pForm.WindowState )) ;
  if pForm.WindowState = wsNormal then
  begin
    writeInteger( sRegKey, 'Top',    pForm.Top ) ;
    writeInteger( sRegKey, 'Left',   pForm.Left ) ;
    if pForm.borderStyle = bsSizeable then
    begin
      writeInteger( sRegKey, 'Height', pForm.Height ) ;
      writeInteger( sRegKey, 'Width',  pForm.Width ) ;
    end ;
  end ;
end ;
{$ENDIF MSWINDOWS}

//------------------------------------------------------------------------------
{$IFDEF MSWINDOWS}
function TtiRegINIFile.ReadDate( const pStrSection : string ; pStrIndent : string ; pDTDefault : TDateTime ) : TDateTime ;
var sDate : string ;
begin
  sDate  := gReg.readString( pStrSection, pStrIndent, DateTimeToStr( pDTDefault )) ;
  try
    result := StrToDateTime( sDate ) ;
  except
    result := date ;
  end ;
end ;
{$ENDIF MSWINDOWS}

//------------------------------------------------------------------------------
{$IFDEF MSWINDOWS}
procedure TtiRegINIFile.WriteDate( const pStrSection : string ; pStrIndent : string ; pDTValue : TDateTime ) ;
var sDate : string ;
begin
  try
    sDate := formatDateTime( csWinDateTimeFormat, pDTValue ) ;
  except
    sDate := formatDateTime( csWinDateTimeFormat, date ) ;
  end ;
  gReg.writeString( pStrSection, pStrIndent, sDate ) ;
end ;
{$ENDIF MSWINDOWS}

//------------------------------------------------------------------------------
{$IFDEF MSWINDOWS}
procedure TtiRegINIFile.WriteStrings( const pStrSection : string ; pStrings : TStrings ) ;
var i : integer ;
begin
  self.eraseSection( pStrSection ) ;
  for i := 0 to pStrings.count - 1 do begin
    self.writeString( pStrSection, 'line' + intToStr( i ), pStrings.strings[i] ) ;
  end ;
end ;
{$ENDIF MSWINDOWS}

//------------------------------------------------------------------------------
{$IFDEF MSWINDOWS}
procedure TtiRegINIFile.ReadStrings( const pStrSection : string ; pStrings : TStrings ) ;
var i : integer ;
    sectionValues : TStringList ;
begin
  sectionValues := TStringList.create ;
  pStrings.clear ;
  try
    self.readSectionValues( pStrSection, sectionValues ) ;
    for i := 0 to sectionValues.count - 1 do begin
      pStrings.add( self.readString( pStrSection, 'line' + intToStr( i ), '' )) ;
    end ;
  finally
    sectionValues.free ;
  end ;
end ;
{$ENDIF MSWINDOWS}

//------------------------------------------------------------------------------
{$IFDEF MSWINDOWS}
constructor TtiRegINIFile.CreateExt;
begin
  Create( tiExtractFileNameOnly( application.exeName )) ;
end;
{$ENDIF MSWINDOWS}

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiINIFile
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiINIFile.CreateExt( const pFileName : string = ''; pReadOnly: Boolean = false ) ;
var
  lDir      : string ;
  lFileName : string ;
begin
  FReadOnly := pReadOnly ;
  lDir := ExtractFilePath(pFileName);
  lFileName := ExtractFileName(pFileName);

  if lDir = '' then
    lDir := ExtractFilePath( ParamStr( 0 ));
  lDir := tiAddTrailingSlash(lDir);

  if lFileName = '' then
  begin
    lFileName := ExtractFileName(ParamStr(0)) ;
    lFileName := tiAddTrailingValue( tiRemoveExtension( lFileName ), '.', false ) + 'ini';
  end else
  begin
    if tiExtractExtension(lFileName) = '' then
      lFileName := tiAddTrailingValue( tiRemoveExtension( lFileName ), '.', false ) + 'ini';
  end;

  lFileName := lDir + lFileName ;
  Create( lFileName ) ;

end;

//------------------------------------------------------------------------------
function TtiINIFile.ReadBool(const Section, Ident: string;Default: Boolean): Boolean;
begin
  result := inherited ReadBool(Section, Ident, Default);
  if ( not ValueExists(Section, Ident)) and 
     ( not FReadOnly ) then
    WriteBool(Section, Ident, Default);
end;

function TtiINIFile.ReadDate(const Section, Name: string;Default: TDateTime): TDateTime;
begin
  result := inherited ReadDate(Section, Name, Default);
  if ( not ValueExists(Section, Name)) and
     ( not FReadOnly ) then
    WriteDate(Section, Name, Default);
end;

function TtiINIFile.ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime;
begin
  result := inherited ReadDateTime(Section, Name, Default);
  if ( not ValueExists(Section, Name)) and
     ( not FReadOnly ) then
    WriteDateTime(Section, Name, Default);
end;

function TtiINIFile.ReadFloat(const Section, Name: string; Default: Double): Double;
begin
  result := inherited ReadFloat(Section, Name, Default);
  if ( not ValueExists(Section, Name)) and
     ( not FReadOnly ) then
    WriteFloat(Section, Name, Default);
end;

procedure TtiINIFile.ReadFormState(const pForm: TForm; pHeight : integer = -1 ; pWidth : integer = -1);
var
  sRegKey : string ;
  liTop  : integer ;
  liLeft : integer ;
  lHeight : integer ;
  lWidth  : integer ;
begin
  Assert(pForm <> nil, 'pForm not assigned' ) ;
  sRegKey := pForm.name + 'State' ;
  // Do not read position if an MDIChild form
  if pForm.formStyle <> fsMDIChild then
  begin
    // Read form position, -1 if not stored in registry
    liTop  := readInteger( sRegKey, 'Top',    -1 ) ;
    liLeft := readInteger( sRegKey, 'Left',   -1 ) ;
    // The form pos was found in the registr
    if (liTop <> -1) and ( liLeft <> -1 ) then
    begin
      pForm.Top    := readInteger( sRegKey, 'Top',    pForm.Top ) ;
      pForm.Left   := readInteger( sRegKey, 'Left',   pForm.Left ) ;
    // No form pos in the registry, so default to screen center
    end else
      pForm.Position := poScreenCenter ;
  end ;
  // Only set the form size if a bsSizable window
  {$IFDEF LINUX}
  if pForm.BorderStyle = fbsSizeable then begin
  {$ELSE}
  if pForm.BorderStyle = bsSizeable then begin
  {$ENDIF LINUX}
    if pHeight = -1 then
      lHeight := pForm.Height
    else
      lHeight := pHeight ;
    if pWidth = -1 then
      lWidth := pForm.Width
    else
      lWidth := pWidth ;
    pForm.Height := readInteger( sRegKey, 'Height', lHeight ) ;
    pForm.Width  := readInteger( sRegKey, 'Width',  lWidth ) ;
  end ;
  pForm.WindowState := TWindowState( ReadInteger( sRegKey, 'WindowState', ord( wsNormal ))) ;
end;

//------------------------------------------------------------------------------
function TtiINIFile.ReadInteger(const Section, Ident: string;Default: Integer): Longint;
begin
  result := inherited ReadInteger(Section, Ident, Default);
  if ( not ValueExists(Section, Ident)) and
     ( not FReadOnly ) then
    WriteInteger(Section, Ident, Default);
end;

function TtiINIFile.ReadString(const Section, Ident,Default: string): string;
begin
  result := inherited ReadString(Section, Ident, Default);
  if ( not ValueExists(Section, Ident)) and
     ( not FReadOnly ) then
    WriteString(Section, Ident, Default);
end;

function TtiINIFile.ReadTime(const Section, Name: string;Default: TDateTime): TDateTime;
begin
  result := inherited ReadTime(Section, Name, Default);
  if ( not ValueExists(Section, Name)) and
     ( not FReadOnly ) then
    WriteTime(Section, Name, Default);
end;

procedure TtiINIFile.WriteFormState(pForm: TForm);
var sRegKey : string ;
begin ;
  sRegKey := pForm.name + 'State' ;
  with TForm( pForm ) do begin
    writeInteger( sRegKey, 'WindowState', ord( pForm.WindowState )) ;
    if pForm.WindowState = wsNormal then begin
      writeInteger( sRegKey, 'Top',    Top ) ;
      writeInteger( sRegKey, 'Left',   Left ) ;
      {$IFDEF LINUX}
      if BorderStyle = fbsSizeable then begin
      {$ELSE}
      if BorderStyle = bsSizeable then begin
      {$ENDIF LINUX}
        writeInteger( sRegKey, 'Height', Height ) ;
        writeInteger( sRegKey, 'Width',  Width ) ;
      end ;
    end ;
  end ;
end;

initialization

finalization
  {$IFDEF MSWINDOWS}
  uReg.free ;
  {$ENDIF MSWINDOWS}
  uINI.Free ;

end.

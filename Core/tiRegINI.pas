unit tiRegINI;

{$I tiDefines.inc}

interface
uses
   Classes
  ,IniFiles
  {$IFDEF MSWINDOWS}
  ,registry
  {$ENDIF MSWINDOWS}
  ,Forms
  ;

type

  // Registry manipulation - Registry key with the same name as the application
  {$IFDEF MSWINDOWS}
  TtiRegINIFile = class(TRegINIFile)
  public
    constructor CreateExt;
    procedure   ReadFormState( AForm : TForm);
    procedure   WriteFormState(AForm : TForm);
    function    ReadDate(      const pStrSection : string; pStrIndent : string; pDTDefault : TDateTime) : TDateTime;
    procedure   WriteDate(     const pStrSection : string; pStrIndent : string; pDTValue : TDateTime);
    procedure   ReadStrings(   const pStrSection : string; pStrings : TStrings);
    procedure   WriteStrings(  const pStrSection : string; pStrings : TStrings);
  end;

  // Don't use this class, use TtiRegINIFile
  TUserRegistry = class(TtiRegINIFile);
  {$ENDIF MSWINDOWS}

  // INI file manipulation - INI file name the same as the application
  // or in the same directory with a file name you specify
  // If a value is not in the INI file and ReadXXX is called, the default
  // value will be written to the file
  TtiINIFile = class(TINIFile)
  private
    FReadOnly : Boolean;
  public
    constructor CreateExt(const pFileName : string = ''; pReadOnly: Boolean = false);

    function    ReadString(const Section, Ident, Default: string): string; override;
    function    ReadInteger(const Section, Ident: string; Default: Longint): Longint; override;
    function    ReadBool(const Section, Ident: string; Default: Boolean): Boolean; override;
    function    ReadDate(const Section, Name: string; Default: TDateTime): TDateTime; override;
    function    ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime; override;
    function    ReadFloat(const Section, Name: string; Default: Double): Double; override;
    function    ReadTime(const Section, Name: string; Default: TDateTime): TDateTime; override;

    procedure   ReadFormState(AForm: TForm; AHeight: integer = -1; AWidth: integer = -1);
    procedure   WriteFormState(AForm : TForm);

  end;

  // Don't use TUserINIFile use TtiINIFile
  TUserINIFile = class(TtiINIFile);

// These are both singletons
{$IFDEF MSWINDOWS}
function gReg : TtiRegINIFile;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
function gReg : TtiINIFile;
{$ENDIF}
function gINI(const pFileName: string = '') : TtiINIFile;

implementation
uses
   tiUtils
  ,SysUtils
  {$IFDEF FPC}
  ,Controls        // used for TFormBorderStyle
  {$ENDIF}
  ,tiConstants
  ;

var
  {$IFDEF MSWINDOWS}
  uReg : TtiRegINIFile;
  {$ENDIF MSWINDOWS}
  uINI : TtiINIFile;

{$IFDEF MSWINDOWS}
function gReg : TtiRegINIFile;
begin
  if uReg = nil then
    uReg := TtiRegINIFile.CreateExt;
    result := uReg;
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}     // Allows us to use gReg under Linux as well
function gReg : TtiINIFile;
begin
  Result := gINI;
end;
{$ENDIF}


function gINI(const pFileName: string = '') : TtiINIFile;
begin
  if uINI = nil then
    uINI := TUserINIFile.CreateExt(pFileName);
  result := uINI;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiRegINIFile
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
{$IFDEF MSWINDOWS}
procedure TtiRegINIFile.ReadFormState(AForm: TForm);
var
  sRegKey : string;
  liTop  : integer;
  liLeft : integer;
  lHeight : integer;
  lWidth  : integer;
begin
  sRegKey := AForm.name + 'State';
  // Only set the form size if a bsSizable window
  if AForm.borderStyle = bsSizeable then
  begin
    lHeight := readInteger(sRegKey, 'Height', -1);
    lWidth  := readInteger(sRegKey, 'Width',  -1);
    if (lHeight = -1) or (lWidth = -1) then
    begin
      lHeight := Screen.Height div 10 * 9;
      lWidth  := Screen.Width  div 10 * 9;
    end;
    AForm.Height := lHeight;
    AForm.Width  := lWidth;
  end;

  // Do not read position if an MDIChild form
  if AForm.FormStyle <> fsMDIChild then
  begin
    // Read form position, -1 if not stored in registry
    liTop  := readInteger(sRegKey, 'Top',    -1);
    liLeft := readInteger(sRegKey, 'Left',   -1);
    // The form pos was found in the registr
    if (liTop <> -1) and (liLeft <> -1) then begin
      AForm.Top    := readInteger(sRegKey, 'Top',    AForm.Top);
      AForm.Left   := readInteger(sRegKey, 'Left',   AForm.Left);
      AForm.Position  := poDesigned;
    // No form pos in the registry, so default to screen center
    end else
    begin
      if Application.MainForm <> AForm then
        AForm.Position := poMainFormCenter
      else
        AForm.Position:= poScreenCenter;
    end;
  end;
  AForm.WindowState := TWindowState(ReadInteger(sRegKey, 'WindowState', ord(wsNormal)));
end;
{$ENDIF MSWINDOWS}

{$IFDEF MSWINDOWS}
procedure TtiRegINIFile.WriteFormState(AForm : TForm);
var
  sRegKey : string;
begin;
  sRegKey := AForm.name + 'State';
  writeInteger(sRegKey, 'WindowState', ord(AForm.WindowState));
  if AForm.WindowState = wsNormal then
  begin
    writeInteger(sRegKey, 'Top',    AForm.Top);
    writeInteger(sRegKey, 'Left',   AForm.Left);
    if AForm.borderStyle = bsSizeable then
    begin
      writeInteger(sRegKey, 'Height', AForm.Height);
      writeInteger(sRegKey, 'Width',  AForm.Width);
    end;
  end;
end;
{$ENDIF MSWINDOWS}

{$IFDEF MSWINDOWS}
function TtiRegINIFile.ReadDate(const pStrSection : string; pStrIndent : string; pDTDefault : TDateTime) : TDateTime;
var sDate : string;
begin
  sDate  := gReg.readString(pStrSection, pStrIndent, DateTimeToStr(pDTDefault));
  try
    result := StrToDateTime(sDate);
  except
    result := date;
  end;
end;
{$ENDIF MSWINDOWS}

{$IFDEF MSWINDOWS}
procedure TtiRegINIFile.WriteDate(const pStrSection : string; pStrIndent : string; pDTValue : TDateTime);
var sDate : string;
begin
  try
    sDate := formatDateTime(csWinDateTimeFormat, pDTValue);
  except
    sDate := formatDateTime(csWinDateTimeFormat, date);
  end;
  gReg.writeString(pStrSection, pStrIndent, sDate);
end;
{$ENDIF MSWINDOWS}

{$IFDEF MSWINDOWS}
procedure TtiRegINIFile.WriteStrings(const pStrSection : string; pStrings : TStrings);
var i : integer;
begin
  self.eraseSection(pStrSection);
  for i := 0 to pStrings.count - 1 do begin
    self.writeString(pStrSection, 'line' + intToStr(i), pStrings.strings[i]);
  end;
end;
{$ENDIF MSWINDOWS}

{$IFDEF MSWINDOWS}
procedure TtiRegINIFile.ReadStrings(const pStrSection : string; pStrings : TStrings);
var i : integer;
    sectionValues : TStringList;
begin
  sectionValues := TStringList.Create;
  pStrings.clear;
  try
    self.readSectionValues(pStrSection, sectionValues);
    for i := 0 to sectionValues.count - 1 do begin
      pStrings.add(self.readString(pStrSection, 'line' + intToStr(i), ''));
    end;
  finally
    sectionValues.free;
  end;
end;
{$ENDIF MSWINDOWS}

{$IFDEF MSWINDOWS}
constructor TtiRegINIFile.CreateExt;
begin
  Create(tiExtractFileNameOnly(application.exeName));
end;
{$ENDIF MSWINDOWS}

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiINIFile
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiINIFile.CreateExt(const pFileName : string = ''; pReadOnly: Boolean = false);
var
  lDir      : string;
  lFileName : string;
begin
  FReadOnly := pReadOnly;
  lDir := ExtractFilePath(pFileName);
  lFileName := ExtractFileName(pFileName);

  if lDir = '' then
    lDir := ExtractFilePath(ParamStr(0));
  lDir := tiAddTrailingSlash(lDir);

  if lFileName = '' then
  begin
    lFileName := ExtractFileName(ParamStr(0));
    lFileName := tiAddTrailingValue(tiRemoveExtension(lFileName), '.', false) + 'ini';
  end else
  begin
    if tiExtractExtension(lFileName) = '' then
      lFileName := tiAddTrailingValue(tiRemoveExtension(lFileName), '.', false) + 'ini';
  end;

  lFileName := lDir + lFileName;
  Create(lFileName);

end;

function TtiINIFile.ReadBool(const Section, Ident: string;Default: Boolean): Boolean;
var
  lValueExists: Boolean;
begin
  lValueExists:= ValueExists(Section, Ident);
  if (not lValueExists) and
     (not FReadOnly) then
    WriteBool(Section, Ident, Default);
  result := inherited ReadBool(Section, Ident, Default);
end;

function TtiINIFile.ReadDate(const Section, Name: string;Default: TDateTime): TDateTime;
begin
  if (not ValueExists(Section, Name)) and
     (not FReadOnly) then
    WriteDate(Section, Name, Default);
  result := inherited ReadDate(Section, Name, Default);
end;

function TtiINIFile.ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime;
begin
  if (not ValueExists(Section, Name)) and
     (not FReadOnly) then
    WriteDateTime(Section, Name, Default);
  result := inherited ReadDateTime(Section, Name, Default);
end;

function TtiINIFile.ReadFloat(const Section, Name: string; Default: Double): Double;
begin
  if (not ValueExists(Section, Name)) and
     (not FReadOnly) then
    WriteFloat(Section, Name, Default);
  result := inherited ReadFloat(Section, Name, Default);
end;

procedure TtiINIFile.ReadFormState(AForm: TForm; AHeight : integer = -1; AWidth : integer = -1);
var
  sRegKey : string;
  liTop  : integer;
  liLeft : integer;
  lHeight : integer;
  lWidth  : integer;
begin
  Assert(AForm <> nil, 'pForm not assigned');
  sRegKey := AForm.name + 'State';
  // Do not read position if an MDIChild form
  if AForm.formStyle <> fsMDIChild then
  begin
    // Read form position, -1 if not stored in registry
    liTop  := readInteger(sRegKey, 'Top',    -1);
    liLeft := readInteger(sRegKey, 'Left',   -1);
    // The form pos was found in the registr
    if (liTop <> -1) and (liLeft <> -1) then
    begin
      AForm.Top    := readInteger(sRegKey, 'Top',    AForm.Top);
      AForm.Left   := readInteger(sRegKey, 'Left',   AForm.Left);
      AForm.Position  := poDesigned;
    // No form pos in the registry, so default to screen center
    end else
    begin
      if Assigned(Application.MainForm) and (Application.MainForm <> AForm) then
        AForm.Position := poMainFormCenter
      else
        AForm.Position:= poScreenCenter;
    end;
  end;
  // Only set the form size if a bsSizable window
  if AForm.BorderStyle = bsSizeable then
  begin
    if AHeight = -1 then
      lHeight := AForm.Height
    else
      lHeight := AHeight;
    if AWidth = -1 then
      lWidth := AForm.Width
    else
      lWidth := AWidth;
    AForm.Height := readInteger(sRegKey, 'Height', lHeight);
    AForm.Width  := readInteger(sRegKey, 'Width',  lWidth);
  end;
  AForm.WindowState := TWindowState(ReadInteger(sRegKey, 'WindowState', ord(wsNormal)));
end;

function TtiINIFile.ReadInteger(const Section, Ident: string;Default: Integer): Longint;
begin
  if (not ValueExists(Section, Ident)) and
     (not FReadOnly) then
    WriteInteger(Section, Ident, Default);
  result := inherited ReadInteger(Section, Ident, Default);
end;

function TtiINIFile.ReadString(const Section, Ident,Default: string): string;
begin
  result := inherited ReadString(Section, Ident, Default);
  if (not ValueExists(Section, Ident)) and
     (not FReadOnly) then
    WriteString(Section, Ident, Default);
end;

function TtiINIFile.ReadTime(const Section, Name: string;Default: TDateTime): TDateTime;
begin
  if (not ValueExists(Section, Name)) and
     (not FReadOnly) then
    WriteTime(Section, Name, Default);
  result := inherited ReadTime(Section, Name, Default);
end;

procedure TtiINIFile.WriteFormState(AForm: TForm);
var
  sRegKey: string;
begin
  sRegKey := AForm.name + 'State';
  writeInteger(sRegKey, 'WindowState', ord(AForm.WindowState));
  if AForm.WindowState = wsNormal then
  begin
    writeInteger(sRegKey, 'Top',    AForm.Top);
    writeInteger(sRegKey, 'Left',   AForm.Left);
    if AForm.BorderStyle = bsSizeable then
    begin
      writeInteger(sRegKey, 'Height', AForm.Height);
      WriteInteger(sRegKey, 'Width',  AForm.Width);
    end;
  end;
end;

initialization

finalization
  {$IFDEF MSWINDOWS}
  uReg.free;
  {$ENDIF MSWINDOWS}
  uINI.Free;

end.

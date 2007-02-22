unit tiRegINI;

{$I tiDefines.inc}



interface
uses
   Classes
  ,IniFiles
  ,registry
  ,Forms
 ;

type

  // Registry manipulation - Registry key with the same name as the application
  TtiRegINIFile = class(TRegINIFile)
  public
    constructor CreateExt;
    procedure   ReadFormState(AForm : TForm);
    procedure   WriteFormState(AForm : TForm);
    function    ReadDate(     const ASection : string; AIndent : string; ADefault : TDateTime): TDateTime;
    procedure   WriteDate(    const ASection : string; AIndent : string; ADateTime : TDateTime);
    procedure   ReadStrings(  const ASection : string; AStrings : TStrings);
    procedure   WriteStrings( const ASection : string; AStrings : TStrings);
  end;

  // Don't use this class, use TtiRegINIFile
  TUserRegistry = class(TtiRegINIFile);


  // INI file manipulation - INI file name the same as the application
  // or in the same directory with a file name you specify
  // If a value is not in the INI file and ReadXXX is called, the default
  // value will be written to the file
  TtiINIFile = class(TINIFile)
  private
    FReadOnly : Boolean;
  public
    constructor CreateExt(const AFileName : string = ''; pReadOnly: Boolean = false);

    function    ReadString(const ASection, AIdent, ADefault: string): string; override;
    function    ReadInteger(const ASection, AIdent: string; ADefault: Longint): Longint; override;
    function    ReadBool(const ASection, AIdent: string; ADefault: Boolean): Boolean; override;
    function    ReadDate(const ASection, AName: string; ADefault: TDateTime): TDateTime; override;
    function    ReadDateTime(const ASection, AName: string; ADefault: TDateTime): TDateTime; override;
    function    ReadFloat(const ASection, AName: string; ADefault: Double): Double; override;
    function    ReadTime(const ASection, AName: string; ADefault: TDateTime): TDateTime; override;

    procedure   ReadFormState(AForm: TForm; AHeight: integer = -1; AWidth: integer = -1);
    procedure   WriteFormState(AForm : TForm);

  end;

  // Don't use TUserINIFile use TtiINIFile
  TUserINIFile = class(TtiINIFile);

// These are both singletons
{$IFDEF MSWINDOWS}
function gReg : TtiRegINIFile;
{$ELSE}
function gReg : TtiINIFile;
{$ENDIF}
function gINI(const AFileName: string = ''): TtiINIFile;

var
 DefaultRegistryCompany : String = '';//better hierarchy in registry key

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
  uReg : TtiRegINIFile;
  uINI : TtiINIFile;


{$IFDEF MSWINDOWS}
function gReg : TtiRegINIFile;
begin
  if uReg = nil then
    uReg := TtiRegINIFile.CreateExt;
    result := uReg;
end;
{$ELSE}
function gReg: TtiINIFile;
begin
  result := gINI;
end;
{$ENDIF}


function gINI(const AFileName: string = ''): TtiINIFile;
begin
  if uINI = nil then
    uINI := TUserINIFile.CreateExt(AFileName);
  result := uINI;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiRegINIFile
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

procedure TtiRegINIFile.ReadFormState(AForm: TForm);
var
  sRegKey : string;
  liTop : integer;
  liLeft : integer;
  lHeight : integer;
  lWidth : integer;
begin
  sRegKey := AForm.name + 'State';
  // Only set the form size if a bsSizable window
  if AForm.borderStyle = bsSizeable then
  begin
    lHeight := readInteger(sRegKey, 'Height', -1);
    lWidth := readInteger(sRegKey, 'Width',  -1);
    if (lHeight = -1) or (lWidth = -1) then
    begin
      lHeight := Screen.Height div 10 * 9;
      lWidth := Screen.Width  div 10 * 9;
    end;
    AForm.Height := lHeight;
    AForm.Width := lWidth;
  end;

  // Do not read position if an MDIChild form
  if AForm.FormStyle <> fsMDIChild then
  begin
    // Read form position, -1 if not stored in registry
    liTop := readInteger(sRegKey, 'Top',    -1);
    liLeft := readInteger(sRegKey, 'Left',   -1);
    // The form pos was found in the registr
    if (liTop <> -1) and (liLeft <> -1) then begin
      AForm.Top   := readInteger(sRegKey, 'Top',    AForm.Top);
      AForm.Left  := readInteger(sRegKey, 'Left',   AForm.Left);
      AForm.Position := poDesigned;
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



function TtiRegINIFile.ReadDate(const ASection : string; AIndent : string; ADefault : TDateTime): TDateTime;
var sDate : string;
begin
  sDate := gReg.readString(ASection, AIndent, DateTimeToStr(ADefault));
  try
    result := StrToDateTime(sDate);
  except
    result := date;
  end;
end;



procedure TtiRegINIFile.WriteDate(const ASection : string; AIndent : string; ADateTime : TDateTime);
var sDate : string;
begin
  try
    sDate := formatDateTime(csWinDateTimeFormat, ADateTime);
  except
    sDate := formatDateTime(csWinDateTimeFormat, date);
  end;
  gReg.writeString(ASection, AIndent, sDate);
end;



procedure TtiRegINIFile.WriteStrings(const ASection : string; AStrings : TStrings);
var
  i: integer;
begin
  self.eraseSection(ASection);
  for i := 0 to AStrings.count - 1 do begin
    self.writeString(ASection, 'line' + intToStr(i), AStrings.strings[i]);
  end;
end;



procedure TtiRegINIFile.ReadStrings(const ASection : string; AStrings : TStrings);
var
  i: integer;
  sectionValues: TStringList;
begin
  sectionValues := TStringList.Create;
  AStrings.clear;
  try
    self.readSectionValues(ASection, sectionValues);
    for i := 0 to sectionValues.count - 1 do begin
      AStrings.add(self.readString(ASection, 'line' + intToStr(i), ''));
    end;
  finally
    sectionValues.free;
  end;
end;



constructor TtiRegINIFile.CreateExt;
begin
  if DefaultRegistryCompany <> '' then
    Create('Software\' + DefaultRegistryCompany + '\' + tiApplicationName)
  else
    Create('Software\' + tiApplicationName);
end;


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiINIFile
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiINIFile.CreateExt(const AFileName : string = ''; pReadOnly: Boolean = false);
var
  lDir     : string;
  lFileName : string;
begin
  FReadOnly := pReadOnly;
  lDir := ExtractFilePath(AFileName);
  lFileName := ExtractFileName(AFileName);

  if lDir = '' then
    lDir := tiGetAppConfigDir;
  lDir := tiAddTrailingSlash(lDir);
  { We used a non-Global config dir, so should be able to create the dir }
  tiForceDirectories(lDir);

  if lFileName = '' then
  begin
    lFileName := tiApplicationName;
    lFileName := tiAddTrailingValue(lFileName, '.', false) + 'ini';
  end
  else
  begin
    if tiExtractExtension(lFileName) = '' then
      lFileName := tiAddTrailingValue(tiRemoveExtension(lFileName), '.', false) + 'ini';
  end;

  lFileName := lDir + lFileName;
  Create(lFileName);
end;

function TtiINIFile.ReadBool(const ASection, AIdent: string;ADefault: Boolean): Boolean;
var
  lValueExists: Boolean;
begin
  lValueExists:= ValueExists(ASection, AIdent);
  if (not lValueExists) and
     (not FReadOnly) then
    WriteBool(ASection, AIdent, ADefault);
  result := inherited ReadBool(ASection, AIdent, ADefault);
end;

function TtiINIFile.ReadDate(const ASection, AName: string; ADefault: TDateTime): TDateTime;
begin
  if (not ValueExists(ASection, AName)) and
     (not FReadOnly) then
    WriteDate(ASection, AName, ADefault);
  result := inherited ReadDate(ASection, AName, ADefault);
end;

function TtiINIFile.ReadDateTime(const ASection, AName: string; ADefault: TDateTime): TDateTime;
begin
  if (not ValueExists(ASection, AName)) and
     (not FReadOnly) then
    WriteDateTime(ASection, AName, ADefault);
  result := inherited ReadDateTime(ASection, AName, ADefault);
end;

function TtiINIFile.ReadFloat(const ASection, AName: string; ADefault: Double): Double;
begin
  if (not ValueExists(ASection, AName)) and
     (not FReadOnly) then
    WriteFloat(ASection, AName, ADefault);
  result := inherited ReadFloat(ASection, AName, ADefault);
end;

procedure TtiINIFile.ReadFormState(AForm: TForm; AHeight : integer = -1; AWidth : integer = -1);
var
  sRegKey : string;
  liTop : integer;
  liLeft : integer;
  lHeight : integer;
  lWidth : integer;
begin
  Assert(AForm <> nil, 'pForm not assigned');
  sRegKey := AForm.name + 'State';
  // Do not read position if an MDIChild form
  if AForm.formStyle <> fsMDIChild then
  begin
    // Read form position, -1 if not stored in registry
    liTop := readInteger(sRegKey, 'Top',    -1);
    liLeft := readInteger(sRegKey, 'Left',   -1);
    // The form pos was found in the registr
    if (liTop <> -1) and (liLeft <> -1) then
    begin
      AForm.Top   := readInteger(sRegKey, 'Top',    AForm.Top);
      AForm.Left  := readInteger(sRegKey, 'Left',   AForm.Left);
      AForm.Position := poDesigned;
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
    AForm.Width := readInteger(sRegKey, 'Width',  lWidth);
  end;
  AForm.WindowState := TWindowState(ReadInteger(sRegKey, 'WindowState', ord(wsNormal)));
end;

function TtiINIFile.ReadInteger(const ASection, AIdent: string; ADefault: Longint): Longint;
begin
  if (not ValueExists(ASection, AIdent)) and
     (not FReadOnly) then
    WriteInteger(ASection, AIdent, ADefault);
  result := inherited ReadInteger(ASection, AIdent, ADefault);
end;

function TtiINIFile.ReadString(const ASection, AIdent,ADefault: string): string;
begin
  result := inherited ReadString(ASection, AIdent, ADefault);
  if (not ValueExists(ASection, AIdent)) and
     (not FReadOnly) then
    WriteString(ASection, AIdent, ADefault);
end;

function TtiINIFile.ReadTime(const ASection, AName: string; ADefault: TDateTime): TDateTime;
begin
  if (not ValueExists(ASection, AName)) and
     (not FReadOnly) then
    WriteTime(ASection, AName, ADefault);
  result := inherited ReadTime(ASection, AName, ADefault);
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
  uReg := nil;//to be sure
  uINI := nil;
  DefaultRegistryCompany := '';

finalization
  if uReg<>nil then uReg.free;
  if uINI<>nil then uINI.Free;

end.









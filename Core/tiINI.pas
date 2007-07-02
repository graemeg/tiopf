unit tiINI;

{$I tiDefines.inc}



interface
uses
   Classes
  ,IniFiles
  ,Forms
 ;

type

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

function gINI(const AFileName: string = ''): TtiINIFile;

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
  uINI : TtiINIFile;


function gINI(const AFileName: string = ''): TtiINIFile;
begin
  if uINI = nil then
    uINI := TtiINIFile.CreateExt(AFileName);
  result := uINI;
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
    lDir := tiGetAppDataDirPrivate;
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
  uINI := nil;

finalization
  uINI.Free;

end.









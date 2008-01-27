unit tiGUIINI;

{$I tiDefines.inc}

interface
uses
   tiINI
  ,Forms
 ;

type

  TtiGUIINIFile = class(TtiINIFile)
  public
    procedure   ReadFormState(AForm: TForm; AHeight: integer = -1; AWidth: integer = -1);
    procedure   WriteFormState(AForm : TForm);
  end;

function GGUIINI(const AFileName: string = ''): TtiGUIINIFile;

implementation
uses
  SysUtils
  {$IFDEF FPC}
  ,Controls        // used for TFormBorderStyle
  {$ENDIF}
  ,tiGUIUtils
 ;

var
  UGUIINI : TtiGUIINIFile;


function GGUIINI(const AFileName: string = ''): TtiGUIINIFile;
begin
  if UGUIINI = nil then
    UGUIINI := TtiGUIINIFile.CreateExt(AFileName);
  result := UGUIINI;
end;

procedure TtiGUIINIFile.ReadFormState(AForm: TForm; AHeight : integer = -1; AWidth : integer = -1);
var
  LINISection : string;
  LTop : integer;
  LLeft : integer;
  LHeight : integer;
  LWidth : integer;
begin
  Assert(AForm <> nil, 'pForm not assigned');
  LINISection := AForm.name + 'State';
  // Do not read position if an MDIChild form
  if AForm.formStyle <> fsMDIChild then
  begin
    // Read form position, -1 if not stored in registry
    LTop := readInteger(LINISection, 'Top',    -1);
    LLeft := readInteger(LINISection, 'Left',   -1);
    // The form pos was found in the registr
    if (LTop <> -1) and (LLeft <> -1) then
    begin
      AForm.Top   := readInteger(LINISection, 'Top',    AForm.Top);
      AForm.Left  := readInteger(LINISection, 'Left',   AForm.Left);
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
      LHeight := AForm.Height
    else
      LHeight := AHeight;
    if AWidth = -1 then
      LWidth := AForm.Width
    else
      LWidth := AWidth;
    AForm.Height := readInteger(LINISection, 'Height', LHeight);
    AForm.Width := readInteger(LINISection, 'Width',  LWidth);
  end;
  AForm.WindowState := TWindowState(ReadInteger(LINISection, 'WindowState', ord(wsNormal)));

  // If the form is off screen (positioned outside all monitor screens) then
  // center the form on screen.
  {$IFDEF MSWINDOWS}
  if (AForm.FormStyle <> fsMDIChild) {$IFNDEF FPC} and tiFormOffScreen(AForm) {$ENDIF} then
  begin
    if Assigned(Application.MainForm) and (Application.MainForm <> AForm) then
      AForm.Position := poMainFormCenter
    else
      AForm.Position:= poScreenCenter;
  end;
  {$ENDIF MSWINDOWS}

end;

procedure TtiGUIINIFile.WriteFormState(AForm: TForm);
var
  LINISection: string;
begin
  LINISection := AForm.name + 'State';
  writeInteger(LINISection, 'WindowState', ord(AForm.WindowState));
  if AForm.WindowState = wsNormal then
  begin
    writeInteger(LINISection, 'Top',    AForm.Top);
    writeInteger(LINISection, 'Left',   AForm.Left);
    if AForm.BorderStyle = bsSizeable then
    begin
      writeInteger(LINISection, 'Height', AForm.Height);
      WriteInteger(LINISection, 'Width',  AForm.Width);
    end;
  end;
end;

initialization
  UGUIINI := nil;

finalization
  UGUIINI.Free;

end.










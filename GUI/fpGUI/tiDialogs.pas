unit tiDialogs;

{$mode objfpc}{$H+}

{ TODO: Port tiProcessing and tiEndProcessing }

interface

uses
  Classes,
  SysUtils,
  Variants,
  fpg_base;

  
  // Call showMessage, but accepts a variant. Good for debugging.
  procedure tiShowMessage(const AArray: Array of Const); overload;
  procedure tiShowMessage(const AValue: variant); overload;

  
  // Show the contents of a TStringList - for debugging
  procedure tiShowStringList(const AStringList: TStringList; const AHeading: TfpgString = 'Show string list'; const AShowModal: boolean = False);
  // Show the contents of a TStrings - for debugging
  procedure tiShowStrings(const AStrings: TStrings; const AHeading: TfpgString = 'Show strings'; const AShowModal: boolean = False);
  // Show a long string - for debugging
  procedure tiShowString(const AStr: TfpgString; const AHeading: TfpgString = 'Show string'; const AShowModal: boolean = False);
  // Show a variant array of variants - for debugging
  procedure tiShowVariant(AValue: Variant; AHeading: TfpgString = 'Show variant');
  // Show the contents of a stream
  procedure tiShowStream(const AValue: TStream; const AHeading: TfpgString = 'Show stream');
  // Show a <Yes>, <No> dialog box, and return true if <Yes> was selected
  function tiAppConfirmation(const AMessage: TfpgString; ATitle: TfpgString = ''): boolean; overload;
  function tiAppConfirmation(const AMessage: TfpgString; const AValues: array of const): boolean; overload;
  // Show a message
  procedure tiAppMessage(const AMessage: TfpgString; ATitle: TfpgString = '');
  // Show a warning
  procedure tiAppWarning(const AMessage: TfpgString; ATitle: TfpgString = '');
  // Show a error message
  procedure tiAppError(const AMessage: TfpgString; ATitle: TfpgString = '');

  // A type of notification window that will disappear by itself
  procedure tiProcessing(const AMessage: TfpgString);
  procedure tiProcessingUpdate(const AUpdateMessage: TfpgString);
  procedure tiEndProcessing;

implementation

uses
  fpg_main,
  fpg_form,
  fpg_memo,
  fpg_label,
  fpg_dialogs,
  fpg_panel,
  fpg_button,
  tiGUIINI,
  tiUtils;

var
  pWorkingForm: TfpgForm;

type
  TProcessingForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: ProcessingForm}
    Bevel1: TfpgBevel;
    lblMessage: TfpgLabel;
    {@VFD_HEAD_END: ProcessingForm}
  public
    procedure AfterCreate; override;
  end;

{ TProcessingForm }

procedure TProcessingForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: ProcessingForm}
  Name := 'ProcessingForm';
  SetPosition(317, 177, 400, 150);
  WindowTitle := 'Processing...';
  Hint := '';
  WindowPosition := wpScreenCenter;
  BackgroundColor := clHilite1;
  WindowType := wtPopup;

  Bevel1 := TfpgBevel.Create(self);
  with Bevel1 do
  begin
    Name := 'Bevel1';
    SetPosition(0, 0, 400, 150);
    Align := alClient;
    Hint := '';
  end;

  lblMessage := TfpgLabel.Create(Bevel1);
  with lblMessage do
  begin
    Name := 'lblMessage';
    SetPosition(2, 2, 396, 146);
    Align := alClient;
    Alignment := taCenter;
    FontDesc := '#Label1';
    Hint := '';
    Layout := tlCenter;
    Text := '...';
    WrapText := True;
    MouseCursor := mcHourGlass;
  end;

  {@VFD_BODY_END: ProcessingForm}
  {%endregion}
end;

type
  TFormShowStrings = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: FormShowStrings}
    Memo1: TfpgMemo;
    btnCopyToClipboard: TfpgButton;
    btnClose: TfpgButton;
    {@VFD_HEAD_END: FormShowStrings}
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CopyToClipboardClicked(Sender: TObject);
    procedure CloseClicked(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterCreate; override;
  end;

{ TFormShowStrings }

procedure TFormShowStrings.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if not (WindowType = wtModalForm) then
  	Action := caFree;
end;

procedure TFormShowStrings.CopyToClipboardClicked(Sender: TObject);
begin
  fpgClipboard.Text := Memo1.Text;
end;

procedure TFormShowStrings.CloseClicked(Sender: TObject);
begin
  Close;
end;

constructor TFormShowStrings.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnClose := @FormClose;
end;

procedure TFormShowStrings.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: FormShowStrings}
  Name := 'FormShowStrings';
  SetPosition(329, 371, 319, 314);
  WindowTitle := 'Show String';
  Hint := '';
  WindowPosition := wpScreenCenter;

  Memo1 := TfpgMemo.Create(self);
  with Memo1 do
  begin
    Name := 'Memo1';
    SetPosition(0, 0, 319, 268);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#Edit2';
    Hint := '';
    TabOrder := 1;
  end;

  btnCopyToClipboard := TfpgButton.Create(self);
  with btnCopyToClipboard do
  begin
    Name := 'btnCopyToClipboard';
    SetPosition(8, 278, 124, 24);
    Anchors := [anLeft,anBottom];
    Text := 'Copy to clipboard';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 1;
    OnClick := @CopyToClipboardClicked;
  end;

  btnClose := TfpgButton.Create(self);
  with btnClose do
  begin
    Name := 'btnClose';
    SetPosition(231, 278, 80, 24);
    Anchors := [anRight,anBottom];
    Text := 'Close';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 2;
    OnClick := @CloseClicked;
  end;

  {@VFD_BODY_END: FormShowStrings}
  {%endregion}
end;


procedure tiShowMessage(const AArray: array of const);
const
  BoolChars: array[Boolean] of Char = ('F', 'T');
var
  i: Integer;
  lsLine: string;
begin
  lsLine := '';
  for I := 0 to High(AArray) do
  begin
    if lsLine <> '' then
      lsLine := lsLine + tiLineEnd;
    with AArray[i] do
      case VType of
        vtInteger:    lsLine := lsLine + IntToStr(VInteger);
        vtBoolean:    lsLine := lsLine + BoolChars[VBoolean];
        vtChar:       lsLine := lsLine + VChar;
        vtExtended:   lsLine := lsLine + FloatToStr(VExtended^);
        vtString:     lsLine := lsLine + VString^;
        vtPChar:      lsLine := lsLine + VPChar;
        vtObject:     lsLine := lsLine + VObject.ClassName;
        vtClass:      lsLine := lsLine + VClass.ClassName;
        vtAnsiString: lsLine := lsLine + string(VAnsiString);
        vtUnicodeString: lsLine := lsLine + UTF8Encode(UnicodeString(VUnicodeString));
        vtCurrency:   lsLine := lsLine + CurrToStr(VCurrency^);
        vtVariant:    lsLine := lsLine + string(VVariant^);
        vtInt64:      lsLine := lsLine + IntToStr(VInt64^);
      end;
  end;
  tiShowMessage(lsLine);
end;

procedure tiShowMessage(const AValue: variant);
begin
  ShowMessage(VarToStr(AValue));
end;

procedure tiShowStringList(const AStringList: TStringList; const AHeading: TfpgString; const AShowModal: boolean);
begin
  tiShowStrings(AStringList, AHeading, AShowModal);
end;

procedure tiShowStrings(const AStrings: TStrings; const AHeading: TfpgString; const AShowModal: boolean);
var
  lForm: TFormShowStrings;
begin
  lForm := TFormShowStrings.Create(nil);
  lForm.WindowTitle := AHeading;
  lForm.Memo1.Lines.Assign(AStrings);
  if AShowModal then
  begin
    lForm.ShowModal;
    lForm.Free;
  end
  else
    lForm.Show;
end;

procedure tiShowString(const AStr: TfpgString; const AHeading: TfpgString; const AShowModal: boolean);
var
  lSL: TStringList;
begin
  lSL := TStringList.Create;
  try
    lSL.Text := AStr;
    tiShowStringList(lSL, AHeading, AShowModal);
  finally
    lSL.Free;
  end;
end;

procedure tiShowVariant(AValue: Variant; AHeading: TfpgString);
var
  ls: string;
begin
  ls := tiVariantArrayToString(AValue);
  tiShowString(ls, AHeading);
end;

procedure tiShowStream(const AValue: TStream; const AHeading: TfpgString);
var
  lStringStream: TStringStream;
begin
  lStringStream := TStringStream.Create('');
  try
    AValue.Position := 0;
    lStringStream.CopyFrom(AValue, AValue.Size);
    tiShowString(lStringStream.DataString, AHeading);
  finally
    lStringStream.Free;
  end;
end;

function tiAppConfirmation(const AMessage: TfpgString; ATitle: TfpgString = ''): boolean;
begin
  Result := TfpgMessageDialog.Question(ATitle, AMessage) = mbYes;
end;

function tiAppConfirmation(const AMessage: TfpgString;
  const AValues: array of const): boolean;
begin
  Result := tiAppConfirmation(Format(AMessage, AValues));
end;

procedure tiAppMessage(const AMessage: TfpgString; ATitle: TfpgString = '');
begin
  TfpgMessageDialog.Information(ATitle, AMessage);
end;

procedure tiAppWarning(const AMessage: TfpgString; ATitle: TfpgString = '');
begin
  TfpgMessageDialog.Warning(ATitle, AMessage);
end;

procedure tiAppError(const AMessage: TfpgString; ATitle: TfpgString = '');
begin
  TfpgMessageDialog.Critical(ATitle, AMessage);
end;

procedure tiProcessing(const AMessage: TfpgString);
begin
  if not Assigned(pWorkingForm) then
  begin
    pWorkingForm := TProcessingForm.Create(nil);
    TProcessingForm(pWorkingForm).lblMessage.Text := AMessage;
    pWorkingForm.Show;
  end
  else
    TProcessingForm(pWorkingForm).lblMessage.Text := AMessage;
  fpgApplication.ProcessMessages;
end;

procedure tiProcessingUpdate(const AUpdateMessage: TfpgString);
begin
  if Assigned(pWorkingForm) then
  begin
    TProcessingForm(pWorkingForm).lblMessage.Text := AUpdateMessage;
    fpgApplication.ProcessMessages;
  end;
end;

procedure tiEndProcessing;
begin
  if Assigned(pWorkingForm) then
    pWorkingForm.Close;
  FreeAndNil(pWorkingForm);
end;

end.


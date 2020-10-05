unit tiDialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Variants,
  fpg_base,
  fpg_main,
  fpg_form,
  fpg_memo,
  fpg_dialogs;

type
  // TtiMessageDlg - A replacement for MessageDlg which gives better control
  // over the message text, and user definable text on the buttons.
  TtiMessageDlg = class(TComponent)
  private
    FForm   : TfpgForm;
    FBtns   : TList;
    FMemo   : TfpgMemo;
    FImage  : TfpgImage;
    FsResult : TfpgString;
    FFont: TfpgFont;
    FImgString: TfpgString;
    procedure   Clear;
    procedure   DoOnClick(Sender: TObject);
    procedure   FormPaint(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function Execute(const AMessage: TfpgString; AOptions: array of TfpgString;
        ACaption: TfpgString; ADialogType: TfpgMsgDlgType): TfpgString;
  end;


  // Call showMessage, but accepts a variant. Good for debugging.
  procedure tiShowMessage(const AArray: Array of Const); overload;
  procedure tiShowMessage(const AValue: variant); overload;

  
  // Show the contents of a TStringList - for debugging
  procedure tiShowStringList(const AStringList: TStringList; const AHeading: TfpgString = 'Show string list'; const AShowModal: boolean = False);
  // Show the contents of a TStrings - for debugging
  procedure tiShowStrings(const AStrings: TStrings; const AHeading: TfpgString = 'Show strings'; const AShowModal: boolean = True);
  // Show a long string - for debugging
  procedure tiShowString(const AStr: TfpgString; const AHeading: TfpgString = 'Show string'; const AShowModal: boolean = True);
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

  // Similar to message dialog of fpGUI, but with fixed width font, and custom buttons
  function tiMessageTextDlg(const AMessage: string; AOptions: array of string;
      ADialogType: TfpgMsgDlgType = mtInformation;
      const ACaption: string = 'Information'): string;


  // A type of notification window that will disappear by itself
  procedure tiProcessing(const AMessage: TfpgString);
  procedure tiProcessingUpdate(const AUpdateMessage: TfpgString);
  procedure tiEndProcessing;

implementation

uses
  Math,
  fpg_label,
  fpg_panel,
  fpg_button,
  tiGUIINI,
  tiUtils,
  tiConstants;

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

{ TtiMessageDlg }

procedure TtiMessageDlg.Clear;
var
  i : integer;
begin
  for i := 0 to FBtns.Count - 1 do
    TObject(FBtns.Items[i]).Free;
end;

procedure TtiMessageDlg.DoOnClick(Sender: TObject);
begin
  FsResult := TfpgButton(Sender).Text;
  FForm.ModalResult := mrOK;
end;

procedure TtiMessageDlg.FormPaint(Sender: TObject);
begin
  FForm.Canvas.DrawImage(cuiBorder, (FForm.Height - cuiImageWidth) div 2,
      fpgImages.GetImage(FImgString));
end;

constructor TtiMessageDlg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FFont := fpgGetFont('#Edit1');

  FForm := TfpgForm.Create(nil);
  with FForm do
  begin
    Name := 'Form_';
    WindowTitle := ' Application error log - ' + ApplicationName;
    WindowPosition := wpScreenCenter;
    OnPaint := @FormPaint;
  end;

  FImage := TfpgImage.Create;

  FMemo := TfpgMemo.Create(FForm);
  with FMemo do
  begin
    Name := 'Memo_';
    BorderStyle := ebsNone;
    FontDesc := '#Edit2';
    SetPosition(cuiImageWidth + cuiBorder*2,
               cuiBorder,
               FForm.Width - cuiImageWidth - cuiBorder*3,
               FForm.Height - cuiBtnHeight - cuiBorder*3);
    ReadOnly   := True;
    TabOrder   := 9999;
    Focusable := False;
//    TabStop    := False;
  end;

  FBtns := TList.Create;
  FsResult := '';
end;

destructor TtiMessageDlg.Destroy;
begin
  Clear;
  FForm.Free;
  FBtns.Free;
  FFont.Free;
  inherited Destroy;
end;

function TtiMessageDlg.Execute(const AMessage: TfpgString;
    AOptions: array of TfpgString; ACaption: TfpgString;
    ADialogType: TfpgMsgDlgType): TfpgString;

const
  cScrollBarHeight = 24;

  function _GetButtonWidth(AOptions: array of TfpgString): integer;
  var
    i : integer;
  begin
    result := 75;
    for i := Low(AOptions) to High(AOptions) do
      result := Max(result, FFont.TextWidth(AOptions[i]));
    result := result + cuiBtnBorder * 2;
  end;

  function _GetMaxLineWidth(ALines: TStrings): integer;
  var
    i : integer;
  begin
    Result := 250; // the default width
    for i := 0 to ALines.Count-1 do
      result := Max(result, FMemo.Font.TextWidth(ALines[i]));
    result := result + cScrollBarHeight;
  end;

var
  i : integer;
  lBtn : TfpgButton;
  lTextRect : TRect;
  lTextWidth : integer;
  lTextHeight : integer;
  lBtnWidth : integer;
  lTotalBtnWidth : integer;
  lBorderWidth : integer;
  lBtnTop  : integer;
  lLHBtn : integer;
  lFormWidth : integer;
  lFormHeight : integer;
  lTextStyle: TfpgTextFlags;
begin
  Clear;

  // Load the correct icon for display
  case ADialogType of
    mtWarning: FImgString := 'stdimg.dlg.warning';
    mtError: FImgString := 'stdimg.dlg.critical';
    mtConfirmation: FImgString := 'stdimg.dlg.help';
    mtInformation: FImgString := 'stdimg.dlg.info';
    else
      FImgString := 'stdimg.dlg.info';
  end;

  lTextStyle := [txtLeft, txtTop, txtWrap];
  FMemo.Text := AMessage;

  lTextWidth := _GetMaxLineWidth(FMemo.Lines);
  lTextHeight := (FMemo.Font.Height * FMemo.Lines.Count) + cScrollBarHeight;

  // Get the required dimenstions of the AMessage
(*
  {$IFNDEF FPC}
  SetRect(lTextRect, 0, 0, Screen.Width div 2, 0);
  DrawText(FForm.Canvas.Handle, PChar(AMessage), Length(AMessage)+1, lTextRect,
    DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK or
    FForm.DrawTextBiDiModeFlagsReadingOnly);
  {$ELSE}
  lTextRect := Bounds(0, 0, Screen.Width div 2, 0);
  lTextStyle.ExpandTabs := True;
  lTextStyle.Wordbreak := True;
  lTextStyle.Alignment := taCenter;
  FForm.Canvas.TextRect(lTextRect, lTextRect.Left, lTextRect.Top, AMessage, lTextStyle);
  {$ENDIF}

  lTextWidth := lTextRect.Right;
  lTextHeight := lTextRect.Bottom;

  if lTextWidth < 250 then
    lTextWidth := 250
  else
    lTextWidth := lTextWidth + 25;
*)

  lBtnWidth := _GetButtonWidth(AOptions);
  lTotalBtnWidth := (lBtnWidth+cuiBorder)*(High(AOptions)+1);
  lBorderWidth  := cuiBorder*3 + cuiImageWidth + 1;

  lFormWidth :=
    Max(lBorderWidth + lTextWidth,
         lTotalBtnWidth + cuiBorder);

  if lFormWidth > (fpgApplication.ScreenWidth div 2) then
  begin
    lFormWidth := fpgApplication.ScreenWidth div 2;
//    FMemo.ScrollBars := ssHorizontal;
//    FMemo.BorderStyle := bsSingle;
    lTextWidth := lFormWidth - lBorderWidth;
    lTextHeight:= lTextHeight + cScrollBarHeight;
  end;

  FForm.Width := lFormWidth;

  lFormHeight :=
    Max(lTextHeight, cuiImageWidth) +
    cuiBorder * 3 + cuiBtnHeight;

  if lFormHeight > (fpgApplication.ScreenHeight div 2) then
  begin
    lFormHeight := fpgApplication.ScreenHeight div 2;
//    if FMemo.ScrollBars = ssHorizontal  then
//      FMemo.ScrollBars := ssBoth
//    else
//      FMemo.ScrollBars := ssVertical;
//    FMemo.BorderStyle := bsSingle;
    FForm.Height := lFormHeight;
    lTextHeight := lFormHeight - FMemo.Top - cuiBtnHeight - cuiBorder*2;
  end;

  FForm.Height := lFormHeight;
  FMemo.SetPosition(cuiBorder*2 + cuiImageWidth, cuiBorder, lTextWidth, lTextHeight);

  lBtnTop := FForm.Height - cuiBtnHeight - cuiBorder;

//  FImage.Top := (lBtnTop - cuiImageWidth) div 2;

//  FMemo.Lines.Text := AMessage;

  lLHBtn := (FForm.Width -
              (lBtnWidth + cuiBorder) * (High(AOptions)+1)) div 2;

  for i := Low(AOptions) to High (AOptions) do
  begin
    lBtn := TfpgButton.Create(FForm);
//    lBtn.ParentFont := False;
//    lBtn.Parent    := FForm;
    lBtn.Top       := lBtnTop;
    lBtn.Width     := lBtnWidth;
    lBtn.Left      := lLHBtn + (lBtn.Width + cuiBorder) * i;
    lBtn.Anchors   := [anBottom, anLeft];
    lBtn.Text      := AOptions[i];
    lBtn.OnClick   := @DoOnClick;
    lBtn.TabOrder  := i;
    if i = Low(AOptions) then
      lBtn.Default := True;
//    if i = High(AOptions) then
//      lBtn.Cancel := True;
    FBtns.Add(lBtn);
  end;
  FForm.WindowTitle := ' ' + ACaption;
  FMemo.Anchors := [ anTop, anLeft, anBottom, anRight ];

  FForm.ShowModal;
  Result := FsResult;
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
  MinWidth := 227;
  MinHeight := 110;

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
        {$IF defined(VER2) and (fpc_release>5)}  // FPC 2.5.1 and above
        vtUnicodeString: lsLine := lsLine + UTF8Encode(UnicodeString(VUnicodeString));
        {$ENDIF}
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

function tiMessageTextDlg(const AMessage: string; AOptions: array of string;
    ADialogType: TfpgMsgDlgType = mtInformation;
    const ACaption: string = 'Information'): string;
var
  lForm : TtiMessageDlg;
begin
  lForm := TtiMessageDlg.Create(nil);
  try
//    lForm.FForm.Font.Name := cDefaultFixedFontName;
//    lForm.FForm.Font.Size := 8;
    result := lForm.Execute(AMessage, AOptions, ACaption, ADialogType);
  finally
    lForm.Free;
  end;
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


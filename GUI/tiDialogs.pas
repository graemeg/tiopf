
{ General purpose routines to supplement SysUtils.pas }

unit tiDialogs;

{$I tiDefines.inc}

interface
uses
  SysUtils
  ,Classes
  {$IFNDEF FPC}
  ,WinProcs
  {$ENDIF}
  ,Graphics
  ,Controls
  ,Forms
  ,Dialogs
  ,StdCtrls
  ,Buttons
  {$IFDEF MSWINDOWS}
  ,shellAPI
  ,registry
  {$ENDIF}
  ,ExtCtrls
  ,Math
  ,TypInfo
  {$IFNDEF VER130}
   ,Variants
  {$ENDIF}
  ,tiConstants
 ;


type

  // TtiMessageDlg - A replacement for MessageDlg which gives better control
  // over the message text, and user definable text on the buttons.
  TtiMessageDlg = class(TComponent)
  private
    FForm   : TForm;
    FBtns   : TList;
    FMemo   : TMemo;
    FImage  : TImage;
    FsResult : string;
    procedure   Clear;
    procedure   DoOnClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    Execute(const AMessage: string;
                         pOptions: array of string;
                         pCaption: string;
                         pDialogType: TMsgDlgType): string;
  end;

  TtiInputDlg = class(TComponent)
  private
    procedure   DoOnChange(Sender: TObject);
  public
    function    Execute(var   AValue   : string;
                         const ACaption : string;
                         const APrompt : string;
                         piMaxLength   : integer): boolean;
  end;

  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // * Dialog boxes
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // Show an error dialog box
{$IFDEF MSWINDOWS}
  procedure tiAppError(       const AMessage : string; const AOwnerWindowHandle: HWnd  = 0);
{$ELSE}
  procedure tiAppError(       const AMessage : string);
{$ENDIF}
  // Show an error dialog box
  procedure tiAppException(   const AMessage : string; const AException : exception); overload;
  // Show an error dialog box
  procedure tiAppException(   const AException : exception); overload;
  // Show a message
  procedure tiAppMessage(     const AMessage : string);
  // Show a warning
  procedure tiAppWarning(     const AMessage : string);
  // Show a very, very series error, and shut down the application
  procedure tiTermError(      const AMessage : string);
  // Show a <Yes>, <No> dialog box, and return true if <Yes> was selected
  function  tiAppConfirmation(const sMessage : string): boolean; overload;
  function  tiAppConfirmation(const sMessage : string;
                               const paValues : array of const): boolean; overload;
  // Show a <Yes>, <No>, <Cancel> dialog box and return a mrValue
  function  tiYesNoCancel(    const AMessage : string): integer;
  // Show a <Yes>, <No>, <Cancel>, <All> dialog box and return a mrValue
  function  tiYesNoCancelAll( sMessage : string): integer;
  // Show a dialog box with a combo box of selections. Return the selection.
  function  tiInputDialogCombo(const pStrCaption, pStrPrompt : string;
                                const pIntDefault : integer;
                                const pItems : TStrings): integer;
  // Call showMessage, but accepts a variant. Good for debugging.
  procedure tiShowMessage( const AArray : Array of Const ); overload;
  procedure tiShowMessage( const AValue : variant ); overload;

  {: Show a message dialog, with better control over the message text and buttons}
  function  tiMessageDlg(const AMessage : string;
                          paOptions : array of string;
                          pDialogType : TMsgDlgType = mtInformation;
                          const psCaption : string = 'Information'
                         ): string;

  {: Show a message dialog, with better control over the message text and buttons, and using a
     fixed Font for the message (good for showing code and SQL)}
  function  tiMessageTextDlg(const AMessage : string;
                              paOptions : array of string;
                              pDialogType : TMsgDlgType = mtInformation;
                              const psCaption : string = 'Information'
                            ): string;

  // This code is cloned from Dialogs.InputQuery, with the additional parameter,
  // editLength. (The parameters have been changed around too)
  function tiInputQuery(var AValue : string;
                         const ACaption : string = 'Enter a value';
                         const APrompt : string = '';
                         piMaxLength   : integer = 255): boolean;


  // Show the contents of a TStringList - for debugging
  procedure tiShowStringList(const pStringList : TStringList; const pHeading : string = 'Show string list');
  // Show the contents of a TStrings - for debugging
  procedure tiShowStrings(const AStrings : TStrings; const pHeading : string = 'Show strings');
  // Show a long string - for debugging
  procedure tiShowString(const AStr : string; const pHeading : string = 'Show string');
  // Show a variant array of variants - for debugging
  procedure tiShowVariant(AValue : oleVariant; pHeading : string = 'Show variant');
  // Show the contents of a stream
  procedure tiShowStream(const AValue : TStream; const pHeading : string = 'Show stream');

  procedure tiProcessing(const AMessage : String);
  procedure tiEndProcessing;

implementation
uses
  ClipBrd
  {$IFDEF MSWINDOWS}
  {$IFDEF DELPHI5}
    ,FileCtrl
  {$ENDIF}
  ,Windows
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  ,Types
  {$ENDIF LINUX}
  ,tiUtils
  ,tiINI
  ,tiLog
  {$IFDEF FPC}
  ,LCLType
  {$ENDIF}
  ,tiGUIConstants
 ;
  
var
  pWorkingForm : TForm;

{$IFDEF MSWINDOWS}
procedure tiAppError(const AMessage : string; const AOwnerWindowHandle: HWnd);
begin
  MessageBox(AOwnerWindowHandle,PChar(AMessage),'Error',mb_ok+mb_iconhand);
end;
{$ELSE}
procedure tiAppError(const AMessage : string);
begin
  messageDlg(AMessage,
              mtError,
              [mbOK],
              0);
end;
{$ENDIF}


procedure tiAppException(const AMessage : string; const AException : exception);
begin
  tiAppError(AMessage + #13 + #13 +
              'Error: ' + AException.message)
end;


procedure tiAppException(const AException: exception);
begin
  tiAppError('Error: ' + AException.message);
end;


procedure tiTermError(const AMessage : string);
begin
  tiAppError(AMessage + #13 + #13 +
             'Application will now terminate.');
  LogError('About to execute Application.Terminate');
  application.terminate;
  application.processMessages;
end;


procedure tiAppMessage(const AMessage : string);
begin
  messageDlg(AMessage,
              mtInformation,
              [mbOK],
              0);
end;


procedure tiAppWarning(const AMessage : string);
begin
  messageDlg(AMessage,
              mtWarning,
              [mbOK],
              0);
end;


function tiAppConfirmation(const sMessage : string): boolean;
begin
  result := messageDlg(sMessage,
                        mtConfirmation,
                        [mbYes, mbNo],
                        0) = mrYes;
end;


function  tiAppConfirmation(const sMessage : string;
                             const paValues : array of const): boolean; overload;
begin
  result := tiAppConfirmation(Format(sMessage, paValues));
end;


function tiYesNoCancelAll(sMessage : string): integer;
begin
  result := messageDlg(sMessage,
                        mtConfirmation,
                        [mbYes, mbNo, mbAll, mbCancel],
                        0);
end;


function  tiYesNoCancel(const AMessage : string): integer;
begin
  result := messageDlg(AMessage,
                        mtConfirmation,
                        [mbYes, mbNo, mbCancel],
                        0);
end;


procedure tiProcessing(const AMessage : String);
var
 lLabel : TLabel;
 shape : TShape;
begin
  if pWorkingForm <> nil then Exit;
  pWorkingForm := TForm.Create(Application);
  pWorkingForm.Position := poScreenCenter;
  pWorkingForm.BorderIcons := [];
  pWorkingForm.BorderStyle := bsNone;
  pWorkingForm.Height := 150;
  pWorkingForm.Width := 400;
  pWorkingForm.Color := clBtnHighlight;
  shape := TShape.Create(pWorkingForm);
  shape.Parent := pWorkingForm;
  shape.Align := alClient;
  shape.Brush.Style := bsClear;
  lLabel := TLabel.Create(pWorkingForm);
  with lLabel do
  begin
    Parent   := pWorkingForm;
    Caption  := AMessage;
    WordWrap := true;
    AutoSize := true;
    Alignment := taCenter;
    Layout   := tlCenter;
    Align    := alClient;
    Cursor   := crHourGlass;
    Font.Size := 10;
    Transparent := true;
  end;
  Screen.Cursor := crHourGlass;
  Application.MainForm.Enabled := false;
  pWorkingForm.Show;
  pWorkingForm.Invalidate;
  Application.ProcessMessages;
end;

procedure tiEndProcessing;
begin
 if pWorkingForm =nil then Exit;
 Screen.Cursor := crDefault;
 Application.MainForm.Enabled := true;
 pWorkingForm.Free;
 pWorkingForm := nil;
end;


procedure tiShowStringList(const pStringList : TStringList; const pHeading : string);
begin
  tiShowStrings(pStringList, pHeading);
end;


procedure tiShowStrings(const AStrings : TStrings; const pHeading : string);
var
  lForm: TForm;
  lMemo: TMemo;
begin
  lForm := TForm.Create(nil);
  lMemo := TMemo.Create(lForm);
  try
    lForm.caption    := pHeading;
    lForm.position   := poScreenCenter;
    lForm.name       := 'FormShowStrings';
    lMemo.parent     := lForm;
    lMemo.align      := alClient;
    lMemo.wordWrap   := False;
    lMemo.scrollBars := ssBoth;
    lMemo.Lines.assign(AStrings);
    lMemo.Font.Name  := cDefaultFixedFontName;
    gINI.ReadFormState(lForm);
    lForm.ShowModal;
    gINI.WriteFormState(lForm);
  finally
    lForm.free;
  end;
end;


procedure tiShowString(const AStr : string; const pHeading : string = 'Show string');
var
  lSL : TStringList;
begin
  lSL := TStringList.Create;
  try
    lSL.Text := AStr;
    tiShowStringList(lSL, pHeading);
  finally
    lSL.Free;
  end;
end;


procedure tiShowVariant(AValue : oleVariant; pHeading : string);
var
  ls : string;
begin
  ls := tiVariantArrayToString(AValue);
  tiShowString(ls, pHeading);
end;


procedure tiShowStream(const AValue : TStream; const pHeading : string = 'Show stream');
var
  lStringStream : TStringStream;
begin
  lStringStream := TStringStream.Create('');
  try
    AValue.Position := 0;
    lStringStream.CopyFrom(AValue, AValue.Size);
    tiShowString(lStringStream.DataString);
  finally
    lStringStream.Free;
  end;
end;


function  tiInputDialogCombo(const pStrCaption, pStrPrompt : string;
                              const pIntDefault : integer;
                              const pItems : TStrings): integer;
  procedure _SetComboWidth(pCombo : TComboBox; pCanvas : TCanvas);
  var
    i : integer;
    liWidth : integer;
  begin
    liWidth := pCombo.Width;
    for i := 0 to pCombo.Items.Count - 1 do
      liWidth := Max(liWidth, pCanvas.TextExtent(pCombo.Items[i]).cx);
    pCombo.Width := liWidth + 24;
  end;

var lForm : TForm;
    lLabel : TLabel;
    lCombo : TComboBox;
    lBtnCancel : TBitBtn;
    lBtnOK    : TBitBtn;
const cBorder = 20;

begin
  lForm  := TForm.Create(application);
  lLabel := TLabel.Create(lForm);
  lCombo := TComboBox.Create(lForm);
  try
    lForm.Caption    := pStrCaption;
    lForm.Position   := poScreenCenter;
    lForm.Name       := 'FormInputDialogCombo';
    lForm.BorderStyle := bsDialog;
    gINI.ReadFormState(lForm);

    lLabel.Parent    := lForm;
    lLabel.Caption   := pStrPrompt + ':';
    lLabel.Top       := cBorder;
    lLabel.Left      := cBorder;

    lCombo.Parent    := lForm;
    lCombo.Style     := csDropDownList;
    lCombo.Items.assign(pItems);
    lCombo.ItemIndex := pIntDefault;
    lCombo.Top       := cBorder;
    lCombo.Left      := lLabel.Left + lLabel.Width + cBorder;
    _SetComboWidth(lCombo, lForm.Canvas);
    lForm.ClientWidth := lCombo.Left + lCombo.Width + cBorder;

    lBtnOK := TBitBtn.Create(lForm);
    lBtnOK.Kind      := bkOK;
    lBtnOK.Top       := lCombo.Top + lCombo.Height + cBorder;
    lBtnOK.Parent    := lForm;

    lBtnCancel := TBitBtn.Create(lForm);
    lBtnCancel.Kind  := bkCancel;
    lBtnCancel.Top   := 60;
    lBtnCancel.Left  := lBtnOK.Left + lBtnOK.Width + cBorder;
    lBtnCancel.Parent := lForm;

    lBtnCancel.Left  := lForm.ClientWidth - cBorder - lBtnCancel.Width;
    lBtnOK.Left      :=  lBtnCancel.Left - cBorder - lBtnOK.Width;

    lForm.ClientHeight := lBtnCancel.Top + lBtnCancel.height + cBorder;

    if lForm.showModal = mrOK then begin
       result := lCombo.itemIndex;
    end else begin
      result := -1;
    end;

    gINI.WriteFormState(lForm);
  finally
    lForm.free;
  end;
end;


procedure tiShowMessage( const AValue : variant );
begin
  ShowMessage(VarToStr(AValue));
end;


procedure tiShowMessage( const AArray : Array of Const );
const
  BoolChars: array[Boolean] of Char = ('F', 'T');
var
  i: Integer;
  lsLine : string;
begin
  lsLine := '';
  for I := 0 to High(AArray) do begin
    if lsLine <> '' then
      lsLine := lsLine + Cr;
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
        vtCurrency:   lsLine := lsLine + CurrToStr(VCurrency^);
        vtVariant:    lsLine := lsLine + string(VVariant^);
        vtInt64:      lsLine := lsLine + IntToStr(VInt64^);
    end;
  end;
  tiShowMessage(lsLine);
end;

function tiMessageDlg(const AMessage : string;
                       paOptions : array of string;
                       pDialogType : TMsgDlgType = mtInformation;
                       const psCaption : string = 'Information'
                      ): string;
var
  lForm : TtiMessageDlg;
begin
  lForm := TtiMessageDlg.Create(nil);
  try
    result := lForm.Execute(AMessage, paOptions, psCaption, pDialogType);
  finally
    lForm.Free;
  end;
end;


function  tiMessageTextDlg(const AMessage : string;
                            paOptions : array of string;
                            pDialogType : TMsgDlgType = mtInformation;
                            const psCaption : string = 'Information'
                          ): string;
var
  lForm : TtiMessageDlg;
begin
  lForm := TtiMessageDlg.Create(nil);
  try
    lForm.FForm.Font.Name := cDefaultFixedFontName;
    lForm.FForm.Font.Size := 8;
    result := lForm.Execute(AMessage, paOptions, psCaption, pDialogType);
  finally
    lForm.Free;
  end;
end;


{ TtiMessageDlg }

const
  {$IFNDEF FPC}
  cuIconIDs: array[TMsgDlgType] of PChar = (IDI_EXCLAMATION, IDI_HAND,
                                             IDI_ASTERISK, IDI_QUESTION, nil);
  {$ELSE}
  { TODO -cLinux outstanding : Not sure how to handle this yet! }
  cuIconIDs: array[TMsgDlgType] of PChar = (#0, #1, #2, #3, nil);
  {$ENDIF}

constructor TtiMessageDlg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FForm := TForm.CreateNew(nil, 0);

  with FForm do
  begin
    Caption      := ' Application error log - ' + Application.Title;
    BorderIcons  := [];
    BorderStyle  := bsSizeable;
    Position     := poScreenCenter;
  end;

  FImage := TImage.Create(FForm);
  with FImage do
  begin
    Name         := 'Image';
    Parent       := FForm;
  end;

  FMemo := TMemo.Create(FForm);
  with FMemo do
  begin
    Parent := FForm;
    FMemo.Anchors := [ akTop, akLeft ];
    SetBounds(cuiImageWidth + cuiBorder*2,
               cuiBorder,
               FForm.ClientWidth - cuiImageWidth - cuiBorder*3,
               FForm.ClientHeight - cuiBtnHeight - cuiBorder*3);
    WordWrap   := False;
    {$IFNDEF FPC}
    ParentColor := True;
    {$ENDIF}
    ScrollBars := ssNone;
    BorderStyle := bsNone;
    ReadOnly   := True;
    TabOrder   := 9999;
    TabStop    := False;
  end;

  FImage.SetBounds(cuiBorder,
                    (FForm.ClientHeight - cuiImageWidth) div 2,
                    cuiImageWidth,
                    cuiImageWidth);

  FBtns := TList.Create;

  FsResult := '';
end;


function TtiMessageDlg.Execute(const AMessage: string;
                                pOptions: array of string;
                                pCaption : string;
                                pDialogType : TMsgDlgType): string;
  function _GetButtonWidth(paOptions : array of string): integer;
  var
    i : integer;
  begin
    result := 75;
    for i := Low(paOptions) to High (paOptions) do
      result := Max(result, FForm.Canvas.TextWidth(paOptions[i]));
    result := result + cuiBtnBorder * 2;
  end;

var
  i : integer;
  lBtn : TButton;
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
  {$IFDEF FPC}
  lTextStyle: TTextStyle;
  {$ENDIF}
const
  cScrollBarHeight = 24;
begin
  Clear;

  // Load the correct icon for display
  {$IFDEF MSWINDOWS}
  FImage.Picture.Icon.Handle := LoadIcon(0, cuIconIDs[pDialogType]);
  {$ENDIF MSWINDOWS}

  // Get the required dimenstions of the AMessage
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

  lBtnWidth := _GetButtonWidth(pOptions);
  lTotalBtnWidth := (lBtnWidth+cuiBorder)*(High(pOptions)+1);
  lBorderWidth  := cuiBorder*3 + cuiImageWidth + 1;

  lFormWidth :=
    Max(lBorderWidth + lTextWidth,
         lTotalBtnWidth + cuiBorder);

  if lFormWidth > (Screen.Width div 2) then
  begin
    lFormWidth := Screen.Width div 2;
    FMemo.ScrollBars := ssHorizontal;
    FMemo.BorderStyle := bsSingle;
    lTextWidth := lFormWidth - lBorderWidth;
    lTextHeight:= lTextHeight + cScrollBarHeight;
  end;

  FForm.ClientWidth := lFormWidth;

  lFormHeight :=
    Max(lTextHeight, cuiImageWidth) +
    cuiBorder * 3 + cuiBtnHeight;

  if lFormHeight > (Screen.Height div 2) then
  begin
    lFormHeight := Screen.Height div 2;
    if FMemo.ScrollBars = ssHorizontal  then
      FMemo.ScrollBars := ssBoth
    else
      FMemo.ScrollBars := ssVertical;
    FMemo.BorderStyle := bsSingle;
    FForm.ClientHeight := lFormHeight;
    lTextHeight := lFormHeight - FMemo.Top -
                     cuiBtnHeight - cuiBorder*2;
  end;

  FForm.ClientHeight := lFormHeight;
  FMemo.SetBounds(cuiBorder*2 + cuiImageWidth, cuiBorder, lTextWidth, lTextHeight);

  lBtnTop := FForm.ClientHeight - cuiBtnHeight - cuiBorder;

  FImage.Top :=
    (lBtnTop - cuiImageWidth) div 2;

  FMemo.Lines.Text := AMessage;

  lLHBtn := (FForm.ClientWidth -
              (lBtnWidth + cuiBorder) * (High(pOptions)+1)) div 2;

  for i := Low(pOptions) to High (pOptions) do
  begin
    lBtn := TButton.Create(nil);
    lBtn.ParentFont := False;
    lBtn.Parent    := FForm;
    lBtn.Top       := lBtnTop;
    lBtn.Width     := lBtnWidth;
    lBtn.Left      := lLHBtn + (lBtn.Width + cuiBorder) * i;
    lBtn.Caption   := pOptions[i];
    lBtn.OnClick   := DoOnClick;
    lBtn.TabOrder  := i;
    if i = Low(pOptions) then
      lBtn.Default := True;
    if i = High(pOptions) then
      lBtn.Cancel := True;
    FBtns.Add(lBtn);
  end;
  FForm.Caption := ' ' + pCaption;
  FMemo.Anchors := [ akTop, akLeft, akBottom, akRight ];

  FForm.ShowModal;
  Result := FsResult;
end;


procedure TtiMessageDlg.Clear;
var
  i : integer;
begin
  for i := 0 to FBtns.Count - 1 do
    TObject(FBtns.Items[i]).Free;
end;


destructor TtiMessageDlg.Destroy;
begin
  Clear;
  FForm.Free;
  FBtns.Free;
  inherited;
end;


procedure TtiMessageDlg.DoOnClick(sender: TObject);
begin
  FsResult := TButton(Sender).Caption;
  FForm.ModalResult := mrOK;
end;


{ This code is cloned from Dialogs.InputQuery, with the additional parameter,
  editLength. (The parameters have been changed around too) }
function tiInputQuery(var   AValue   : string;
                       const ACaption : string  = 'Enter a value';
                       const APrompt : string  = '';
                       piMaxLength   : integer = 255): boolean;
var
  lForm : TtiInputDlg;
begin
  lForm := TtiInputDlg.Create(nil);
  try
    result := lForm.Execute(AValue, aCaption, aPrompt, piMaxLength);
  finally
    lForm.Free;
  end;
end;


{ TtiInputDlg }

procedure TtiInputDlg.DoOnChange(Sender: TObject);
var
  lButton : TButton;
begin
  lButton := TButton(TEdit(Sender).Parent.FindComponent('btnOK'));
  lButton.Enabled := TEdit(Sender).Text <> '';
end;


function TtiInputDlg.Execute(var AValue: string; const ACaption,
  APrompt: string; piMaxLength: integer): boolean;
  function GetAveCharSize(Canvas: TCanvas): TPoint;
  var
    I: Integer;
    Buffer: array[0..51] of Char;
  begin
    for I := 0 to 25 do Buffer[I]:= Chr(I + Ord('A'));
    for I := 0 to 25 do Buffer[I + 26]:= Chr(I + Ord('a'));
    {$IFDEF MSWINDOWS}
    GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    TSize(Result):= Canvas.TextExtent(Buffer);
    {$ENDIF LINUX}
    Result.X := Result.X div 52;
  end;
var
  Form: TForm;
  Prompt: TLabel;
  Edit: TEdit;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
  lBtnOK, lBtnCancel: TButton;
begin
  inherited;
  Result := False;
  Form := TForm.Create(Application);
  with Form do
    try
      Canvas.Font  := Font;
      DialogUnits  := GetAveCharSize(Canvas);
      BorderStyle  := bsDialog;
      Caption      := ACaption;
      ClientWidth  := MulDiv(180, DialogUnits.X, 4);
      ClientHeight := MulDiv(63, DialogUnits.Y, 8);
      Position     := poScreenCenter;

      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent     := Form;
        AutoSize   := True;
        Left       := MulDiv(8, DialogUnits.X, 4);
        Top        := MulDiv(8, DialogUnits.Y, 8);
        if APrompt = '' then
          Caption := ACaption
        else
          Caption := APrompt;

      end;

      Edit := TEdit.Create(Form);
      with Edit do
      begin
        Parent     := Form;
        Left       := Prompt.Left;
        Top        := MulDiv(19, DialogUnits.Y, 8);
        Width      := MulDiv(164, DialogUnits.X, 4);
        MaxLength  := piMaxLength;
        Text       := AValue;
        OnChange   := DoOnChange;
        SelectAll;
      end;
      ButtonTop    := MulDiv(41, DialogUnits.Y, 8);
      ButtonWidth  := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);

      lBtnOK := TButton.Create(Form);
      with lBtnOK do
      begin
        Name       := 'btnOK';
        Parent     := Form;
        Caption    := '&OK';
        ModalResult := mrOK;
        Default    := True;
        SetBounds(MulDiv(38, DialogUnits.X, 4), ButtonTop, ButtonWidth, ButtonHeight);
      end;

      lBtnCancel := TButton.Create(Form);
      with lBtnCancel do
      begin
        Name       := 'btnCancel';
        Parent     := Form;
        Caption    := '&Cancel';
        ModalResult := mrCancel;
        Cancel     := True;
        SetBounds(MulDiv(92, DialogUnits.X, 4), ButtonTop, ButtonWidth, ButtonHeight);
      end;

      if ShowModal = mrOk then
      begin
        AValue      := Edit.Text;
        Result     := True;
      end;
    finally
      Form.Free;
    end;
end;

end.


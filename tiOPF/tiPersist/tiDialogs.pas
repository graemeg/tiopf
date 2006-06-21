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
    May 2000, Peter Hinrichsen, Made open source

  Purpose:
    General purpose routines to supplement SysUtils.pas

  ToDo:
    Make string params const

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiDialogs;

interface
uses
  SysUtils
  ,Classes
  {$IFDEF MSWINDOWS}
  ,WinProcs
  ,Graphics
  ,Controls
  ,Forms
  ,Dialogs
  ,StdCtrls
  ,Buttons
  ,shellAPI
  ,registry
  ,ExtCtrls
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  ,QGraphics
  ,QControls
  ,QForms
  ,QDialogs
  ,QStdCtrls
  ,QButtons
  ,QExtCtrls
  {$ENDIF LINUX}
  ,Math
  ,IniFiles
  ,TypInfo
  {$IFNDEF VER130}
   ,Variants
  {$ENDIF}
  ;

const
  csWinDateFormat     = 'dd/MM/yyyy' ;
  csWinTimeFormat     = 'hh:mm:ss' ;
  csWinDateTimeFormat = 'dd/MM/YYYY hh:mm:ss' ;


type

  // TtiMessageDlg - A replacement for MessageDlg which gives better control
  // over the message text, and user definable text on the buttons.
  //----------------------------------------------------------------------------
  TtiMessageDlg = class( TComponent )
  private
    FForm    : TForm ;
    FBtns    : TList ;
    FMemo    : TMemo ;
    FImage   : TImage ;
    FsResult : string ;
    Procedure Clear ;
    Procedure DoOnClick( sender : TObject ) ;
  public
    Constructor Create( owner : TComponent ) ; override ;
    Destructor  Destroy ; override ;
    Function    Execute( const pMessage : string ;
                         pOptions : array of string ;
                         pCaption : string ;
                         pDialogType : TMsgDlgType ) : string ;
  end ;

  TtiInputDlg = class( TComponent )
  private
    procedure DoOnChange( Sender : TObject ) ;
  public
    function    Execute( var   Value    : string ;
                         const ACaption : string ;
                         const APrompt  : string ;
                         piMaxLength    : integer ) : boolean ;
  end ;

  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // * Dialog boxes
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // Show an error dialog box
  procedure tiAppError(        const pMessage : string ) ;
  // Show an error dialog box
  procedure tiAppException(    const pMessage : string ; const pException : exception ) ; overload ;
  // Show an error dialog box
  procedure tiAppException(    const pException : exception ) ; overload ;
  // Show a message
  procedure tiAppMessage(      const pMessage : string ) ;
  // Show a warning
  procedure tiAppWarning(      const pMessage : string ) ;
  // Show a very, very series error, and shut down the application
  procedure tiTermError(       const pMessage : string ) ;
  // Show a <Yes>, <No> dialog box, and return true if <Yes> was selected
  function  tiAppConfirmation( const sMessage : string ) : boolean ; overload ;
  function  tiAppConfirmation( const sMessage : string ;
                               const paValues : array of const ) : boolean ; overload ;
  // Show a <Yes>, <No>, <Cancel> dialog box and return a mrValue
  function  tiYesNoCancel(     const pMessage : string ) : word ;
  // Show a <Yes>, <No>, <Cancel>, <All> dialog box and return a mrValue
  { NOTE: Qt & CLX doesn't allow more than 3 buttons. }
  {$IFDEF MSWINDOWS}
  function  tiYesNoCancelAll(  sMessage : string ) : word ;
  {$ENDIF MSWINDOWS}
  // Show a dialog box with a combo box of selections. Return the selection.
  function  tiInputDialogCombo( const pStrCaption, pStrPrompt : string ;
                                const pIntDefault : integer ;
                                const pItems : TStrings ) : integer ;
  // Call showMessage, but accepts a variant. Good for debugging.
  procedure tiShowMessage(  const pA : Array of Const  ) ; overload ;
  procedure tiShowMessage(  const pValue : variant  ) ; overload ;
  // Show a message dialog, with better control over the message text and buttons
  function  tiMessageDlg( const psMessage : string ;
                          paOptions : array of string ;
                          pDialogType : TMsgDlgType = mtInformation ;
                          const psCaption : string = 'Information'
                          ) : string ;
  // This code is cloned from Dialogs.InputQuery, with the additional parameter,
  // editLength. (The parameters have been changed around too)
  function tiInputQuery( var Value : string ;
                         const ACaption : string = 'Enter a value' ;
                         const APrompt  : string = '' ;
                         piMaxLength    : integer = 255 ) : boolean ;


  // Show the contents of a TStringList - for debugging
  procedure tiShowStringList( const pStringList : TStringList ; const pHeading : string = 'Show string list' ) ;
  // Show the contents of a TStrings - for debugging
  procedure tiShowStrings( const pStrings : TStrings ; const pHeading : string = 'Show strings' ) ;
  // Show a long string - for debugging
  procedure tiShowString( const pStr : string ; const pHeading : string = 'Show string' ) ;
  // Show a variant array of variants - for debugging
  procedure tiShowVariant( pValue : oleVariant ; pHeading : string = 'Show variant' ) ;
  // Show a TPersistent's property names and value (for debugging)
  procedure tiShowProperties( const pPersistent : TPersistent ) ;
  // Show the contents of a stream
  procedure tiShowStream( const pValue : TStream ; const pHeading : string = 'Show stream' ) ;

// Some global constants
const
  ciOK                = 0 ;
  ciRetry             = 1 ;
  ciAbort             = 2 ;

implementation
uses
  {$IFDEF MSWINDOWS}
  ClipBrd
{$IFDEF DELPHI5}
  ,FileCtrl
{$ENDIF}  
  ,Windows
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  QClipBrd
  ,libc
  ,Types
  ,Qt
  {$ENDIF LINUX}
  ,tiUtils
  ,tiRegINI
  ,tiLog
  ;

//------------------------------------------------------------------------------
procedure tiAppError( const pMessage : string ) ;
begin
  messageDlg( pMessage,
              mtError,
              [mbOK],
              0 ) ;
end ;

//------------------------------------------------------------------------------
procedure tiAppException( const pMessage : string ; const pException : exception ) ;
begin
  tiAppError( pMessage + #13 + #13 +
              'Error: ' + pException.message )
end ;

//------------------------------------------------------------------------------
procedure tiAppException(  const pException : exception ) ;
begin
  tiAppError( 'Error: ' + pException.message ) ;
end ;

//------------------------------------------------------------------------------
procedure tiTermError( const pMessage : string ) ;
begin
  tiAppError( pMessage + #13 + #13 +
             'Application will now terminate.' ) ;
  LogError('About to execute Application.Terminate');
  application.terminate ;
  application.processMessages ;
end ;

//------------------------------------------------------------------------------
procedure tiAppMessage( const pMessage : string ) ;
begin
  messageDlg( pMessage,
              mtInformation,
              [mbOK],
              0 ) ;
end ;

//------------------------------------------------------------------------------
procedure tiAppWarning( const pMessage : string ) ;
begin
  messageDlg( pMessage,
              mtWarning,
              [mbOK],
              0 ) ;
end ;

//------------------------------------------------------------------------------
function tiAppConfirmation( const sMessage : string ) : boolean ;
begin
  result := messageDlg( sMessage,
                        mtConfirmation,
                        [mbYes, mbNo],
                        0 ) = mrYes ;
end ;

//------------------------------------------------------------------------------
function  tiAppConfirmation( const sMessage : string ;
                             const paValues : array of const ) : boolean ; overload ;
begin
  result := tiAppConfirmation( Format( sMessage, paValues )) ;
end ;

//------------------------------------------------------------------------------
{$IFDEF MSWINDOWS}
function tiYesNoCancelAll( sMessage : string ) : word ;
begin
  result := messageDlg( sMessage,
                        mtConfirmation,
                        [mbYes, mbNo, mbAll, mbCancel],
                        0 ) ;
end ;
{$ENDIF MSWINDOWS}

//------------------------------------------------------------------------------
function  tiYesNoCancel( const pMessage : string ) : word ;
begin
  result := messageDlg( pMessage,
                        mtConfirmation,
                        [mbYes, mbNo, mbCancel],
                        0 ) ;
end ;

//------------------------------------------------------------------------------
procedure tiShowStringList( const pStringList : TStringList ; const pHeading : string ) ;
begin
  tiShowStrings( pStringList, pHeading ) ;
end ;

//------------------------------------------------------------------------------
procedure tiShowStrings( const pStrings : TStrings ; const pHeading : string ) ;
var
  lForm : TForm ;
  lMemo : TMemo ;
begin
  lForm := TForm.create( nil ) ;
  lMemo := TMemo.create( lForm ) ;
  try
    lForm.caption := pHeading ;
    lForm.position := poScreenCenter ;
    lForm.name := 'FormShowStrings' ;
    lMemo.parent := lForm ;
    lMemo.align := alClient ;
    lMemo.wordWrap  := false ;
    lMemo.scrollBars := ssBoth ;
    lMemo.Lines.assign( Pstrings ) ;
    {$IFDEF MSWINDOWS}
    lMemo.font.name := 'Courier New' ;
    gReg.ReadFormState( lForm ) ;
    lForm.showModal ;
    gReg.WriteFormState( lForm ) ;
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    lMemo.font.name := 'courier' ;
    gINI.ReadFormState( lForm ) ;
    lForm.showModal ;
    gINI.WriteFormState( lForm ) ;
    {$ENDIF LINUX}
  finally
    lForm.free ;
  end ;
end ;

//------------------------------------------------------------------------------
procedure tiShowString( const pStr : string ; const pHeading : string = 'Show string' ) ;
var
  lSL : TStringList ;
begin
  lSL := TStringList.Create ;
  try
    lSL.Text := pStr ;
    tiShowStringList( lSL, pHeading ) ;
  finally
    lSL.Free ;
  end ;
end ;

//------------------------------------------------------------------------------
procedure tiShowVariant( pValue : oleVariant ; pHeading : string ) ;
var
  ls : string ;
begin
  ls := tiVariantArrayToString( pValue ) ;
  tiShowString( ls, pHeading ) ;
end ;

//------------------------------------------------------------------------------
procedure tiShowStream( const pValue : TStream ; const pHeading : string = 'Show stream' ) ;
var
  lStringStream : TStringStream ;
begin
  lStringStream := TStringStream.Create( '' ) ;
  try
    pValue.Position := 0 ;
    lStringStream.CopyFrom( pValue, pValue.Size ) ;
    tiShowString( lStringStream.DataString ) ;
  finally
    lStringStream.Free ;
  end;
end ;

//------------------------------------------------------------------------------
function  tiInputDialogCombo( const pStrCaption, pStrPrompt : string ;
                              const pIntDefault : integer ;
                              const pItems : TStrings ) : integer ;
  procedure _SetComboWidth( pCombo : TComboBox ; pCanvas : TCanvas ) ;
  var
    i : integer ;
    liWidth : integer ;
  begin
    liWidth := pCombo.Width ;
    for i := 0 to pCombo.Items.Count - 1 do
      liWidth := Max( liWidth, pCanvas.TextExtent( pCombo.Items[i] ).cx) ;
    pCombo.Width := liWidth + 24 ;
  end ;

var lForm : TForm ;
    lLabel : TLabel ;
    lCombo : TComboBox ;
    lBtnCancel : TBitBtn ;
    lBtnOK     : TBitBtn ;
const cBorder = 20 ;

begin
  lForm := TForm.create( application ) ;
  lLabel := TLabel.create( lForm ) ;
  lCombo := TComboBox.create( lForm ) ;
  try
    lForm.caption := pStrCaption ;
    lForm.position := poScreenCenter ;
    lForm.name := 'FormInputDialogCombo' ;
    {$IFDEF MSWINDOWS}
    lForm.BorderStyle := bsDialog ;
    gReg.ReadFormState( lForm ) ;
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    lForm.BorderStyle := fbsDialog ;
    gINI.ReadFormState( lForm ) ;
    {$ENDIF LINUX}

    lLabel.parent := lForm ;
    lLabel.caption := pStrPrompt + ':' ;
    lLabel.top     := cBorder ;
    lLabel.left    := cBorder ;

    lCombo.parent := lForm ;
    lCombo.Style := csDropDownList ;
    lCombo.Items.assign( pItems ) ;
    lCombo.ItemIndex := pIntDefault ;
    lCombo.Top := cBorder ;
    lCombo.Left := lLabel.Left + lLabel.Width + cBorder ;
    _SetComboWidth( lCombo, lForm.Canvas ) ;
    lForm.ClientWidth := lCombo.Left + lCombo.Width + cBorder ;

    lBtnOK := TBitBtn.create( lForm ) ;
    lBtnOK.kind := bkOK ;
    lBtnOK.top  := lCombo.Top + lCombo.Height + cBorder ;
    lBtnOK.parent := lForm ;

    lBtnCancel := TBitBtn.create( lForm ) ;
    lBtnCancel.kind := bkCancel ;
    lBtnCancel.top := 60 ;
    lBtnCancel.Left := lBtnOK.Left + lBtnOK.Width + cBorder ;
    lBtnCancel.parent := lForm ;

    lBtnCancel.Left := lForm.ClientWidth - cBorder - lBtnCancel.Width ;
    lBtnOK.Left     :=  lBtnCancel.Left - cBorder - lBtnOK.Width ;

    lForm.ClientHeight := lBtnCancel.Top + lBtnCancel.height + cBorder ;

    if lForm.showModal = mrOK then begin
       result := lCombo.itemIndex ;
    end else begin
      result := -1 ;
    end ;

    {$IFDEF MSWINDOWS}
    gReg.WriteFormState( lForm ) ;
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    gINI.WriteFormState( lForm ) ;
    {$ENDIF LINUX}

  finally
    lForm.free ;
  end ;

end ;

//------------------------------------------------------------------------------
procedure tiShowMessage(  const pValue : variant  ) ;
begin
  ShowMessage( VarToStr( pValue )) ;
end ;

//------------------------------------------------------------------------------
procedure tiShowMessage(  const pA : Array of Const  ) ;
const
  BoolChars: array[Boolean] of Char = ('F', 'T');
var
  i: Integer;
  lsLine : string ;
begin
  lsLine := '';
  for I := 0 to High(pA) do begin
    if lsLine <> '' then
      lsLine := lsLine + Cr ;
    with pA[i] do
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
  end ;
  tiShowMessage( lsLine ) ;
end ;

//------------------------------------------------------------------------------
function tiMessageDlg( const psMessage : string ;
                       paOptions : array of string ;
                       pDialogType : TMsgDlgType = mtInformation ;
                       const psCaption : string = 'Information'
                       ) : string ;
var
  lForm : TtiMessageDlg ;
begin
  lForm := TtiMessageDlg.Create( nil ) ;
  try
    result := lForm.Execute( psMessage, paOptions, psCaption, pDialogType ) ;
  finally
    lForm.Free ;
  end ;
end ;


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiMessageDlg
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
const
  cuiBorder    = 16 ;
  cuiBtnBorder = 8 ;
  cuiBtnHeight = 25 ;
  cuiBtnWidth  = 75 ;
  cuiImageWidth = 32 ;
  {$IFDEF MSWINDOWS}
  cuIconIDs: array[TMsgDlgType] of PChar = ( IDI_EXCLAMATION, IDI_HAND,
                                             IDI_ASTERISK, IDI_QUESTION, nil);
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  { TODO -cLinux outstanding : Not sure how to handle this yet! }
  cuIconIDs: array[TMsgDlgType] of PChar = ( #0, #1, #2, #3, nil);
  {$ENDIF LINUX}

constructor TtiMessageDlg.Create(owner: TComponent);
begin
  inherited Create( Owner ) ;
  FForm := TForm.CreateNew( nil ) ;

  with FForm do begin
    Caption := ' Application error log - ' + Application.Title ;
    BorderIcons := [] ;
    {$IFDEF MSWINDOWS}
    BorderStyle   := bsDialog ;
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    BorderStyle   := fbsDialog ;
    {$ENDIF LINUX}
    Position := poScreenCenter ;
  end ;

  FImage := TImage.Create(FForm) ;
  with FImage do
  begin
    Name := 'Image';
    Parent := FForm ;
  end;

  FMemo := TMemo.Create( FForm ) ;
  with FMemo do begin
    Parent := FForm ;
    FMemo.Anchors := [ akTop, akLeft ] ;
    SetBounds( cuiImageWidth + cuiBorder*2,
               cuiBorder,
               FForm.ClientWidth - cuiImageWidth - cuiBorder*3,
               FForm.ClientHeight - cuiBtnHeight - cuiBorder*3 ) ;
    WordWrap    := false ;
    ParentColor := true ;
    ScrollBars  := ssNone ;
    BorderStyle := bsNone ;
    ReadOnly    := true ;
    TabOrder    := 9999 ;
    TabStop     := false ;
  end ;

  FImage.SetBounds( cuiBorder,
                    (FForm.ClientHeight - cuiImageWidth ) div 2,
                    cuiImageWidth,
                    cuiImageWidth );

  FBtns := TList.Create ;

  FsResult := '' ;

end;

// -----------------------------------------------------------------------------
function TtiMessageDlg.Execute( const pMessage: string;
                                pOptions: array of string;
                                pCaption : string ;
                                pDialogType : TMsgDlgType ): string;
  function _GetButtonWidth( paOptions : array of string ) : integer ;
  var
    i : integer ;
  begin
    result := 75 ;
    for i := Low( paOptions ) to High ( paOptions ) do
      result := Max( result, FForm.Canvas.TextWidth( paOptions[i] )) ;
    result := result + cuiBtnBorder * 2 ;
  end ;

var
  i : integer ;
  lBtn : TButton ;
  lTextRect : TRect ;
  lTextWidth : integer ;
  lTextHeight : integer ;
  lBtnWidth : integer ;
  lTotalBtnWidth : integer ;
  lBorderWidth  : integer ;
  lBtnTop   : integer ;
  lLHBtn : integer ;
  lFormWidth : integer ;
  lFormHeight : integer ;
begin
  Clear ;

  // Load the correct icon for display
  {$IFDEF MSWINDOWS}
  FImage.Picture.Icon.Handle := LoadIcon(0, cuIconIDs[pDialogType]);
  {$ENDIF MSWINDOWS}

  // Get the required dimenstions of the psMessage
  {$IFDEF MSWINDOWS}
  SetRect(lTextRect, 0, 0, Screen.Width div 2, 0);
  DrawText(FForm.Canvas.Handle, PChar(pMessage), Length(pMessage)+1, lTextRect,
    DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK or
    FForm.DrawTextBiDiModeFlagsReadingOnly ) ;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  lTextRect := Bounds(0, 0, Screen.Width div 2, 0);
  FForm.Canvas.TextRect(lTextRect,lTextRect.Left,lTextRect.Top, pMessage,
      Integer(AlignmentFlags_WordBreak) or
      Integer(AlignmentFlags_ExpandTabs) or
      Integer(AlignmentFlags_AlignCenter) );
  {$ENDIF LINUX}

  lTextWidth := lTextRect.Right;
  lTextHeight := lTextRect.Bottom ;

  if lTextWidth < 250 then
    lTextWidth := 250
  else
    lTextWidth := lTextWidth + 25 ;

  lBtnWidth := _GetButtonWidth( pOptions ) ;
  lTotalBtnWidth := (lBtnWidth+cuiBorder)*(High( pOptions )+1) ;
  lBorderWidth   := cuiBorder*3 + cuiImageWidth + 1 ;

  lFormWidth :=
    Max( lBorderWidth + lTextWidth,
         lTotalBtnWidth + cuiBorder ) ;

  if lFormWidth > ( Screen.Width * 4 div 5 ) then
  begin
    lFormWidth := Screen.Width * 4 div 5 ;
    FMemo.ScrollBars  := ssHorizontal ;
    FMemo.BorderStyle := bsSingle ;
    lTextWidth := lFormWidth - lBorderWidth ;
  end ;

  FForm.ClientWidth := lFormWidth ;

  lFormHeight :=
    Max( lTextHeight, cuiImageWidth ) +
    cuiBorder * 3 + cuiBtnHeight ;

  if lFormHeight > ( Screen.Height * 3 div 4 ) then
  begin
    lFormHeight := Screen.Height * 3 div 4 ;
    if FMemo.ScrollBars = ssHorizontal  then
      FMemo.ScrollBars := ssBoth
    else
      FMemo.ScrollBars := ssVertical;
    FMemo.BorderStyle := bsSingle ;
    FForm.ClientHeight := lFormHeight ;
    lTextHeight := lFormHeight - FMemo.Top -
                     cuiBtnHeight - cuiBorder*2 ;
  end ;

  FForm.ClientHeight := lFormHeight ;
  FMemo.SetBounds( cuiBorder*2 + cuiImageWidth, cuiBorder, lTextWidth, lTextHeight ) ;

  lBtnTop := FForm.ClientHeight - cuiBtnHeight - cuiBorder ;

  FImage.Top :=
    ( lBtnTop - cuiImageWidth ) div 2 ;

  FMemo.Lines.Text := pMessage ;

  lLHBtn := ( FForm.ClientWidth -
              ( lBtnWidth + cuiBorder ) * (High( pOptions )+1)) div 2 ;

  for i := Low( pOptions ) to High ( pOptions ) do begin
    lBtn := TButton.Create( nil ) ;
    lBtn.Parent := FForm ;
    lBtn.Top := lBtnTop ;
    lBtn.Width := lBtnWidth ;
    lBtn.Left := lLHBtn + ( lBtn.Width + cuiBorder ) * i ;
    lBtn.Caption := pOptions[i] ;
    lBtn.OnClick := DoOnClick ;
    lBtn.TabOrder := i ;
    if i = Low( pOptions ) then
      lBtn.Default := true;
    if i = High( pOptions ) then
      lBtn.Cancel := true;
    FBtns.Add( lBtn ) ;
  end ;
  FForm.Caption := ' ' + pCaption ;
  FMemo.Anchors := [ akTop, akLeft, akBottom, akRight ] ;

  FForm.ShowModal ;
  Result := FsResult ;

end;

// -----------------------------------------------------------------------------
procedure TtiMessageDlg.Clear;
var
  i : integer ;
begin
  for i := 0 to FBtns.Count - 1 do
    TObject( FBtns.Items[i] ).Free ;
end;

// -----------------------------------------------------------------------------
destructor TtiMessageDlg.Destroy;
begin
  Clear ;
  FForm.Free ;
  FBtns.Free ;
  inherited;
end;

// -----------------------------------------------------------------------------
procedure TtiMessageDlg.DoOnClick(sender: TObject);
begin
  FsResult := TButton( Sender ).Caption ;
  FForm.ModalResult := mrOK ;
end;

// This code is cloned from Dialogs.InputQuery, with the additional parameter,
// editLength. (The parameters have been changed around too)
//------------------------------------------------------------------------------
function tiInputQuery( var   Value    : string ;
                       const ACaption : string  = 'Enter a value' ;
                       const APrompt  : string  = '' ;
                       piMaxLength    : integer = 255 ) : boolean ;
var
  lForm : TtiInputDlg ;
begin
  lForm := TtiInputDlg.Create( nil ) ;
  try
    result := lForm.Execute( Value, aCaption, aPrompt, piMaxLength ) ;
  finally
    lForm.Free;
  end ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiInputDlg
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiInputDlg.DoOnChange(Sender: TObject);
var
  lButton : TButton ;
begin
  lButton := TButton( TEdit( Sender ).Parent.FindComponent( 'btnOK' ));
  lButton.Enabled := TEdit( Sender ).Text <> '' ;
end;

function TtiInputDlg.Execute(var Value: string; const ACaption,
  APrompt: string; piMaxLength: integer): boolean;
  function GetAveCharSize(Canvas: TCanvas): TPoint;
  var
    I: Integer;
    Buffer: array[0..51] of Char;
  begin
    for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
    for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));
    {$IFDEF MSWINDOWS}
    GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    TSize(Result) := Canvas.TextExtent(Buffer);
    {$ENDIF LINUX}
    Result.X := Result.X div 52;
  end;
var
  Form : TForm ;
  Prompt : TLabel ;
  Edit : TEdit ;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
  lBtnOK, lBtnCancel : TButton ;
begin
  inherited ;
  Result := False;
  Form := TForm.Create(Application);
  with Form do
    try
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      {$IFDEF MSWINDOWS}
      BorderStyle := bsDialog;
      {$ENDIF MSWINDOWS}
      {$IFDEF LINUX}
      BorderStyle := fbsDialog;
      {$ENDIF LINUX}
      Caption := ACaption;
      ClientWidth := MulDiv(180, DialogUnits.X, 4);
      ClientHeight := MulDiv(63, DialogUnits.Y, 8);
      Position := poScreenCenter;
      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        AutoSize := True;
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := MulDiv(8, DialogUnits.Y, 8);
        if APrompt = '' then
          Caption := ACaption
        else
          Caption := APrompt ;

      end;
      Edit := TEdit.Create(Form);
      with Edit do
      begin
        Parent := Form;
        Left := Prompt.Left;
        Top := MulDiv(19, DialogUnits.Y, 8);
        Width := MulDiv(164, DialogUnits.X, 4);
        MaxLength := piMaxLength;
        Text := Value;
        OnChange := DoOnChange ;
        SelectAll;
      end;
      ButtonTop := MulDiv(41, DialogUnits.Y, 8);
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      lBtnOK := TButton.Create(Form) ;
      with lBtnOK do
      begin
        Name   := 'btnOK' ;
        Parent := Form;
        Caption := '&OK' ;
        ModalResult := mrOK ;
        Default := true ;
        SetBounds(MulDiv(38, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;
      lBtnCancel := TBitBtn.Create(Form) ;
      with lBtnCancel do
      begin
        Parent := Form;
        Caption := '&Cancel' ;
        ModalResult := mrCancel ;
        Cancel := true ;
        SetBounds(MulDiv(92, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;
      if ShowModal = mrOk then
      begin
        Value := Edit.Text;
        Result := True;
      end;
    finally
      Form.Free;
    end;
end;

//------------------------------------------------------------------------------
procedure tiShowProperties( const pPersistent : TPersistent ) ;
var
  i : integer ;
  ls : string ;
  lsl : TStringList ;
  lslProps : TStringList ;
begin
  lsl := TStringList.Create ;
  try
    lslProps := TStringList.Create ;
    try
      tiGetPropertyNames( pPersistent, lslProps ) ;
      for i := 0 to lslProps.Count - 1 do begin
        ls := GetPropValue( pPersistent, lslProps.Strings[i] ) ;
        lsl.Add( lslProps.Strings[i] + ': ' + ls ) ;
      end;
    finally
      lslProps.free ;
    end ;
    tiShowStringList( lsl ) ;
  finally
    lsl.Free ;
  end ;

end;

end.
















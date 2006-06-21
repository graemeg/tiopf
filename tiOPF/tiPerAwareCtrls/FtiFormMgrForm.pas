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
    November 2000, Peter Hinrichsen, Made open source

  Purpose:
    An abstract form for editing a tiPerAware business object in a modal dialog.

  Classes:
    TFormTIPerEditDialog - The form

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit FtiFormMgrForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  tiPtnVisPerObj, ActnList, StdCtrls, Buttons, FtiDialogAbs, tiReadOnly,
  ExtCtrls, ComCtrls, ToolWin ;

type

  TLogEvent = procedure( const pMessage : string ) of object ;

  TFormLeaveState = (
    flsCanLeaveOpen,
    flsMustFree,
    flsCanNotLeave ) ;

// Paste this into the concrete class
//  protected
//    procedure SetData(const Value: TPerObjAbs); override ;
//    function  FormIsValid : boolean ; override ;

  TFormTIFormMgrForm = class( TForm )
    RO: TtiReadOnly;
    pnlCaption: TPanel;
    lblCaption: TLabel;
    btnSave: TBitBtn;
    btnUndo: TBitBtn;
    btnClose: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbEnterAsTabClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FAL         : TActionList ;
    FOwnsData: boolean;
    FButtonsVisible: boolean;
    FForceCanLeave: boolean;
    FModalForm : boolean ;
    function  GetFormCaption: string;
    procedure SetFormCaption(const Value: string);
    procedure SetButtonsVisible(const Value: boolean);
    procedure SetOwnsData(const Value: boolean);
    function  GetBorderColor: TColor;
    procedure SetBorderColor(const Value: TColor);

  protected
    FaSave      : TAction ;
    FaUndo      : TAction ;
    FaClose     : TAction ;
    FData       : TPerObjAbs ;
    FDataBuffer : TPerObjAbs ;
    FUpdateButtons : boolean ;

    procedure alUpdate(Action: TBasicAction; var Handled: Boolean);virtual;
    procedure aUndoExecute(Sender: TObject);virtual;
    procedure aSaveExecute(Sender: TObject);virtual;
    function  FormIsValid : boolean ; virtual ;
    function  FormIsDirty : boolean ; virtual ;
    procedure SetData(const Value: TPerObjAbs); virtual ;

    procedure aCloseExecute(Sender: TObject);virtual;
    property  Databuffer : TPerObjAbs read FDataBuffer write FDataBuffer ;

    // Implement these in the concrete...
    property  ButtonsVisible : boolean read FButtonsVisible write SetButtonsVisible ;
    property  ForceCanLeave : boolean read FForceCanLeave write FForceCanLeave ;
    procedure DoBeforeSave ; virtual ;
    procedure DoBeforeDisgard ; virtual ;

  public
//    class function Execute( const pData : TPerObjAbs ; pReadOnly : boolean = false ) : boolean ; virtual ;
    property  Data : TPerObjAbs read FData write SetData ;
    property  FormCaption : string read GetFormCaption write SetFormCaption ;
    property  OwnsData : boolean read FOwnsData write SetOwnsData ;
    procedure PositionButtons ;
    procedure SetupButtons ; virtual ;
    function  CanLeaveForm : TFormLeaveState ; virtual ;
    property  ModalForm : Boolean read FModalForm write FModalForm ;
    property  UpdateButtons : boolean read FUpdateButtons write FUpdateButtons ;
    property  BorderColor : TColor read GetBorderColor write SetBorderColor ;
  end;

  TFormTIFormMgrFormClass = class of TFormTIFormMgrForm ;

implementation
uses
  tiUtils
  ,tiRegINI
  ,tiFormMgr
  ,tiDialogs
  ,cTIPersist
  ,Menus
  ,tiLog
  ,tiPtnVisPerObj_Cli
  ,tiObjAbs
  ;

{$R *.DFM}

{
// -----------------------------------------------------------------------------
class function TFormTIFormMgrForm.Execute( const pData: TPerObjAbs ; pReadOnly : boolean = false ): boolean;
var
  lForm : TFormTIFormMgrForm ;
begin
  lForm := Create( nil ) ;
  gFormMgr.ShowForm(lForm, pData, pReadOnly );
end;
}


// -----------------------------------------------------------------------------
procedure TFormTIFormMgrForm.FormCreate(Sender: TObject);
begin
  inherited;
  KeyPreview := true ;

  FAL := TActionList.Create( Self ) ;
  FAL.OnUpdate := ALUpdate ;

  FaSave := TAction.Create( FAL ) ;
  FaSave.ActionList := FAL ;
  FaSave.Caption   := '&Save' ;
  FaSave.OnExecute := aSaveExecute ;
  FaSave.ShortCut  := Shortcut(Word('s'), [ssCtrl]);
  FaSave.Hint      := 'Save changes' ;
  btnSave.Action := FaSave ;

  FaUndo := TAction.Create( FAL ) ;
  FaUndo.ActionList := FAL ;
  FaUndo.Caption   := '&Undo' ;
  FaUndo.Hint := 'Un-do changes' ;
  FaUndo.OnExecute := aUndoExecute ;
  FaUndo.ShortCut  := Shortcut(Word('z'), [ssCtrl]);
  btnUndo.Action := FaUndo ;

  FaClose := TAction.Create( FAL ) ;
  FaClose.ActionList := FAL ;
  FaClose.Caption   := '&Close' ;
  FaClose.OnExecute := aCloseExecute ;
  FaClose.ShortCut  := Shortcut(VK_F4, [ssCtrl]);
  FaClose.Hint      := 'Close this page' ;
  btnClose.Action := FaClose ;

  FOwnsData := false ;
  FForceCanLeave := false ;
  FModalForm := false ;
  FUpdateButtons := true ;

  pnlCaption.TabOrder := ControlCount - 1 ;

end;

// -----------------------------------------------------------------------------
procedure TFormTIFormMgrForm.FormDestroy(Sender: TObject);
begin
  // The currently active form will have it's parent set. If the application is
  // shutting down, then the parent will be destroyed when the main form
  // is destroyed. This will cause the active form to be destroyed twice,
  // with the associated AV. The If statement below stops this.
  if gFormMgr.IndexOf(Self) <> -1 then
    gFormMgr.RemoveForm(Self);
  if OwnsData then
    FData.Free ;
  FDataBuffer.Free ;
  inherited;
end;

// -----------------------------------------------------------------------------
procedure TFormTIFormMgrForm.SetData(const Value: TPerObjAbs);
begin
  FData := Value;
  FDataBuffer.Free ;
  FDataBuffer := FData.Clone ;
  SetupButtons ;
end;

// -----------------------------------------------------------------------------
procedure TFormTIFormMgrForm.alUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  Handled := true ;
  if not FUpdateButtons then
    Exit ; //==>
  Assert( FData.TestValid(TPerObjAbs, true), cTIInvalidObjectError + ' <FData>') ;
  Assert( FDataBuffer.TestValid(TPerObjAbs, true ), cTIInvalidObjectError + '<FDataBuffer>') ;
  try
    FaSave.Enabled := ( FData <> nil ) and FormIsValid and FormIsDirty ;
    FaUndo.Enabled := ( FData <> nil ) and FormIsDirty ;
  except
    on e:exception do
      tiFmtException(e, ClassName, 'alUpdate' ) ;
  end ;
end;

// -----------------------------------------------------------------------------
function TFormTIFormMgrForm.FormIsValid: boolean;
begin
  result := true ;
end;


// -----------------------------------------------------------------------------
procedure TFormTIFormMgrForm.cbEnterAsTabClick(Sender: TObject);
begin
  inherited;
  SetupButtons ;
end;

// -----------------------------------------------------------------------------
procedure TFormTIFormMgrForm.SetupButtons ;
begin
  // No data
  if ( FData = nil ) or
     ( FDataBuffer = nil ) then
  begin
    FaSave.Visible := false ;
    FaUndo.Visible := false ;
  end else
  begin
    FaSave.Visible := FButtonsVisible ;
    FaUndo.Visible := FButtonsVisible and (not FModalForm) ;
  end ;
  PositionButtons;
end ;

procedure TFormTIFormMgrForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
//Log(IntToStr(Ord(Key)) + '  ' + Key );
  if Key = #13 then
  begin
    Key := #0; // Eat the Beep
    SelectNext(ActiveControl AS TWinControl, True, True) // Forward
  end
  else
  if Key = #2 then
    SelectNext(ActiveControl AS TWinControl, False, True) // Backward
end;

procedure TFormTIFormMgrForm.FormShow(Sender: TObject);
{
  procedure _SelectFirst( pWinControl : TCustomControl ) ;
  var
    lControl : TWinControl ;
  begin
    pWinControl.SelectFirst ;
    lControl := pWinControl.Selected ;
    if ( lControl <> nil ) and
       ( lControl.ControlCount > 0 ) then
      _SelectFirst( lControl ) ;
  end ;
}
begin
//  _SelectFirst( Self ) ;
  SelectFirst ;
end;

function TFormTIFormMgrForm.GetFormCaption: string;
begin
  result := lblCaption.Caption ;
end;

procedure TFormTIFormMgrForm.SetFormCaption(const Value: string);
begin
  lblCaption.Caption := Value ;
end;

procedure TFormTIFormMgrForm.PositionButtons;
var
  lLeft : integer ;
begin
  lLeft := ClientWidth - btnClose.Width - 8 ;
  if btnClose.Visible then
  begin
    btnClose.Left := lLeft ;
    Dec( lLeft, btnClose.Width + 8 );
  end ;
  if btnUndo.Visible then
  begin
    btnUndo.Left := lLeft ;
    Dec( lLeft, btnClose.Width + 8 );
  end ;
  if btnSave.Visible then
    btnSave.Left := lLeft ;
end;

// -----------------------------------------------------------------------------
procedure TFormTIFormMgrForm.aSaveExecute(Sender: TObject);
begin
  Assert( FData <> nil, 'FData not assigned' ) ;
  Assert( FDataBuffer <> nil, 'FDataBuffer not assigned' ) ;
  DoBeforeSave ;
  FDataBuffer.Dirty := true ;
  FDataBuffer.Save ;
  FData.Assign( FDataBuffer ) ;
  // Another way, (perhaps safer, but slower) would be to set the form's data
  // to nil, free FDataBuffer and re-clone from Data, then reset the forms data.
  FDataBuffer.ObjectState := Data.ObjectState ;
end;

// -----------------------------------------------------------------------------
procedure TFormTIFormMgrForm.aUndoExecute(Sender: TObject);
begin
  SetData( Data ) ;
end;

// -----------------------------------------------------------------------------
procedure TFormTIFormMgrForm.aCloseExecute(Sender: TObject);
begin
  SendMessage( Application.MainForm.Handle, TI_CLOSEACTIVEFORM, 0, 0 ) ;
end;

procedure TFormTIFormMgrForm.SetButtonsVisible(const Value: boolean);
begin
  FButtonsVisible := Value;
  FaSave.Visible  := Value ;
  FaUndo.Visible  := Value ;
end;

function TFormTIFormMgrForm.CanLeaveForm: TFormLeaveState ;
var
  lResult  : string ;
  lMessage : string ;
  lOptions : array of string  ;
  lFormIsValid : boolean ;
  lFormIsDirty : boolean ;
const
  cSave     = '&Save' ;
  cDisgard  = '&Disgard' ;
  cContinue = '&Continue' ;
begin

  if ForceCanLeave then
  begin
    result := flsCanLeaveOpen;
    Exit ; //==>
  end ;

  lFormIsValid := FormIsValid ;
  lFormIsDirty := FormIsDirty ;
  if ( FData = nil ) or
     (( not lFormIsDirty ) and
      ( lFormIsValid )) then
  begin
      if ModalForm then
        result := flsMustFree
      else
        result := flsCanLeaveOpen ;
  end
  else
  begin
    if lFormIsValid then
    begin
      lMessage :=
        'Do you want to save your changes?' + Cr +
        'Disgard your changes?' + Cr +
        'Or continue editing?';
      SetLength( lOptions, 3 );
      lOptions[0] := cSave;
      lOptions[1] := cDisgard;
      lOptions[2] := cContinue;
    end else
    begin
      lMessage :=
        'Do you want to disgard your changes?' + Cr +
        'Or continue editing?';
      SetLength( lOptions, 2 );
      lOptions[0] := cDisgard;
      lOptions[1] := cContinue;
    end ;

    lResult := tiMessageDlg(lMessage,
                            lOptions, mtConfirmation, 'Save changes?');
    if lResult = cSave then
    begin
      aSaveExecute(nil) ;
      if ModalForm then
        result := flsMustFree 
      else
        result := flsCanLeaveOpen ;
    end
    else if lResult = cDisgard then
    begin
      DoBeforeDisgard ;
      result := flsMustFree ;
    end
    else
      result := flsCanNotLeave ;
  end ;
end;

procedure TFormTIFormMgrForm.DoBeforeDisgard;
begin
  // Implement in concrete
end;

procedure TFormTIFormMgrForm.DoBeforeSave;
begin
  // Implement in concrete
end;

procedure TFormTIFormMgrForm.SetOwnsData(const Value: boolean);
begin
  FOwnsData := Value;
end;

function TFormTIFormMgrForm.FormIsDirty: boolean;
begin
  Assert( FData.TestValid(TtiObjAbs, true), cTIInvalidObjectError );
  result := false ;
  try
    result := ( FData <> nil ) and ( not FData.Equals( FDataBuffer )) ;
  except
    on e:exception do
      tiFmtException(e, ClassName, 'FormIsDirty' ) ;
  end ;
end;

procedure TFormTIFormMgrForm.FormResize(Sender: TObject);
begin
  PositionButtons;
//  btnClose.Left := pnlCaption.ClientWidth - 0 - btnClose.Width ;
//  btnUndo.Left  := btnClose.Left - btnUndo.Width - 4 ;
//  btnSave.Left  := btnUndo.Left - btnSave.Width - 4 ;
end;

function TFormTIFormMgrForm.GetBorderColor: TColor;
begin
  result := pnlCaption.Color ;
end;

procedure TFormTIFormMgrForm.SetBorderColor(const Value: TColor);
begin
  pnlCaption.Color := Value ;
end;

end.




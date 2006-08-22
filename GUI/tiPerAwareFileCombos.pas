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

  Created: Mid 1998

  Purpose: Custom controls and components found on the TechInsite component
           pallet

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiPerAwareFileCombos;

interface

uses
  SysUtils,
{$IFNDEF FPC}
  WinProcs,
  Messages,
{$ENDIF}
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  buttons,
  extCtrls,
  printers,
  fileCtrl,
  comctrls,
  registry,
  Menus,
  tiPerAwareCtrls,
  tiPerAwareCombosAbs,
  tiObject
  ;

type

  TtiPickFile = class( TtiPickerAbs )
  private
    FsDefaultExt : string ;
    FsFilter     : string ;
    FsTitle      : string ;
    FiFilterIndex: integer;
    FInitialDir: string;
  protected
    procedure DoButtonClick( sender : TObject ) ; override ;
  published
    property OnExit ;
    property DefaultExt : string read FsDefaultExt write FsDefaultExt ;
    property Filter     : string read FsFilter  write FsFilter ;
    property FilterIndex : integer read FiFilterIndex write FiFilterIndex ;
    property InitialDir  : string  read FInitialDir   write FInitialDir ;
    property Title      : string   read FsTitle    write FsTitle ;
    property Visible ;
  public
    constructor Create( AOwner : TComponent); override ;
  end ;

  TtiUserDefinedPicker = class ;
  TUserDefinedPickerClick = procedure( var pData : TtiObject ; const pSender : TtiUserDefinedPicker ) of object ;

  TtiUserDefinedPicker = class( TtiPerAwareAbs )
  private
    FOnGetObjectProp: TOnGetObjectPropEvent;
    FOnClick: TUserDefinedPickerClick;
    FFieldNameDisplay: string;
    FValue: TtiObject;
    procedure SetValue(const Value: TtiObject);
  protected
    procedure   DoOnClick( Sender : TObject ) ; override ;
    procedure   DoChange( Sender : TObject ) ; override ;
    procedure   DataToWinControl ; override ;
    procedure   WinControlToData ; override ;
    procedure   SetOnChangeActive( Value : boolean ) ; override ;
    procedure   SetReadOnly(const Value: Boolean);override ;
    procedure   SetControlColor ; override ;
  public
    constructor Create( AOwner : TComponent ) ; override ;
    property    Value : TtiObject read FValue write SetValue ;
    procedure   LinkToData(pData: TtiObject;const pFieldName, pFieldNameDisplay : string); reintroduce ;
  published
    property    OnClick : TUserDefinedPickerClick read FOnClick write FOnClick ;
    property    OnGetObjectProp  : TOnGetObjectPropEvent read FOnGetObjectProp write FOnGetObjectProp ;
    property    FieldNameDisplay : string read FFieldNameDisplay write FFieldNameDisplay ;
  end ;

  TtiPerAwarePickFile = class( TtiPerAwarePickAbs )
  private
    function  GetDefaultExt: string;
    function  GetFilter: string;
    function  GetFilterIndex: integer;
    procedure SetDefaultExt(const AValue: string);
    procedure SetFilter(const AValue: string);
    procedure SetFilterIndex(const AValue: integer);
    function  GetInitialDir: string;
    procedure SetInitialDir(const AValue: string);
  public
    constructor Create( AOwner : TComponent ) ; override ;
  published
    property DefaultExt  : string  read GetDefaultExt  write SetDefaultExt ;
    property Filter      : string  read GetFilter      write SetFilter ;
    property FilterIndex : integer read GetFilterIndex write SetFilterIndex ;
    property InitialDir  : string  read GetInitialDir  write SetInitialDir ;
  end ;

implementation
uses
  TypInfo
{$IFNDEF FPC}
  ,Windows
{$ENDIF}
  ,tiUtils
  ;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiPickFile
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TtiPickFile.Create( AOwner : TComponent ) ;
begin
  inherited Create( AOwner ) ;
  FsDefaultExt := '' ;
  FsFilter     := 'All files|*.*' ;
  FsTitle      := '' ;
end ;

procedure TtiPickFile.DoButtonClick( sender : TObject ) ;
var
  lOD : TOpenDialog ;
begin
  lOD := TOpenDialog.Create( self ) ;
  try
    lOD.title := self.title ;
    lOD.Filter := self.filter ;
    lOD.defaultExt := self.defaultExt ;
    lOD.fileName := self.text ;
    lOD.FilterIndex := FiFilterIndex ;
    lOD.InitialDir := FInitialDir ;
    if lOD.execute then
    begin
      self.text := lOD.fileName ;
      FiFilterIndex := lOD.FilterIndex ;
      DoOnChange( Self ) ;
    end ;
  finally
    lOD.free ;
  end ;
  inherited ;
end ;

{ TtiPerAwarePickFile }

constructor TtiPerAwarePickFile.Create(AOwner: TComponent);
begin
  WinControl := TtiPickFile.Create( self ) ;
  TtiPickFile( WinControl ).OnChange := DoChange ;
  OnDblClick := TtiPickFile( WinControl ).DoButtonClick;
  inherited;
  FLabel.OnDblClick := OnDblClick;
end;

function TtiPerAwarePickFile.GetDefaultExt: string;
begin
  result := TtiPickFile( WinControl ).DefaultExt ;
end;

function TtiPerAwarePickFile.GetFilter: string;
begin
  result := TtiPickFile( WinControl ).Filter ;
end;

function TtiPerAwarePickFile.GetFilterIndex: integer;
begin
  result := TtiPickFile( WinControl ).FilterIndex ;
end;

function TtiPerAwarePickFile.GetInitialDir: string;
begin
  result := TtiPickFile( WinControl ).InitialDir ;
end;

procedure TtiPerAwarePickFile.SetDefaultExt(const AValue: string);
begin
  TtiPickFile( WinControl ).DefaultExt := AValue ;
end;

procedure TtiPerAwarePickFile.SetFilter(const AValue: string);
begin
  TtiPickFile( WinControl ).Filter := AValue ;
end;

procedure TtiPerAwarePickFile.SetFilterIndex(const AValue: integer);
begin
  TtiPickFile( WinControl ).FilterIndex := AValue ;
end;

procedure TtiPerAwarePickFile.SetInitialDir(const AValue: string);
begin
  TtiPickFile( WinControl ).InitialDir := AValue ;
end;

{ TtiUserDefinedPicker }

constructor TtiUserDefinedPicker.Create(AOwner: TComponent);
begin
  WinControl := TtiPickerAbs.Create(Self);
  TtiPickerAbs(WinControl).SpeedButton.OnClick := DoOnClick ;
  TtiPickerAbs(WinControl).Edit.OnChange := DoOnClick;
  CenterWhenLabelIsLeft := true ;
  inherited;
  Height := cuiMinHeight ;
end;

procedure TtiUserDefinedPicker.DataToWinControl;
var
  lValue : TtiObject ;
  lCaption : string ;
begin
{
  if not DataAndPropertyValid then
    Exit ; //==>
  SetOnChangeActive( false ) ;
  TtiPickerAbs( WinControl ).Text := GetPropValue( Data, FieldName ) ;
  SetOnChangeActive( true ) ;
}
  if not DataAndPropertyValid then
    Exit ; //==>
  SetOnChangeActive( false ) ;

  Assert( PropType( Data, FieldName ) = tkClass, 'Property <' + FieldName + '> not an object' ) ;
  Assert( FieldNameDisplay <> '', 'FieldNameDisplay not assigned' ) ;
  if IsPublishedProp( Data, FieldName ) then
    lValue := (GetObjectProp( Data, FieldName ) as TtiObject )
  else
  begin
    Assert( Assigned( OnGetObjectProp ), 'OnGetObjectProp not assigned' ) ;
    lValue := nil ;
    OnGetObjectProp(lValue);
  end ;

  if lValue <> nil then
    lCaption := GetPropValue( lValue, FieldNameDisplay )
  else
    lCaption := '' ;

  TtiPickerAbs( WinControl ).Text := lCaption ;
  SetOnChangeActive( true ) ;

end;

procedure TtiUserDefinedPicker.DoChange(Sender: TObject);
begin
  DataToWinControl;
  DoOnClick(Sender);
end;

procedure TtiUserDefinedPicker.DoOnClick(Sender: TObject);
var
  lData : TtiObject ;
begin
  if Assigned( FOnClick ) then
  begin
    lData := Data ;
    FOnClick( lData, Self ) ;
    // ToDo: OnClick may popup a window that's not modal, so DataToWinControl called
    //       here will be wrong. Require some generic way of Assigning the data back
    //       from the popup window.
    // DataToWinControl;
  end ;
end;

procedure TtiUserDefinedPicker.LinkToData(pData: TtiObject;
  const pFieldName, pFieldNameDisplay: string);
begin
  FieldNameDisplay := pFieldNameDisplay ;
  inherited LinkToData( pData, pFieldName ) ;
end;

procedure TtiUserDefinedPicker.SetControlColor;
begin
  inherited ;

  // First condition, control is read only
  if ReadOnly then
  begin
    TtiPickerAbs( FWinControl ).Edit.Brush.Color := clBtnFace ;
  end ;                                  

  // Second condition, the control is not enabled.
  if not Enabled then
  begin
    TtiPickerAbs( FWinControl ).Edit.Brush.Color := clBtnFace ;
  end ;

  // Third condition, the control is enabled, and not read only
  if Enabled and Not ReadOnly then
  begin
    TtiPickerAbs( FWinControl ).Edit.Brush.Color := clWindow ;
  end ;

  TtiPickerAbs( FWinControl ).Edit.Refresh ;
end;

procedure TtiUserDefinedPicker.SetOnChangeActive(Value: boolean);
begin
  if Value then
    TtiPickerAbs( WinControl ).Edit.OnChange := DoChange
  else
    TtiPickerAbs( WinControl ).Edit.OnChange := nil ;
end;

procedure TtiUserDefinedPicker.SetReadOnly(const Value: Boolean);
begin
  inherited SetReadOnly( Value ) ;
  TtiPickerAbs( WinControl ).ReadOnly := Value ;
end;

procedure TtiUserDefinedPicker.SetValue(const Value: TtiObject);
begin
  FValue := Value;
  DataToWinControl ;
end;

procedure TtiUserDefinedPicker.WinControlToData;
begin
  if not DataAndPropertyValid then
    Exit ; //==>
  Assert( PropType( Data, FieldName ) = tkClass, 'Property <' + FieldName + '> not an object' ) ;
  if IsPublishedProp( Data, FieldName ) then
    SetObjectProp( Data, FieldName, Value );
end;

end.



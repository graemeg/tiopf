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

unit tiButtons;

interface

uses
  SysUtils
  ,Classes
  ,Controls
  ,buttons
  ,extCtrls
  ,comctrls
  ;

type

  // A TCustomPanel with the border set to none, and the caption turned off.
  // This control is not registered with the component pallet, as it
  // is intended for use as the starting point for composite controls.
  TtiPanel = class( TCustomPanel )
  public
    Constructor Create( owner : TComponent ) ; override ;
  end ;

  // An exception which is used in the tiFloat, tiInt, tiCurrency edits
  RangeException = class( Exception ) ;

  // TtiToolBar
  TtiToolBar = class( TToolBar )
  private
  protected
  published
  public
    constructor Create( owner : TComponent ) ; override ;
  end ;

  // TtiButtonPanel
  TtiButtonPanel = class( TCustomPanel )
  private
    FOnBtn2Click: TNotifyEvent;
    FOnBtn1Click: TNotifyEvent;
    FBtn1 : TBitBtn ;
    FBtn2 : TBitBtn ;
    procedure SetOnBtn1Click(const Value: TNotifyEvent);
    procedure SetOnBtn2Click(const Value: TNotifyEvent);
    function GetBtn1Enabled: boolean;
    function GetBtn2Enabled: boolean;
    procedure SetBtn1Enabled(const Value: boolean);
    procedure SetBtn2Enabled(const Value: boolean);
  protected
  published
    property OnBtn1Click : TNotifyEvent read FOnBtn1Click write SetOnBtn1Click ;
    property OnBtn2Click : TNotifyEvent read FOnBtn2Click write SetOnBtn2Click ;
    property Btn1Enabled : boolean      read GetBtn1Enabled write SetBtn1Enabled ;
    property Btn2Enabled : boolean      read GetBtn2Enabled write SetBtn2Enabled ;
    property Visible ;
  public
    Constructor Create( owner : TComponent ) ; override ;
    Destructor  Destroy ; override ;
    procedure   DoBtn1Click( sender : TObject ) ; virtual ;
    procedure   DoBtn2Click( sender : TObject ) ; virtual ;
  end ;

  TtiMicroButton = class( TSpeedButton )
  public
    constructor Create( Owner : TComponent ) ; override ;
  end ;

implementation
uses
  Forms
  ,tiResources
  ;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* File wide funcs and procs
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
{
function tiNumToken( sString, sToken : string ) : integer ;
var i, iCount : integer ;
begin
  iCount := 0 ;
  i := pos( sToken, sString ) ;
  while i <> 0 do begin
    delete( sString, i, length( sToken )) ;
    inc( iCount ) ;
    i := pos( sToken, sString ) ;
  end ;
  result := iCount + 1 ;
end ;
}

{
function tiToken( sString, sToken : string; iNum : integer ) : string ;
var i, iCount, iNumToken : integer ;
begin

  result := '' ;

  iNumToken := tiNumToken( sString, sToken ) ;
  if iNum = 1 then begin
    if pos( sToken, sString ) = 0 then result := sString
    else result := copy( sString, 1, pos( sToken, sString )-1) ;
    end
  else if (iNumToken < iNum-1) or (iNum<1) then begin
    result := '' ;
    end
  else begin

    // Remove leading blocks
    iCount := 1 ;
    i := pos( sToken, sString ) ;
    while (i<>0) and (iCount<iNum) do begin
      delete( sString, 1, i ) ;
      inc( iCount ) ;
      i := pos( sToken, sString ) ;
    end ;

    if (i=0) and (iCount=iNum) then result := sString
    else if (i=0) and (iCount<>iNum) then result := ''
    else result := copy( sString, 1, i-length( sToken )) ;

  end ;
end ;
}

{
function tiStrTran( sStr, sDel, sIns : string ) : string ;
var i : integer ;
begin
  i := pos( sDel, sStr ) ;
  while i <> 0 do begin
    delete( sStr, i, length( sDel )) ;
    insert( sIns, sStr, i ) ;
    i := pos( sDel, sStr ) ;
  end ;
  result := sStr ;
end ;
}
{
function tiRemoveExtension( sValue : string ) : string ;
var i : integer ;
begin
  i := pos( '.', sValue ) ;
  if i <> 0 then begin
    result := copy( sValue, 1, i - 1 ) ;
  end else begin
    result := sValue ;
  end ;
end ;
}

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiToolBar
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiToolBar.Create(owner: TComponent);
begin
  inherited create( owner ) ;
  Flat     := true ;
  Height   := 25 ;
  ShowHint := true ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiPanel
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiPanel.Create(owner: TComponent);
begin
  inherited Create( owner ) ;
  ControlStyle := ControlStyle - [csSetCaption] ;
  BevelInner  := bvNone ;
  BevelOuter  := bvNone ;
  BorderStyle := bsNone ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiButtonPanel
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
{ ToDo 5 -cComponents: TtiButtonPanel: Add variable number of buttons, with var captions and glyphs }
constructor TtiButtonPanel.Create(owner: TComponent);
begin
  inherited Create( owner ) ;
  Width  := 253 ;
  Height := 33  ;
  Align  := alBottom ;
  BevelOuter := bvNone ;
  ControlStyle := ControlStyle - [csSetCaption] ;

  FBtn1 := TBitBtn.Create( nil ) ;
  with FBtn1 do begin
    Parent      := self ;
    Left        := 94 ;
    Top         := 4  ;
    Width       := 75 ;
    Height      := 25 ;
    Anchors     := [akRight, akBottom] ;
    TabOrder    := 0  ;
    Caption     := '&OK' ;
    OnClick     := DoBtn1Click ;
    Default     := true ;
    ModalResult := mrOK ;
    NumGlyphs   := 2 ;
    Glyph.LoadFromResourceName(HInstance, cResTI_Tick16ND);
  end ;

  FBtn2 := TBitBtn.Create( nil ) ;
  with FBtn2 do begin
    Parent      := self ;
    Left        := 174 ;
    Top         := 4 ;
    Width       := 75 ;
    Height      := 25 ;
    Anchors     := [akRight, akBottom] ;
    TabOrder    := 1 ;
    Caption     := '&Cancel' ;
    OnClick     := DoBtn2Click ;
    Cancel      := true ;
    ModalResult := mrCancel ;
    NumGlyphs   := 2 ;
    Glyph.LoadFromResourceName(HInstance, cResTI_Cross16ND);
  end ;
end ;

destructor TtiButtonPanel.Destroy;
begin
  FBtn1.Free ;
  FBtn2.Free ;
  inherited;
end;

procedure TtiButtonPanel.DoBtn1Click(sender: TObject);
begin
  if Assigned( FOnBtn1Click ) then
    FOnBtn1Click( self ) ;
end;

procedure TtiButtonPanel.DoBtn2Click(sender: TObject);
begin
  if Assigned( FOnBtn2Click ) then
    FOnBtn2Click( self ) ;
end;

function TtiButtonPanel.GetBtn1Enabled: boolean;
begin
  result := FBtn1.Enabled ;
end;

function TtiButtonPanel.GetBtn2Enabled: boolean;
begin
  result := FBtn2.Enabled ;
end;

procedure TtiButtonPanel.SetBtn1Enabled(const Value: boolean);
begin
  FBtn1.Enabled := Value ;
end;

procedure TtiButtonPanel.SetBtn2Enabled(const Value: boolean);
begin
  FBtn2.Enabled := Value ;
end;

procedure TtiButtonPanel.SetOnBtn1Click(const Value: TNotifyEvent);
begin
  FOnBtn1Click := Value;
  if Assigned( FOnBtn1Click ) then
    FBtn1.ModalResult := mrNone
  else
    FBtn1.ModalResult := mrOK ;
end;

procedure TtiButtonPanel.SetOnBtn2Click(const Value: TNotifyEvent);
begin
  FOnBtn2Click := Value;
  if Assigned( FOnBtn2Click ) then
    FBtn2.ModalResult := mrNone
  else
    FBtn2.ModalResult := mrCancel ;
end;

{ TtiMicroButton }

constructor TtiMicroButton.Create(Owner: TComponent);
begin
  inherited Create( Owner ) ;
  Height := 12 ;
  Width  := 12 ;
  Flat   := true ;
end;

end.


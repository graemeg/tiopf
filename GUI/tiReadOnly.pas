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
    Aug 2002 Ian Krigsman  Added OnProcessEvent

  Purpose:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiReadOnly;

interface
uses
  Classes
  ;

type
  TtiOnChangeReadOnlyEvent = procedure(pSender: TObject; pReadOnly: boolean) of object;
  TtiOnProcessEvent = procedure(pSender: TObject; var pReadOnly, pProcess: boolean) of object;
  TtiOnProcessFrmEvent = procedure(pFrame: TComponent; var pProcessDetails: boolean) of object;
  TtiOnGetParentEvent = procedure(var pParent: TComponent) of object;

  TtiReadOnly = class(TComponent)
  private
    FReadOnly: boolean;
    FOnChange: TtiOnChangeReadOnlyEvent;
    FOnProcess: TtiOnProcessEvent;
    FEnabled: boolean;
    FOnProcessFrm: TtiOnProcessFrmEvent;
    FProcessFormAndFrame: boolean;
    FOnGetParent: TtiOnGetPArentEvent;
    procedure SetEnabled(const Value: boolean);
  protected
    function GetReadOnly: boolean; virtual;
    procedure SetReadOnly(const Value: boolean); virtual;
    procedure SetComponentsReadOnly(pReadOnly: boolean); virtual;
    function GetParentForm: TComponent;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
  published
    property ReadOnly: boolean read GetReadOnly write SetReadOnly default false;
    property OnChange: TtiOnChangeReadOnlyEvent read FOnChange write FOnChange;
    property OnProcess: TtiOnProcessEvent read FOnProcess write FOnProcess;
    property OnProcessFrm: TtiOnProcessFrmEvent read FOnProcessFrm write FOnProcessFrm;
    property OnGetParent: TtiOnGetPArentEvent read FOnGetParent write FOnGetParent;
    property Enabled: boolean read FEnabled write SetEnabled default false;
    property ProcessFormAndFrame: boolean read FProcessFormAndFrame write FProcessFormAndFrame default false;
  end;

implementation
uses
  Forms
  , TypInfo
  , Dialogs
  ;

{ TtiReadOnly }

constructor TtiReadOnly.Create(owner: TComponent);
begin
  inherited;
  FReadOnly := false;
  FEnabled := false;
end;

destructor TtiReadOnly.Destroy;
begin
  inherited;

end;

function TtiReadOnly.GetParentForm: TComponent;
begin
  result := self;
  while true do
  begin
    if (result is TForm) and
      ((result.Owner is TApplication) or
      (result.Owner = nil)) then
      Break; //==>
    result := result.Owner;
  end;
  if Assigned(FOnGetParent) then
    FOnGetParent(result);
end;

function TtiReadOnly.GetReadOnly: boolean;
begin
  result := FReadOnly;
end;

procedure TtiReadOnly.SetComponentsReadOnly(pReadOnly: boolean);
  procedure _SetComponentsReadOnly(pParent: TComponent);
  var
    i: integer;
    lComponent: TComponent;
    lReadOnly, lProcess: boolean;
  begin
    if pParent=nil then
      exit;
    for i := 0 to pParent.ComponentCount - 1 do
    begin
      lComponent := pParent.Components[i];
      if lComponent = self then
        Continue; //==>
      if IsPublishedProp(lComponent, 'ReadOnly') then
      begin
        lReadOnly := pReadOnly;
        lProcess := True;
        if Assigned(FOnProcess) then
          FOnProcess(lComponent, lReadOnly, lProcess);
        if lProcess then
          SetOrdProp(lComponent, 'ReadOnly', Ord(lReadOnly));
      end;
      if (lComponent is TFrame) or (lComponent is TForm) then
      begin
        lProcess := FProcessFormAndFrame; // Now lProcess is: can I go Deep?
        if Assigned(FOnProcessFrm) then
          FOnProcessFrm(lComponent, lProcess);
        if lProcess then
          _SetComponentsReadOnly(lComponent);
      end;
    end;
  end;
begin
  _SetComponentsReadOnly(GetParentForm);
end;

procedure TtiReadOnly.SetEnabled(const Value: boolean);
begin
  FEnabled := Value;
// Removed IPK
//  if FEnabled then
//    SetReadOnly(FReadOnly);
end;

procedure TtiReadOnly.SetReadOnly(const Value: boolean);
begin
  if not FEnabled then
    Exit; //==>
  FReadOnly := Value;
  SetComponentsReadOnly(FReadOnly);
  if Assigned(FOnChange) then
    FOnChange(Self, FReadOnly);
end;

end.


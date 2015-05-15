unit FClientEdit;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, tiObject, tiModelMediator;

type
  TFormClientEdit = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    paeOID: TEdit;
    paeClientName: TEdit;
    paeClientID: TEdit;
    memoErrors: TMemo;
  private
    FData: TtiObject;
    FMed: TtiModelMediator;
  protected
    procedure   SetData(const Value: TtiObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor  Destroy; override;
    property    Data: TtiObject read FData write SetData;
    class function Execute(const AData : TtiObject; pReadOnly : boolean = false): boolean;
  end;

implementation

{$R *.lfm}

{ TFormClientEdit }

constructor TFormClientEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FMed := TtiModelMediator.Create(self);
end;

destructor TFormClientEdit.Destroy;
begin
  FMed.Active := False;
  FMed.Subject := nil;
  inherited Destroy;
end;

class function TFormClientEdit.Execute(const AData: TtiObject; pReadOnly: boolean): boolean;
var
  lForm: TFormClientEdit;
begin
  lForm := Create(nil);
  try
    lForm.Data := AData;
    // TODO: ignoring pReadOnly for now
    result := lForm.ShowModal = mrOK;
    // TODO: ignoring Undo support for now
  finally
    lForm.Free;
  end;
end;

procedure TFormClientEdit.SetData(const Value: TtiObject);
begin
  paeOID.Text := Value.OID.AsString;
  FMed.AddProperty('ClientName', paeClientName);
  FMed.AddProperty('ClientID', paeClientID);
  FMed.Subject := Value;
  FMed.Active := True;
end;


end.
 

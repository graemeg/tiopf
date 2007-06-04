unit formcontactmaint;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources,{$ENDIF} Forms, Controls, Graphics,
  Dialogs, StdCtrls, Buttons, model, views, ComCtrls;

type
  TContactMaintFrm = class(TForm)
    btnSave: TButton;
    btnCancel: TButton;
    edFName: TEdit;
    edLName: TEdit;
    edEmail: TEdit;
    edMobile: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    ListView1: TListView;
    meComments: TMemo;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edEmailChange(Sender: TObject);
    procedure edFNameChange(Sender: TObject);
    procedure edLNameChange(Sender: TObject);
    procedure edMobileChange(Sender: TObject);
    procedure meCommentsEnter(Sender: TObject);
  private
    FData: TContact;
    mFName: TContact_FirstName_TEdit_Mediator;
    mLName: TContact_LastName_TEdit_Mediator;
    mEMail: TContact_EMail_TEdit_Mediator;
    mMobile: TContact_Mobile_TEdit_Mediator;
    mComments: TContact_Comments_TMemo_Mediator;
    mCity: TContact_City_TCombobox_Mediator;
    procedure SetData(const AValue: TContact);
    procedure SetupMediators;
  public
    class function EditContact(AData: TContact): Boolean;
    property  Data: TContact read FData write SetData;
    
  end; 


implementation

{$IFNDEF FPC}
  {$R *.dfm}
{$ENDIF}

uses
  contactmanager;

{ TContactMaintFrm }

procedure TContactMaintFrm.FormDestroy(Sender: TObject);
begin
  mComments.Free;
  mMobile.Free;
  mEMail.Free;
  mLName.Free;
  mFName.Free;
end;

procedure TContactMaintFrm.FormShow(Sender: TObject);
begin
  SetupMediators;
end;

procedure TContactMaintFrm.edEmailChange(Sender: TObject);
begin
  mEMail.GUIChanged;
end;

procedure TContactMaintFrm.edFNameChange(Sender: TObject);
begin
  mFName.GUIChanged;
end;

procedure TContactMaintFrm.edLNameChange(Sender: TObject);
begin
  mLName.GUIChanged;
end;

procedure TContactMaintFrm.edMobileChange(Sender: TObject);
begin
  mMobile.GUIChanged;
end;

procedure TContactMaintFrm.meCommentsEnter(Sender: TObject);
begin
  mComments.GUIChanged;
end;

procedure TContactMaintFrm.SetData(const AValue: TContact);
begin
  if FData=AValue then exit;
  FData:=AValue;
end;

procedure TContactMaintFrm.SetupMediators;
begin
//  writeln('>>> SetupMediators');

  mFName := TContact_FirstName_TEdit_Mediator.CreateCustom(edFName, Data, 'FirstName', 'Text');
  mLName := TContact_LastName_TEdit_Mediator.CreateCustom(edLName, Data, 'LastName', 'Text');
  mEMail := TContact_EMail_TEdit_Mediator.CreateCustom(edEmail, Data, 'EMail', 'Text');
  mMobile := TContact_Mobile_TEdit_Mediator.CreateCustom(edMobile, Data, 'Mobile', 'Text');
  mComments := TContact_Comments_TMemo_Mediator.CreateCustom(meComments, Data, 'Comments', 'Lines');
//  mCity := TContact_City_TCombobox_Mediator.CreateCustom(gContactManager.CityList, cbCity, Data, 'City');
  
  Data.NotifyObservers;

//  writeln('<<< SetupMediators');
end;

class function TContactMaintFrm.EditContact(AData: TContact): Boolean;
var
  frm: TContactMaintFrm;
begin
  frm := TContactMaintFrm.Create(nil);
  try
    frm.SetData(AData);
    result := frm.ShowModal = mrOK;
  finally
    frm.Free;
  end;
end;

{$IFDEF FPC}
initialization
  {$I formcontactmaint.lrs}
{$ENDIF}

end.


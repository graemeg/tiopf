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

  Purpose:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit FEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  tiDBConnectionPool, StdCtrls, ExtCtrls,  tiPerAwareCtrls,
  tiButtons, tiFocusPanel ;

type
  TFormEdit = class(TForm)
    tiButtonPanel1: TtiButtonPanel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    paeConnectionName: TtiPerAwareEdit;
    paeDatabaseName: TtiPerAwareEdit;
    paeHostName: TtiPerAwareEdit;
    paeUserName: TtiPerAwareEdit;
    paeUserPassword: TtiPerAwareEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tiButtonPanel1Btn1Click(Sender: TObject);
    procedure tiButtonPanel1Btn2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FData: TDBConnectParams;
    procedure SetData(const Value: TDBConnectParams);
    { Private declarations }
  public
    property Data : TDBConnectParams read FData write SetData ;
  end;


implementation
uses
  tiUtils
  ,tiRegINI
  ;

{$R *.DFM}

procedure TFormEdit.FormCreate(Sender: TObject);
begin
  gReg.ReadFormState( self ) ;
end;

procedure TFormEdit.FormDestroy(Sender: TObject);
begin
  gReg.WriteFormState( self ) ;
end;

procedure TFormEdit.SetData(const Value: TDBConnectParams);
begin
  FData := Value;
  paeConnectionName.LinkToData( FData, 'ConnectionName' ) ;
  paeDatabaseName.LinkToData(   FData, 'DatabaseName' ) ;
  paeHostName.LinkToData(       FData, 'HostName' ) ;
  paeUserName.LinkToData(       FData, 'UserName' ) ;
  paeUserPassword.LinkToData(   FData, 'UserPassword' ) ;
end;

procedure TFormEdit.tiButtonPanel1Btn1Click(Sender: TObject);
begin
  if paeConnectionName.Value = '' then
  begin
    paeConnectionName.SetFocus ;
    raise exception.create( 'Please enter a ' + paeConnectionName.Caption ) ;
  end ;

  if paeDatabaseName.Value = '' then
  begin
    paeDatabaseName.SetFocus ;
    raise exception.create( 'Please enter a ' + paeDatabaseName.Caption ) ;
  end ;

  if paeHostName.Value = '' then
  begin
    paeHostName.SetFocus ;
    raise exception.create( 'Please enter a ' + paeHostName.Caption ) ;
  end ;

  if paeUserName.Value = '' then
  begin
    paeUserName.SetFocus ;
    raise exception.create( 'Please enter a ' + paeUserName.Caption ) ;
  end ;

  if paeUserPassword.Value = '' then
  begin
    paeUserPassword.SetFocus ;
    raise exception.create( 'Please enter a ' + paeUserPassword.Caption ) ;
  end ;

  ModalResult := mrOK ;

end;

procedure TFormEdit.tiButtonPanel1Btn2Click(Sender: TObject);
begin
  modalResult := mrCancel ;
end;

procedure TFormEdit.FormShow(Sender: TObject);
Begin
  SelectFirst ;
end;

end.

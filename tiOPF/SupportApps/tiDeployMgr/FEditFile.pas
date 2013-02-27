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

unit FEditFile;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FtiPerEditDialog, tiPerAwareCtrls, ExtCtrls,
  StdCtrls, Buttons, tiPtnVisPerObj, tiPerAwareFileCombos, tiReadOnly,
  tiFocusPanel;

type
  TFormEditFile = class(TFormTIPerEditDialog)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    paeFileName: TtiPerAwarePickFile;
    paeDeployTo: TtiPerAwarePickDirectory;
    paeLaunch: TtiPerAwareCheckBox;
    Label1: TLabel;
    procedure paeFileNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  protected
    procedure SetData(const Value: TPerObjAbs); override ;
    function  FormIsValid : boolean ; override ;
  public
    { Public declarations }
  end;

implementation
uses
  tiUtils
  ,tiRegINI
  ;

{$R *.DFM}

{ TFormEdit }

function TFormEditFile.FormIsValid: boolean;
begin
  result :=
    ( paeFileName.Value <> '' ) and
    ( paeDeployTo.Value <> '' ) ;
end;

procedure TFormEditFile.SetData(const Value: TPerObjAbs);
begin
  inherited SetData( Value ) ;
  paeFileName.LinkToData( DataBuffer, 'DeployFromPathAndName' ) ;
  paeDeployTo.LinkToData( DataBuffer, 'DeployToRoot' ) ;
  paeLaunch.LinkToData(   DataBuffer, 'Launch' ) ;
  if paeDeployTo.Value = '' then
    paeDeployTo.Value :=
      gINI.ReadString( Name, 'DeployTo', '' ) ;
  if paeFileName.Value = '' then
    paeFileName.InitialDir :=
      gINI.ReadString( Name, 'InitialDir', '' ) ;

end;

procedure TFormEditFile.paeFileNameChange(Sender: TObject);
begin
  inherited;
  if paeDeployTo.Value = '' then
    paeDeployTo.Value := ExtractFilePath( paeFileName.Value ) ;
end;

procedure TFormEditFile.FormCreate(Sender: TObject);
begin
  inherited;
  paeFileName.FilterIndex := gINI.ReadInteger( Name, 'FilterIndex', 0 ) ;
end;

procedure TFormEditFile.FormDestroy(Sender: TObject);
begin
  inherited;
  gINI.WriteInteger( Name, 'FilterIndex', paeFileName.FilterIndex ) ;
  gINI.WriteString(  Name, 'DeployTo', paeDeployTo.Value ) ;
  gINI.WriteString(  Name, 'InitialDir', paeFileName.InitialDir ) ;
end;

end.

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
    Run an adhoc query as a script - good for multiple queries, separated by ;s
    Bit of a hack though to get multiple queries running.

  Classes:
    TFormRunAsScript - The form

  ToDo:
    Remove the need to shell out to a script engine. Better to run the quesies
    from within the SQLManager process.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit FRunAsScript;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, tiButtons, tiPerAwareCtrls, tiFocusPanel;

type
  TFormRunAsScript = class(TForm)
    tiButtonPanel1: TtiButtonPanel;
    Label3: TLabel;
    hcAppToRun: TtiPerAwareComboBoxHistory;
    hcParams: TtiPerAwareComboBoxHistory;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tiButtonPanel1Btn1Click(Sender: TObject);
    procedure tiButtonPanel1Btn2Click(Sender: TObject);
  private
    function GetAppToRun: string;
    function GetParams: string;
    { Private declarations }
  public
    property AppToRun : string read GetAppToRun ;
    property Params   : string read GetParams ;
  end;

implementation
uses
  tiUtils
  ,tiRegINI
  ;

{$R *.DFM}

procedure TFormRunAsScript.FormCreate(Sender: TObject);
begin
  gINI.ReadFormState( self ) ;
  hcAppToRun.Value := gINI.ReadString( name, 'AppToRun', '' ) ;
  hcParams.Value   := gINI.ReadString( name, 'Params',   '' ) ;
end;

procedure TFormRunAsScript.FormDestroy(Sender: TObject);
begin
  gINI.WriteFormState( self ) ;
  gINI.WriteString( name, 'AppToRun', hcAppToRun.Value ) ;
  gINI.WriteString( name, 'Params',   hcParams.Value   ) ;
end;

function TFormRunAsScript.GetAppToRun: string;
begin
  result := hcAppToRun.Value ;
end;

function TFormRunAsScript.GetParams: string;
begin
  result := hcParams.Value ;
end;

procedure TFormRunAsScript.tiButtonPanel1Btn1Click(Sender: TObject);
begin
  ModalResult := mrOK ;
end;

procedure TFormRunAsScript.tiButtonPanel1Btn2Click(Sender: TObject);
begin
  ModalResult := mrCancel ;
end;

end.

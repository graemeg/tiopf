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

unit FSplash;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls;

type
  TFormSplash = class(TForm)
    tmr: TTimer;
    Panel1: TPanel;
    Panel2: TPanel;
    PB: TProgressBar;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure FormShow(Sender: TObject);
    procedure tmrTimer(Sender: TObject);
    procedure Label5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure ShowSplash ;

implementation
uses
  tiUtils
  ,tiCommandLineParams
  ;

{$R *.DFM}

procedure ShowSplash ;
var
  lForm : TFormSplash ;
begin
  if gCommandLineParams.IsParam( 'ns' ) then
    Exit ; //==>
  lForm := TFormSplash.Create( nil ) ;
  try
    lForm.ShowModal ;
  finally
    lForm.Free ;
  end ;
end ;

procedure TFormSplash.FormShow(Sender: TObject);
begin
  tmr.Enabled := true ;
end;

procedure TFormSplash.tmrTimer(Sender: TObject);
  procedure _Delay( pMS : DWord ) ;
  var
    lNow : DWord ;
  begin
    lNow := GetTickCount ;
    while GetTickCount < lNow + pMS do
    begin
      Sleep( 100 ) ;
      Application.ProcessMessages ;
    end ;
  end ;
begin
  Tmr.Enabled := false ;
  _Delay( 1000 ) ;
  PB.Position := 1 ;
  _Delay( 1000 ) ;
  PB.Position := 2 ;
  _Delay( 1000 ) ;
  PB.Position := 3 ;
  _Delay( 1000 ) ;
  PB.Position := 4 ;
  _Delay( 1000 ) ;
  Close ;
end ;

procedure TFormSplash.Label5Click(Sender: TObject);
begin
  tiShellExecute(( Sender as TLabel ).Caption ) ;
end;

end.


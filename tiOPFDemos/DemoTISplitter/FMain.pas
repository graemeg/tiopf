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
    January 2001, Peter Hinrichsen, Made open source

  Purpose:
    A demonstration for the TtiSplitterPanel

  Classes:
    TFormMain - The main form

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit FMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, tiSplitter, ComCtrls, Spin, Buttons ;

type


  TFormMain = class(TForm)
    tiSplitterPanel1: TtiSplitterPanel;
    tiSplitterPanel2: TtiSplitterPanel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    cbKeepSplitterPosPercent1: TCheckBox;
    rgSplitterOrientation1: TRadioGroup;
    rgPanelStyle1: TRadioGroup;
    rgSplitterOrientation2: TRadioGroup;
    cbKeepSplitterPosPercent2: TCheckBox;
    rgPanelStyle2: TRadioGroup;
    Memo1: TMemo;
    Memo2: TMemo;
    StatusBar1: TStatusBar;
    procedure cbKeepSplitterPosPercent1Click(Sender: TObject);
    procedure rgSplitterOrientation1Click(Sender: TObject);
    procedure rgPanelStyle1Click(Sender: TObject);
    procedure rgSplitterOrientation2Click(Sender: TObject);
    procedure cbKeepSplitterPosPercent2Click(Sender: TObject);
    procedure rgPanelStyle2Click(Sender: TObject);
  private
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.DFM}

// -----------------------------------------------------------------------------
procedure TFormMain.cbKeepSplitterPosPercent1Click(Sender: TObject);
begin
  tiSplitterPanel1.KeepSplitterPosPercent := cbKeepSplitterPosPercent1.Checked ;
end;

// -----------------------------------------------------------------------------
procedure TFormMain.rgSplitterOrientation1Click(Sender: TObject);
begin
  tiSplitterPanel1.SplitterOrientation :=
    TtiSplitterOrientation( rgSplitterOrientation1.ItemIndex ) ;
end;

// -----------------------------------------------------------------------------
procedure TFormMain.rgPanelStyle1Click(Sender: TObject);
begin
  tiSplitterPanel1.PanelStyle := TtiSplitterPanelStyle( rgPanelStyle1.ItemIndex ) ;
end;

// -----------------------------------------------------------------------------
procedure TFormMain.rgSplitterOrientation2Click(Sender: TObject);
begin
  tiSplitterPanel2.SplitterOrientation :=
    TtiSplitterOrientation( rgSplitterOrientation2.ItemIndex ) ;
end;

// -----------------------------------------------------------------------------
procedure TFormMain.cbKeepSplitterPosPercent2Click(Sender: TObject);
begin
  tiSplitterPanel2.KeepSplitterPosPercent := cbKeepSplitterPosPercent2.Checked ;
end;

// -----------------------------------------------------------------------------
procedure TFormMain.rgPanelStyle2Click(Sender: TObject);
begin
  tiSplitterPanel2.PanelStyle := TtiSplitterPanelStyle( rgPanelStyle2.ItemIndex ) ;
end;

end.



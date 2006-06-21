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
    May 2000, Peter Hinrichsen, Made open source

  Purpose:
    Form to demonstrate the features of the TtiListViewPlus

  ToDo:
    Nothing

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit FtiListViewPlusDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, tiListView, tiListViewPlus, StdCtrls, ImgList, ExtCtrls,
  ToolWin, DemoData ;

type
  TFormListViewPlusDemo = class(TForm)
    LVP: TtiListViewPlus;
    ToolBar1: TToolBar;
    Panel1: TPanel;
    tbColumns: TToolButton;
    tbQuery: TToolButton;
    tbFind: TToolButton;
    tbSort: TToolButton;
    ToolButton5: TToolButton;
    tbClose: TToolButton;
    ImageList1: TImageList;
    Memo1: TMemo;
    SB: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure tbColumnsClick(Sender: TObject);
    procedure tbQueryClick(Sender: TObject);
    procedure tbFindClick(Sender: TObject);
    procedure tbSortClick(Sender: TObject);
    procedure tbCloseClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LVPAfterRefreshData(Sender: TtiCustomListView);
  private
    FData : TDemoData ;
  public
    { Public declarations }
  end;

var
  FormListViewPlusDemo: TFormListViewPlusDemo;

implementation

{$R *.DFM}

// -----------------------------------------------------------------------------
procedure TFormListViewPlusDemo.FormCreate(Sender: TObject);
begin
  FData := TDemoData.Create ;
  LVP.Data := FData ;
end;

// -----------------------------------------------------------------------------
procedure TFormListViewPlusDemo.FormDestroy(Sender: TObject);
begin
  // Must set LVP.Data := nil as the list view may try to redraw itself after
  // its data has been deleted giving a hard to tract AV.
  LVP.Data := nil ;
  FData.Free ;
end;

// -----------------------------------------------------------------------------
procedure TFormListViewPlusDemo.tbColumnsClick(Sender: TObject);
begin
  LVP.DoColumns ;
end;

// -----------------------------------------------------------------------------
procedure TFormListViewPlusDemo.tbQueryClick(Sender: TObject);
begin
  LVP.DoQuery ;
end;

// -----------------------------------------------------------------------------
procedure TFormListViewPlusDemo.tbFindClick(Sender: TObject);
begin
  LVP.DoFind ;
end;

// -----------------------------------------------------------------------------
procedure TFormListViewPlusDemo.tbSortClick(Sender: TObject);
begin
  LVP.DoSort ;
end;

// -----------------------------------------------------------------------------
procedure TFormListViewPlusDemo.tbCloseClick(Sender: TObject);
begin
  Close ;
end;

procedure TFormListViewPlusDemo.LVPAfterRefreshData(
  Sender: TtiCustomListView);
begin
  SB.SimpleText := 'Number of items in list: ' +
                   IntToStr( Sender.Items.Count ) ;
end;

end.

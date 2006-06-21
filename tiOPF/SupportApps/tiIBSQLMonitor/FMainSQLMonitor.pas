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
unit FMainSQLMonitor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolWin, IBSQLMonitor, ExtCtrls, tiFocusPanel,
  tiListView
 ,TIBSQLEvent_BOM
 ,IB, StdCtrls, tiSplitter
 ;

type
  TfrmIBSQLMonitor = class(TForm)
    ToolBar: TToolBar;
    ConnectButton: TToolButton;
    ToolButton2: TToolButton;
    TraceQPrepareButton: TToolButton;
    TraceQExecuteButton: TToolButton;
    TraceQFetchButton: TToolButton;
    TraceErrorButton: TToolButton;
    TraceStmtButton: TToolButton;
    TraceConnectButton: TToolButton;
    TraceTransactButton: TToolButton;
    TraceBlobButton: TToolButton;
    TraceServiceButton: TToolButton;
    TraceMiscButton: TToolButton;
    ClearButton: TToolButton;
    ToolButton13: TToolButton;
    IBSQLMonitor: TIBSQLMonitor;
    EventListView: TtiListView;
    Timer: TTimer;
    tiSplitter: TtiSplitter;
    SQLLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure IBSQLMonitorSQL(EventText: String; EventTime: TDateTime);
    procedure ConnectButtonClick(Sender: TObject);
    procedure TraceQPrepareButtonClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure EventListViewItemArive(pLV: TtiCustomListView; pData: TPersistent; pItem: TListItem);
    procedure EventListViewItemLeave(pLV: TtiCustomListView; pData: TPersistent; pItem: TListItem);
  private
    { Private declarations }
    fIBSQLEventList : TIBSQLEventList;
    function GetTraceFlags : TTraceFlags;
    procedure SetTraceFlags(flags : TTraceFlags);
  public
    { Public declarations }
    property TraceFlags : TTraceFlags read GetTraceFlags write SetTraceFlags;
  end;

var
  frmIBSQLMonitor: TfrmIBSQLMonitor;

implementation

{$R *.dfm}

procedure TfrmIBSQLMonitor.FormCreate(Sender: TObject);
begin
  fIBSQLEventList         := TIBSQLEventList.Create;
  EventListView.Data      := fIBSQLEventList.List;
  IBSQLMonitor.TraceFlags := [tfQExecute];
  TraceFlags              := IBSQLMonitor.TraceFlags ;
end;

procedure TfrmIBSQLMonitor.FormDestroy(Sender: TObject);
begin
  FreeAndNil( fIBSQLEventList ) ;
end;

procedure TfrmIBSQLMonitor.IBSQLMonitorSQL(EventText: String; EventTime: TDateTime);
begin
  fIBSQLEventList.AddEvent( EventText, EventTime );
  Timer.Enabled := True;
end;

procedure TfrmIBSQLMonitor.ClearButtonClick(Sender: TObject);
begin
  fIBSQLEventList.Clear;
  EventListView.Refresh;
  SQLLabel.Caption := '';
end;

procedure TfrmIBSQLMonitor.TimerTimer(Sender: TObject);
begin
  Timer.Enabled := False;
  EventListView.Refresh;
end;

procedure TfrmIBSQLMonitor.ConnectButtonClick(Sender: TObject);
begin
  IBSQLMonitor.Enabled := ConnectButton.Down;
end;

procedure TfrmIBSQLMonitor.TraceQPrepareButtonClick(Sender: TObject);
begin
  IBSQLMonitor.TraceFlags := TraceFlags;
end;

function TfrmIBSQLMonitor.GetTraceFlags : TTraceFlags;
begin
  if TraceQPrepareButton.Down then
    Include(Result, tfQPrepare);
  if TraceQExecuteButton.Down then
    Include(Result, tfQExecute);
  if TraceQFetchButton.Down then
    Include(Result, tfQFetch);
  if TraceErrorButton.Down then
    Include(Result, tfError);
  if TraceStmtButton.Down then
    Include(Result, tfStmt);
  if TraceConnectButton.Down then
    Include(Result, tfConnect);
  if TraceTransactButton.Down then
    Include(Result, tfTransact);
  if TraceBlobButton.Down then
    Include(Result, tfBlob);
  if TraceServiceButton.Down then
    Include(Result, tfService);
  if TraceMiscButton.Down then
    Include(Result, tfMisc);
end;

procedure TfrmIBSQLMonitor.SetTraceFlags(flags: TTraceFlags);
begin
  TraceQPrepareButton.Down := tfQPrepare in flags;
  TraceQExecuteButton.Down := tfQExecute in flags;
  TraceQFetchButton.Down   := tfQFetch   in flags;
  TraceErrorButton.Down    := tfError    in flags;
  TraceStmtButton.Down     := tfStmt     in flags;
  TraceConnectButton.Down  := tfConnect  in flags;
  TraceTransactButton.Down := tfTransact in flags;
  TraceBlobButton.Down     := tfBlob     in flags;
  TraceServiceButton.Down  := tfService  in flags;
  TraceMiscButton.Down     := tfMisc     in flags;
end;

procedure TfrmIBSQLMonitor.EventListViewItemArive(pLV: TtiCustomListView; pData: TPersistent; pItem: TListItem);
begin
  if Assigned( pData )
  and (pData is TIBSQLEvent) then
    SQLLabel.Caption := TIBSQLEvent( pData ).Description
  else
    SQLLabel.Caption := '';
end;

procedure TfrmIBSQLMonitor.EventListViewItemLeave(pLV: TtiCustomListView; pData: TPersistent; pItem: TListItem);
begin
  SQLLabel.Caption := '';
end;

end.

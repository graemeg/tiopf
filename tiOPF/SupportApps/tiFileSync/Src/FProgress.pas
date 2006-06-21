unit FProgress;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, Buttons;

type

  TFormProgress = class(TForm)
    memoLog: TMemo;
    PB: TProgressBar;
    bbCancel: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bbCancelClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FThread: TThread;
    { Private declarations }
  public
    procedure Log( const pMessage : string; pAppendToPrevRow: boolean ) ;
    procedure UpdateProgress( piMax, piPos : integer ) ;
    property  Thread : TThread read FThread write FThread ;
  end;

implementation
uses
  tiUtils
  ,tiRegINI
  ,tiDialogs
  ,tiFileSync_Mgr
  ;
  
{$R *.DFM}

{ TFormProgress }

procedure TFormProgress.Log(const pMessage: string; pAppendToPrevRow: boolean);
var
  lIndex: Integer ;
  lLine: string;
begin
  if pAppendToPrevRow and ( MemoLog.Lines.Count > 0 ) then
  begin
    lIndex := memoLog.Lines.Count-1 ;
    lLine := MemoLog.Lines[lIndex] + ' ' + pMessage;
    MemoLog.Lines[lIndex] := lLine;
  end else
    MemoLog.Lines.Add(pMessage)
end;

procedure TFormProgress.FormCreate(Sender: TObject);
begin
  gReg.ReadFormState( self ) ;
end;

procedure TFormProgress.FormDestroy(Sender: TObject);
begin
  gReg.WriteFormState( self ) ;
end;

procedure TFormProgress.bbCancelClick(Sender: TObject);
begin
  Assert( Assigned( FThread ), 'FThread not assigned' ) ;
  Assert( FThread is TthrdFileSyncMgr, 'FThread not a TthrdFileSyncMgr' ) ;
  if tiAppConfirmation(
    'Are you sure you want to terminate this process?' ) then
  begin
    bbCancel.Enabled := false ;
    TthrdFileSyncMgr(FThread).CustomTerminate ;
  end ;
end;

procedure TFormProgress.FormResize(Sender: TObject);
begin
  bbCancel.Left := ( ClientWidth - bbCancel.Width ) div 2 ;
end;

procedure TFormProgress.UpdateProgress(piMax, piPos: integer);
begin
  if PB.Max <> piMax then
    PB.Max := piMax ;
  if PB.Position <> piPos then
    PB.Position := piPos ;
end;

end.

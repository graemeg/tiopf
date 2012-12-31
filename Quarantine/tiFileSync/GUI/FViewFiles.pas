unit FViewFiles;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  tiListView, tiListViewPlus, ComCtrls, ExtCtrls, tiFileSync_Mgr,
  tiFocusPanel, StdCtrls ;

type
  TFormViewFiles = class(TForm)
    pnlAdvanced: TPanel;
    PC: TPageControl;
    tsSource: TTabSheet;
    lv: TtiListViewPlus;
    tsTarget: TTabSheet;
    tsCopy: TTabSheet;
    tsUpdate: TTabSheet;
    tsDelete: TTabSheet;
    lblSource: TLabel;
    lblTarget: TLabel;
    procedure PCChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FFileSyncMgr: TtiFileSyncMgr;
    procedure SetFileSyncMgr(const Value: TtiFileSyncMgr);
    { Private declarations }
  public
    property FileSyncMgr : TtiFileSyncMgr read FFileSyncMgr write SetFileSyncMgr ;

  end;

implementation
uses
  tiUtils
  ,tiINI
  ,tiGUIUtils
  ,tiDialogs
  , tiFileName_BOM;
  
{$R *.DFM}

procedure TFormViewFiles.PCChange(Sender: TObject);
begin
  if FFileSyncMgr = nil then
    Exit ; //==>
  lv.Parent := PC.ActivePage ;
  if PC.ActivePage = tsSource then
    lv.Data  := FFileSyncMgr.SourceFileNames.List
  else if PC.ActivePage = tsTarget then
    lv.Data := FFileSyncMgr.TargetFileNames.List
  else if PC.ActivePage = tsCopy then
    lv.Data   := FFileSyncMgr.CopyFileNames.List
  else if PC.ActivePage = tsDelete then
    lv.Data := FFileSyncMgr.DeleteFileNames.List
  else if PC.ActivePage = tsUpdate then
    lv.Data := FFileSyncMgr.UpdateFileNames.List
  else
    LV.Data := nil ;

end;

procedure TFormViewFiles.SetFileSyncMgr(const Value: TtiFileSyncMgr);
begin
  FFileSyncMgr := Value;
  if Value <> nil then
  begin
    lblSource.Caption := 'Source: ' + FFileSyncMgr.SourceReader + ' - ' + FFileSyncMgr.SourceFileNames.StartDir;
    lblTarget.Caption := 'Target: ' + FFileSyncMgr.TargetReader + ' - ' + FFileSyncMgr.TargetFileNames.StartDir;
    tsSource.Caption := 'Source [' + IntToStr( FileSyncMgr.SourceFileNames.Count) + ']';
    tsTarget.Caption := 'Target [' + IntToStr( FileSyncMgr.TargetFileNames.Count) + ']';
    tsCopy.Caption   := 'Copy [' + IntToStr( FileSyncMgr.CopyFileNames.Count) + ']';
    tsUpdate.Caption := 'Update [' + IntToStr( FileSyncMgr.UpdateFileNames.Count) + ']';
    tsDelete.Caption := 'Delete [' + IntToStr( FileSyncMgr.DeleteFileNames.Count) + ']';
  end ;
  PCChange( nil ) ;
end;

procedure TFormViewFiles.FormCreate(Sender: TObject);
begin
  gINI.ReadFormState( self ) ;
  pc.ActivePage := tsSource ;
end;

procedure TFormViewFiles.FormDestroy(Sender: TObject);
begin
  gINI.WriteFormState( self ) ;
end;

end.

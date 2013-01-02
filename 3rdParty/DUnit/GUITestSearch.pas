unit GUISearchPanel;

interface

uses
  ExtCtrls
  ,StdCtrls
//PVS  ,tiSpeedButton
  ,Classes
  ;

type
  TGUISearchPanel = class;

  TtiVTSearchPanelFindTextChange = procedure(
    const ASender: TGUISearchPanel; const AFindText: string) of object;

  TtiVTSearchPanelShowing = procedure(
    const ASender: TGUISearchPanel; const AIsShowing: Boolean) of object;

{
 Search behaviour to imitate FireFox - if we have matched, incremental searching
 begins from last matched node. If there is no match or the search text is empty
 we begin from the root.
}

  TGUISearchPanel = class(TCustomPanel)
    FFindLabel: TLabel;
    FFindText: TEdit;
//PVS    FClose: TtiSpeedButton;
//PVS    FFindNext: TtiSpeedButton;
//PVS    FFindPrevious: TtiSpeedButton;
    FClose: TSpeedButton;
    FFindNext: TSpeedButton;
    FFindPrevious: TSpeedButton;
    FWrapLabel: TLabel;

  private
    FOnFindTextChange: TtiVTSearchPanelFindTextChange;
    FTextMatching: boolean;
    FShowing: boolean;
    FOnShowing: TtiVTSearchPanelShowing;
    FOnFindNext: TNotifyEvent;
    FOnFindPrevious: TNotifyEvent;
    FOnReturnKey: TNotifyEvent;
    FOnEnterFindEdit: TNotifyEvent;
    FOnExitFindEdit: TNotifyEvent;

    procedure SetOnFindTextChange(const AValue: TtiVTSearchPanelFindTextChange);
    procedure SetTextMatching(const Value: boolean);
    procedure SetShowing(const AValue: boolean);
    procedure SetOnShowing(const AValue: TtiVTSearchPanelShowing);
    procedure DoFindText(Sender: TObject);
    procedure DoFindNext(Sender: TObject);
    procedure DoClose(Sender: TObject);
    procedure SetOnFindNext(const AValue: TNotifyEvent);
    procedure DoFindPrevious(Sender: TObject);
    procedure SetOnFindPrevious(const AValue: TNotifyEvent);
    procedure SetOnReturnKey(const Value: TNotifyEvent);
    procedure SetOnEnterFindEdit(const Value: TNotifyEvent);
    procedure SetOnExitFindEdit(const Value: TNotifyEvent);
    function  GetSearchText: string;
    procedure DoFindKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure DoReturnKey;
    procedure DoOnEnterFindEdit(Sender: TObject);
    procedure DoOnExitFindEdit(Sender: TObject);
    procedure ClearWrapMessage;

  public
    constructor Create(AOwner: TComponent); override;
    property TextMatching: boolean read FTextMatching write SetTextMatching;
    property SearchText: string read GetSearchText;
    procedure Wrapped(const AForward: boolean = true);

  published
    property Showing: boolean read FShowing write SetShowing;
    property OnFindNext: TNotifyEvent read FOnFindNext write SetOnFindNext;
    property OnFindPrevious: TNotifyEvent read FOnFindPrevious write SetOnFindPrevious;
    property OnReturnKey: TNotifyEvent read FOnReturnKey write SetOnReturnKey;
    property OnEnterFindEdit: TNotifyEvent read FOnEnterFindEdit write SetOnEnterFindEdit;
    property OnExitFindEdit: TNotifyEvent read FOnExitFindEdit write SetOnExitFindEdit;

    property OnFindTextChange: TtiVTSearchPanelFindTextChange
      read FOnFindTextChange write SetOnFindTextChange;
    property OnShowing: TtiVTSearchPanelShowing
      read FOnShowing write SetOnShowing;
  end;

const
  cSearchFailureColor = $7f7fff;  // pink

implementation

uses
  Controls
//PVS  ,tiImageMgr
//PVS  ,tiResources
  ,Windows
  ,Graphics
  ;

{ TGUISearchPanel }

procedure TGUISearchPanel.ClearWrapMessage;
begin
  FWrapLabel.Caption := '';
end;

constructor TGUISearchPanel.Create(AOwner: TComponent);
begin
  inherited;
  SetSubComponent(True);
  Height := 26;
  BevelOuter := bvNone;
  BevelInner := bvNone;
  Align := alBottom;
  Visible := false;

//PVS  FClose:= TtiSpeedButton.Create(Self);
  FClose:= TSpeedButton.Create(Self);
  FClose.Parent := self;
  FClose.Top:= 2;
  FClose.Height := Height-4;
  FClose.Width := FClose.Height;
  FClose.Margin:= 2;
  FClose.Left := 0;
  FClose.Hint:= 'Close find bar';
  FClose.ShowHint:= True;
//PVS  gTIImageListMgr.LoadBMPToTISPeedButton16(cResTI_Cross, FClose);
  FClose.OnClick := DoClose;

  FFindLabel := TLabel.Create(self);
  FFindLabel.Parent := self;
  FFindLabel.Top:= 5;
  FFindLabel.Left:= FClose.Left + FClose.Width + 4;
  FFindLabel.Layout := tlCenter;
  FFindLabel.Caption := 'Find: ';

  FFindText := TEdit.Create(self);
  FFindText.Parent := self;
  FFindText.Top:= 2;
  FFindText.Height := Height-4;
  FFindText.Width := 100;
  FFindText.Left := FFindLabel.Left + FFindLabel.Width;
  FFindText.OnChange := DoFindText;
  FFindText.OnKeyDown := DoFindKeyDown;
  FFindText.OnEnter:= DoOnEnterFindEdit;
  FFindText.OnExit:= DoOnExitFindEdit;

//PVS  FFindNext := TtiSpeedButton.Create(self);
  FFindNext := TSpeedButton.Create(self);
  FFindNext.Parent := self;
  FFindNext.Top:= 2;
  FFindNext.Height := Height-4;
  FFindNext.Width := 50;
  FFindNext.Margin:= 2;
  FFindNext.Caption := 'Next';
  FFindNext.Hint:= 'Find the next occurrence of the phrase';
  FFindNext.ShowHint:= True;
  FFindNext.Left := FFindText.Left + FFindText.Width + 4;
//PVS  gTIImageListMgr.LoadBMPToTISPeedButton16(cResTI_FindNext, FFindNext);
  FFindNext.OnClick := DoFindNext;

//PVS  FFindPrevious := TtiSpeedButton.Create(self);
  FFindPrevious := TSpeedButton.Create(self);
  FFindPrevious.Parent := self;
  FFindPrevious.Top:= 2;
  FFindPrevious.Height := Height-4;
  FFindPrevious.Width := 70;
  FFindPrevious.Margin:= 2;
  FFindPrevious.Caption := 'Previous';
  FFindPrevious.Hint:= 'Find the previous occurrence of the phrase';
  FFindPrevious.ShowHint:= True;
  FFindPrevious.Left := FFindNext.Left + FFindNext.Width + 3;
//PVS  gTIImageListMgr.LoadBMPToTISPeedButton16(cResTI_FindPrevious, FFindPrevious);
  FFindPrevious.OnClick := DoFindPrevious;

  FWrapLabel := TLabel.Create(self);
  FWrapLabel.Parent := self;
  FWrapLabel.Left := FFindPrevious.Left + FFindPrevious.Width + 3;
  FWrapLabel.AutoSize := false;
  FWrapLabel.Top := 3;
  FWrapLabel.Height := Height;
  FWrapLabel.Width := 250;
  FWrapLabel.Layout := tlCenter;
end;

procedure TGUISearchPanel.DoClose(Sender: TObject);
begin
  Showing:= False;
end;

procedure TGUISearchPanel.DoReturnKey;
begin
  if Showing and Assigned(FOnReturnKey) and TextMatching then
  begin
    Showing:= False;
    FOnReturnKey(Self);
  end;
end;

procedure TGUISearchPanel.DoFindKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  ClearWrapMessage;
  case Key of
    VK_F3:
      if Shift = [] then
        DoFindNext(FFindNext)
      else if Shift = [ssShift] then
        DoFindPrevious(FFindPrevious);
    VK_DOWN: begin
               DoFindNext(FFindNext);
               Key:= 0;
             end;
    VK_UP:   begin
               DoFindPrevious(FFindPrevious);
               Key:= 0;
             end;
    VK_RETURN: DoReturnKey;
    VK_ESCAPE: Showing := false;
  end;

end;

procedure TGUISearchPanel.DoFindNext(Sender: TObject);
begin

  if Showing and Assigned(OnFindNext) then
  begin
    ClearWrapMessage;
    OnFindNext(Sender);
  end;

end;

procedure TGUISearchPanel.DoFindPrevious(Sender: TObject);
begin

  if Showing and Assigned(OnFindPrevious) then
  begin
    ClearWrapMessage;
    OnFindPrevious(Sender);
  end;

end;

procedure TGUISearchPanel.DoFindText(Sender: TObject);
begin

  if Showing and Assigned(OnFindTextChange) then
    OnFindTextChange(self, FFindText.Text);

end;


procedure TGUISearchPanel.DoOnEnterFindEdit(Sender: TObject);
begin
  if Showing and Assigned(FOnEnterFindEdit) then
    FOnEnterFindEdit(Self);
end;

procedure TGUISearchPanel.DoOnExitFindEdit(Sender: TObject);
begin
  if Showing and Assigned(FOnExitFindEdit) then
    FOnExitFindEdit(Self);
end;

function TGUISearchPanel.GetSearchText: string;
begin
  Result := FFindText.Text;
end;

procedure TGUISearchPanel.SetOnReturnKey(const Value: TNotifyEvent);
begin
  FOnReturnKey := Value;
end;

procedure TGUISearchPanel.SetOnEnterFindEdit(const Value: TNotifyEvent);
begin
  FOnEnterFindEdit := Value;
end;

procedure TGUISearchPanel.SetOnExitFindEdit(const Value: TNotifyEvent);
begin
  FOnExitFindEdit := Value;
end;

procedure TGUISearchPanel.SetOnFindNext(const AValue: TNotifyEvent);
begin
  FOnFindNext := AValue;
end;

procedure TGUISearchPanel.SetOnFindPrevious(const AValue: TNotifyEvent);
begin
  FOnFindPrevious := AValue;
end;

procedure TGUISearchPanel.SetOnFindTextChange(const AValue: TtiVTSearchPanelFindTextChange);
begin
  FOnFindTextChange := AValue;
end;

procedure TGUISearchPanel.SetOnShowing(const AValue: TtiVTSearchPanelShowing);
begin
  FOnShowing := AValue;
end;

procedure TGUISearchPanel.SetShowing(const AValue: boolean);
begin
  FShowing := AValue;
  Visible := AValue;

  if FShowing then
    FFindText.SetFocus
  else
  begin
    Parent.SetFocus;
    FFindText.Text := '';
    ClearWrapMessage;
    FTextMatching:= False;
  end;

  if Assigned(FOnShowing) then
    FOnShowing(self, AValue);

end;

procedure TGUISearchPanel.SetTextMatching(const Value: boolean);
begin
  FTextMatching := Value;

  if FTextMatching then
  begin
    FFindText.Color := clWindow;
  end
  else
  begin
    FFindText.Color :=  cSearchFailureColor; // pink
    ClearWrapMessage;
  end;

end;

procedure TGUISearchPanel.Wrapped(const AForward: boolean);
begin

   if AForward then
     FWrapLabel.Caption := ' Search reached bottom, continued from top'
   else
     FWrapLabel.Caption := ' Search reached top, continued from bottom';

end;

end.

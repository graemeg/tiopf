unit GUISearchPanel;

interface

uses
  ExtCtrls
  ,StdCtrls
  ,Buttons  // TSpeedButton
  ,Classes
  ,Controls  // TImageList
  ;

type
  TGUISearchPanel = class;

  TGUISearchPanelFindTextChange = procedure(
    const ASender: TGUISearchPanel; const AFindText: string) of object;

  TGUISearchPanelShowing = procedure(
    const ASender: TGUISearchPanel; const AIsShowing: Boolean) of object;

{
 Search behaviour to imitate FireFox - if we have matched, incremental searching
 begins from last matched node. If there is no match or the search text is empty
 we begin from the root.
}

  TGUISearchPanel = class(TCustomPanel)
    FFindLabel: TLabel;
    FFindText: TEdit;
    FClose: TSpeedButton;
    FFindNext: TSpeedButton;
    FFindPrevious: TSpeedButton;
    FWrapLabel: TLabel;

  private
    FOnFindTextChange: TGUISearchPanelFindTextChange;
    FTextMatching: boolean;
    FShowing: boolean;
    FOnFindNext: TNotifyEvent;
    FOnFindPrevious: TNotifyEvent;
    FOnReturnKey: TNotifyEvent;
    FOnEnterFindEdit: TNotifyEvent;
    FOnExitFindEdit: TNotifyEvent;
    FImages: TImageList;

    procedure SetOnFindTextChange(const AValue: TGUISearchPanelFindTextChange);
    procedure SetTextMatching(const Value: boolean);
    procedure DoFindText(Sender: TObject);
    procedure DoFindNext(Sender: TObject);
    procedure SetOnFindNext(const AValue: TNotifyEvent);
    procedure DoFindPrevious(Sender: TObject);
    procedure SetOnFindPrevious(const AValue: TNotifyEvent);
    procedure SetOnReturnKey(const Value: TNotifyEvent);
    procedure SetOnEnterFindEdit(const Value: TNotifyEvent);
    procedure SetOnExitFindEdit(const Value: TNotifyEvent);
    function  GetSearchText: string;
    procedure DoFindKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure DoOnEnterFindEdit(Sender: TObject);
    procedure DoOnExitFindEdit(Sender: TObject);
    procedure ClearWrapMessage;
    procedure SetImages(const Value: TImageList);

  public
    constructor Create(AOwner: TComponent); override;
    property TextMatching: boolean read FTextMatching write SetTextMatching;
    property SearchText: string read GetSearchText;
    procedure Wrapped(const AForward: boolean = true);

  published
    property Showing: boolean read FShowing;
    property OnFindNext: TNotifyEvent read FOnFindNext write SetOnFindNext;
    property OnFindPrevious: TNotifyEvent read FOnFindPrevious write SetOnFindPrevious;
    property OnReturnKey: TNotifyEvent read FOnReturnKey write SetOnReturnKey;
    property OnEnterFindEdit: TNotifyEvent read FOnEnterFindEdit write SetOnEnterFindEdit;
    property OnExitFindEdit: TNotifyEvent read FOnExitFindEdit write SetOnExitFindEdit;

    property OnFindTextChange: TGUISearchPanelFindTextChange
      read FOnFindTextChange write SetOnFindTextChange;
    property Images: TImageList read FImages write SetImages;
  end;

const
  cSearchFailureColor = $7f7fff;  // pink
  cSearchWrappedToTop = ' Search wrapped to top';
  cSearchWrappedToBottom = ' Search wrapped to bottom';
  cPrevGlyphIndex = 0;
  cNextGlyphIndex = 1;

implementation

uses
  Windows
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

  FFindLabel := TLabel.Create(self);
  FFindLabel.Parent := self;
  FFindLabel.Top:= 5;
  FFindLabel.Left:= 4;
  FFindLabel.Layout := tlCenter;
  FFindLabel.Caption := 'Search: ';

  FFindText := TEdit.Create(self);
  FFindText.Parent := self;
  FFindText.Top:= 2;
  FFindText.Height := Height-4;
  FFindText.Width := 100;
  FFindText.Left := FFindLabel.Left + FFindLabel.Width + 4;
  FFindText.OnChange := DoFindText;
  FFindText.OnKeyDown := DoFindKeyDown;
  FFindText.OnEnter:= DoOnEnterFindEdit;
  FFindText.OnExit:= DoOnExitFindEdit;

  FFindPrevious := TSpeedButton.Create(self);
  FFindPrevious.Parent := self;
  FFindPrevious.Top:= 2;
  FFindPrevious.Height := Height-4;
  FFindPrevious.Width := 70;
  FFindPrevious.Margin:= 2;
  FFindPrevious.Caption := 'Previous';
  FFindPrevious.Hint:= 'Find the previous occurrence of the phrase';
  FFindPrevious.ShowHint:= True;
  FFindPrevious.Left := FFindText.Left + FFindText.Width + 4;
  FFindPrevious.OnClick := DoFindPrevious;
  FFindPrevious.Flat := true;
  FFindPrevious.Layout := blGlyphRight;

  FFindNext := TSpeedButton.Create(self);
  FFindNext.Parent := self;
  FFindNext.Top:= FFindPrevious.Top;
  FFindNext.Height := FFindPrevious.Height;
  FFindNext.Width := 50;
  FFindNext.Margin:= FFindPrevious.Margin;
  FFindNext.Caption := 'Next';
  FFindNext.Hint:= 'Find the next occurrence of the phrase';
  FFindNext.ShowHint:= FFindPrevious.ShowHint;
  FFindNext.Left := FFindPrevious.Left + FFindPrevious.Width;
  FFindNext.OnClick := DoFindNext;
  FFindNext.Flat := FFindPrevious.Flat;
  FFindNext.Layout := blGlyphLeft;


  FWrapLabel := TLabel.Create(self);
  FWrapLabel.Parent := self;
  FWrapLabel.Left := FFindNext.Left + FFindNext.Width + 3;
  FWrapLabel.AutoSize := false;
  FWrapLabel.Top := FFindLabel.Top;
  FWrapLabel.Width := 130;
  FWrapLabel.Layout := FFindLabel.Layout;

  Width := FWrapLabel.Left + FWrapLabel.Width + 3;
  Align := alRight;
  FShowing := true;
  Visible := true;
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

procedure TGUISearchPanel.SetImages(const Value: TImageList);
begin
  FImages := Value;

  if Assigned(FImages) then
  begin
    FImages.GetBitmap(cPrevGlyphIndex, FFindPrevious.Glyph);
    FImages.GetBitmap(cNextGlyphIndex, FFindNext.Glyph);
  end;

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

procedure TGUISearchPanel.SetOnFindTextChange(const AValue: TGUISearchPanelFindTextChange);
begin
  FOnFindTextChange := AValue;
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
     FWrapLabel.Caption := cSearchWrappedToTop
   else
     FWrapLabel.Caption := cSearchWrappedToBottom

end;

end.

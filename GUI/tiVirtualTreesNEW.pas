unit tiVirtualTreesNEW;

interface
uses
  VirtualTrees
 ,Graphics
 ,Classes
 ,Controls
 ;

type
  TVTAdvancedPaintText = procedure(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
    TextType: TVSTTextType; NodeDisabled: boolean; NodeSelectedAndUnfocused: boolean) of object;

  TtiVTHeaderClickEvent = procedure(Sender: TVTHeader; Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;

{ TtiVTEdit is 'work in progress'
  Discovered that there is an equivalent in tiVTListView that, IMO, is a better implementation based
  on the use of the VT.OnCreateEditor event. The tiVirtualTrees implementation modified
  VT.DoCreateEditor in place as well as modifying TStringEditLink in place.

  Consequently, I'm inclined to ditch the tiVirtualTrees approach. It would require considerable modifications
  to the base code. Question arises as to how best to implement existing logic into tiVTListView's version.
 }
(*
  TtiVTEdit = class(TVTEdit)
  private
    FLink: TStringEditLink;
  protected
    function IsValidValue: Boolean; virtual;
    procedure DoOnKeyPress(Sender: TObject; var Key: Char); virtual;
    property Link: TStringEditLink read FLink;
  published
  public
    constructor Create(Link: TStringEditLink); reintroduce;
  end;
*)

  TtiVirtualTreeColumn = class(TVirtualTreeColumn)
  private
    FDisplayNameList: TStringList;
    function GetDisplayNameCount: integer;
    function GetDisplayNames(Index: Integer): string;
    procedure SetDisplayNames(Index: Integer; const Value: string);
  protected
    function GetOwner: TVirtualTreeColumns; reintroduce;
    function GetText: UnicodeString; override;
    procedure SetText(const Value: UnicodeString); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Equals(OtherColumnObj: TObject): Boolean; override;
    procedure ClearDisplayNames;
    property DisplayNameCount: integer read GetDisplayNameCount;
    property DisplayNames[Index: Integer]: string read GetDisplayNames write SetDisplayNames;
    property DisplayNameList: TStringList read FDisplayNameList;
  published

  end;


  TtiVSTHeader = class(TVTHeader)
  private
    FRowCount: Integer;                // Number of header rows
    FRowHeight: Integer;               // Height of each header row
    procedure SetRowCount(Value: Integer);
    procedure SetRowHeight(Value: Integer);
  public
    constructor Create(AOwner: TBaseVirtualTree); override;
  published
    property RowCount: Integer read FRowCount write SetRowCount default 1;
    property RowHeight: Integer read FRowHeight write SetRowHeight default 17;
  end;



  TtiVirtualStringTree = class(TVirtualStringTree)
  private
    FOnAdvancedPaintText: TVTAdvancedPaintText;  // triggered before either normal or fixed text is painted to allow
                                                 // even finer customization (kind of sub cell painting). Allows
                                                 // setting of color when node is disabled or unfocused.
    function IsDisabled(Node: PVirtualNode): boolean;
    function IsSelectedAndUnfocused(Node: PVirtualNode; Column: TColumnIndex): boolean;
  protected
    procedure PaintNormalText(var PaintInfo: TVTPaintInfo; TextOutFlags: Integer; Text: UnicodeString); override;
    procedure PaintStaticText(const PaintInfo: TVTPaintInfo; TextOutFlags: Integer; const Text: UnicodeString); override;
    procedure DoAdvancedPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex;
      TextType: TVSTTextType; NodeDisabled: boolean; NodeSelectedAndUnfocused: boolean); virtual;
    function GetColumnClass: TVirtualTreeColumnClass; override;
    function GetHeaderClass: TVTHeaderClass; override;
  published
    property OnAdvancedPaintText: TVTAdvancedPaintText read FOnAdvancedPaintText write FOnAdvancedPaintText;
  end;


implementation
uses
  Windows
 ;

resourcestring

  SRowCountLEZero = 'RowCount must be greater than 0.';
  SDisplayNameIndexGERowCount = 'DisplayNames index must be less than Header RowCount.';

const
  AlignmentToDrawFlag: array[TAlignment] of Cardinal = (DT_LEFT, DT_RIGHT, DT_CENTER); // FROM VirtualTrees.pas


//----------------------------------------------------------------------------------------------------------------------

{ TtiVirtualTreeColumn }

//----------------------------------------------------------------------------------------------------------------------

constructor TtiVirtualTreeColumn.Create(Collection: TCollection);

begin
  FDisplayNameList := TStringList.Create;
  FDisplayNameList.Add('');
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TtiVirtualTreeColumn.Destroy;

begin
  FDisplayNameList.Free;
  inherited;
end;

function TtiVirtualTreeColumn.Equals(OtherColumnObj: TObject): Boolean;
begin
  Result := inherited Equals(OtherColumnObj);
  if Result
  and (OtherColumnObj is TtiVirtualTreeColumn) then
    Result := (FDisplayNameList.CommaText = (OtherColumnObj as TtiVirtualTreeColumn).DisplayNameList.CommaText);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TtiVirtualTreeColumn.Assign(Source: TPersistent);

begin
  inherited Assign(Source);
  FDisplayNameList.Assign(TtiVirtualTreeColumn(Source).DisplayNameList);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TtiVirtualTreeColumn.ClearDisplayNames;

begin
  FDisplayNameList.Clear;
  FDisplayNameList.Add('');
  Changed(False);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TtiVirtualTreeColumn.SetDisplayNames(Index: Integer; const Value: string);

begin
  if Index >= (Owner.Header as TtiVSTHeader).RowCount then
    ShowError(SDisplayNameIndexGERowCount, 0);

  while Index >= FDisplayNameList.Count do
    FDisplayNameList.Add('');
  if FDisplayNameList[Index] <> Value then
  begin
    FDisplayNameList[Index] := Value;
    if Index = 0 then
      DisplayName := Value;
    Changed(False);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TtiVirtualTreeColumn.SetText(const Value: UnicodeString);

begin
  inherited;
  DisplayNames[0] := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

function TtiVirtualTreeColumn.GetDisplayNameCount: integer;

begin
  Result := FDisplayNameList.Count;
end;

//----------------------------------------------------------------------------------------------------------------------

function TtiVirtualTreeColumn.GetDisplayNames(Index: Integer): string;

begin
  if Index >= (Owner.Header as TtiVSTHeader).RowCount then
    ShowError(SDisplayNameIndexGERowCount, 0);
  if Index < FDisplayNameList.Count then
    Result := FDisplayNameList.Strings[Index]
  else
    Result := '';
end;

//----------------------------------------------------------------------------------------------------------------------

function TtiVirtualTreeColumn.GetOwner: TVirtualTreeColumns;

begin
  Result := Collection as TVirtualTreeColumns;
end;

//----------------------------------------------------------------------------------------------------------------------

function TtiVirtualTreeColumn.GetText: UnicodeString;

begin
  Result := GetDisplayName;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TtiVTHeader }

constructor TtiVSTHeader.Create(AOwner: TBaseVirtualTree);

begin
  inherited;
  FRowCount := 1;
  FRowHeight := 17;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TtiVSTHeader.SetRowCount(Value: Integer);

var
  I: Integer;
  LtiVirtualTreeColumn: TtiVirtualTreeColumn;

begin
  if Value <= 0 then
    ShowError(SRowCountLEZero, 0);

  if FRowCount <> Value then
  begin
    FRowCount := Value;
    for I := 0 to Columns.Count - 1 do
    begin
      LtiVirtualTreeColumn := (Columns.Items[I] as TtiVirtualTreeColumn);
      while LtiVirtualTreeColumn.DisplayNameCount > FRowCount do
        LtiVirtualTreeColumn.DisplayNameList.Delete(LtiVirtualTreeColumn.DisplayNameCount - 1);
    end;

    if not (csLoading in Treeview.ComponentState) and Treeview.HandleAllocated then
    begin
      if hoVisible in Options then
      begin
        RecalculateHeader;
        Invalidate(nil);
        Treeview.Invalidate;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TtiVSTHeader.SetRowHeight(Value: Integer);

begin
  if FRowHeight <> Value then
  begin
    FRowHeight := Value;
    if not (csLoading in Treeview.ComponentState) and Treeview.HandleAllocated then
    begin
      if hoVisible in Options then
      begin
        Invalidate(nil);
        Treeview.Invalidate;
      end;
    end;
  end;
end;


//----------------------------------------------------------------------------------------------------------------------

  { TtiVirtualStringTree }

procedure TtiVirtualStringTree.DoAdvancedPaintText(Node: PVirtualNode;
  const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType;
  NodeDisabled: boolean; NodeSelectedAndUnfocused: boolean);

begin
  if Assigned(FOnAdvancedPaintText) then
    FOnAdvancedPaintText(Self, Canvas, Node, Column, TextType, NodeDisabled,
        NodeSelectedAndUnfocused);
end;

//----------------------------------------------------------------------------------------------------------------------

function TtiVirtualStringTree.GetColumnClass: TVirtualTreeColumnClass;

begin
  Result := TtiVirtualTreeColumn;
end;

//----------------------------------------------------------------------------------------------------------------------

function TtiVirtualStringTree.GetHeaderClass: TVTHeaderClass;

begin
  Result := TtiVSTHeader;
end;

//----------------------------------------------------------------------------------------------------------------------

function TtiVirtualStringTree.IsDisabled(Node: PVirtualNode): boolean;

begin
  result := (Assigned(Node) and (vsDisabled in Node.States)) or (not Enabled);
end;

//----------------------------------------------------------------------------------------------------------------------

function TtiVirtualStringTree.IsSelectedAndUnfocused(Node: PVirtualNode;
  Column: TColumnIndex): boolean;

begin
  result := (Assigned(Node) and (vsSelected in Node.States)) and
      (not Self.Focused) and
      ((toFullRowSelect in TreeOptions.SelectionOptions) or (Column = FocusedColumn));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TtiVirtualStringTree.PaintNormalText(var PaintInfo: TVTPaintInfo; TextOutFlags: Integer; Text: UnicodeString);

{
  This method is a clone of TVirtualStringTree.PaintNormalText from the released VirtualTrees.pas, with modifications tagged with [tiOPF].
}

// This method is responsible for painting the given text to target canvas (under consideration of the given rectangles).
// The text drawn here is considered as the normal text in a node.
// Note: NodeWidth is the actual width of the text to be drawn. This does not necessarily correspond to the width of
//       the node rectangle. The clipping rectangle comprises the entire node (including tree lines, buttons etc.).

var
  TripleWidth: Integer;
  R: TRect;
  DrawFormat: Cardinal;
  Size: TSize;
  Height: Integer;
  LNodeDisabled: boolean;  // [tiOPF]
  LNodeSelectedAndUnfocused: boolean;  // [tiOPF]

begin
  InitializeTextProperties(PaintInfo);
  with PaintInfo do
  begin
    R := ContentRect;
    Canvas.TextFlags := 0;
    InflateRect(R, -TextMargin, 0); // [tiOPF] TextMargin rather than FTextMargin
    LNodeDisabled := IsDisabled(Node);  // [tiOPF]
    LNodeSelectedAndUnfocused := IsSelectedAndUnfocused(Node, Column);  // [tiOPF]

    // Multiline nodes don't need special font handling or text manipulation.
    // Note: multiline support requires the Unicode version of DrawText, which is able to do word breaking.
    //       The emulation in this unit does not support this so we have to use the OS version. However
    //       DrawTextW is only available on NT/2000/XP and up. Hence there is only partial multiline support
    //       for 9x/Me.
    if vsMultiline in Node.States then
    begin
      Height := ComputeNodeHeight(Canvas, Node, Column);
      DoPaintText(Node, Canvas, Column, ttNormal);
      // Disabled node color overrides all other variants.
      if (vsDisabled in Node.States) or not Enabled then
        Canvas.Font.Color := Colors.DisabledColor; // [tiOPF] Colors rather than FColors

 // >>>> [tiOPF] block start
      // Disabled and unfocused node color overrides all other variants.
      if LNodeDisabled then
        Canvas.Font.Color := Colors.DisabledColor
      else if LNodeSelectedAndUnfocused then
        Canvas.Font.Color := Colors.UnfocusedColor;

      DoAdvancedPaintText(Node, Canvas, Column, ttNormal, LNodeDisabled, LNodeSelectedAndUnfocused);
 // <<<< [tiOPF] block end

      // The edit control flag will ensure that no partial line is displayed, that is, only lines
      // which are (vertically) fully visible are drawn.
      DrawFormat := DT_NOPREFIX or DT_WORDBREAK or DT_END_ELLIPSIS or DT_EDITCONTROL or AlignmentToDrawFlag[Alignment];
      if BidiMode <> bdLeftToRight then
        DrawFormat := DrawFormat or DT_RTLREADING;

      // Center the text vertically if it fits entirely into the content rect.
      if R.Bottom - R.Top > Height then
        InflateRect(R, 0, (Height - R.Bottom - R.Top) div 2);
    end
    else
    begin
      FFontChanged := False;
      TripleWidth := EllipsisWidth; // [tiOPF] EllipsisWidth rather than FEllipsisWidth
      DoPaintText(Node, Canvas, Column, ttNormal);

 // >>>> [tiOPF] block start
      // Disabled and unfocused node color overrides all other variants.
      if LNodeDisabled then
        Canvas.Font.Color := Colors.DisabledColor
      else if LNodeSelectedAndUnfocused then
        Canvas.Font.Color := Colors.UnfocusedColor;
      DoAdvancedPaintText(Node, Canvas, Column, ttNormal, LNodeDisabled, LNodeSelectedAndUnfocused);

 // <<<< [tiOPF] block end

      if FFontChanged then
      begin
        // If the font has been changed then the ellipsis width must be recalculated.
        TripleWidth := 0;
        // Recalculate also the width of the normal text.
        GetTextExtentPoint32W(Canvas.Handle, PWideChar(Text), Length(Text), Size);
        NodeWidth := Size.cx + 2 * TextMargin; // [tiOPF] TextMargin rather than FTextMargin
      end;

      // Disabled node color overrides all other variants. // [tiOPF] (commented out)
//      if (vsDisabled in Node.States) or not Enabled then
//        Canvas.Font.Color := FColors.DisabledColor;

      DrawFormat := DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE;
      if BidiMode <> bdLeftToRight then
        DrawFormat := DrawFormat or DT_RTLREADING;
      // Check if the text must be shortend.
      if (Column > -1) and ((NodeWidth - 2 * TextMargin) > R.Right - R.Left) then  // [tiOPF] TextMargin rather than FTextMargin
      begin
        Text := DoShortenString(Canvas, Node, Column, Text, R.Right - R.Left, TripleWidth);
        if Alignment = taRightJustify then
          DrawFormat := DrawFormat or DT_RIGHT
        else
          DrawFormat := DrawFormat or DT_LEFT;
      end
      else
        DrawFormat := DrawFormat or AlignmentToDrawFlag[Alignment];
    end;

    if Canvas.TextFlags and ETO_OPAQUE = 0 then
      SetBkMode(Canvas.Handle, TRANSPARENT)
    else
      SetBkMode(Canvas.Handle, OPAQUE);

    DoTextDrawing(PaintInfo, Text, R, DrawFormat);  // [tiOPF] TBD - replace (comment out) this with...
//    DoTextDrawing(Canvas.Handle, Text, R, DrawFormat, False {AdjustRight}); // [tiOPF] ... this
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TtiVirtualStringTree.PaintStaticText(const PaintInfo: TVTPaintInfo; TextOutFlags: Integer;
  const Text: UnicodeString);

// This method retrives and draws the static text bound to a particular node.

var
  R: TRect;
  DrawFormat: Cardinal;
  LNodeDisabled: boolean; // [tiOPF]
  LNodeSelectedAndUnfocused: boolean; // [tiOPF]

begin
  with PaintInfo do
  begin
    Canvas.Font := Font;
    if toFullRowSelect in TreeOptions.SelectionOptions then  // [tiOPF] TreeOptions.SelectionOptions rather than FOptions.FSelectionOptions
    begin
      if Node = DropTargetNode then // [tiOPF] DropTargetNode rather than FDropTargetNode
      begin
        if (LastDropMode = dmOnNode) or (vsSelected in Node.States) then // [tiOPF] LastDropMode rather than FLastDropMode
          Canvas.Font.Color := Colors.SelectionTextColor // [tiOPF] Colors rather than FColors
        else
          Canvas.Font.Color := Font.Color;
      end
      else
        if vsSelected in Node.States then
        begin
          if Focused or (toPopupMode in TreeOptions.PaintOptions) then // [tiOPF] LastDropMode rather than FLastDropMode
            Canvas.Font.Color := Colors.SelectionTextColor // [tiOPF] Colors rather than FColors
          else
            Canvas.Font.Color := Font.Color;
        end;
    end;

    DrawFormat := DT_NOPREFIX or DT_VCENTER or DT_SINGLELINE;
    Canvas.TextFlags := 0;
    DoPaintText(Node, Canvas, Column, ttStatic);

    // Disabled node color overrides all other variants. // [tiOPF]
    if (vsDisabled in Node.States) or not Enabled then
      Canvas.Font.Color := Colors.DisabledColor; // [tiOPF] Colors rather than FColors

 // >>>> [tiOPF] block start
    // Disabled and unfocused node color overrides all other variants.
    LNodeDisabled := IsDisabled(Node);
    LNodeSelectedAndUnfocused := IsSelectedAndUnfocused(Node, Column);
    if LNodeDisabled then
      Canvas.Font.Color := Colors.DisabledColor
    else if LNodeSelectedAndUnfocused then
      Canvas.Font.Color := Colors.UnfocusedColor;

    DoAdvancedPaintText(Node, Canvas, Column, ttNormal, LNodeDisabled, LNodeSelectedAndUnfocused);
 // <<<< [tiOPF] block end

    R := ContentRect;
    if Alignment = taRightJustify then
      Dec(R.Right, NodeWidth + TextMargin)    // [tiOPF] TextMargin rather than FTextMargin
    else
      Inc(R.Left, NodeWidth + TextMargin);    // [tiOPF] TextMargin rather than FTextMargin

    if Canvas.TextFlags and ETO_OPAQUE = 0 then
      SetBkMode(Canvas.Handle, TRANSPARENT)
    else
      SetBkMode(Canvas.Handle, OPAQUE);


 // TBD - [tiOPF] has refactored these lines into DoTextDrawing (and added AdjustRight)
    Windows.DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), R, DrawFormat)
//    DoTextDrawing(Canvas.Handle, Text, R, DrawFormat, False {AdjustRight}); // [tiOPF]'s version
  end;
end;


{ TtiVTEdit }

//----------------------------------------------------------------------------------------------------------------------
(*
constructor TtiVTEdit.Create(Link: TStringEditLink);

begin
  inherited Create(nil);
  ShowHint := False;
  ParentShowHint := False;
  // This assignment increases the reference count for the interface.
  FRefLink := Link;
  // This reference is used to access the link.
  FLink := Link;
  Self.OnKeyPress := DoOnKeyPress;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TtiVTEdit.DoOnKeyPress(Sender: TObject; var Key: Char);

begin
  // Don't beep when valid value is entered.
  if (Key = Chr(VK_RETURN)) and ((not Assigned(FLink)) or (not (vsMultiline in FLink.FNode.States))) then
    Key := #0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TtiVTEdit.IsValidValue: Boolean;

begin
  // Override to implement specific validation rules.
  Result := True;
end;
*)

end.

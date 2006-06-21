unit tiTreeViewChildForm;

interface
uses
  Classes
  ,Forms
  ,Contnrs
  ,ExtCtrls
  ,Messages
  ,Windows
  ,Controls
  ,ComCtrls
  ,tiTreeView
  ,tiObject
  ;

const
  cuColorSplitterGrabBar = $00FE9E83; // Pale navy blue
  cuiSplitterWidth = 8;


type
  {
    // This stub of code defines the interface a child form
    // must have if it is to be displayed as in the right
    // hand pane of the TtiTreeView
    TMyChildForm = class(TForm)
    private
      FData: TPersistent; // Can be any TPersistent descendant
      FTreeNode: TTreeNode;
      function  GetValid: boolean;
      procedure SetData(const Value: TPersistent);
    published
      property Data : TPersistent read FData write SetData ;
      property Valid : boolean    read GetValid ;
      property TreeNode : TTreeNode read FTreeNode write FTreeNode ;
    public
    end;
  }


  TtiTVDataFormMapping = class(TObject)
  private
    FDataClass: TClass;
    FForm: TForm;
  public
    property DataClass: TClass read FDataClass write FDataClass;
    property FormInstance: TForm read FForm write FForm;
  end;

  TtiTVDataFormMappings = class(TObjectList)
  public
    function FindByDataClass(pDataClass: TClass; pExactMatch: boolean = false): TtiTVDataFormMapping;
  end;

  TtiTVFormInstances = class(TObjectList)
  public
    function FindByFormClass(pFormClass: TFormClass;
      pbMultiInstance: boolean): TForm;
  end;

  TtiTVSplitter = class(TSplitter)
  private
    FbMouseOver: boolean;
  protected
    procedure Paint; override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure DrawGrabBar(pRect: TRect);
  public
    constructor Create(Owner: TComponent); override;
  end;

  TtiTreeViewChildForm = class( TtiTreeView )
  private
    FSplitter: TtiTVSplitter;
    FFormInstances: TtiTVFormInstances;
    FCurrentChildForm: TForm;
    FOnAfterGetChildForm: TTVOnGetChildFormEvent;
    FDataFormMappings: TtiTVDataFormMappings;
    function GetSplitterVisible: boolean;
    procedure SetSplitterVisible(const Value: boolean);
    function GetSplitterPos: integer;
    procedure SetSplitterPos(const Value: integer);
  protected
    procedure DoSetData(Sender: TObject); override ;
    procedure SetData(const Value: TtiObject); override;
    procedure DoOnChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
    procedure GetDataPage(pData: TObject; pNode: TTreeNode);
    procedure DoReSize(Sender: TObject); override ;
  public
    constructor Create(owner: TComponent); override;
    destructor  Destroy; override;
    property    CurrentChildForm: TForm read FCurrentChildForm;
    procedure   RegisterChildForm(pDataClassRef: TtiTVDataClassRef;
      pFormClassRef: TtiTVFormClassRef;
      pbMultiInstance: boolean = false);
    function IsCurrentChildFormValid: boolean;
    procedure ClearCurrentChildForm;
  published
    property SplitterVisible: boolean
      read GetSplitterVisible
      write SetSplitterVisible default true;
    property SplitterPos: integer read GetSplitterPos write SetSplitterPos default cuiSplitterPos;
    property OnAfterGetChildForm: TTVOnGetChildFormEvent
      read FOnAfterGetChildForm
      write FOnAfterGetChildForm;
  end;

implementation
uses
  TypInfo
  ,SysUtils
  ,Graphics
  ;
  
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiTVDataFormMappings
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

function TtiTVDataFormMappings.FindByDataClass(pDataClass: TClass; pExactMatch: boolean = false): TtiTVDataFormMapping;
var
  i: integer;
begin
  result := nil;
  if pExactMatch then
  begin
    i := 0;
    while (i < Count) and (result = nil) do
    begin
      if pDataClass = TtiTVDataFormMapping(Items[i]).DataClass then
        result := TtiTVDataFormMapping(Items[i]);
      inc(i);
    end;
  end
  else
    for i := 0 to Count - 1 do
      if pDataClass.InheritsFrom(TtiTVDataFormMapping(Items[i]).DataClass) then
      begin
        if result = nil then
          result := TtiTVDataFormMapping(Items[i])
        else if TtiTVDataFormMapping(Items[i]).DataClass.InheritsFrom(result.DataClass) then
          result := TtiTVDataFormMapping(Items[i]);
      end;

  if result = nil then
  begin
    // A mapping for pDataClass was not found, so create one
    result := TtiTVDataFormMapping.Create;
    result.DataClass := pDataClass;
    result.FormInstance := nil;
    Add(result);
  end;
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiTVFormInstances
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

function TtiTVFormInstances.FindByFormClass(pFormClass: TFormClass;
  pbMultiInstance: boolean): TForm;
var
  i: integer;
  lsMessage: string;
  lForm: TForm;
begin
  result := nil;
  if not pbMultiInstance then
    for i := 0 to Count - 1 do
      if TForm(Items[i]).ClassType = pFormClass then
      begin
        result := TForm(Items[i]);
      end;

  if result = nil then
  begin
    // The form was not found, so create it.
    lForm := pFormClass.Create(nil);
    lsMessage := '';

    // Confirm there is a Data property on lForm
    if not IsPublishedProp(lForm, 'Data') then
    begin
      if lsMessage <> '' then
        lsMessage := lsMessage + #13;
      lsMessage := lsMessage +
        '''Data'' property not published on form ' +
        lForm.ClassName;
    end;

    // Confirm there is a Valid property on lForm
    if not IsPublishedProp(lForm, 'Valid') then
    begin
      if lsMessage <> '' then
        lsMessage := lsMessage + #13;
      lsMessage := lsMessage +
        '''Valid'' property not published on form ' +
        lForm.ClassName;
    end;

    // Confirm there is a TreeNode property on lForm
    if not IsPublishedProp(lForm, 'TreeNode') then
    begin
      if lsMessage <> '' then
        lsMessage := lsMessage + #13;
      lsMessage := lsMessage +
        '''TreeNode'' property not published on form' +
        lForm.ClassName;
    end;

    // Set the data property to nil
    try
      SetObjectProp(lForm, 'Data', nil);
    except
      on e: exception do
      begin
        if lsMessage <> '' then
          lsMessage := lsMessage + #13;
        lsMessage := lsMessage +
          'Error setting ' +
          lForm.ClassName +
          '.Data := nil. Message: ' +
          e.Message;
      end;
    end;

    if lsMessage <> '' then
    begin
      lForm.Free;
      raise exception.Create(lsMessage + #13 + #13 +
        'Called in TtiTreeView.RegisterChildForm');
    end;

    Add(lForm);
    result := lForm;
  end;

end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiTVSplitter
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

constructor TtiTVSplitter.Create(Owner: TComponent);
begin
  inherited;
  Width := cuiSplitterWidth;
  FbMouseOver := false;
  ResizeStyle := rsUpdate;
end;

procedure TtiTVSplitter.CMMouseEnter(var Message: TMessage);
begin
  FbMouseOver := true;
  Paint;
end;

procedure TtiTVSplitter.CMMouseLeave(var Message: TMessage);
begin
  FbMouseOver := false;
  Paint;
end;

procedure TtiTVSplitter.DrawGrabBar(pRect: TRect);
var
  lFillRect: TRect;
  lSaveColor: TColor;
begin
  lSaveColor := Canvas.Brush.Color;

  // Draw the outline of the rectangle
  Canvas.Pen.Color := clGray;
  Canvas.Rectangle(pRect);

  // If the mouse is over the splitter bar, then fill the grab bar part
  // with colour.
  if FbMouseOver then
  begin
    lFillRect.Top := pRect.Top + 1;
    lFillRect.Left := pRect.Left + 1;
    lFillRect.Bottom := pRect.Bottom - 2;
    lFillRect.Right := pRect.Right - 2;
    Canvas.Brush.Color := cuColorSplitterGrabBar;
    Canvas.FillRect(lFillRect);
  end;

  // Draw a shadow around the inside of the grab bar
  Canvas.Pen.Color := clWhite;
  Canvas.MoveTo(pRect.Left + 1, pRect.Top + 1);
  Canvas.LineTo(pRect.Right - 1, pRect.Top + 1);
  Canvas.MoveTo(pRect.Left + 1, pRect.Top + 1);
  Canvas.LineTo(pRect.Left + 1, pRect.Bottom - 1);

  // Draw some texture inside the grab bar
  Canvas.Pen.Style := psDot;
  Canvas.MoveTo(pRect.Left + 3, pRect.Top + 15);
  Canvas.LineTo(pRect.Left + 3, pRect.Bottom - 15);
  Canvas.Pen.Color := clGray;
  Canvas.MoveTo(pRect.Left + 4, pRect.Top + 16);
  Canvas.LineTo(pRect.Left + 4, pRect.Bottom - 16);

  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := clBlack;

  // Draw the top triangle
  Canvas.Polygon([Point(pRect.Left + 2, pRect.Top + 5),
    Point(pRect.Left + 2, pRect.Top + 10),
      Point(pRect.Left + 4, pRect.Top + 7)]);

  // Draw the bottom triangle
  Canvas.Polygon([Point(pRect.Left + 2, pRect.Bottom - 5),
    Point(pRect.Left + 2, pRect.Bottom - 10),
      Point(pRect.Left + 4, pRect.Bottom - 7)]);
  Canvas.Brush.Color := lSaveColor;

end;

procedure TtiTVSplitter.Paint;
var
  lRect: TRect;
begin
  inherited;
  lRect.Top := Height div 4;
  lRect.Bottom := Height div 4 * 3;
  lRect.Left := 1;
  lRect.Right := 7;
  DrawGrabBar(lRect);
end;

{ TtiTreeViewChildForm }

constructor TtiTreeViewChildForm.Create(owner: TComponent);
begin
  inherited;
  TV.Align := alLeft ;
  TV.OnChanging := DoOnChanging;
  TV.ChangeDelay := 500;
  FSplitter          := TtiTVSplitter.Create(self);
  FSplitter.Parent   := self;
  FSplitter.Left     := cuiSplitterPos;
  FSplitter.Align    := alLeft;
  FSplitter.Beveled  := false;
  FSplitter.Width    := 0;
  FSplitter.Visible  := true;
  FSplitter.AutoSnap := false ;
  FSplitter.MinSize  := 10 ;
  FDataFormMappings := TtiTVDataFormMappings.Create;
  FFormInstances := TtiTVFormInstances.Create;
  VisibleButtons := [];
end;

destructor TtiTreeViewChildForm.Destroy;
begin
  FDataFormMappings.Free;
  FFormInstances.Free;
  inherited;
end;

procedure TtiTreeViewChildForm.DoSetData(Sender: TObject);
begin
  inherited;
  if (FData = nil) then
    if CurrentChildForm <> nil then
      ClearCurrentChildForm;
end;

function TtiTreeViewChildForm.GetSplitterVisible: boolean;
begin
  result := FSplitter.Visible;
end;

procedure TtiTreeViewChildForm.SetSplitterVisible(const Value: boolean);
begin
  FSplitter.Visible := Value;
end;

procedure TtiTreeViewChildForm.SetSplitterPos(const Value: integer);
var
  lValue : integer ;
begin
  lValue := Value ;
  TV.Width := lValue;
  FSplitter.Left := lValue;
{
    FSplitter.Width := cuiSplitterWidth;
    FSplitter.Visible := true;
    TV.Align := alLeft;
    if csDesigning in ComponentState then
      SplitterPos := ClientWidth div 2;

}

end;

function TtiTreeViewChildForm.GetSplitterPos: integer;
begin
  result := TV.Width;
end;

procedure TtiTreeViewChildForm.RegisterChildForm(pDataClassRef: TtiTVDataClassRef;
  pFormClassRef: TtiTVFormClassRef;
  pbMultiInstance: boolean = false);
var
  lForm: TForm;
  lDataFormMapping: TtiTVDataFormMapping;
begin

  lDataFormMapping :=
    FDataFormMappings.FindByDataClass(pDataClassRef, true);

  lForm := FFormInstances.FindByFormClass(pFormClassRef, pbMultiInstance);
  lDataFormMapping.FormInstance := lForm;

  lForm.Visible := false;
  lForm.Parent := self;
  lForm.BorderStyle := bsNone;
  lForm.Align := alClient;

end;

function TtiTreeViewChildForm.IsCurrentChildFormValid: boolean;
begin
  result := true;
  if (FCurrentChildForm <> nil) and
    //     ( IsPublishedProp( FCurrentChildForm, 'Data' )      ) and
//     ( GetObjectProp( FCurrentChildForm, 'Data' ) <> nil ) and
  (IsPublishedProp(FCurrentChildForm, 'Valid')) then
    result := GetPropValue(FCurrentChildForm, 'Valid', false);
end;

procedure TtiTreeViewChildForm.ClearCurrentChildForm;
begin
  if FCurrentChildForm = nil then
    Exit; //==>
  SetObjectProp(FCurrentChildForm, 'Data', nil);
  SetObjectProp(FCurrentChildForm, 'TreeNode', nil);
  FCurrentChildForm.Visible := false;
  FCurrentChildForm := nil;
end;

procedure TtiTreeViewChildForm.SetData(const Value: TtiObject);
begin
  inherited;
  if Value = nil then
    ClearCurrentChildForm;
end;

procedure TtiTreeViewChildForm.DoOnChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
begin
  AllowChange := IsCurrentChildFormValid;
end;

procedure TtiTreeViewChildForm.GetDataPage(pData: TObject; pNode: TTreeNode);
begin

  // If CurrentChildForm was assigned, then do some cleaning up.
  if FCurrentChildForm <> nil then
    ClearCurrentChildForm;

  // Find a childForm for editing the Data class attached to the selected node.
  FCurrentChildForm :=
    FDataFormMappings.FindByDataClass(pData.ClassType).FormInstance;

  // There was a form for this node, so setup the form
  if FCurrentChildForm <> nil then
  begin

    // To here...
    if Assigned(FOnAfterGetChildForm) then
      FOnAfterGetChildForm(FCurrentChildForm,
        pData,
        pNode);

    SetObjectProp(FCurrentChildForm, 'TreeNode', pNode);
    SetObjectProp(FCurrentChildForm, 'Data', pData);
    FCurrentChildForm.Visible := true;

  end;

end;

procedure TtiTreeViewChildForm.DoReSize(Sender: TObject);
begin
  //inherited;

end;

end.

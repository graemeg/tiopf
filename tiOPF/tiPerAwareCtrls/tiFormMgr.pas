unit tiFormMgr;

interface
uses
  tiObjAbs
  ,Forms
  ,Contnrs
  ,ExtCtrls
  ,Controls
  ,tiPtnVisPerObj
  ,FtiFormMgrForm
  ,Messages
  ,Graphics
  ,Classes
  ;

const
  TI_CLOSEACTIVEFORM = WM_USER + 1000 ;

type

  TtiOnShowFormEvent = procedure( const pForm : TFormTIFormMgrForm ) of object ;

  TtiFormMgr = class( TtiObjAbs )
  private
    FForms : TObjectList ;
    FActiveForm : TFormTIFormMgrForm ;
    FActiveFormIndex : integer ;
    FParentPnl: TPanel;
    FModalResult: TModalResult;
    FBorderColor: TColor;
    FOnShowForm: TtiOnShowFormEvent;
    function    GetNextForm: TFormTIFormMgrForm;
    function    GetPrevForm: TFormTIFormMgrForm;
    function    GetForms(i: integer): TFormTIFormMgrForm;
    procedure   SetForms(i: integer; const Value: TFormTIFormMgrForm);
    function    GetActiveForm: TFormTIFormMgrForm;
    procedure   SetActiveForm(const Value: TFormTIFormMgrForm);
    procedure   Add( const pForm : TFormTIFormMgrForm ) ;
    procedure   DoCloseForm( const pForm : TFormTIFormMgrForm ) ;
    function    SaveFormActiveForm( pClose : boolean ) : boolean ;
    procedure   UpdateActiveFormIndex;
    procedure   SetBorderColor(const Value: TColor);
    procedure   DoOnShowForm(const pForm : TFormTIFormMgrForm);
    procedure SetParentPnl(const Value: TPanel);
  public
    constructor Create ;
    destructor  Destroy ; override ;

    procedure   ShowForm( const pFormClass      : TFormTIFormMgrFormClass ; const pData : TPerObjAbs = nil ; pOwnsData : boolean = true ; pReadOnly : boolean = false ) ; overload ;
    procedure   ShowForm( const pForm           : TFormTIFormMgrForm      ; const pData : TPerObjAbs = nil ; pOwnsData : boolean = true ; pReadOnly : boolean = false ) ; overload ;
    procedure   BringToFront( const pForm : TFormTIFormMgrForm ) ;

    function    FindForm( const pFormClass : TFormTIFormMgrFormClass ; const pData : TPerObjAbs ) : TFormTIFormMgrForm ;
    function    IndexOf( const pForm : TFormTIFormMgrForm ) : integer ;

    procedure   CloseForm( const pForm : TFormTIFormMgrForm ) ;
    procedure   CloseAllForms ;
    procedure   RemoveForm( const pForm : TFormTIFormMgrForm ) ;

    property    ActiveForm : TFormTIFormMgrForm read GetActiveForm write SetActiveForm ;
    property    Forms[i:integer] : TFormTIFormMgrForm read GetForms write SetForms ;
    property    ParentPnl : TPanel read FParentPnl write SetParentPnl ;
    property    ModalResult : TModalResult read FModalResult write FModalResult ;
    procedure   ShowPrevForm ;
    procedure   ShowNextForm ;
    property    NextForm : TFormTIFormMgrForm read GetNextForm ;
    property    PrevForm : TFOrmTIFormMgrForm read GetPrevForm ;
    property    BorderColor : TColor read FBorderColor write SetBorderColor ;
    property    OnShowForm : TtiOnShowFormEvent read FOnShowForm write FOnShowForm ;
    procedure   AssignFormList(const pStrings : TStrings);
  end ;

function gFormMgr : TtiFormMgr ;

implementation
uses
  tiUtils
  ,dialogs // debugging
  ,SysUtils
  ;

var
  uFormMgr : TtiFormMgr ;

function gFormMgr : TtiFormMgr ;
begin
  if uFormMgr = nil then
    uFormMgr := TtiFormMgr.Create ;
  result := uFormMgr ;
end ;

procedure TtiFormMgr.Add(const pForm: TFormTIFormMgrForm);
begin
  pForm.BorderStyle := bsNone ;
  pForm.Parent := ParentPnl ;
  pForm.Align := alClient ;
  pForm.PositionButtons ;
  FForms.Add(pForm);
end;

procedure TtiFormMgr.BringToFront(const pForm: TFormTIFormMgrForm);
begin
  Assert(pForm<>nil, 'pForm not assigned');
  if pForm = FActiveForm then
    Exit ; //==>
  pForm.Visible := true ;
  pForm.SetFocus;
  if FActiveForm <> nil then
    FActiveForm.Visible := false ;
  FActiveForm := pForm ;
  UpdateActiveFormIndex ;
  DoOnShowForm(FActiveForm);
end;

procedure TtiFormMgr.CloseAllForms;
begin
  while FForms.Count > 0 do
    DoCloseForm( Forms[FForms.Count-1] ) ;
end;

procedure TtiFormMgr.DoCloseForm(const pForm: TFormTIFormMgrForm);
begin
  if ActiveForm = pForm then
  begin
    if GetPrevForm <> nil then
      ActiveForm := GetPrevForm
    else
      ActiveForm := GetNextForm ;
  end ;
  FForms.Extract( pForm ) ;
  ModalResult := pForm.ModalResult ;
  if ActiveForm = pForm then
    ActiveForm := nil ;
  UpdateActiveFormIndex ;
  pForm.Free;
end;

procedure TtiFormMgr.UpdateActiveFormIndex ;
begin
  FActiveFormIndex := FForms.IndexOf(FActiveForm);
end ;

constructor TtiFormMgr.Create;
begin
  inherited ;
  FForms := TObjectList.Create(false) ;
end;

destructor TtiFormMgr.Destroy;
var
  i : integer ;
  lForm : TFormTIFormMgrForm ;
  lName : string ;
begin
  for i := FForms.Count - 1 downto 0 do
  begin
    lForm := FForms.Items[i] as TFormTIFormMgrForm ;
    lName := lForm.ClassName ;
    FForms.Extract(lForm);
    try
      lForm.Free ;
    except
      on e:exception do
        ShowMessage( 'Error destroying form <' + lName + '>' + Cr +
                     'Message: ' + e.Message + Cr +
                     'Location: ' + ClassName + '.Destroy' ) ;
    end ;
  end ;
  FForms.Free ;
  inherited;
end;

function TtiFormMgr.FindForm(const pFormClass: TFormTIFormMgrFormClass ; const pData : TPerObjAbs ) : TFormTIFormMgrForm ;
var
  i : integer ;
begin
  result := nil ;
  for i := 0 to FForms.Count - 1 do
    if ( FForms.Items[i] is pFormClass ) and
       ( TFormTIFormMgrForm( FForms.Items[i] ).Data = pData ) then
    begin
      result := FForms.Items[i] as TFormTIFormMgrForm ;
      Exit ; //==>
    end ;
end;

function TtiFormMgr.GetActiveForm: TFormTIFormMgrForm;
begin
  result := FActiveForm ;
end;

function TtiFormMgr.GetForms(i: integer): TFormTIFormMgrForm;
begin
  result := FForms[i] as TFormTIFormMgrForm ;
end;

function TtiFormMgr.GetNextForm: TFormTIFormMgrForm;
begin
  if FActiveFormIndex < FForms.Count - 1 then
    result := Forms[FActiveFormIndex+1]
  else
    result := nil ;
end;

function TtiFormMgr.GetPrevForm: TFormTIFormMgrForm;
begin
  if FActiveFormIndex > 0 then
    result := Forms[FActiveFormIndex-1]
  else
    result := nil;
end;

function TtiFormMgr.IndexOf(const pForm: TFormTIFormMgrForm): integer;
begin
  result := FForms.IndexOf(pForm);
end;

procedure TtiFormMgr.RemoveForm(const pForm: TFormTIFormMgrForm);
begin
  FForms.Remove(pForm)
end;

procedure TtiFormMgr.SetActiveForm(const Value: TFormTIFormMgrForm);
begin
  if Value = nil then
  begin
    FActiveForm := Value ;
    Exit ; //==>
  end ;
  ShowForm(Value, Value.Data);
end;

procedure TtiFormMgr.SetForms(i: integer; const Value: TFormTIFormMgrForm);
begin
  FForms[i] := Value ;
end;

procedure  TtiFormMgr.ShowForm(
  const pForm           : TFormTIFormMgrForm      ;
  const pData : TPerObjAbs = nil ;
  pOwnsData : boolean = true ;
  pReadOnly : boolean = false ) ; 
begin
  ShowForm( TFormTIFormMgrFormClass(pForm.ClassType), pData, pOwnsData, pReadOnly ) ;
end ;

procedure TtiFormMgr.ShowForm(
  const pFormClass      : TFormTIFormMgrFormClass ;
  const pData : TPerObjAbs = nil ;
  pOwnsData : boolean = true ;
  pReadOnly : boolean = false );
var
  lForm : TFormTIFormMgrForm ;
begin
  Assert( ParentPnl <> nil, 'ParentPnl not assigned' ) ;
  if ( FActiveForm <> nil ) and
     ( not SaveFormActiveForm(false) ) then
    Exit ; //==>
  lForm := FindForm( pFormClass, pData ) ;
  if lForm <> nil then
    BringToFront( lForm )
  else
  begin
    lForm := pFormClass.Create(nil);
    lForm.BorderColor := BorderColor ;
    Add(lForm);
    lForm.OwnsData := pOwnsData ;
    if lForm.Data <> pData then
      lForm.Data := pData ;
    BringToFront( lForm );
  end ;
end;

procedure TtiFormMgr.CloseForm(const pForm: TFormTIFormMgrForm);
begin
  if FActiveForm <> nil then
    SaveFormActiveForm( true ) ;
end;

function TtiFormMgr.SaveFormActiveForm( pClose : boolean ) : boolean;
var
  lForm : TFormTIFormMgrForm ;
  lFormLeaveState : TFormLeaveState ;
  lCanClose : boolean ;
begin
  if Assigned(FActiveForm.OnCloseQuery) then
  begin
    lCanClose := true ;
    FActiveForm.OnCloseQuery(FActiveForm, lCanClose);
    if not lCanClose then
    begin
      result := false;
      Exit ; //==>
    end ;
  end ;

  lFormLeaveState := FActiveForm.CanLeaveForm ;
  case lFormLeaveState of
  flsCanLeaveOpen : begin
                      if pClose then
                      begin
                        DoCloseForm(FActiveForm);
                        result := true ;
                      end
                      else begin
                        FActiveForm.Visible := false ;
                        FActiveForm := nil ;
                        result := true ;
                        DoOnShowForm(Nil);
                      end ;
                    end ;
  flsMustFree     : begin
                      lForm := FActiveForm ;
                      FActiveForm := nil;
                      FForms.Extract( lForm ) ;
                      lForm.Free;
                      if GetPrevForm <> nil then
                        ActiveForm := GetPrevForm
                      else
                        ActiveForm := GetNextForm;
                      result := true ;
                      DoOnShowForm(ActiveForm);
                    end ;
  flsCanNotLeave  : result := false ;
  else
    result := false ; // To shut the compiler up
    tiFmtException('Invalid FormLeaveState',ClassName,'ShowForm');
  end ;
end;

procedure TtiFormMgr.ShowNextForm;
var
  lForm : TFormTIFormMgrForm;
begin
  lForm := GetNextForm ;
  if lForm = nil then
    ShowForm(nil, nil)
  else
    ShowForm(lForm, lForm.Data) ;
end;

procedure TtiFormMgr.ShowPrevForm;
var
  lForm : TFormTIFormMgrForm;
begin
  lForm := GetPrevForm ;
  if lForm = nil then
    ShowForm(nil, nil)
  else
    ShowForm(lForm, lForm.Data) ;
end;

{
function TtiFormMgr.ShowFormModal( const pFormClass: TFormTIFormMgrFormClass; const pData: TPerObjAbs) : boolean ;
var
  lForm : TFormTIFormMgrForm ;
begin
  Assert( ParentPnl <> nil, 'ParentPnl not assigned' ) ;
  if ( FActiveForm <> nil ) and
     ( not SaveFormActiveForm(false) ) then
  begin
    Result := false ;
    Exit ; //==>
  end ;
  lForm := FindForm( pFormClass, pData ) ;
  if lForm <> nil then
    result := BringToFrontModal( lForm )
  else
  begin
    lForm := pFormClass.Create(nil);
    Add(lForm);
    if lForm.Data <> pData then
      lForm.Data := pData ;
    result := BringToFrontModal( lForm )
  end ;
end;
}

{
function TtiFormMgr.BringToFrontModal(const pForm: TFormTIFormMgrForm): boolean ;
begin
  Assert(pForm<>nil, 'pForm not assigned');
  if pForm = FActiveForm then
    Exit ; //==>
  if FActiveForm <> nil then
    FActiveForm.Visible := false ;
  FActiveForm := pForm ;
  UpdateActiveFormIndex;
  pForm.ModalForm := true ;
  pForm.Visible := true ;
  pForm.SetupButtons ;
  while pForm.Visible do
    Application.ProcessMessages ;
  ShowMessage('Your out');
  result := ModalResult = mrOK ;
end;
}

procedure TtiFormMgr.SetBorderColor(const Value: TColor);
var
  i : integer ;
begin
  FBorderColor := Value;
  for i := 0 to FForms.Count - 1 do
    ( FForms.Items[i] as TFormTIFormMgrForm ).BorderColor := FBorderColor ;
end;

procedure TtiFormMgr.AssignFormList(const pStrings: TStrings);
var
  i : integer ;
begin
  pStrings.Clear ;
  for i := 0 to FForms.Count - 1 do
    pStrings.AddObject(( FForms.Items[i] as TFormTIFormMgrForm ).FormCaption,
                        FForms.Items[i] ) ;
end;

procedure TtiFormMgr.DoOnShowForm(const pForm : TFormTIFormMgrForm);
begin
  if assigned(FOnShowForm) then
     FOnShowForm(pForm);
end;


procedure TtiFormMgr.SetParentPnl(const Value: TPanel);
begin
  FParentPnl := Value;
end;

initialization

finalization
  FreeAndNil(uFormMgr) ;

end.

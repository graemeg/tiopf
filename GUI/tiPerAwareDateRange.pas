unit tiPerAwareDateRange;

{$I tiDefines.inc}

interface
uses
  SysUtils
  ,Classes
  ,StdCtrls
  ,Buttons
  ,ExtCtrls
  ,Comctrls
  ,Registry
{$IFNDEF FPC}
  ,Messages
{$ELSE}
  ,LMessages
  ,EditBtn
{$ENDIF}
 ;

const
  ctiDateRangeMinDate       = 1.0;
  ctiDateRangeMaxDate       = 2958465.0;

type

 {$IFDEF FPC}
  TDateTimePicker = class(TDateEdit);
 {$ENDIF}

  TRangeType  = (rtSingleDate, rtDateRange{, rtBoth});
  TAllowRangeType = (artSingleDate, artDateRange, artBoth);

  // forward declaration
  TtiDateRange = class;

  // An abstract range setter - Used to set the date range to values like:
  // Tomorrow, Yesterday, Today, Last Week, Last Month, Year to date, etc
  TRangeSetter = class(TObject)
  private
    FsCaption: string;
    FRangeType: TRangeType;
    FtiDateRange : TtiDateRange;
    function  GetDateFrom : TDateTime;
    function  GetDateTo  : TDateTime;
    procedure SetDateFrom(const AValue: TDateTime);
    procedure SetDateTo(const AValue: TDateTime);
    function  Month : word;
    function  Year : word;
    function  DayOfWeek : word;
  protected
    property DateFrom : TDateTime read GetDateFrom write SetDateFrom;
    property DateTo : TDateTime read GetDateTo write SetDateTo;
  public
    Constructor Create; virtual;
    property    RangeType : TRangeType read FRangeType write FRangeType;
    property    Caption  : string     read FsCaption  write FsCaption;
    procedure   Apply(ptiDateRange : TtiDateRange); virtual;
  end;

  TrsCustomSingleDate = class(TRangeSetter)
  public
    Constructor Create; override;
    procedure   Apply(ptiDateRange : TtiDateRange); override;
  end;

  TrsCustomDateRange = class(TRangeSetter)
  public
    Constructor Create; override;
    procedure   Apply(ptiDateRange : TtiDateRange); override;
  end;

  TrsToday = class(TRangeSetter)
  public
    Constructor Create; override;
    procedure   Apply(ptiDateRange : TtiDateRange); override;
  end;

  TrsYesterday = class(TRangeSetter)
  public
    Constructor Create; override;
    procedure   Apply(ptiDateRange : TtiDateRange); override;
  end;

  TrsThisWeek = class(TRangeSetter)
  public
    Constructor Create; override;
    procedure   Apply(ptiDateRange : TtiDateRange); override;
  end;

  TrsLastWeek = class(TRangeSetter)
  public
    Constructor Create; override;
    procedure   Apply(ptiDateRange : TtiDateRange); override;
  end;

  TrsThisMonth = class(TRangeSetter)
  public
    Constructor Create; override;
    procedure   Apply(ptiDateRange : TtiDateRange); override;
  end;

  TrsLastMonth = class(TRangeSetter)
  public
    Constructor Create; override;
    procedure   Apply(ptiDateRange : TtiDateRange); override;
  end;

  TrsCalYearToDate = class(TRangeSetter)
  public
    Constructor Create; override;
    procedure   Apply(ptiDateRange : TtiDateRange); override;
  end;

  TrsAllDates = class(TRangeSetter)
  public
    Constructor Create; override;
    procedure   Apply(ptiDateRange : TtiDateRange); override;
  end;

  TrsThisFinancialYear = class(TRangeSetter)
  public
    Constructor Create; override;
    procedure   Apply(ptiDateRange : TtiDateRange); override;
  end;

  TrsLastFinancialYear = class(TRangeSetter)
  public
    Constructor Create; override;
    procedure   Apply(ptiDateRange : TtiDateRange); override;
  end;

  TRangeSetterClass = class of TRangeSetter;

  // Factory to create range setters as required.
  TFactoryRangeSetter = class(TStringList)
  private
    FtiDateRange: TtiDateRange;

  public
    Constructor CreateExt(ptiDateRange : TtiDateRange);
    Destructor Destroy; override;
    Procedure  RegisterRangeSetter(AClass : TRangeSetterClass);
    property   tiDateRange : TtiDateRange read FtiDateRange write FtiDateRange;
    procedure  Apply(const psCaption : string;
                      pRangeType : TRangeType);
    procedure  ReadRangeSetterCaptions;
    function   FindRangeSetter(const psCaption : string;
                                pRangeType : TRangeType): TRangeSetter;
  end;

  TtiDateRangeOption = (droptShowRememberNextTime);
  TtiDateRangeOptions = set of TtiDateRangeOption;

  // TDateRange component
  TtiDateRange  = class(TCustomGroupBox)
  private
    FlblFrom : TLabel;
    FlblTo   : TLabel;
    FlblQuick : TLabel;

    FdtpFrom : TDateTimePicker;
    FdtpTo  : TDateTimePicker;

    FrbSingleDate      : TRadioButton;
    FrbDateRange       : TRadioButton;

    FcbQuick : TComboBox;
    FiQuick : integer;
    FchbSave : TCheckBox;

    FRangeType            : TRangeType  ;

    FOnChange              : TNotifyEvent;

    FFactoryRangeSetter    : TFactoryRangeSetter;
    FAllowRangeType: TAllowRangeType;
    FOptions: TtiDateRangeOptions;

    procedure rbRangeTypeClick(sender: TObject);
    procedure chbSaveClick(sender: TObject);
    function  GetDateFrom : TDateTime                ;
    procedure SetDateFrom(const pDate : TDateTime)       ;
    function  GetDateTo : TDateTime                  ;
    procedure SetDateTo(const pDate : TDateTime)         ;
    procedure SetRangeType(const dgDateGroup : TRangeType);
    procedure DoOnChangeDateFrom(sender : TObject);
    procedure DoOnChangeDateTo(sender : TObject);
    procedure DoOnChangeQuick(sender : TObject);

    procedure DoOnChange;
    {$IFNDEF FPC}
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    {$ELSE}
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    {$ENDIF}
    function  getRangeSetterCaptions: TStrings;
    function  AppFileName : string;
    function  RegKeyName   : string;
    function  GetMaxDate: TDateTime;
    function  GetMinDate: TDateTime;
    procedure SetMaxDate(const AValue: TDateTime);
    procedure SetMinDate(const AValue: TDateTime);
    procedure SetAllowRangeType(const AValue: TAllowRangeType);
    procedure SetOptions(const AValue: TtiDateRangeOptions);
    function  GetRangeSetterIndex: Integer;
    procedure SetRangeSetterIndex(const AValue: Integer);

  protected
    procedure Loaded; override;
    Property  RangeSetterCaptions : TStrings read getRangeSetterCaptions;

  published
    property MinDate  : TDateTime    read GetMinDate  write SetMinDate;
    property MaxDate  : TDateTime    read GetMaxDate  write SetMaxDate;
    property DateFrom : TDateTime    read getDateFrom write SetDateFrom       ;
    property DateTo   : TDateTime    read getDateTo   write SetDateTo         ;
    property RangeType : TRangeType   read FRangeType  write SetRangeType default rtDateRange;
    property RangeSetterIndex: Integer Read GetRangeSetterIndex Write SetRangeSetterIndex;
    property Options  : TtiDateRangeOptions Read FOptions Write SetOptions default [droptShowRememberNextTime];
    property OnChange : TNotifyEvent read FOnChange   write FOnChange;
    property AllowRangeType : TAllowRangeType read FAllowRangeType write SetAllowRangeType default artBoth;
    property Caption;
    property Anchors;
  public
    Constructor Create(oOwner : TComponent); override;
    Destructor  Destroy; override;

    procedure Save;
    procedure Read;

    procedure ShowInternalState;

  end;





implementation
uses
  Forms
  ,Math
  ,Dialogs // for debuggin
 ;

const
  cuiHeight =  94;
  cuiWidth  = 195;





// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDateRange
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TtiDateRange.Create(oOwner : TComponent);
begin
  inherited create(oOwner);

  FRangeType := rtDateRange;
  Width := cuiWidth;
  Height := cuiHeight;
  Caption := ' &Date range ';

  FdtpFrom := TDateTimePicker.Create(self);
  with FdtpFrom do begin
    parent  := self;
    top     := 16;
    left    := 40;
    height  := 22;
    width   := 130;
    OnChange := DoOnChangeDateFrom;
    //MinDate := StrToDate('01/01/0001');
    //MaxDate := StrToDate('31/12/9999');
    MinDate := ctiDateRangeMinDate;
    MaxDate := ctiDateRangeMaxDate;
    Date    := SysUtils.Date;
    {$IFNDEF FPC}Time    := 0.0;{$ENDIF}
  end;

  FdtpTo  := TDateTimePicker.Create(self);
  with FdtpTo do begin
    parent  := self;
    top     := 40;
    left    := 40;
    height  := 22;
    width   := 130;
    OnChange := DoOnChangeDateTo;
    MinDate := ctiDateRangeMinDate;
    MaxDate := ctiDateRangeMaxDate;
    Date    := SysUtils.Date;
    {$IFNDEF FPC}Time    := 0.0;{$ENDIF}
  end;

  FlblFrom := TLabel.Create(self);
  with FlblFrom do begin
    parent := self;
    top    := 20;
    left   :=  8;
    caption := '&From';
    FocusControl := FdtpFrom;
  end;

  FlblTo := TLabel.Create(self);
  with FlblTo do begin
    parent := self;
    top    :=  44 ;
    left   :=   8 ;
    caption := '&To';
    FocusControl := FdtpTo;
  end;

  FrbSingleDate   := TRadioButton.Create(self);
  with FrbSingleDate do begin
    parent  := self;
    top     := FdtpFrom.Top + (FdtpFrom.Height - Height) div 2  ;
    left    := FdtpFrom.Left + FdtpFrom.Width + 4  ;
    width   := 14;
    caption := '';
    onClick := rbRangeTypeClick;
    tag     := 0;
    Hint    := 'Select a single date';
    ShowHint := true;
  end;

  FrbDateRange  := TRadioButton.Create(self);
  with FrbDateRange do begin
    parent  := self;
    top     := FdtpTo.Top  ;
    top     := FdtpTo.Top + (FdtpTo.Height - Height) div 2  ;
    left    := FdtpTo.Left + FdtpTo.Width + 4  ;
    width   := 14;
    caption := '';
    checked := true;
    onClick := rbRangeTypeClick;
    tag     := 1;
    Hint    := 'Select a date range';
    ShowHint := true;
  end;

  FcbQuick  := TComboBox.Create(self);
  with FcbQuick do begin
    parent  := self;
    top     := FdtpTo.Top + FdtpTo.Height + 4;
    left    := 40;
    height  := 22;
    width   := 130;
    style   := csDropDownList;
    OnChange := DoOnChangeQuick;
  end;

  FlblQuick := TLabel.Create(self);
  with FlblQuick do begin
    parent := self;
    top    :=  FcbQuick.Top + (FcbQuick.Height - Height) div 2 ;
    left   :=   8 ;
    caption := '&Quick';
    FocusControl := FcbQuick;
  end;

  FchbSave := TCheckBox.Create(self);
  with FchbSave do begin
    parent  := self;
    top     := FcbQuick.Top + (FcbQuick.Height - Height) div 2  ;
    left    := FcbQuick.Left + FcbQuick.Width + 4  ;
    width   := 14;
    caption := '';
    onClick := chbSaveClick;
    Hint    := 'Remember these settings next time';
    ShowHint := true;
  end;

  FFactoryRangeSetter := TFactoryRangeSetter.CreateExt(self);

//  DateFrom := Date;
//  DateTo  := Date;
  FrbDateRange.Checked := true;
  AllowRangeType := artBoth;
  FOptions := [droptShowRememberNextTime];

end;

destructor TtiDateRange.Destroy;
begin
  Save;
  FlblFrom.Free           ;
  FlblTo.Free             ;
  FdtpFrom.Free           ;
  FdtpTo.Free             ;
  FrbSingleDate.Free      ;
  FrbDateRange.Free       ;
  FFactoryRangeSetter.Free;
  inherited;
end;

procedure TtiDateRange.Loaded;
begin
  inherited;

  FcbQuick.ItemIndex := 0;
  FiQuick := 0;
  FFactoryRangeSetter.ReadRangeSetterCaptions;
  FcbQuick.DropDownCount := FcbQuick.Items.Count;
  SetOptions(FOptions);
  Read;

end;


procedure TtiDateRange.rbRangeTypeClick(Sender: TObject);
begin
  RangeType := TRangeType((Sender as TRadioButton).Tag);
  FcbQuick.ItemIndex := 0;
  DoOnChange;
end;

function  TtiDateRange.getDateFrom : TDateTime                ;
begin
  result := trunc(FdtpFrom.Date);
end;

procedure TtiDateRange.SetDateFrom(const pDate : TDateTime)       ;
var
  lDate : TDateTime;
begin
  lDate := trunc(pDate);
  lDate := Max(lDate, MinDate);
  lDate := Min(lDate, MaxDate);

  FdtpFrom.Date := lDate;

  if RangeType = rtSingleDate then
    FdtpTo.Date := lDate;

  if DateFrom > DateTo then
    DateTo := DateFrom;

  Assert(DateFrom = lDate,
          'Failed setting DateFrom: ' +
          FloatToStr(DateFrom) + ' <> ' +
          FloatToStr(lDate));
end;

function TtiDateRange.getDateTo : TDateTime                  ;
begin
  if FRangeType = rtSingleDate then
    result := trunc(FdtpFrom.Date)
  else
    result := trunc(FdtpTo.Date);
end;

procedure TtiDateRange.SetDateTo(const pDate : TDateTime)         ;
var
  lDate : TDateTime;
begin                                                       
  lDate := trunc(pDate);
  lDate := Max(lDate, MinDate);
  lDate := Min(lDate, MaxDate);

  FdtpTo.Date := lDate;
  if RangeType = rtSingleDate then
    FdtpFrom.Date := lDate;
  if DateFrom > DateTo then
    DateFrom := DateTo;
  Assert(DateTo = lDate,
          'Failed setting DateTo: ' +
          FloatToStr(DateTo) + ' <> ' +
          FloatToStr(lDate));

end;

procedure TtiDateRange.SetRangeType(const dgDateGroup : TRangeType);
begin
  if FRangeType = dgDateGroup then
    exit; //==>

  FRangeType := dgDateGroup;
  case FRangeType of
  rtSingleDate : begin
                   FrbSingleDate.Checked := true;
                   FlblTo.Visible := false;
                   FdtpTo.Visible := false;
                   FlblFrom.Caption := '&Date';
                 end;
  rtDateRange : begin
                   FrbDateRange.Checked := true;
                   FlblTo.Visible := true;
                   FdtpTo.Visible := true;
                   FlblFrom.Caption := '&From';
                 end;
  else
    raise exception.Create('Invalid RangeType passed to TtiDateRange.SetRangeType');
  end;

  FFactoryRangeSetter.ReadRangeSetterCaptions;
  FcbQuick.DropDownCount := FcbQuick.Items.Count;
end;

procedure TtiDateRange.DoOnChange;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TtiDateRange.DoOnChangeDateFrom(sender: TObject);
begin
  // This will force the validation of the DateFrom value
  DateFrom := FdtpFrom.Date;
  FcbQuick.ItemIndex := 0;
  FiQuick := 0;
  DoOnChange;
end;

procedure TtiDateRange.DoOnChangeDateTo(sender: TObject);
begin
  // This will force the validation of the DateTo value
  DateTo := FdtpTo.Date;
  FcbQuick.ItemIndex := 0;
  FiQuick := 0;
  DoOnChange;
end;

procedure TtiDateRange.DoOnChangeQuick(sender: TObject);
begin

  if FcbQuick.ItemIndex = -1 then
    exit;

  FiQuick := FcbQuick.ItemIndex;
  FFactoryRangeSetter.Apply(FcbQuick.Items[ FcbQuick.ItemIndex ],
                             RangeType);

  DoOnChange;

end;

procedure TtiDateRange.chbSaveClick(sender: TObject);
begin
  //
end;

{$IFNDEF FPC}
procedure TtiDateRange.WMSize(var Message: TWMSize);
begin
  Height := cuiHeight;
  Width := cuiWidth ;
end;
{$ELSE}
procedure TtiDateRange.WMSize(var Message: TLMSize);
begin
  Height := cuiHeight + 20;
  Width := cuiWidth + 10;
end;
{$ENDIF}

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TFactoryRangeSetter
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TFactoryRangeSetter.CreateExt(ptiDateRange: TtiDateRange);
begin
  Create;
  FtiDateRange := ptiDateRange;
  RegisterRangeSetter(TrsCustomSingleDate);
  RegisterRangeSetter(TrsToday    );
  RegisterRangeSetter(TrsYesterday);

  RegisterRangeSetter(TrsCustomDateRange) ;
  RegisterRangeSetter(TrsThisWeek);
  RegisterRangeSetter(TrsLastWeek);
  RegisterRangeSetter(TrsThisMonth);
  RegisterRangeSetter(TrsLastMonth);
  RegisterRangeSetter(TrsCalYearToDate);
  RegisterRangeSetter(TrsThisFinancialYear);
  RegisterRangeSetter(TrsLastFinancialYear);
  RegisterRangeSetter(TrsAllDates);

end;

destructor TFactoryRangeSetter.Destroy;
var
  i : integer;
begin
  for i := 0 to Count - 1 do
    TObject(Objects[i]).Free;
  inherited;
end;

procedure TFactoryRangeSetter.Apply(const psCaption: string;
                                     pRangeType : TRangeType);
var
  lRangeSetter : TRangeSetter;
begin
  lRangeSetter := FindRangeSetter(psCaption, pRangeType);
  Assert(lRangeSetter <> nil,
          'RangeSetter <' +
          psCaption +
          '> not found');
  lRangeSetter.Apply(FtiDateRange);
end;

function TFactoryRangeSetter.FindRangeSetter(const psCaption: string;
                                   pRangeType: TRangeType): TRangeSetter;
var
  i : integer;
  lRangeSetter : TRangeSetter;
begin
  result := nil;
  for i := 0 to Count - 1 do begin
    lRangeSetter := TRangeSetter(Objects[i]);
    if (lRangeSetter.Caption = psCaption) and
       (lRangeSetter.RangeType = pRangeType) then begin
      result := lRangeSetter;
      break;
    end;
  end;
end;

procedure TFactoryRangeSetter.ReadRangeSetterCaptions;
var
  i : integer;
  lRangeSetter : TRangeSetter;
begin
  tiDateRange.RangeSetterCaptions.Clear;
  for i := 0 to count - 1 do begin
    lRangeSetter := TRangeSetter(Objects[i]);
    if lRangeSetter.RangeType = tiDateRange.RangeType then
      tiDateRange.RangeSetterCaptions.Add(lRangeSetter.Caption);
  end;
end;

procedure TFactoryRangeSetter.RegisterRangeSetter(AClass: TRangeSetterClass);
var
  lRangeSetter : TRangeSetter;
begin
  lRangeSetter := AClass.Create;
  Assert(FindRangeSetter(lRangeSetter.Caption, lRangeSetter.RangeType) = nil,
          'Attempt to register duplicate RangeSetter <' +
          lRangeSetter.Caption + '>');
  AddObject(lRangeSetter.Caption, lRangeSetter);
end;

function TtiDateRange.getRangeSetterCaptions: TStrings;
begin
  result := FcbQuick.Items;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TrsCustomSingleDate
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TrsCustomSingleDate.Create;
begin
  inherited;
  RangeType := rtSingleDate;
  Caption  := 'You choose';
end;

procedure TrsCustomSingleDate.Apply(ptiDateRange: TtiDateRange);
begin
  inherited Apply(ptiDateRange);
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TrsCustomDateRange
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TrsCustomDateRange.Apply(ptiDateRange: TtiDateRange);
begin
  inherited Apply(ptiDateRange);
end;

constructor TrsCustomDateRange.Create;
begin
  inherited;
  RangeType := rtDateRange;
  Caption  := 'You choose';
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TRangeSetter
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TRangeSetter.Apply(ptiDateRange: TtiDateRange);
begin
  FtiDateRange := ptiDateRange;
end;

constructor TRangeSetter.Create;
begin
  inherited;
  // Do nothing, implemented here for the concretes.
end;

function TRangeSetter.DayOfWeek: word;
var
  li : word;
begin
  li := SysUtils.DayOfWeek(date);
  case li of
  1 : result := 7;
  2..7 : result := li - 1;
  else
    raise exception.Create('Invalid dayOfWeek passed to TRangeSetter.DayOfWeek');
  end;
end;

function TRangeSetter.GetDateFrom: TDateTime;
begin
  result := FtiDateRange.DateFrom;
end;

function TRangeSetter.GetDateTo: TDateTime;
begin
  result := FtiDateRange.DateTo;
end;

function TRangeSetter.Month: word;
var
  liD, liM, liY : Word;
begin
  DecodeDate(Date, liY, liM, liD);
  result := liM;
end;

procedure TRangeSetter.SetDateFrom(const AValue: TDateTime);
begin
  FtiDateRange.DateFrom := AValue;
end;

procedure TRangeSetter.SetDateTo(const AValue: TDateTime);
begin
  FtiDateRange.DateTo := AValue;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TrsToday
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TrsToday.Create;
begin
  inherited;
  RangeType := rtSingleDate;
  Caption  := 'Today';
end;

procedure TrsToday.Apply(ptiDateRange: TtiDateRange);
begin
  inherited Apply(ptiDateRange);
  DateFrom := Date;
  DateTo  := DateFrom;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TrsYesterday
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TrsYesterday.Create;
begin
  inherited;
  RangeType := rtSingleDate;
  Caption  := 'Yesterday';
end;

procedure TrsYesterday.Apply(ptiDateRange: TtiDateRange);
begin
  inherited Apply(ptiDateRange);
  DateFrom := Date - 1;
  DateTo  := DateFrom;
end;

function TRangeSetter.Year: word;
var
  liD, liM, liY : Word;
begin
  DecodeDate(Date, liY, liM, liD);
  result := liY;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TrsThisWeek
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TrsThisWeek.Create;
begin
  inherited;
  RangeType := rtDateRange;
  Caption  := 'This week';
end;

procedure TrsThisWeek.Apply(ptiDateRange: TtiDateRange);
begin
  inherited Apply(ptiDateRange);
  DateFrom := Date - (DayOfWeek - 1);
  DateTo  := Date + (7 - DayOfWeek);
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TrsLastWeek
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TrsLastWeek.Create;
begin
  inherited;
  RangeType := rtDateRange;
  Caption  := 'Last week';
end;

procedure TrsLastWeek.Apply(ptiDateRange: TtiDateRange);
begin
  inherited Apply(ptiDateRange);
  DateFrom := Date - (DayOfWeek - 1) - 7;
  DateTo  := Date + (7 - DayOfWeek) - 7;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TrsThisMonth
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TrsThisMonth.Create;
begin
  inherited;
  RangeType := rtDateRange;
  Caption  := 'This month';
end;

procedure TrsThisMonth.Apply(ptiDateRange: TtiDateRange);
var
  LYear: Word;
  LMonth: Word;
  LDay: Word;
  LDate: TDateTime;
begin
  inherited Apply(ptiDateRange);
  DecodeDate(Date, LYear, LMonth, LDay);
  LDate:= EncodeDate(LYear, LMonth, 1);
  DateFrom := LDate;
  DateTo  := IncMonth(LDate) -1;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TrsLastMonth
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TrsLastMonth.Create;
begin
  inherited;
  RangeType := rtDateRange;
  Caption  := 'Last month';
end;

procedure TrsLastMonth.Apply(ptiDateRange: TtiDateRange);
begin
  inherited Apply(ptiDateRange);
  DateFrom := IncMonth(EncodeDate(Year, Month, 1), -1);
  DateTo  := IncMonth(DateFrom, 1) - 1;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TrsCalYearToDate
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TrsCalYearToDate.Create;
begin
  inherited;
  RangeType := rtDateRange;
  Caption  := 'Year to date';
end;

procedure TrsCalYearToDate.Apply(ptiDateRange: TtiDateRange);
begin
  inherited Apply(ptiDateRange);
  DateFrom := EncodeDate(Year, 1, 1);
  DateTo  := Date;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TrsAllDates
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TrsAllDates.Create;
begin
  inherited;
  RangeType := rtDateRange;
  Caption  := 'All dates';
end;

procedure TrsAllDates.Apply(ptiDateRange: TtiDateRange);
begin
  inherited Apply(ptiDateRange);
  DateFrom := FtiDateRange.MinDate;
  DateTo  := FtiDateRange.MaxDate;
end;

procedure TtiDateRange.Read;
var
  lReg : TRegINIFile;
begin
  lReg := TRegINIFile.Create(AppFileName);
  try
    if lReg.ReadInteger(RegKeyName, 'Save', 0) = 1 then begin
      FchbSave.Checked := true;
      RangeType := TRangeType(lReg.ReadInteger(  RegKeyName, 'RangeType', 0));

      FcbQuick.ItemIndex := lReg.ReadInteger(RegKeyName, 'QuickIndex', 0);
      FiQuick := FcbQuick.ItemIndex;
      DoOnChangeQuick(FcbQuick);

      DateFrom := StrToDate(lReg.ReadString(RegKeyName, 'DateFrom', DateToStr(Date)));
      DateTo  := StrToDate(lReg.ReadString(RegKeyName, 'DateTo',   DateToStr(Date)));

    end else begin
      FcbQuick.ItemIndex := 0;
      FiQuick           := 0;
    end;
  finally
    lReg.Free;
  end;
end;

procedure TtiDateRange.Save;
var
  lReg : TRegINIFile;
begin
  lReg := TRegINIFile.Create(AppFileName);
  try
    if FchbSave.Checked then begin
      lReg.WriteInteger(RegKeyName, 'Save', 1);
      lReg.WriteInteger(RegKeyName, 'RangeType', Ord(RangeType));
      lReg.WriteInteger(RegKeyName, 'QuickIndex', FiQuick);
      lReg.WriteString( RegKeyName, 'DateFrom', DateToStr(DateFrom));
      lReg.WriteString( RegKeyName, 'DateTo',   DateToStr(DateTo));
    end else begin
      lReg.WriteInteger(RegKeyName, 'Save', 0);
    end;
  finally
    lReg.Free;
  end;
end;

function TtiDateRange.AppFileName: string;
begin
  result := Application.EXEName;
  result := ExtractFileName(result);
  result := ChangeFileExt(result, '');
end;

function TtiDateRange.RegKeyName: string;
var
  lComponent : TComponent;
begin
  lComponent := TComponent(Owner);
  while not (lComponent is TForm) do begin
    lComponent := lComponent.Owner;
  end;
  result := lComponent.Name;
end;

function TtiDateRange.GetMaxDate: TDateTime;
begin
{$IFNDEF FPC}
  Assert(FdtpFrom.MaxDate = FdtpTo.MaxDate,
          'FdtpFrom.MaxDate <> FdtpTo.MaxDate');
  result := FdtpFrom.MaxDate;
{$ENDIF}
end;

function TtiDateRange.GetMinDate: TDateTime;
begin
{$IFNDEF FPC}
  Assert(FdtpFrom.MinDate = FdtpTo.MinDate,
          'FdtpFrom.MinDate <> FdtpTo.MinDate');
  result := FdtpFrom.MinDate;
{$ENDIF}
end;

procedure TtiDateRange.SetMaxDate(const AValue: TDateTime);
begin
{$IFNDEF FPC}
  FdtpFrom.MaxDate := Trunc(AValue);
  FdtpTo.MaxDate  := Trunc(AValue);
{$ENDIF}
end;

procedure TtiDateRange.SetMinDate(const AValue: TDateTime);
var
  lDateTime : TDateTime;
begin
//  Assert(AValue > 0,
//          FloatToStr(AValue) + ' is not a valid TtiDateRange date');
  if AValue <= 0 then
    lDateTime := ctiDateRangeMinDate
  else
    lDateTime := Trunc(AValue);
{$IFNDEF FPC}
  FdtpFrom.MinDate := lDateTime;
  FdtpTo.MinDate  := lDateTime;
{$ENDIF}
end;

procedure TtiDateRange.SetAllowRangeType(const AValue: TAllowRangeType);
begin
  FAllowRangeType := AValue;

  if FAllowRangeType in [artSingleDate] then
    RangeType     := rtSingleDate
  else
    RangeType     := rtDateRange;

  if FAllowRangeType in [artBoth] then begin
    FrbSingleDate.Visible := true;
    FrbDateRange.Visible := true;
  end else begin
    FrbSingleDate.Visible := false;
    FrbDateRange.Visible := false;
  end;

end;

procedure TtiDateRange.ShowInternalState;
begin
{$IFNDEF FPC}
ShowMessage(
  'FdtpFrom.MinDate ' + DateTimeToStr(FdtpFrom.MinDate) + #13 +
  'FdtpFrom.Date    ' + DateTimeToStr(FdtpFrom.Date   ) + #13 +
  'FdtpFrom.MaxDate ' + DateTimeToStr(FdtpFrom.MaxDate) + #13 +
  'FdtpTo.MinDate   ' + DateTimeToStr(FdtpTo.MinDate  ) + #13 +
  'FdtpTo.Date      ' + DateTimeToStr(FdtpTo.Date     ) + #13 +
  'FdtpTo.MaxDate   ' + DateTimeToStr(FdtpTo.MaxDate  )
 );
{$ELSE}
ShowMessage(
  'FdtpFrom.Date    ' + DateTimeToStr(FdtpFrom.Date   ) + #13 +
  'FdtpTo.Date      ' + DateTimeToStr(FdtpTo.Date     ) + #13 +
  'MaxDate          ' + DateTimeToStr(MaxDate     ) + #13 +
  'MinDate          ' + DateTimeToStr(MinDate     ) + #13
 );
{$ENDIF}
end;

{ TrsThisFinancialYear }

procedure TrsThisFinancialYear.Apply(ptiDateRange: TtiDateRange);
var
  liD, liM, liY : Word;
begin
  inherited Apply(ptiDateRange);
  DecodeDate(Date, liY, liM, liD);
  if liM <= 6 then
  begin
    DateFrom := EncodeDate(liY-1, 7, 1);
    DateTo  := EncodeDate(liY, 7, 1) -1;
  end
  else
  begin
    DateFrom := EncodeDate(liY, 7, 1);
    DateTo  := EncodeDate(liY+1, 7, 1) -1;
  end;
end;

constructor TrsThisFinancialYear.Create;
begin
  inherited;
  RangeType := rtDateRange;
  Caption  := 'This financial year';
end;

{ TrsLastFinancialYear }

procedure TrsLastFinancialYear.Apply(ptiDateRange: TtiDateRange);
var
  liD, liM, liY : Word;
begin
  inherited Apply(ptiDateRange);
  DecodeDate(Date, liY, liM, liD);
  if liM <= 6 then
  begin
    DateFrom := EncodeDate(liY-2, 7, 1);
    DateTo  := EncodeDate(liY-1, 7, 1) -1;
  end
  else
  begin
    DateFrom := EncodeDate(liY-1, 7, 1);
    DateTo  := EncodeDate(liY, 7, 1) -1;
  end;
end;

constructor TrsLastFinancialYear.Create;
begin
  inherited;
  RangeType := rtDateRange;
  Caption  := 'Last financial year';
end;

procedure TtiDateRange.SetOptions(const AValue: TtiDateRangeOptions);
begin
  FOptions := AValue;
  FchbSave.Visible := (droptShowRememberNextTime in FOptions);
end;

function TtiDateRange.GetRangeSetterIndex: Integer;
begin
  Result:= FcbQuick.ItemIndex;
end;

procedure TtiDateRange.SetRangeSetterIndex(const AValue: Integer);
begin
  if AValue < FcbQuick.Items.Count then
    FcbQuick.ItemIndex:= AValue
  else
    FcbQuick.ItemIndex:= 0;
  DoOnChangeQuick(nil);
end;
end.

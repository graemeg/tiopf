{$I tiDefines.inc}

unit tiMaskEdit;

interface
uses
  tiPerAwareCtrls
  ,StdCtrls
  ,Classes
{$IFNDEF FPC}
  ,MaskUtils
{$ELSE}
  ,MaskEdit
{$ENDIF}

 ;

type

  {$IFDEF FPC} TEditMask = string;{$ENDIF}
  // A wrapper for the TMaskEdit control
  TtiPerAwareMaskEdit = class(TtiPerAwareAbs)
  private
    function  GetValue: String;
    procedure SetValue(const AValue: String);
    function  GetMaxLength: integer;
    procedure SetMaxLength(const AValue: integer);
    function  GetCharCase: TEditCharCase;
    procedure SetCharCase(const AValue: TEditCharCase);
    function  GetPasswordChar: Char;
    procedure SetPasswordChar(const AValue: Char);
    function  GetEditMask: TEditMask;
    procedure SetEditMask(const AValue: TEditMask);
    function  GetEditText: string;
    procedure SetEditText(const AValue: string);
    function  GetIsMasked: Boolean;
  protected
    procedure   DataToWinControl; override;
    procedure   WinControlToData; override;
    procedure   SetOnChangeActive(AValue : boolean); override;
    procedure   SetReadOnly(const AValue: Boolean);override;
  published
    property AValue : String read GetValue write SetValue;
    property MaxLength : integer read GetMaxLength write SetMaxLength;
    property CharCase : TEditCharCase read GetCharCase write SetCharCase;
    property PasswordChar : Char read GetPasswordChar write SetPasswordChar;
    property EditMask : TEditMask read GetEditMask write SetEditMask;
    property EditText : string read GetEditText write SetEditText;
    property IsMasked : Boolean read GetIsMasked;
    property OnKeyPress;
    property OnKeyDown;
  public
    constructor Create(AOwner : TComponent); override;
  end;


implementation
uses
{$IFNDEF FPC}
  Mask,
{$ENDIF}
  TypInfo
 ;

{ TtiPerAwareMaskEdit }

constructor TtiPerAwareMaskEdit.Create(AOwner: TComponent);
begin
  FWinControl := TMaskEdit.Create(self);
  TEdit(FWinControl).OnChange  := DoChange;
  TEdit(FWinControl).OnKeyPress := DoOnKeyPress;
  TEdit(FWinControl).OnKeyDown := DoOnKeyDown;
  FbCenterWhenLabelIsLeft := true;
  inherited;
  TMaskEdit(FWinControl).Font.Name := cDefaultFixedFontName;
  Height := cDefaultHeightSingleRow;
end;

procedure TtiPerAwareMaskEdit.DataToWinControl;
begin
  if not DataAndPropertyValid then
    Exit; //==>
  SetOnChangeActive(false);
  TMaskEdit(FWinControl).Text := GetPropValue(FData, FsFieldName);
  SetOnChangeActive(true);
end;

function TtiPerAwareMaskEdit.GetCharCase: TEditCharCase;
begin
  result := TMaskEdit(FWinControl).CharCase;
end;

function TtiPerAwareMaskEdit.GetEditMask: TEditMask;
begin
  result := TMaskEdit(FWinControl).EditMask;
end;

function TtiPerAwareMaskEdit.GetEditText: string;
begin
  result := TMaskEdit(FWinControl).EditText;
end;

function TtiPerAwareMaskEdit.GetIsMasked: Boolean;
begin
  result := TMaskEdit(FWinControl).IsMasked;
end;

function TtiPerAwareMaskEdit.GetMaxLength: integer;
begin
  result := TMaskEdit(FWinControl).MaxLength;
end;

function TtiPerAwareMaskEdit.GetPasswordChar: Char;
begin
  result := TMaskEdit(FWinControl).PasswordChar;
end;

function TtiPerAwareMaskEdit.GetValue: String;
begin
  result := TMaskEdit(FWinControl).Text;
end;

procedure TtiPerAwareMaskEdit.SetCharCase(const AValue: TEditCharCase);
begin
  TMaskEdit(FWinControl).CharCase := AValue;
end;

procedure TtiPerAwareMaskEdit.SetEditMask(const AValue: TEditMask);
begin
  TMaskEdit(FWinControl).EditMask := AValue;
end;

procedure TtiPerAwareMaskEdit.SetEditText(const AValue: string);
begin
  TMaskEdit(FWinControl).EditText := AValue;
end;

procedure TtiPerAwareMaskEdit.SetMaxLength(const AValue: integer);
begin
  TMaskEdit(FWinControl).MaxLength := AValue;
end;

procedure TtiPerAwareMaskEdit.SetOnChangeActive(AValue: boolean);
begin
  if AValue then
    TMaskEdit(FWinControl).OnChange := DoChange
  else
    TMaskEdit(FWinControl).OnChange := nil;
end;

procedure TtiPerAwareMaskEdit.SetPasswordChar(const AValue: Char);
begin
  TMaskEdit(FWinControl).PasswordChar := AValue;
end;

procedure TtiPerAwareMaskEdit.SetReadOnly(const AValue: Boolean);
begin
  inherited SetReadOnly(AValue);
  TMaskEdit(FWinControl).ReadOnly := AValue;
end;

procedure TtiPerAwareMaskEdit.SetValue(const AValue: String);
begin
  SetOnChangeActive(false);
  TMaskEdit(FWinControl).Text := AValue;
  WinControlToData;
  SetOnChangeActive(true);
end;

procedure TtiPerAwareMaskEdit.WinControlToData;
begin
  if not DataAndPropertyValid then
    Exit; //==>
  if ReadOnly then
    Exit; //==>
  SetPropValue(FData, FsFieldName, TMaskEdit(FWinControl).Text);
end;

end.

unit tiTestSetup;

{$i tiDefines.inc}

interface
uses
  tiBaseObject,
  tiQuery,
  tiTestFramework,
  TestFramework,
  Classes;

type

  TtiTestSetup = class( TtiBaseObject )
  private
    FTestCase: TtiTestCase;
  protected
    property    TC : TtiTestCase read FTestCase;
  public
    constructor Create(const ATestCase : TtiTestCase); virtual ;
    destructor  Destroy; override;

    function    tvToStr(const AValue: string; const AInc: integer = 0): string;
    function    tvToDateTime( const AValue: string; const AInc: integer = 0): TDateTime;
    function    tvToDate(     const AValue: string; const AInc: integer = 0): TDateTime;
    function    tvToInt(      const AValue: string; const AInc: integer = 0): Int64;
    function    tvToIntWithLimit(const AValue: string; const ALimit: integer): Int64;
    function    tvToBoolean(  const AValue: string): Boolean;
    function    tvToFloat(    const AValue: string): Real;
    function    tvToStatus(   const AValue: string): byte;
    procedure   tvToStream(   const AStream: TStream; const AValue: string);

  end;


implementation
uses
  SysUtils
  ,tiConstants
  ,tiUtils
  ;

function TtiTestSetup.tvToBoolean(const AValue: string): Boolean;
begin
  result := ( tvToInt(AValue) mod 2 ) = 0 ;
end;

function TtiTestSetup.tvToDateTime(const AValue: string; const AInc: integer = 0): TDateTime;
begin
  result := Now + tvToInt(AValue, AInc);
end;

function TtiTestSetup.tvToFloat(const AValue: string): Real;
var
  lInt : Integer ;
begin
  lInt := tvToInt(AValue);
  Result := lInt + Frac(lInt / 10) + Frac(lInt / 100);
end;

function TtiTestSetup.tvToInt(const AValue: string; const AInc: integer = 0): Int64;
begin
  Result := StrToInt64(AValue) + AInc;
end;

function TtiTestSetup.tvToIntWithLimit(const AValue: string;
  const ALimit: integer): Int64;
begin
  if ALimit = 0 then
    Result := StrToInt64(AValue)
  else
    Result := StrToInt64(tiPad0(AValue, ALimit));
end;

function TtiTestSetup.tvToStatus(const AValue: string): byte;
begin
  result:= Ord(StrToInt(AValue) mod 2 = 0);
end;

function TtiTestSetup.tvToStr(const AValue: string;
  const AInc: integer): string;
begin
  if AInc = 0 then
    result:= AValue
  else
    result:= AValue + IntToStr(AInc);
end;

procedure TtiTestSetup.tvToStream(
  const AStream: TStream;
  const AValue: string);
var
  L: string;
begin
  L:= tiReplicate(AValue, 1000);
  tiStringToStream(L, AStream);
end;

function TtiTestSetup.tvToDate(const AValue: string; const AInc: integer = 0): TDateTime;
begin
  result := Date + tvToInt(AValue, AInc);
end;

constructor TtiTestSetup.Create(const ATestCase: TtiTestCase);
begin
  inherited Create;
  Assert(ATestCase <> nil, 'TestCase not assigned');
  FTestCase := ATestCase;
end;

destructor TtiTestSetup.Destroy;
begin
  inherited;
end;

end.


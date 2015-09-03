unit demoUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

  // Helpers
  function  CurrRounding(const AValue: Currency): Currency;

  // Display and formatting helpers
  function  CurrToInt(const AValue: Currency): Integer;
  function  IntToCurr(const AValue: Integer): Currency;
  function  DisplayDate(const AValue: TDateTime): string;
  function  DisplayTime(const AValue: TDateTime): string;
  function  DisplayCurrency(const AValue: Currency): string;
  function  DisplayPercentage(const AValue: Double): string;


implementation

uses
  Math;

const
  cDate = 'dd MMM yyyy';
  cTime = 'HH:mm';

function CurrRounding(const AValue: Currency): Currency;
var
  v: Currency;
begin
  v := AValue * 100.0;
  v := Ceil(v);
  v := v / 100;
  Result := v;
end;

function CurrToInt(const AValue: Currency): Integer;
begin
  Result := Trunc(CurrRounding(AValue) * 100);
end;

function IntToCurr(const AValue: Integer): Currency;
begin
  Result := (AValue / 100);
end;

function DisplayDate(const AValue: TDateTime): string;
begin
  Result := FormatDateTime(cDate, AValue);
end;

function DisplayTime(const AValue: TDateTime): string;
begin
  Result := FormatDateTime(cTime, AValue);
end;

function DisplayCurrency(const AValue: Currency): string;
begin
  if AValue < 0.0 then
    Result := Format('(%.2f)', [Abs(AValue)])
  else
    Result := Format('%.2f', [AValue]);
end;

function DisplayPercentage(const AValue: Double): string;
begin
  Result := Format('%.2f', [AValue]);
end;

end.


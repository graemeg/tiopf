unit tiSemaphore;

{$IFDEF fpc}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, SyncObjs;

type
  TtiSemaphore = class(TObject)
  private
    FCount: LongWord;
    FMaximumCount: LongWord;
    FTimeout: LongWord;
    FCriticalSection: TCriticalSection;
    procedure   SetTimeout(AValue: LongWord);
  public
    constructor Create(const AMaximumCount: LongWord);
    destructor  Destroy; override;
    function    Acquire: Boolean;
    procedure   Release;
    property    Timeout: LongWord read FTimeout write SetTimeout;
    property    MaximumCount: LongWord read FMaximumCount;
    property    Count: LongWord read FCount;
  end;

implementation

{ TtiSemaphore }

procedure TtiSemaphore.SetTimeout(AValue: LongWord);
begin
  if FTimeout = AValue then
    Exit;
  FTimeout := AValue;
end;

constructor TtiSemaphore.Create(const AMaximumCount: LongWord);
begin
  inherited Create;
  FCriticalSection := TCriticalSection.Create;
  FMaximumCount := AMaximumCount;
  FCount := AMaximumCount;
  FTimeout := 0;
end;

destructor TtiSemaphore.Destroy;
begin
  FCriticalSection.Free;
  inherited Destroy;
end;

function TtiSemaphore.Acquire: Boolean;
var
  dt: TDateTime;
begin
  Result := False;
  if FCount > 0 then
  begin
    FCriticalSection.Enter;
    if FCount > 0 then
      FCount := FCount - 1;
    FCriticalSection.Leave;
    Result := True;
  end
  else
  begin
    if (Timeout > 0) and (Timeout < INFINITE) then
    begin
      dt := Now;
      while (Result = False) or (Now < (dt+Timeout)) do
      begin
        FCriticalSection.Enter;
        if FCount > 0 then
        begin
          FCount := FCount -1;
          Result := True;
        end;
        FCriticalSection.Leave;
      end;
    end
    else if (Timeout = INFINITE) then
    begin
      while (Result = False) do
      begin
        FCriticalSection.Enter;
        if FCount > 0 then
        begin
          FCount := FCount -1;
          Result := True;
        end;
        FCriticalSection.Leave;
      end;
    end;
  end;
end;

procedure TtiSemaphore.Release;
begin
  FCriticalSection.Enter;
  if FCount < FMaximumCount then
    FCount := FCount + 1;
  FCriticalSection.Leave;
end;

end.


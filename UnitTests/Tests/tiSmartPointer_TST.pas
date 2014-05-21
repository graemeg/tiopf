unit tiSmartPointer_TST;

interface

uses
  tiTestFramework;

type
  TTestTiGarbageCollector = class(TtiTestCase)
  published
    procedure TestAdd;
    procedure TestRemove;
  end;

procedure RegisterTests;

implementation

uses
  tiTestDependencies,
  tiSmartPointer,
  Classes,
  SysUtils;

var
  uLog: TStrings;

function Log: TStrings;
begin
  if not Assigned(uLog) then
    uLog := TStringList.Create;
  Result := uLog;
end;

type
  TCollected = class
  private
    FID: Char;
  public
    constructor Create(const AID: Char = Char(0));
    destructor Destroy; override;
  end;


procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TTestTiGarbageCollector);
end;

{ TTestTiGarbageCollector }

const
{$IFDEF MSWINDOWS}
  NL = #13#10;
{$ELSE}
  //TODO: Distinguish between FPC on Windows and *nix targets?
  NL = #10;
{$ENDIF}

procedure TTestTiGarbageCollector.TestAdd;
var
  GC: ItiGC;
  A, B: TCollected;
begin
  Log.Clear;
  GC := CreateGC;
  CheckEquals('', Log.Text, 'log empty initially');
  Check(GC.Add(A, TCollected.Create('A')), 'add A');
  Check(GC.Add(B, TCollected.Create('B')), 'add B');
  CheckFalse(GC.Add(A), 'add A again');
  GC := nil;
  CheckEquals('Destroying B' + NL + 'Destroying A' + NL, Log.Text, 'two additions');
end;

procedure TTestTiGarbageCollector.TestRemove;
var
  GC: ItiGC;
  A, B, C: TCollected;
begin
  Log.Clear;
  GC := CreateGC;
  CheckEquals('', Log.Text, 'log empty initially');
  Check(GC.Add(A, TCollected.Create('A')), 'add A');
  Check(GC.Remove(A), 'remove A');
  CheckFalse(GC.Remove(A), 'remove A again');
  GC := nil;
  CheckEquals('', Log.Text, 'empty - A removed');
  FreeAndNil(A);

  Log.Clear;
  GC := CreateGC;
  Check(GC.Add(A, TCollected.Create('A')), 'add A');
  Check(GC.Add(B, TCollected.Create('B')), 'add B');
  Check(GC.Add(C, TCollected.Create('C')), 'add C');
  Check(GC.Remove(B), 'remove B');
  GC := nil;
  CheckEquals('Destroying C' + NL + 'Destroying A' + NL, Log.Text, 'removed B (middle) from A,B,C');
  FreeAndNil(B);

  Log.Clear;
  GC := CreateGC;
  Check(GC.Add(A, TCollected.Create('A')), 'add A');
  Check(GC.Add(B, TCollected.Create('B')), 'add B');
  Check(GC.Add(C, TCollected.Create('C')), 'add C');
  Check(GC.Remove(A), 'remove A');
  GC := nil;
  CheckEquals('Destroying C' + NL + 'Destroying B' + NL, Log.Text, 'removed A (head) from A,B,C');
  FreeAndNil(A);

  Log.Clear;
  GC := CreateGC;
  Check(GC.Add(A, TCollected.Create('A')), 'add A');
  Check(GC.Add(B, TCollected.Create('B')), 'add B');
  Check(GC.Add(C, TCollected.Create('C')), 'add C');
  Check(GC.Remove(C), 'remove C');
  GC := nil;
  CheckEquals('Destroying B'+ NL + 'Destroying A' + NL, Log.Text, 'removed C (tail) from A,B,C');
  FreeAndNil(C);
end;

{ TCollected }

constructor TCollected.Create(const AID: Char);
begin
  FID := AID;
end;

destructor TCollected.Destroy;
begin
  Log.Add(Format('Destroying %s', [FID]));
  inherited;
end;

initialization
finalization


  FreeAndNil(uLog);

end.

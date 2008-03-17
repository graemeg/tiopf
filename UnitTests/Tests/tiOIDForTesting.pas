unit tiOIDForTesting;

{$I tiDefines.inc}

interface

uses
  tiOID,
  tiBaseObject;

type

  TOIDForTesting = class(TOIDStringAbs)
  end;

  TtiOIDGeneratorForTesting = class(TtiOIDGenerator)
  public
    class function OIDClass: TtiOIDClass; override;
    procedure AssignNextOID(const AAssignTo: TtiOID; const ADatabaseAliasName: string = '';
      const APersistenceLayerName: string = ''); override;
  end;

implementation
uses
  tiConstants,
  SysUtils,
  SyncObjs;

var
  UNextOID: Cardinal = 0;
  UCritSect: TCriticalSection;

{ TtiOIDGeneratorForTesting }

procedure TtiOIDGeneratorForTesting.AssignNextOID(const AAssignTo: TtiOID; const ADatabaseAliasName: string = '';
  const APersistenceLayerName: string = '');
begin
  Assert(AAssignTo.TestValid(TtiOID), CTIErrorInvalidObject);
  UCritSect.Enter;
  try
    Inc(UNextOID);
    AAssignTo.AsString := IntToStr(UNextOID);
  finally
    UCritSect.Leave;
  end;
end;

class function TtiOIDGeneratorForTesting.OIDClass: TtiOIDClass;
begin
  Result := TOIDForTesting;
end;

initialization
  UCritSect:= TCriticalSection.Create;

finalization
  UCritSect.Free;


end.

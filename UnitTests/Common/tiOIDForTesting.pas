unit tiOIDForTesting;

{$I tiDefines.inc}

interface

uses
  tiOID;

type

{$IFNDEF OID_AS_INT64}
  TOIDForTesting = class(TOIDStringAbs);

  TtiOIDGeneratorForTesting = class(TtiOIDGenerator)
  public
    class function OIDClass: TtiOIDClass; override;
    procedure AssignNextOID(const AAssignTo: TtiOID; const ADatabaseAliasName: string = '';
      const APersistenceLayerName: string = ''); override;
  end;
{$ELSE}

  TOIDForTesting = TtiOID;

  TtiOIDGeneratorForTesting = class(TtiOIDAsInt64Generator)
  public
//    class function OIDClass: TtiOIDClass; override;
    function NextOID: TtiOID; override;
  end;
{$ENDIF}

implementation
uses
  tiConstants
  ,SysUtils
  ,SyncObjs
 ;

var
  UNextOID: Cardinal = 0;
  UCritSect: TCriticalSection;

{ TtiOIDGeneratorForTesting }

{$IFNDEF OID_AS_INT64}
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

{$ELSE}
function TtiOIDGeneratorForTesting.NextOID: TtiOID;
begin
  UCritSect.Enter;
  try
    Inc(UNextOID);
    Result := UNextOID;
  finally
    UCritSect.Leave;
  end;
end;
{$ENDIF}

initialization
  UCritSect:= TCriticalSection.Create;

finalization
  UCritSect.Free;


end.

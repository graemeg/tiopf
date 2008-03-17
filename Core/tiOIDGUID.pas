unit tiOIDGUID;

{
  Usage:
  Assign the TIOPFManager's Default OID Generator property like this:

  GTIOPFManager.DefaultOIDGenerator := TtiOIDGeneratorGUID.Create;
}

{$I tiDefines.inc}

interface

uses
  tiOID,
  tiBaseObject;

type

  TOIDGUID = class(TOIDStringAbs)
  end;

  TtiOIDGeneratorGUID = class(TtiOIDGenerator)
  public
    class function OIDClass: TtiOIDClass; override;
    procedure AssignNextOID(const AAssignTo: TtiOID; const ADatabaseAliasName: string = '';
      const APersistenceLayerName: string = ''); override;
  end;

implementation

uses
  tiQuery,
  tiOPFManager,
  tiConstants,
  tiUtils,
  SysUtils;

{ TOIDGUID }

{ TtiOIDGeneratorGUID }

procedure TtiOIDGeneratorGUID.AssignNextOID(const AAssignTo: TtiOID; const ADatabaseAliasName: string = '';
  const APersistenceLayerName: string = '');
var
  LValue: string;
const
  cGUIDLength = 38;
begin
  Assert(AAssignTo.TestValid(TtiOID), CTIErrorInvalidObject);
  LValue := tiCreateGUIDString;
  //           10        20        30
  // 01234567890123456789012345678901234567
  // {81A9C48C-DEF3-11D6-81C4-0002E31296EB}
  // A GUID will be 38 chars long when created,
  // or 36 chars long when {} are removed.
  if (LValue[1] = '{') and
    (LValue[cGUIDLength] = '}') then
    LValue := Copy(lValue, 2, cGUIDLength - 2);
  AAssignTo.AsString := LValue;
end;

class function TtiOIDGeneratorGUID.OIDClass: TtiOIDClass;
begin
  Result := TOIDGUID;
end;

end.

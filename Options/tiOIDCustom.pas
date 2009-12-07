unit tiOIDCustom;
{
  TtiOIDGeneratorCustom is designed to defer creation of a string based OID to a delegate
  object using the Visitor pattern built into tiOPF.

  In order to use the TtiOIDGeneratorCustom object, you simply create a visitor and
  register it with the Visitor Manager using the group name "tioidcustom".  The visitor
  that you create, must use whatever logic you deem necessary to create a unique
  string based OID.

  As you can see from the source below, TtiOIDGeneratorCustom creates a
  TtiOIDStringReceiver and executes whatever visitor is available and registered
  with the groupname "tioidcustom" against it.  It then uses the value assigned to
  TtiOIDStringReceiver in the AssignNextOID overridden method to return the
  generated OID.

  Usage:
  Assign the TIOPFManager's Default OID Generator property like this:

  GTIOPFManager.DefaultOIDGenerator := TtiOIDGeneratorCustom.Create;

}

{$I tiDefines.inc}

interface

uses
  tiOID
  ,tiObject
  ;


type

  TOIDCustom = class(TOIDStringAbs)
  end;

  {: Used by TtiOIDGeneratorCustom to receive a new string based OID from a visitor object
     which is registered with group name of "tioidcustom". }
  TtiOIDStringReceiver = class(TtiObject)
  private
    FNewOIDValue: string;
    procedure SetNewOIDValue(const Value: string);
  published
    {: Property to hold the new string based oid assigned from the visitor. }
    property    NewOIDValue: string read FNewOIDValue write SetNewOIDValue;
  end;

  {: TtiOIDGeneratorCustom.  Used to generate custom OID's by delegating responsibility of
     creating a new string based OID to an outside visitor. }
  TtiOIDGeneratorCustom = class(TtiOIDGenerator)
  private
    FReceiver: TtiOIDStringReceiver;
  public
    class function OIDClass: TtiOIDClass; override;
    procedure   AssignNextOID(const AAssignTo: TtiOID; const ADatabaseAliasName: string = '';
      const APersistenceLayerName: string = ''); override;
    // ---> Construction Ahead
    constructor Create; override;
    destructor  Destroy; override;
  end;

implementation

uses
   tiBaseObject
  ,tiConstants
  ,tiUtils
  ,SysUtils
  ,tiOPFManager

 ;

{ TtiOIDStringReceiver }

procedure TtiOIDStringReceiver.SetNewOIDValue(const Value: string);
begin
  FNewOIDValue := Value;
end;

{ TtiOIDCustomGeneratorCustom }

procedure TtiOIDGeneratorCustom.AssignNextOID(const AAssignTo: TtiOID;
  const ADatabaseAliasName, APersistenceLayerName: string);
begin
  // Reset receiver's string property
  FReceiver.NewOIDValue := '';

  GTIOPFManager.VisitorManager.Execute('tioidcustom', FReceiver);

  // Check to see if the visitor successfully populated receiver with a value.
  Assert(FReceiver.NewOIDValue <> '', ClassName + '.AssignNextOID: ' +
    'FReceiver.NewOIDValue was not set by visitor.');

  // Finally pass the value back.
  AAssignTo.AsString := FReceiver.NewOIDValue;

end;

constructor TtiOIDGeneratorCustom.Create;
begin
  inherited;
  FReceiver := TtiOIDStringReceiver.Create;
end;

destructor TtiOIDGeneratorCustom.Destroy;
begin
  FReceiver.Free;
  inherited;
end;

class function TtiOIDGeneratorCustom.OIDClass: TtiOIDClass;
begin
  Result := TOIDCustom;
end;

end.

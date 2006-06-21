{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

    This file is part of the PBPro Pawnbroking System
    Copyright (c) 2003 Eventide Systems Pty Ltd

    The PBPro Pawnbroking System is free software; you can redistribute it
    and/or modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2 of
    the License, or (at your option) any later version.

    The PBPro Pawnbroking System is distributed in the hope that it will be
    useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with the PBPro Pawnbroking System; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit pbpPersistence;

interface

uses
  SysUtils, Dialogs,

  jclStrings,

  tiPersist, tiPtnVisPerObj, tiPtnVisSQL, tiQuery, tiPtnVis, TIUtils, tiLog,
  tiPerObjOIDAbs, tiPerObjOIDInteger,

  pbpBusinessClasses;

type
  // DefaultValues visitor <<template>>
  TVisDefaultValues = class(TVisitorAbs)
  protected
    function GetVisited: TPerObjAbs; reintroduce;
    procedure ReadData; virtual; abstract;
    procedure SetVisited(const Value: TPerObjAbs); reintroduce;
  public
    procedure Execute(const pVisited: TVisitedAbs); override;
    property Visited: TPerObjAbs read GetVisited write SetVisited;
  end;

  // TClient visitors
  TVisClient_Create = class(TVisOwnedQryUpdate)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure SetupParams; override;
  end;

  TVisClient_DefaultValues = class(TVisDefaultValues)
  protected
    function AcceptVisitor: boolean; override;
    procedure ReadData; override;
  end;

  TVisClient_Read = class(TVisOwnedQrySelect)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure MapRowToObject; override;
    procedure SetupParams; override;
  end;

  TVisClient_Refresh = class(TVisOwnedQrySelect)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure MapRowToObject; override;
    procedure SetupParams; override;
  end;

  TVisClientAndContract_ReadPK = class(TVisOwnedQrySelect)
  private
    FLastClient: TClient;
    procedure MapRowToClient;
    procedure MapRowToContract;
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure MapRowToObject; override;
    procedure SetupParams; override;
  end;

  TVisClient_Update = class(TVisOwnedQryUpdate)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure SetupParams; override;
  end;

  // ClientAddress visitors
  TVisClientAddress_Create = class(TVisOwnedQryUpdate)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure SetupParams; override;
  end;

  TVisClientAddress_Read = class(TVisOwnedQrySelect)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure MapRowToObject; override;
    procedure SetupParams; override;
  end;

  TVisClientAddress_Read_InUse = class(TVisOwnedQrySelect)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure MapRowToObject; override;
    procedure SetupParams; override;
  end;

  TVisClientAddress_Refresh = class(TVisOwnedQrySelect)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure MapRowToObject; override;
    procedure SetupParams; override;
  end;

  TVisClientAddress_Refresh_RemoveUnusedObjects = class(TVisPerObjAwareAbs)
  protected
    function AcceptVisitor: boolean; override;
  public
    procedure Execute(const pVisited : TVisitedAbs ); override;
  end;

  TVisClientAddress_Update = class(TVisOwnedQryUpdate)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure SetupParams; override;
  end;

  TVisClientAddress_Delete = class(TVisOwnedQryUpdate)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure SetupParams; override;
  end;

  // TClientIdentityRecord visitors
  TVisClientIdentityRecord_Create = class(TVisOwnedQryUpdate)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure SetupParams; override;
  end;

  TVisClientIdentityRecord_Read = class(TVisOwnedQrySelect)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure MapRowToObject; override;
    procedure SetupParams; override;
  end;

  TVisClientIdentityRecord_Read_InUse = class(TVisOwnedQrySelect)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure MapRowToObject; override;
    procedure SetupParams; override;
  end;

  TVisClientIdentityRecord_Refresh = class(TVisOwnedQrySelect)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure MapRowToObject; override;
    procedure SetupParams; override;
  end;

  TVisClientIdentityRecord_Refresh_RemoveUnusedObjects = class(TVisPerObjAwareAbs)
  protected
    function AcceptVisitor: boolean; override;
  public  
    procedure Execute(const pVisited : TVisitedAbs ); override;
  end;

  TVisClientIdentityRecord_Update = class(TVisOwnedQryUpdate)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure SetupParams; override;
  end;

  // TClientIdentityRecordType visitors
  TVisClientIdentityRecordType_Create = class(TVisOwnedQryUpdate)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure SetupParams; override;
  end;

  TVisClientIdentityRecordType_Read = class(TVisOwnedQrySelect)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure MapRowToObject; override;
    procedure SetupParams; override;
  end;

  // TContract visitors
  TVisContract_Create = class(TVisOwnedQryUpdate)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure SetupParams; override;
  end;

  TVisContractDefaultValues = class(TVisDefaultValues)
  protected
    function AcceptVisitor: boolean; override;
    procedure ReadData; override;
  end;

  TVisContract_ReadPK = class(TVisOwnedQrySelect)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure MapRowToObject; override;
    procedure SetupParams; override;
  end;

  TVisContract_Read = class(TVisOwnedQrySelect)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure MapRowToObject; override;
    procedure SetupParams; override;
  end;

//  TVisContract_Read = class(TVisPerObjAwareAbs)
//  protected
//    function AcceptVisitor: boolean; override;
//  public
//    procedure Execute(const pVisited: TVisitedAbs); override;
//  end;

  TVisContract_ReadClient = class(TVisPerObjAwareAbs)
  protected
    function AcceptVisitor: boolean; override;
  public
    procedure Execute(const pVisited: TVisitedAbs); override;
  end;

//  TVisContract_ReadPKPK = class(TVisOwnedQrySelect)
//  protected
//    function AcceptVisitor: boolean; override;
//    procedure Init; override;
//    procedure MapRowToObject; override;
//    procedure SetupParams; override;
//  end;

  TVisContract_Update = class(TVisOwnedQryUpdate)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure SetupParams; override;
  end;

  // ContractClientIdentityRecordProxy visitors

  TVisContractClientIdentityRecordProxy_Create = class(TVisOwnedQryUpdate)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure SetupParams; override;
  end;

  TVisContractClientIdentityRecordProxy_Delete = class(TVisOwnedQryUpdate)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure SetupParams; override;
  end;

  TVisContractClientIdentityRecordProxy_Read = class(TVisOwnedQrySelect)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure MapRowToObject; override;
    procedure SetupParams; override;
  end;

  // ContractItemCategory visitors
  TVisContractItemCategory_Create = class(TVisOwnedQryUpdate)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure SetupParams; override;
  end;

  TVisContractItemCategory_Read = class(TVisOwnedQrySelect)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure MapRowToObject; override;
    procedure SetupParams; override;
  end;

  // ContractItem visitors
  TVisContractItem_Create = class(TVisOwnedQryUpdate)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure SetupParams; override;
  end;

  TVisContractItem_Delete = class(TVisOwnedQryUpdate)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure SetupParams; override;
  end;

  TVisContractItem_Read = class(TVisOwnedQrySelect)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure MapRowToObject; override;
    procedure SetupParams; override;
  end;

  TVisContractItem_Update = class(TVisOwnedQryUpdate)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure SetupParams; override;
  end;

  // ContractPaymentType visitor
  TVisContractPaymentType_Create = class(TVisOwnedQryUpdate)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure SetupParams; override;
  end;

  TVisContractPaymentType_Read = class(TVisOwnedQrySelect)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure MapRowToObject; override;
    procedure SetupParams; override;
  end;

{ TODO -oTT -cRemove :
The DefaultPaymentType feature is already part of the
SystemValues object }
//  TVisContractPaymentTypeList_Read = class(TVisOwnedQrySelect)
//  protected
//    function AcceptVisitor: boolean; override;
//    procedure Init; override;
//    procedure MapRowToObject; override;
//    procedure SetupParams; override;
//  end;

  // ContractTransaction visitors
  TVisContractTransaction_Create = class(TVisOwnedQryUpdate)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure SetupParams; override;
  end;

  TVisContractTransaction_Delete = class(TVisOwnedQryUpdate)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure SetupParams; override;
  end;

  TVisContractTransaction_Read = class(TVisOwnedQrySelect)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure MapRowToObject; override;
    procedure SetupParams; override;
  end;

  TVisContractTransaction_Update = class(TVisOwnedQryUpdate)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure SetupParams; override;
  end;

  // Manufacturer visitors
  TVisManufacturer_Create = class(TVisOwnedQryUpdate)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure SetupParams; override;
  end;

  TVisManufacturer_Read = class(TVisOwnedQrySelect)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure MapRowToObject; override;
    procedure SetupParams; override;
  end;

  // NextClientNumber visitors
  TVisNextClientNumberRead = class(TVisOwnedQrySelect)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure MapRowToObject; override;
    procedure SetupParams; override;
  end;

  TVisNextClientNumberUpdate = class(TVisOwnedQryUpdate)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure SetupParams; override;
  end;

  // NextContractNumber visitors
  TVisNextContractNumberRead = class(TVisOwnedQrySelect)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure MapRowToObject; override;
    procedure SetupParams; override;
  end;

  TVisNextContractNumberUpdate = class(TVisOwnedQryUpdate)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure SetupParams; override;
  end;

  // TPawnbroker visitors
  TVisPawnBrokerRead = class(TVisOwnedQrySelect)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure MapRowToObject; override;
    procedure SetupParams; override;
  end;

  // TSystemValues visitors
  TVisSystemValues_Read = class(TVisOwnedQrySelect)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure MapRowToObject; override;
    procedure SetupParams; override;
  end;

  TVisSystemValues_Update = class(TVisOwnedQryUpdate)
  protected
    function AcceptVisitor: boolean; override;
    procedure Init; override;
    procedure SetupParams; override;
  end;

  // TUndesirableClientType visitors
  procedure LoadPersistenceFramework;
  procedure UnloadPersistenceFramework;

  procedure RegisterVisitors;

var
  gDatabaseName: string;

implementation

procedure LoadPersistenceFramework;
var
  RootPath: string;
  UserName: string;
  UserPassword: string;
begin
{ TODO -oPBPRO -cSMELL : The application expects the data base to be at the relative position <root>\data. }
  RootPath := ExtractFileDir(ParamStr(0));
  RootPath := Copy(RootPath, 1, StrLastPos('\', RootPath)-1);

  gDatabaseName := RootPath + '\data\pbpro.gdb';
  UserName := 'SYSDBA';
  UserPassword := 'masterkey';

  gTIPerMgr.DefaultDBConnectionName := gDatabaseName;
  gTIPerMgr.DBConnectionPools.Connect(gTIPerMgr.DefaultDBConnectionName, UserName, UserPassword);

//  gTIPerMgr.LoadPersistenceFramework(PackageID, DatabaseName, UserName, UserPassword);
{ TODO -oTT -cRequired : Write code to read persistence information (from ini file, from registry?) }
end;

procedure UnloadPersistenceFramework;
begin
  gTIPerMgr.Terminate ;
  gTIPerMgr.UnLoadDatabaseLayer( '', gTIPerMgr.DefaultDBConnectionName ) ;

//  gTIPerMgr.UnloadPersistenceFramework;
{ TODO -oTT -cRequired : 
Write code to unload persistence layer, perform any application 
specific clean up. }
end;

procedure RegisterVisitors;
begin
  // NextClientRead visitors
  gTIPerMgr.VisMgr.RegisterVisitor('read.defaultValues', TVisNextClientNumberRead);
  gTIPerMgr.VisMgr.RegisterVisitor('read.defaultValues', TVisNextClientNumberUpdate);

  // NextContractRead visitors
  gTIPerMgr.VisMgr.RegisterVisitor('read.defaultValues', TVisNextContractNumberRead);
  gTIPerMgr.VisMgr.RegisterVisitor('read.defaultValues', TVisNextContractNumberUpdate);

  // PawnBroker visitors
{ TODO -oTT -cIncomplete : Update visitor required }
  gTIPerMgr.RegReadVisitor(TVisPawnBrokerRead);

  // Client visitors
  gTIPerMgr.VisMgr.RegisterVisitor('read.defaultValues', TVisClient_DefaultValues);
  gTIPerMgr.VisMgr.RegisterVisitor('read.refresh', TVisClient_Refresh);
  gTIPerMgr.RegReadVisitor(TVisClient_Read);
  gTIPerMgr.RegReadPKVisitor(TVisClientAndContract_ReadPK);
{ TODO -oTT:2002-07-20 -cIncomplete : Delete visitor required }
  gTIPerMgr.RegSaveVisitor(TVisClient_Create);
  gTIPerMgr.RegSaveVisitor(TVisClient_Update);

  // ClientAddress visitors
  gTIPerMgr.RegSaveVisitor(TVisClientAddress_Create);
  gTIPerMgr.RegReadVisitor(TVisClientAddress_Read);
  gTIPerMgr.RegReadVisitor(TVisClientAddress_Read_InUse);
  gTIPerMgr.RegSaveVisitor(TVisClientAddress_Update);
  gTIPerMgr.RegSaveVisitor(TVisClientAddress_Delete);
  gTIPerMgr.VisMgr.RegisterVisitor('read.refresh', TVisClientAddress_Refresh_RemoveUnusedObjects);
  gTIPerMgr.VisMgr.RegisterVisitor('read.refresh', TVisClientAddress_Refresh);

  // ClientIdentityRecord visitors
{ TODO -oTT:2002-07-16 -cIncomplete : Delete visitor required }
  gTIPerMgr.RegSaveVisitor(TVisClientIdentityRecord_Create);
  gTIPerMgr.RegReadVisitor(TVisClientIdentityRecord_Read);
  gTIPerMgr.RegReadVisitor(TVisClientIdentityRecord_Read_InUse);
  gTIPerMgr.RegSaveVisitor(TVisClientIdentityRecord_Update);
  gTIPerMgr.VisMgr.RegisterVisitor('read.refresh', TVisClientIdentityRecord_Refresh_RemoveUnusedObjects);
  gTIPerMgr.VisMgr.RegisterVisitor('read.refresh', TVisClientIdentityRecord_Refresh);

  // ClientIdentityRecordType visitors
{ TODO -oTT:2002-07-16 -cIncomplete : Update and Delete visitors required }
  gTIPerMgr.RegSaveVisitor(TVisClientIdentityRecordType_Create);
  gTIPerMgr.RegReadVisitor(TVisClientIdentityRecordType_Read);

  // Contract visitors
  gTIPerMgr.VisMgr.RegisterVisitor('read.defaultValues', TVisContractDefaultValues);
  gTIPerMgr.VisMgr.RegisterVisitor('read.contractClient', TVisContract_ReadClient);

  gTIPerMgr.RegSaveVisitor(TVisContract_Create);
{ TODO -oTT:2002-07-20 -cIncomplete : Delete visitor required }
  gTIPerMgr.RegReadVisitor(TVisContract_Read);
//  gTIPerMgr.RegReadVisitor(TVisContract_ReadClient);
//  gTIPerMgr.RegReadPKVisitor(TVisContract_ReadPK);
  gTIPerMgr.RegSaveVisitor(TVisContract_Update);

  // ContractClientIdentityRecordProxy visitors
  gTIPerMgr.RegSaveVisitor(TVisContractClientIdentityRecordProxy_Create);
  gTIPerMgr.RegSaveVisitor(TVisContractClientIdentityRecordProxy_Delete);
  gTIPerMgr.RegReadVisitor(TVisContractClientIdentityRecordProxy_Read);

  // ContractItem visitors
  gTIPerMgr.RegSaveVisitor(TVisContractItem_Create);
  gTIPerMgr.RegSaveVisitor(TVisContractItem_Delete);
  gTIPerMgr.RegReadVisitor(TVisContractItem_Read);
  gTIPerMgr.RegSaveVisitor(TVisContractItem_Update);

  // ContractItemCategory visitors
  gTIPerMgr.RegSaveVisitor(TVisContractItemCategory_Create);
  gTIPerMgr.RegReadVisitor(TVisContractItemCategory_Read);

  // ContractPaymentType visitors
  gTIPerMgr.RegSaveVisitor(TVisContractPaymentType_Create);
  gTIPerMgr.RegReadVisitor(TVisContractPaymentType_Read);

  // ContractTransaction visitors
  gTIPerMgr.RegSaveVisitor(TVisContractTransaction_Create);
  gTIPerMgr.RegSaveVisitor(TVisContractTransaction_Delete);
  gTIPerMgr.RegReadVisitor(TVisContractTransaction_Read);
  gTIPerMgr.RegSaveVisitor(TVisContractTransaction_Update);

 // SystemValues visitors
  gTIPerMgr.RegReadVisitor(TVisSystemValues_Read);
{ TODO -oTT -cIncomplete : Update visitor required }

  // Manufacturer visitors
  gTIPerMgr.RegSaveVisitor(TVisManufacturer_Create);
  gTIPerMgr.RegReadVisitor(TVisManufacturer_Read);
end;

{ TVisDefaultValues }

procedure TVisDefaultValues.Execute(const pVisited: TVisitedAbs);
begin
  if gTIPerMgr.Terminated then
    Exit; //==>

  try
    inherited Execute(pVisited);
    if not DoAcceptVisitor then
      Exit; //==>

    ReadData;
  except
    on e: exception do
      tiFmtException(e, ClassName, 'Execute',
        'Visited:        ' + Visited.ClassName + Cr +
        '  OID:          ' + TPerObjAbs(Visited).OID.AsString + Cr +
        '  ObjectState:  ' + TPerObjAbs(Visited).ObjectStateAsString);
  end;
end;

function TVisDefaultValues.GetVisited: TPerObjAbs;
begin
  Result := TPerObjAbs(inherited GetVisited);
end;

procedure TVisDefaultValues.SetVisited(const Value: TPerObjAbs);
begin
  inherited SetVisited(Value);
end;

{ TVisClientIdentityRecord_Create }

function TVisClientIdentityRecord_Create.AcceptVisitor: boolean;
begin
  Result := (Visited is TClientIdentityRecord) and
    (Visited.ObjectState = posCreate) and
    (Visited.Owner.Owner <> nil) and
    (Visited.Owner.Owner is TClient) and
    ((Visited as TClientIdentityRecord).IdentityRecordType <> nil);
end;

procedure TVisClientIdentityRecord_Create.Init;
begin
  Query.SQL.Text :=
    'INSERT INTO CLIENT_IDENTITY ' +
    '  ( ' +
    '    OID, ' +
    '    OWNER_OID, ' +
    '    CLIENT_IDENTITY_TYPE_OID, ' +
    '    DETAILS ' +
    '  ) ' +
    'VALUES ' +
    '  ( ' +
    '    :OID, ' +
    '    :OWNER_OID, ' +
    '    :CLIENT_IDENTITY_TYPE_OID, ' +
    '    :DETAILS ' +
    '  ) ';
end;

procedure TVisClientIdentityRecord_Create.SetupParams;
var
  Data: TClientIdentityRecord;
begin
  Data := Visited as TClientIdentityRecord;
  Query.ParamAsVariant['OID'] := Data.OID.AsVariant;
  Query.ParamAsVariant['OWNER_OID'] := Data.Owner.Owner.OID.AsVariant;
  Query.ParamAsVariant['CLIENT_IDENTITY_TYPE_OID'] := Data.IdentityRecordType.OID.AsVariant;
  Query.ParamAsString['DETAILS'] := Data.Details;
end;

{ TVisClientIdentityRecord_Read }

function TVisClientIdentityRecord_Read.AcceptVisitor: boolean;
begin
  Result := (Visited is TClientIdentityRecordList) and
    (Visited.ObjectState = posEmpty) and
    (Visited.Owner <> nil);
end;

procedure TVisClientIdentityRecord_Read.Init;
begin
  Query.SQL.Text :=
    'SELECT ' +
    '  OID, ' +
    '  OWNER_OID, ' +
    '  CLIENT_IDENTITY_TYPE_OID, ' +
    '  DETAILS ' +
    'FROM ' +
    '  CLIENT_IDENTITY ' +
    'WHERE ' +
    '  OWNER_OID = :OWNER_OID';
end;

procedure TVisClientIdentityRecord_Read.MapRowToObject;
var
  Data: TClientIdentityRecord;
begin
  Data := TClientIdentityRecord.Create;
  Data.OID.AssignFromTIQuery( Query ) ;

  Data.IdentityRecordType := PawnBroker.ClientIdentityRecordTypes.Find(Query.FieldAsString['CLIENT_IDENTITY_TYPE_OID']);
  Data.Details := Query.FieldAsString['DETAILS'];
  Data.ObjectState := posClean;

  (Visited as TClientIdentityRecordList).Add(Data);
end;

procedure TVisClientIdentityRecord_Read.SetupParams;
begin
  Visited.Owner.OID.AssignToTIQuery( 'OWNER_OID', Query ) ;
end;

{ TVisClientIdentityRecord_Read_InUse }

function TVisClientIdentityRecord_Read_InUse.AcceptVisitor: boolean;
begin
  Result := Visited is TClientIdentityRecord;
end;

procedure TVisClientIdentityRecord_Read_InUse.Init;
begin
  Query.SQL.Text :=
    'SELECT COUNT(CONTRACT_OID) AS USAGE_COUNT ' +
    '  FROM CONTRACT_IDENTITY ' +
    'WHERE ' +
    '  CLIENT_IDENTITY_OID = :CLIENT_IDENTITY_OID';
end;

procedure TVisClientIdentityRecord_Read_InUse.MapRowToObject;
var
  Data: TClientIdentityRecord;
begin
  Data := Visited as TClientIdentityRecord;
  Data.InUse := Query.FieldAsInteger['USAGE_COUNT'] > 0;
end;

procedure TVisClientIdentityRecord_Read_InUse.SetupParams;
begin
  Visited.OID.AssignToTIQuery( 'CLIENT_IDENTITY_OID', Query ) ;
end;

{ TVisClientIdentityRecord_Refresh }

function TVisClientIdentityRecord_Refresh.AcceptVisitor: boolean;
begin
  Result :=
    (Visited is TClientIdentityRecordList) and
    (Visited.Owner <> nil);
end;

procedure TVisClientIdentityRecord_Refresh.Init;
begin
  Query.SQL.Text :=
    'SELECT ' +
    '  OID, ' +
    '  OWNER_OID, ' +
    '  CLIENT_IDENTITY_TYPE_OID, ' +
    '  DETAILS ' +
    'FROM ' +
    '  CLIENT_IDENTITY ' +
    'WHERE ' +
    '  OWNER_OID = :OWNER_OID';
end;

procedure TVisClientIdentityRecord_Refresh.MapRowToObject;
var
  Data: TClientIdentityRecord;
  AssociatedObj: TPerObjAbs;
  List: TClientIdentityRecordList;
begin
  List := Visited as TClientIdentityRecordList;
  Data := List.Find(Query.FieldAsString['OID']) as TClientIdentityRecord;

  if Data = nil then
  begin
    Data := TClientIdentityRecord.Create;
    Data.OID.AssignFromTIQuery( Query ) ;
    List.Add(Data);
  end;

  AssociatedObj := PawnBroker.ClientIdentityRecordTypes.Find(Query.FieldAsString['CLIENT_IDENTITY_TYPE_OID']);
  Assert(AssociatedObj <> nil);
  Assert(AssociatedObj is TClientIdentityRecordType);
  Data.IdentityRecordType := (AssociatedObj as TClientIdentityRecordType);
  Data.Details := Query.FieldAsString['DETAILS'];
  Data.ObjectState := posClean;
end;

procedure TVisClientIdentityRecord_Refresh.SetupParams;
begin
  Visited.Owner.OID.AssignToTIQuery( 'OWNER_OID', Query ) ;
end;

{ TVisClientIdentityRecord_Refresh_RemoveUnusedObjects }

function TVisClientIdentityRecord_Refresh_RemoveUnusedObjects.AcceptVisitor: boolean;
begin
  Result := (Visited is TClientIdentityRecordList);
end;

procedure TVisClientIdentityRecord_Refresh_RemoveUnusedObjects.Execute(
  const pVisited: TVisitedAbs);
var
  IdentityRecord: TClientIdentityRecord;
  Counter: Integer;
  Data: TClientIdentityRecordList;
begin
  inherited;
  if AcceptVisitor then
  begin
    Data := pVisited as TClientIdentityRecordList;
    for Counter := Data.Count -1 downto 0 do
    begin
      IdentityRecord := Data.Items[Counter];
      if IdentityRecord.ObjectState = posCreate then
        Data.Remove(IdentityRecord);
    end;
  end;
end;

{ TVisClientIdentityRecord_Update }

function TVisClientIdentityRecord_Update.AcceptVisitor: boolean;
begin
  Result := (Visited is TClientIdentityRecord) and
    (Visited.ObjectState = posUpdate) and
    (Visited.Owner.Owner <> nil) and
    (Visited.Owner.Owner is TClient) and
    ((Visited as TClientIdentityRecord).IdentityRecordType <> nil);
end;

procedure TVisClientIdentityRecord_Update.Init;
begin
  Query.SQL.Text :=
    'UPDATE CLIENT_IDENTITY SET ' +
    '  OWNER_OID = :OWNER_OID, ' +
    '  CLIENT_IDENTITY_TYPE_OID = :CLIENT_IDENTITY_TYPE_OID, ' +
    '  DETAILS = :DETAILS ' +
    'WHERE ' +
    '  OID = :OID';
end;

procedure TVisClientIdentityRecord_Update.SetupParams;
var
  Data: TClientIdentityRecord;
begin
  Data := Visited as TClientIdentityRecord;

  Visited.OID.AssignToTIQuery( 'OID', Query ) ;
  Visited.Owner.Owner.OID.AssignToTIQuery( 'OWNER_OID', Query ) ;
  (Visited as TClientIdentityRecord).IdentityRecordType.OID.AssignToTIQuery('CLIENT_IDENTITY_TYPE_OID', Query);
  Query.ParamAsString['DETAILS'] := Data.Details;
end;

{ TVisClientIdentityRecordType_Create }

function TVisClientIdentityRecordType_Create.AcceptVisitor: boolean;
begin
  Result := (Visited is TClientIdentityRecordType) and
    (Visited.ObjectState = posCreate);
end;

procedure TVisClientIdentityRecordType_Create.Init;
begin
  Query.SQL.Text :=
    'INSERT INTO CLIENT_IDENTITY_TYPE ' +
    '  ( ' +
    '    OID, ' +
    '    NAME ' +
    '  ) ' +
    'VALUES ' +
    '  ( ' +
    '    :OID, ' +
    '    :NAME ' +
    '  ) ';
end;

procedure TVisClientIdentityRecordType_Create.SetupParams;
var
  Data: TClientIdentityRecordType;
begin
  Data := Visited as TClientIdentityRecordType;
  Data.OID.AssignToTIQuery(Query);
  Query.ParamAsString['NAME'] := Data.Name;
end;

{ TVisClientIdentityRecordType_Read }

function TVisClientIdentityRecordType_Read.AcceptVisitor: boolean;
begin
  Result := (Visited is TClientIdentityRecordTypeList) and
    (Visited.ObjectState = posEmpty);
end;

procedure TVisClientIdentityRecordType_Read.Init;
begin
  Query.SQL.Text :=
    'SELECT ' +
    '  OID, ' +
    '  NAME ' +
    'FROM ' +
    '  CLIENT_IDENTITY_TYPE ';
end;

procedure TVisClientIdentityRecordType_Read.MapRowToObject;
var
  Data: TClientIdentityRecordType;
begin
  Data := TClientIdentityRecordType.Create;
  Data.OID.AssignFromTiQuery(Query);
  Data.Name := Query.FieldAsString['NAME'];
  Data.ObjectState := posClean;
  (Visited as TPerObjList).Add(Data);
end;

procedure TVisClientIdentityRecordType_Read.SetupParams;
begin
  // Nothing to do
end;

{ TVisContractDefaultValues }

function TVisContractDefaultValues.AcceptVisitor: boolean;
begin
  Result := (Visited is TContract) and
            (Visited.ObjectState = posCreate);
end;

procedure TVisContractDefaultValues.ReadData;
var
  Data: TContract;
begin
  Data := Visited as TContract;

  Data.StartDate := Trunc(Now);
  Data.EndDate := Data.StartDate + PawnBroker.SystemValues.ContractPeriod;
  Data.ContractState := csNew;

  Data.ContractFee := PawnBroker.SystemValues.ContractFee;
  Data.InterestRate := PawnBroker.SystemValues.ContractInterestRate;

end;

{ TVisContract_Create }

function TVisContract_Create.AcceptVisitor: boolean;
begin
  Result := (Visited is TContract) and
    (Visited.ObjectState = posCreate);
end;

procedure TVisContract_Create.Init;
begin
  Query.SQL.Text :=
    'INSERT INTO CONTRACT ' +
    '  ( ' +
    '    OID, ' +
    '    CONTRACT_NUMBER, ' +
    '    CONTRACT_FEE, ' +
    '    EXTENSION_NUMBER, ' +
    '    END_DATE, ' +
    '    INTEREST_RATE, ' +
    '    CONTRACT_STATE, ' +
    '    START_DATE, ' +
    '    CLIENT_OID, ' +
    '    CLIENT_ADDRESS_OID ' +
    '  ) ' +
    'VALUES ' +
    '  ( ' +
    '    :OID, ' +
    '    :CONTRACT_NUMBER, ' +
    '    :CONTRACT_FEE, ' +
    '    :EXTENSION_NUMBER, ' +
    '    :END_DATE, ' +
    '    :INTEREST_RATE, ' +
    '    :CONTRACT_STATE, ' +
    '    :START_DATE, ' +
    '    :CLIENT_OID, ' +
    '    :CLIENT_ADDRESS_OID ' +
    '  ) ';
end;

procedure TVisContract_Create.SetupParams;
var
  Data: TContract;
begin
  Data := TContract(Visited);

  Data.OID.AssignToTiQuery(Query);
  Query.ParamAsInteger['CONTRACT_NUMBER'] := Data.ContractNumber;
  Query.ParamAsFloat['CONTRACT_FEE'] := Data.ContractFee;
  Query.ParamAsInteger['EXTENSION_NUMBER'] := Data.ExtensionNumber;
  Query.ParamAsDateTime['END_DATE'] := Data.EndDate;
  Query.ParamAsFloat['INTEREST_RATE'] := Data.InterestRate;
  Query.ParamAsInteger['CONTRACT_STATE'] := Ord(Data.ContractState);
  Query.ParamAsDateTime['START_DATE'] := Data.StartDate;
  if Data.Client = nil then
    Query.ParamAsInteger['CLIENT_OID'] := -1
  else
    Data.Client.OID.AssignToTIQuery('CLIENT_OID', Query);

  if Data.ClientAddress = nil then
    Query.ParamAsInteger['CLIENT_ADDRESS_OID'] := -1
  else
  begin
    Data.ClientAddress.OID.AssignToTIQuery('CLIENT_ADDRESS_OID', Query);
    Data.ClientAddress.InUse := True;
  end;
end;

{ TVisContract_Read }

function TVisContract_Read.AcceptVisitor: boolean;
begin
  Result :=
    (Visited is TContract) and
    (Visited.ObjectState = posPK) and
    ((Visited as TContract).Client <> nil);
end;

procedure TVisContract_Read.Init;
begin
  Query.SQL.Text :=
    'SELECT' +
    '  CLIENT_ADDRESS_OID ' +
    'FROM ' +
    '  CONTRACT ' +
    'WHERE ' +
    '  OID = :OID'
end;

procedure TVisContract_Read.MapRowToObject;
var
  Data: TContract;
begin
  Data := Visited as TContract;
  Data.ClientAddress := Data.Client.Addresses.Find(Query.FieldAsString['CLIENT_ADDRESS_OID']);
  Data.ObjectState := posClean;
end;

procedure TVisContract_Read.SetupParams;
begin
  Visited.OID.AssignToTIQuery(Query);
end;

//function TVisContract_Read.AcceptVisitor: boolean;
//begin
//  Result := (Visited is TContract) and
//    ((Visited as TContract).ObjectState = posPK);
//end;
//
//procedure TVisContract_Read.Execute(const pVisited: TVisitedAbs);
//var
//  Data: TContract;
//  AddressObject: TPerObjAbs;
//begin
//  if gTIPerMgr.Terminated then
//    Exit; //==>
//
//  try
//    inherited Execute(pVisited);
//
//    if not DoAcceptVisitor then
//      Exit; //==>
//
//    Data := (Visited as TContract);
//
//    Assert(Data.Client.ObjectState <> posPK);
//
//    AddressObject := Data.Client.Addresses.Find(Data.ClientAddressOID);
//    if AddressObject <> nil then
//    begin
//      Data.ClientAddress := AddressObject as TClientAddress;
//    end
//    else
//    begin
//{ TODO -oTT -cReview :
//Do we need to raise an exception here? How do we determine null
//addresses? }
//      Data.ClientAddress := nil;
//    end;
//
//    (pVisited as TContract).ObjectState := posClean;
//
//  except
//    on e: exception do
//      tiFmtException(e, ClassName, 'Execute',
//        'Visited:        ' + Visited.ClassName + Cr +
//        '  OID:          ' + TPerObjAbs(Visited).OID.AsString + Cr +
//        '  ObjectState:  ' + TPerObjAbs(Visited).ObjectStateAsString);
//  end;
//end;

{ TVisContract_ReadClient }

function TVisContract_ReadClient.AcceptVisitor: boolean;
begin
  Result := (Visited is TContract) and
            ((Visited as TContract).Client = nil) and
            ((Visited as TContract).ClientOID <> nil);
end;

procedure TVisContract_ReadClient.Execute(const pVisited: TVisitedAbs);
var
  Data: TContract;
begin
  if gTIPerMgr.Terminated then
    Exit; //==>

  try
    inherited Execute(pVisited);

    if not DoAcceptVisitor then
      Exit; //==>

    Data := (pVisited as TContract);
    Data.Client := TClient(PawnBroker.Clients.Find(Data.ClientOID));
    Assert(Data.Client <> nil);

  except
    on e: exception do
      tiFmtException(e, ClassName, 'Execute',
        'Visited:        ' + Visited.ClassName + Cr +
        '  OID:          ' + TPerObjAbs(Visited).OID.AsString + Cr +
        '  ObjectState:  ' + TPerObjAbs(Visited).ObjectStateAsString);
  end;
end;

{ TVisContract_ReadPK }

function TVisContract_ReadPK.AcceptVisitor: boolean;
begin
  Result := (Visited is TContractList) and
    (Visited.ObjectState = posEmpty);
end;

procedure TVisContract_ReadPK.Init;
begin
  Query.SQL.Text :=
    'SELECT ' +
    '  OID, ' +
    '  CONTRACT_FEE, ' +
    '  CONTRACT_NUMBER, ' +
    '  END_DATE, ' +
    '  EXTENSION_NUMBER, ' +
    '  INTEREST_RATE, ' +
    '  CONTRACT_STATE, ' +
    '  START_DATE, ' +
    '  CLIENT_OID ' +
    'FROM ' +
    '  CONTRACT ' +
    'ORDER BY CONTRACT_NUMBER DESC';
end;

procedure TVisContract_ReadPK.MapRowToObject;
var
  Data: TContract;
begin
  Data := TContract.Create;

  Data.ContractFee := Query.FieldAsFloat['CONTRACT_FEE'];
  Data.ClientOID.AssignFromTIQuery('CLIENT_OID', Query);
  Data.OID.AssignFromTiQuery(Query);
  Data.ContractNumber := Query.FieldAsInteger['CONTRACT_NUMBER'];
  Data.ContractState := TContractState(Query.FieldAsInteger['CONTRACT_STATE']);
  Data.ExtensionNumber := Query.FieldAsInteger['EXTENSION_NUMBER'];
  Data.EndDate := Query.FieldAsDateTime['END_DATE'];
  Data.StartDate := Query.FieldAsDateTime['START_DATE'];
  Data.InterestRate := Query.FieldAsFloat['INTEREST_RATE'];

  Data.ObjectState := posPK;

  (Visited as TContractList).Add(Data);
end;

procedure TVisContract_ReadPK.SetupParams;
begin
  // No params
end;

//{ TVisContract_ReadPKPK }
//
//function TVisContract_ReadPKPK.AcceptVisitor: boolean;
//begin
//  Result := (Visited is TContractList) and
//            (Visited.ObjectState = posEmpty) and
//            (Visited.Owner is TPawnBroker);
//end;
//
//procedure TVisContract_ReadPKPK.Init;
//begin
//  Query.SQL.Text :=
//    'SELECT ' +
//    '  OID, ' +
//    '  CONTRACT_FEE, ' +
//    '  CONTRACT_NUMBER, ' +
//    '  END_DATE, ' +
//    '  EXTENSION_NUMBER, ' +
//    '  INTEREST_RATE, ' +
//    '  CONTRACT_STATE, ' +
//    '  START_DATE ' +
//    '  CLIENT_OID ' +
//    'FROM ' +
//    '  CONTRACT ' +
//    'ORDER BY CONTRACT_NUMBER DESC';
//end;
//
//procedure TVisContract_ReadPKPK.MapRowToObject;
//var
//  Data: TContract;
//begin
//  Data := TContract.Create;
//
//  Data.OID := Query.FieldAsInteger['OID'];
//  Data.ContractNumber := Query.FieldAsInteger['CONTRACT_NUMBER'];
//  Data.ContractState := TContractState(Query.FieldAsInteger['CONTRACT_STATE']);
//  Data.ExtensionNumber := Query.FieldAsInteger['EXTENSION_NUMBER'];
//  Data.EndDate := Query.FieldAsDateTime['END_DATE'];
//  Data.StartDate := Query.FieldAsDateTime['START_DATE'];
//  Data.ObjectState := posPK;
//
//  (Visited as TContractList).Add(Data);
//end;
//
//procedure TVisContract_ReadPKPK.SetupParams;
//begin
//   No params
//end;

{ TVisContract_Update }

function TVisContract_Update.AcceptVisitor: boolean;
begin
  Result := (Visited is TContract) and
    (Visited.ObjectState = posUpdate);
end;

procedure TVisContract_Update.Init;
begin
  Query.SQL.Text :=
    'UPDATE CONTRACT SET ' +
    '  OID = :OID, ' +
    '  CONTRACT_FEE = :CONTRACT_FEE, ' +
    '  CONTRACT_NUMBER = :CONTRACT_NUMBER, ' +
    '  END_DATE = :END_DATE, ' +
    '  EXTENSION_NUMBER = :EXTENSION_NUMBER, ' +
    '  INTEREST_RATE = :INTEREST_RATE, ' +
    '  CONTRACT_STATE = :CONTRACT_STATE, ' +
    '  START_DATE = :START_DATE, ' +
    '  CLIENT_OID = :CLIENT_OID ' +
    'WHERE ' +
    '  OID = :OID ';
end;

procedure TVisContract_Update.SetupParams;
var
  Data: TContract;
begin
  Data := (Visited as TContract);

  Data.OID.AssignToTiQuery(Query);
  Query.ParamAsFloat['CONTRACT_FEE'] := Data.ContractFee;
  Query.ParamAsInteger['CONTRACT_NUMBER'] := Data.ContractNumber;
  Query.ParamAsDateTime['END_DATE'] := Data.EndDate;
  Query.ParamAsInteger['EXTENSION_NUMBER'] := Data.ExtensionNumber;
  Query.ParamAsFloat['INTEREST_RATE'] := Data.InterestRate;
  Query.ParamAsInteger['CONTRACT_STATE'] := Ord(Data.ContractState);
  Query.ParamAsDateTime['START_DATE'] := Data.StartDate;
  if Data.Client = nil then
    Query.ParamAsInteger['CLIENT_OID'] := -1
  else
    Data.Client.OID.AssignToTiQuery('CLIENT_OID', Query);
end;

{ TVisContractClientIdentityRecordProxy_Create }

function TVisContractClientIdentityRecordProxy_Create.AcceptVisitor: boolean;
begin
  Result :=
    (Visited is TClientIdentityRecordProxy) and
    ((Visited as TClientIdentityRecordProxy).ObjectState = posCreate) and
    (Visited.Owner.Owner <> nil) and
    (Visited.Owner.Owner is TContract);
end;

procedure TVisContractClientIdentityRecordProxy_Create.Init;
begin
  Query.SQL.Text :=
    'INSERT INTO CONTRACT_IDENTITY ' +
    '  ( ' +
    '    CONTRACT_OID, ' +
    '    CLIENT_IDENTITY_OID ' +
    '  ) ' +
    'VALUES ' +
    '  ( ' +
    '    :CONTRACT_OID, ' +
    '    :CLIENT_IDENTITY_OID ' +
    '  ) ';
end;

procedure TVisContractClientIdentityRecordProxy_Create.SetupParams;
var
  Data: TClientIdentityRecordProxy;
  Contract: TContract;
begin
  Data := Visited as TClientIdentityRecordProxy;
  Contract := (Visited.Owner.Owner as TContract);
  Contract.OID.AssignToTiQuery('CONTRACT_OID', Query);
  Data.OID.AssignToTiQuery('CLIENT_IDENTITY_OID', Query);
  Data.InUse := True;
end;

{ TVisContractClientIdentityRecordProxy_Delete }

function TVisContractClientIdentityRecordProxy_Delete.AcceptVisitor: boolean;
begin
  Result := (Visited is TClientIdentityRecordProxy) and
    ((Visited As TClientIdentityRecordProxy).ObjectState = posDelete) and
    (Visited.Owner.Owner <> nil) and
    (Visited.Owner.Owner is TContract);
end;

procedure TVisContractClientIdentityRecordProxy_Delete.Init;
begin
  Query.SQL.Text :=
    'DELETE FROM CONTRACT_IDENTITY ' +
    'WHERE ' +
    '  CONTRACT_OID = :CONTRACT_OID AND ' +
    '  CLIENT_IDENTITY_OID = :CLIENT_IDENTITY_OID';
end;

procedure TVisContractClientIdentityRecordProxy_Delete.SetupParams;
var
  Data: TClientIdentityRecordProxy;
  Contract: TContract;
begin
  Contract := Visited.Owner.Owner as TContract;
  Data := Visited as TClientIdentityRecordProxy;

  Contract.OID.AssignToTiQuery('CONTRACT_OID', Query);
  Data.OID.AssignToTiQuery('CLIENT_IDENTITY_OID', Query);
end;


{ TVisContractClientIdentityRecordProxy_Read }

function TVisContractClientIdentityRecordProxy_Read.AcceptVisitor: boolean;
begin
  Result := (Visited is TClientIdentityRecordList) and
    (Visited.Owner <> nil) and
    (Visited.Owner is TContract) and
    ((Visited.Owner as TContract).Client <> nil) and
    (Visited.ObjectState = posEmpty);
end;

procedure TVisContractClientIdentityRecordProxy_Read.Init;
begin
  Query.SQL.Text :=
    'SELECT ' +
    '  CONTRACT_OID, ' +
    '  CLIENT_IDENTITY_OID ' +
    'FROM ' +
    '  CONTRACT_IDENTITY ' +
    'WHERE ' +
    '  CONTRACT_OID = :CONTRACT_OID';
end;

procedure TVisContractClientIdentityRecordProxy_Read.MapRowToObject;
var
  Contract: TContract;
  ClientIdentityRecord: TClientIdentityRecord;
  ClientIdentityRecordProxy: TClientIdentityRecordProxy;
begin
  Contract := (Visited.Owner as TContract);
  ClientIdentityRecord :=
    TClientIdentityRecord(
      Contract.Client.IdentityRecords.Find(Query.FieldAsString['CLIENT_IDENTITY_OID']));

  Assert(ClientIdentityRecord <> nil);

  ClientIdentityRecordProxy := TClientIdentityRecordProxy.Create(ClientIdentityRecord);
  ClientIdentityRecordProxy.InUse := True;
  (Visited as TClientIdentityRecordList).Add(ClientIdentityRecordProxy);
end;

procedure TVisContractClientIdentityRecordProxy_Read.SetupParams;
begin
  (Visited.Owner as TContract).OID.AssignToTiQuery('CONTRACT_OID', Query);
end;

{ TVisContractItemCategory_Create }

function TVisContractItemCategory_Create.AcceptVisitor: boolean;
begin
  Result :=
    (Visited is TContractItemCategory) and
    (Visited.ObjectState = posCreate);
end;

procedure TVisContractItemCategory_Create.Init;
begin
  Query.SQL.Text :=
    'INSERT INTO CONTRACT_ITEM_CATEGORY ' +
    '  ( ' +
    '    OID, ' +
    '    NAME ' +
    '  ) ' +
    'VALUES ' +
    '  ( ' +
    '    :OID, ' +
    '    :NAME ' +
    '  )';
end;

procedure TVisContractItemCategory_Create.SetupParams;
var
  Data: TContractItemCategory;
begin
  Data := Visited as TContractItemCategory;
  Data.OID.AssignToTiQuery(Query);
  Query.ParamAsString['NAME'] := Data.Name;
end;

{ TVisContractItemCategory_Read }

function TVisContractItemCategory_Read.AcceptVisitor: boolean;
begin
  Result :=
    (Visited is TContractItemCategoryList) and
    (Visited.ObjectState = posEmpty);
end;

procedure TVisContractItemCategory_Read.Init;
begin
  Query.SQL.Text :=
    'SELECT ' +
    '  OID, ' +
    '  NAME ' +
    'FROM ' +
    '  CONTRACT_ITEM_CATEGORY ';
end;

procedure TVisContractItemCategory_Read.MapRowToObject;
var
  Data: TContractItemCategory;
begin
  Data := TContractItemCategory.Create;
  Data.OID.AssignFromTiQuery(Query);
  Data.Name := Query.FieldAsString['NAME'];
  Data.ObjectState := posClean;

  (Visited as TContractItemCategoryList).Add(Data);
end;

procedure TVisContractItemCategory_Read.SetupParams;
begin
  // Nothing to do.
end;

{ TVisContractItem_Create }

function TVisContractItem_Create.AcceptVisitor: boolean;
begin
  Result := (Visited is TContractItem) and
    (Visited.ObjectState = posCreate) and
    (Visited.Owner <> nil);
end;

procedure TVisContractItem_Create.Init;
begin
  Query.SQL.Text :=
    'INSERT INTO CONTRACT_ITEM ' +
    '  ( ' +
    '    OID, ' +
    '    OWNER_OID, ' +
    '    CATEGORY_OID, ' +
    '    DESCRIPTION, ' +
    '    MANUFACTURER_OID, ' +
    '    MODEL_NUMBER, ' +
    '    NOTES, ' +
//    '    PHOTO, ' +
  '    QUANTITY, ' +
    '    SERIAL_NUMBER, ' +
    '    CONTRACT_ITEM_VALUE ' +
    '  ) ' +
    'VALUES ' +
    '  ( ' +
    '    :OID, ' +
    '    :OWNER_OID, ' +
    '    :CATEGORY_OID, ' +
    '    :DESCRIPTION, ' +
    '    :MANUFACTURER_OID, ' +
    '    :MODEL_NUMBER, ' +
    '    :NOTES, ' +
//    '    :PHOTO, ' +
  '    :QUANTITY, ' +
    '    :SERIAL_NUMBER, ' +
    '    :CONTRACT_ITEM_VALUE ' +
    '  ) ';
end;

procedure TVisContractItem_Create.SetupParams;
var
  Data: TContractItem;
begin
  Data := Visited as TContractItem;

  Data.OID.AssignToTiQuery(Query);
  Data.Owner.OID.AssignToTiQuery('OWNER_OID', Query);
  if Data.Category <> nil then
    Data.Category.OID.AssignToTiQuery('CATEGORY_OID', Query)
  else
    Query.ParamAsInteger['CATEGORY_OID'] := -1;
  Query.ParamAsString['DESCRIPTION'] := Data.Description;
  if Data.Manufacturer <> nil then
    Data.Manufacturer.OID.AssignToTiQuery('MANUFACTURER_OID', Query)
  else
    Query.ParamAsInteger['MANUFACTURER_OID'] := -1;
  Query.ParamAsString['MODEL_NUMBER'] := Data.ModelNumber;
  Query.ParamAsString['NOTES'] := Data.Notes;
 // Query.ParamAsString['PHOTO'] := Data.
  Query.ParamAsInteger['QUANTITY'] := Data.Quantity;
  Query.ParamAsString['SERIAL_NUMBER'] := Data.SerialNumber;
  Query.ParamAsFloat['CONTRACT_ITEM_VALUE'] := Data.Value;
end;

{ TVisContractItem_Delete }

function TVisContractItem_Delete.AcceptVisitor: boolean;
begin
  Result := (Visited is TContractItem) and
    (Visited.ObjectState = posDelete);
end;

procedure TVisContractItem_Delete.Init;
begin
  Query.SQL.Text :=
    'DELETE FROM CONTRACT_ITEM ' +
    'WHERE OID = :OID';
end;

procedure TVisContractItem_Delete.SetupParams;
begin
  Visited.OID.AssignToTiQuery(Query);
end;

{ TVisContractItem_Read }

function TVisContractItem_Read.AcceptVisitor: boolean;
begin
  Result := (Visited is TContractItemList) and
    (Visited.ObjectState = posEmpty) and
    (Visited.Owner <> nil);
end;

procedure TVisContractItem_Read.Init;
begin
  Query.SQL.Text :=
    'SELECT ' +
    '  OID, ' +
    '  OWNER_OID, ' +
    '  CATEGORY_OID, ' +
    '  DESCRIPTION, ' +
    '  MANUFACTURER_OID, ' +
    '  MODEL_NUMBER, ' +
    '  NOTES, ' +
//    '  PHOTO, ' +
  '  QUANTITY, ' +
    '  SERIAL_NUMBER, ' +
    '  CONTRACT_ITEM_VALUE ' +
    'FROM ' +
    '  CONTRACT_ITEM ' +
    'WHERE ' +
    '  OWNER_OID = :OWNER_OID';
end;

procedure TVisContractItem_Read.MapRowToObject;
var
  Data: TContractItem;
begin
  Data := TContractItem.Create;

  Data.OID.AssignFromTiQuery(Query);
  if Query.FieldAsInteger['CATEGORY_OID'] <> -1 then
    Data.Category := PawnBroker.ContractItemCategories.Find(Query.FieldAsString['CATEGORY_OID'])
  else
    Data.Category := nil;
  Data.Description := Query.FieldAsString['DESCRIPTION'];
  if Query.FieldAsInteger['MANUFACTURER_OID'] <> -1 then
    Data.Manufacturer := PawnBroker.Manufacturers.Find(Query.FieldAsString['MANUFACTURER_OID'])
  else
    Data.Manufacturer := nil;
  Data.ModelNumber := Query.FieldAsString['MODEL_NUMBER'];
  Data.Notes := Query.FieldAsString['NOTES'];
//  Data.Photo
  Data.Quantity := Query.FieldAsInteger['QUANTITY'];
  Data.SerialNumber := Query.FieldAsString['SERIAL_NUMBER'];
  Data.Value := Query.FieldAsFloat['CONTRACT_ITEM_VALUE'];
  Data.ObjectState := posClean;

  (Visited as TContractItemList).Add(Data);
end;

procedure TVisContractItem_Read.SetupParams;
begin
  Visited.Owner.OID.AssignToTiQuery('OWNER_OID', Query);
end;

{ TVisContractItem_Update }

function TVisContractItem_Update.AcceptVisitor: boolean;
begin
  Result := (Visited is TContractItem) and
    (Visited.ObjectState = posUpdate);
end;

procedure TVisContractItem_Update.Init;
begin
  Query.SQL.Text :=
    'UPDATE CONTRACT_ITEM SET ' +
    '  OWNER_OID = :OWNER_OID, ' +
    '  CATEGORY_OID = :CATEGORY_OID, ' +
    '  DESCRIPTION = :DESCRIPTION, ' +
    '  MANUFACTURER_OID = :MANUFACTURER_OID, ' +
    '  MODEL_NUMBER = :MODEL_NUMBER, ' +
    '  NOTES = :NOTES, ' +
//    '  PHOTO, ' +
  '  QUANTITY = :QUANTITY, ' +
    '  SERIAL_NUMBER = :SERIAL_NUMBER, ' +
    '  CONTRACT_ITEM_VALUE = :CONTRACT_ITEM_VALUE ' +
    'WHERE ' +
    '  OID          = :OID ';

end;

procedure TVisContractItem_Update.SetupParams;
var
  Data: TContractItem;
begin
  Data := Visited as TContractItem;

  Assert(Data.Category <> nil);
  Assert(Data.Manufacturer <> nil);

  Data.OID.AssignToTiQuery(Query);
  Data.Owner.OID.AssignToTiQuery('OWNER_OID', Query);
  Data.Category.OID.AssignToTiQuery('CATEGORY_OID', Query);
  Query.ParamAsString['DESCRIPTION'] := Data.Description;
  Data.Manufacturer.OID.AssignToTiQuery('MANUFACTURER_OID', Query);
  Query.ParamAsString['MODEL_NUMBER'] := Data.ModelNumber;
  Query.ParamAsString['NOTES'] := Data.Notes;
 // Query.ParamAsString['PHOTO'] := Data.
  Query.ParamAsInteger['QUANTITY'] := Data.Quantity;
  Query.ParamAsString['SERIAL_NUMBER'] := Data.SerialNumber;
  Query.ParamAsFloat['CONTRACT_ITEM_VALUE'] := Data.Value;
end;

{ TVisContractPaymentType_Create }

function TVisContractPaymentType_Create.AcceptVisitor: boolean;
begin
  Result := (Visited is TContractPaymentType) and
    (Visited.ObjectState = posCreate);
end;

procedure TVisContractPaymentType_Create.Init;
begin
  Query.SQL.Text :=
    'INSERT INTO PAYMENT_TYPE' +
    '  ( ' +
    '   OID, ' +
    '   NAME, ' +
    '   DESCRIPTION, ' +
    '   REQUIRES_DETAILS ' +
    '  ) ' +
    'VALUES ' +
    '  ( ' +
    '   :OID, ' +
    '   :NAME, ' +
    '   :DESCRIPTION, ' +
    '   :REQUIRES_DETAILS ' +
    '  ) ';
end;

procedure TVisContractPaymentType_Create.SetupParams;
var
  Data: TContractPaymentType;
begin
  Data := Visited as TContractPaymentType;
  Data.OID.AssignToTiQuery(Query);
  Query.ParamAsString['NAME'] := Data.Name;
  Query.ParamAsString['DESCRIPTION'] := Data.Description;
  Query.ParamAsBoolean['REQUIRES_DETAILS'] := Data.RequiresDetails;
end;

{ TVisContractPaymentType_Read }

function TVisContractPaymentType_Read.AcceptVisitor: boolean;
begin
  Result :=
    (Visited is TContractPaymentTypeList) and
    (Visited.ObjectState = posEmpty);
end;

procedure TVisContractPaymentType_Read.Init;
begin
  Query.SQL.Text :=
    'SELECT ' +
    '  OID, ' +
    '  NAME, ' +
    '  DESCRIPTION, ' +
    '  REQUIRES_DETAILS ' +
    'FROM ' +
    '  PAYMENT_TYPE';
end;

procedure TVisContractPaymentType_Read.MapRowToObject;
var
  Data: TContractPaymentType;
begin
  Data := TContractPaymentType.Create;
  Data.OID.AssignFromTiQuery(Query);
  Data.Name := Query.FieldAsString['NAME'];
  Data.Description := Query.FieldAsString['DESCRIPTION'];
  Data.RequiresDetails := Query.FieldAsBoolean['REQUIRES_DETAILS'];

  (Visited as TContractPaymentTypeList).Add(Data);
end;

procedure TVisContractPaymentType_Read.SetupParams;
begin
  // no params
end;

{ TODO -oTT -cRemove : The defaultPaymentType feature is part of the
  systemValues object }
//{ TVisContractPaymentTypeList_Read }
//
//function TVisContractPaymentTypeList_Read.AcceptVisitor: boolean;
//begin
//  Result := (Visited is TContractPaymentTypeList) and
//    (Visited.ObjectState = posEmpty);
//end;
//
//procedure TVisContractPaymentTypeList_Read.Init;
//begin
//  Query.SQL.Text :=
//    'SELECT ' +
//    '  DEFAULT_PAYMENT_TYPE_OID ' +
//    'FROM ' +
//    '  SYSTEM';
//end;
//
//procedure TVisContractPaymentTypeList_Read.MapRowToObject;
//var
//  Data: TContractPaymentTypeList;
//  DefaultContractPaymentType: TContractPaymentType;
//begin
//  Data := (Visited as TContractPaymentTypeList);
//
//  DefaultContractPaymentType :=
//    Data.Find(Query.FieldAsInteger['DEFAULT_PAYMENT_TYPE_OID']) as TContractPaymentType;
//
//  if DefaultContractPaymentType <> nil then
//  begin
//    Data.DefaultPaymentType := DefaultContractPaymentType;
//  end
//  else
//  begin
//    Data.DefaultPaymentType := nil;
//    Log('TVisContractPaymentTypeList_Read.MapRowToObject; Invalid DEFAULT_PAYMENT_TYPE_OID');
//  end;
//end;
//
//procedure TVisContractPaymentTypeList_Read.SetupParams;
//begin
//  // no params
//end;

{ TVisContractTransaction_Create }

function TVisContractTransaction_Create.AcceptVisitor: boolean;
begin
  Result := (Visited is TContractTransaction) and
    (Visited.ObjectState = posCreate) and
    (Visited.Owner.Owner is TContract);
end;

procedure TVisContractTransaction_Create.Init;
begin
  Query.SQL.Text :=
    'INSERT INTO CONTRACT_TRANSACTION ' +
    '  ( ' +
    '    OID, ' +
    '    OWNER_OID, ' +
    '    DETAILS, ' +
    '    EXTENSION_NUMBER, ' +
    '    TRANSACTION_TIMESTAMP, ' +
    '    TRANSACTION_TYPE, ' +
    '    PAYMENT_TYPE_OID, ' +
    '    TRANSACTION_VALUE ' +
    '  ) ' +
    '  VALUES ' +
    '  ( ' +
    '    :OID, ' +
    '    :OWNER_OID, ' +
    '    :DETAILS, ' +
    '    :EXTENSION_NUMBER, ' +
    '    :TRANSACTION_TIMESTAMP, ' +
    '    :TRANSACTION_TYPE, ' +
    '    :PAYMENT_TYPE_OID, ' +
    '    :TRANSACTION_VALUE ' +
    '  ) ';
end;

procedure TVisContractTransaction_Create.SetupParams;
var
  Data: TContractTransaction;
begin
  Data := Visited as TContractTransaction;

  Data.OID.AssignToTiQuery(Query);
  Data.Owner.Owner.OID.AssignToTiQuery('OWNER_OID', Query);
  Query.ParamAsString['DETAILS'] := Data.Details;
  Query.ParamAsInteger['EXTENSION_NUMBER'] := Data.ExtensionNumber;
  Query.ParamAsDateTime['TRANSACTION_TIMESTAMP'] := Data.TimeStamp;
  Query.ParamAsInteger['TRANSACTION_TYPE'] := ord(Data.TransactionType);
  if Data.PaymentType = nil then
    Query.ParamAsInteger['PAYMENT_TYPE_OID'] := -1
  else
    Data.PaymentType.OID.AssignToTiQuery('PAYMENT_TYPE_OID', Query);
  Query.ParamAsFloat['TRANSACTION_VALUE'] := Data.Value;
end;

{ TVisContractTransaction_Delete }

function TVisContractTransaction_Delete.AcceptVisitor: boolean;
begin
  Result := (Visited is TContractTransaction) and
    (Visited.ObjectState = posDelete);
end;

procedure TVisContractTransaction_Delete.Init;
begin
  Query.SQL.Text :=
    'DELETE FROM CONTRACT_TRANSACTION ' +
    'WHERE OID = :OID';
end;

procedure TVisContractTransaction_Delete.SetupParams;
begin
  Visited.OID.AssignToTiQuery(Query);
end;

{ TVisContractTransaction_Read }

function TVisContractTransaction_Read.AcceptVisitor: boolean;
begin
  Result := (Visited is TContractTransactionList) and
    (Visited.ObjectState = posEmpty) and
    (Visited.Owner is TContract) and
    ((Visited.Owner as TContract).Client <> nil);
end;

procedure TVisContractTransaction_Read.Init;
begin
  Query.SQL.Text :=
    'SELECT ' +
    '  OID, ' +
    '  OWNER_OID, ' +
    '  DETAILS, ' +
    '  EXTENSION_NUMBER, ' +
    '  TRANSACTION_TIMESTAMP, ' +
    '  TRANSACTION_TYPE, ' +
    '  PAYMENT_TYPE_OID, ' +
    '  TRANSACTION_VALUE ' +
    'FROM ' +
    '  CONTRACT_TRANSACTION ' +
    'WHERE ' +
    '  OWNER_OID = :OWNER_OID ' +
    'ORDER BY ' +
    '  TRANSACTION_TIMESTAMP';
end;

procedure TVisContractTransaction_Read.MapRowToObject;
var
  Data: TContractTransaction;
  Client: TClient;
begin
  Client := (Visited.Owner as TContract).Client;
  Assert(Client.ObjectState = posClean);

  Data := TContractTransaction.Create;
  Data.OID.AssignFromTiQuery(Query);
  Data.Details := Query.FieldAsString['DETAILS'];
  Data.ExtensionNumber := Query.FieldAsInteger['EXTENSION_NUMBER'];
  Data.TimeStamp := Query.FieldAsDateTime['TRANSACTION_TIMESTAMP'];
  Data.TransactionType := TContractTransactionType(Query.FieldAsInteger['TRANSACTION_TYPE']);

  if Query.FieldAsInteger['PAYMENT_TYPE_OID'] <> -1 then
    Data.PaymentType := PawnBroker.ContractPaymentTypes.Find(Query.FieldAsString['PAYMENT_TYPE_OID'])
  else
    Data.PaymentType := nil;
  Data.Value := Query.FieldAsFloat['TRANSACTION_VALUE'];
  Data.ObjectState := posClean;

  (Visited as TContractTransactionList).Add(Data);
end;

procedure TVisContractTransaction_Read.SetupParams;
begin
  Visited.Owner.OID.AssignToTiQuery('OWNER_OID', Query);
end;

{ TVisContractTransaction_Update }

function TVisContractTransaction_Update.AcceptVisitor: boolean;
begin
  Result := (Visited is TContractTransaction) and
    (Visited.ObjectState = posUpdate) and
    (Visited.Owner.Owner is TContract);
end;

procedure TVisContractTransaction_Update.Init;
begin
  Query.SQL.Text :=
    'UPDATE CONTRACT_TRANSACTION SET' +
    '  OWNER_OID = , :OWNER_OID' +
    '  DETAILS = :DETAILS, ' +
    '  EXTENSION_NUMBER = :EXTENSION_NUMBER, ' +
    '  TRANSACTION_TIMESTAMP = :TRANSACTION_TIMESTAMP, ' +
    '  TRANSACTION_TYPE = :TRANSACTION_TYPE, ' +
    '  PAYMENT_TYPE_OID = :PAYMENT_TYPE_OID, ' +
    '  TRANSACTION_VALUE = :TRANSACTION_VALUE ' +
    'WHERE ' +
    '  OID = :OID';
end;

procedure TVisContractTransaction_Update.SetupParams;
var
  Data: TContractTransaction;
begin
  Data := Visited as TContractTransaction;

  Data.OID.AssignToTiQuery(Query);
  Data.Owner.Owner.OID.AssignToTiQuery('OWNER_OID', Query);
  Query.ParamAsString['DETAILS'] := Data.Details;
  Query.ParamAsInteger['EXTENSION_NUMBER'] := Data.ExtensionNumber;
  Query.ParamAsDateTime['TRANSACTION_TIMESTAMP'] := Data.TimeStamp;
  Query.ParamAsInteger['TRANSACTION_TYPE'] := ord(Data.TransactionType);
  if Data.PaymentType = nil then
    Query.ParamAsInteger['PAYMENT_TYPE_OID'] := -1
  else
    Data.PaymentType.OID.AssignToTiQuery('PAYMENT_TYPE_OID', Query);
  Query.ParamAsFloat['TRANSACTION_VALUE'] := Data.Value;
end;

{ TVisManufacturer_Create }

function TVisManufacturer_Create.AcceptVisitor: boolean;
begin
  Result :=
    (Visited is TManufacturer) and
    (Visited.ObjectState = posCreate);
end;

procedure TVisManufacturer_Create.Init;
begin
  Query.SQL.Text :=
    'INSERT INTO MANUFACTURER ' +
    '  ( ' +
    '    OID, ' +
    '    NAME ' +
    '  ) ' +
    'VALUES ' +
    '  ( ' +
    '    :OID, ' +
    '    :NAME ' +
    '  )';
end;

procedure TVisManufacturer_Create.SetupParams;
var
  Data: TManufacturer;
begin
  Data := Visited as TManufacturer;
  Data.OID.AssignToTiQuery(Query);
  Query.ParamAsString['NAME'] := Data.Name;
end;

{ TVisManufacturer_Read }

function TVisManufacturer_Read.AcceptVisitor: boolean;
begin
  Result :=
    (Visited is TManufacturerList) and
    (Visited.ObjectState = posEmpty);
end;

procedure TVisManufacturer_Read.Init;
begin
  Query.SQL.Text :=
    'SELECT ' +
    '  OID, ' +
    '  NAME ' +
    'FROM ' +
    '  MANUFACTURER ';
end;

procedure TVisManufacturer_Read.MapRowToObject;
var
  Data: TManufacturer;
begin
  Data := TManufacturer.Create;
  Data.OID.AssignFromTiQuery(Query);
  Data.Name := Query.FieldAsString['NAME'];
  Data.ObjectState := posClean;

  (Visited as TManufacturerList).Add(Data);
end;

procedure TVisManufacturer_Read.SetupParams;
begin
  // no params
end;

{ TVisNextClientNumberRead }

function TVisNextClientNumberRead.AcceptVisitor: boolean;
begin
  result := (Visited is TClient) and
    (Visited.ObjectState = posCreate);
end;

procedure TVisNextClientNumberRead.Init;
begin
  Query.SQL.Text := 'SELECT CLIENT_NUMBER FROM NEXT_CLIENT_NUMBER';
end;

procedure TVisNextClientNumberRead.SetupParams;
begin
  // do nothing
end;

procedure TVisNextClientNumberRead.MapRowToObject;
begin
  TClient(Visited).ClientNumber := Query.FieldAsInteger['CLIENT_NUMBER'];
end;

{ TVisNextClientNumberUpdate }

function TVisNextClientNumberUpdate.AcceptVisitor: boolean;
begin
  result := (Visited is TClient) and
    (Visited.ObjectState = posCreate);
end;

procedure TVisNextClientNumberUpdate.Init;
begin
  Query.SQL.Text := 'UPDATE NEXT_CLIENT_NUMBER SET CLIENT_NUMBER = CLIENT_NUMBER + 1';
end;

procedure TVisNextClientNumberUpdate.SetupParams;
begin
// Do nothing
end;

{ TVisNextContractNumberRead }

function TVisNextContractNumberRead.AcceptVisitor: boolean;
begin
  result := (Visited is TContract) and
    (Visited.ObjectState = posCreate);
end;

procedure TVisNextContractNumberRead.Init;
begin
  Query.SQL.Text := 'SELECT CONTRACT_NUMBER FROM NEXT_CONTRACT_NUMBER';
end;

procedure TVisNextContractNumberRead.SetupParams;
begin
  inherited;
  // do nothing
end;

procedure TVisNextContractNumberRead.MapRowToObject;
begin
  TContract(Visited).ContractNumber := Query.FieldAsInteger['CONTRACT_NUMBER'];
end;

{ TVisNextContractNumberUpdate }

function TVisNextContractNumberUpdate.AcceptVisitor: boolean;
begin
  result := (Visited is TContract) and
    (Visited.ObjectState = posCreate);
end;

procedure TVisNextContractNumberUpdate.Init;
begin
  Query.SQL.Text := 'UPDATE NEXT_CONTRACT_NUMBER SET CONTRACT_NUMBER = CONTRACT_NUMBER + 1';
end;

procedure TVisNextContractNumberUpdate.SetupParams;
begin
// Do nothing
end;

{ TVisPawnBrokerRead }

function TVisPawnBrokerRead.AcceptVisitor: boolean;
begin
  Result := (Visited is TPawnBroker);
end;

procedure TVisPawnBrokerRead.Init;
begin
  Query.SQL.Text :=
    'SELECT ' +
    '  PAWNBROKER_NAME, ' +
    '  CONTACT_DETAILS ' +
    'FROM ' +
    '  PAWNBROKER ';
end;

procedure TVisPawnBrokerRead.MapRowToObject;
var
  Data: TPawnBroker;
begin
  Data := (Visited as TPawnBroker);
  Data.Name := Query.FieldAsString['PAWNBROKER_NAME'];
  Data.ContactDetails := Query.FieldAsString['CONTACT_DETAILS'];
end;

procedure TVisPawnBrokerRead.SetupParams;
begin
  // Do nothing
end;

{ TVisSystemValues_Read }

function TVisSystemValues_Read.AcceptVisitor: boolean;
begin
  Result := (Visited is TSystemValues) and
    (Visited.ObjectState = posEmpty);
end;

procedure TVisSystemValues_Read.Init;
begin
  Query.SQL.Text :=
    'SELECT ' +
    '  CONTRACT_PERIOD, ' +
    '  CONTRACT_FEE, ' +
    '  CONTRACT_INTEREST_RATE, ' +
    '  DEFAULT_PAYMENT_TYPE_OID, ' +
    '  CONTRACT_TERMS, ' +
    '  DEFAULT_NUM_OF_COPIES_TO_PRINT ' +
    'FROM ' +
    '  SYSTEM ';
end;

procedure TVisSystemValues_Read.MapRowToObject;
var
  Data: TSystemValues;
begin
  Data := Visited as TSystemValues;
  Data.ContractPeriod := Query.FieldAsInteger['CONTRACT_PERIOD'];
  Data.ContractFee := Query.FieldAsFloat['CONTRACT_FEE'];
  Data.ContractInterestRate := Query.FieldAsFloat['CONTRACT_INTEREST_RATE'];
  Data.DefaultContractPaymentType :=
    PawnBroker.ContractPaymentTypes.Find(Query.FieldAsString['DEFAULT_PAYMENT_TYPE_OID']);
  Data.ContractTerms := Query.FieldAsString['CONTRACT_TERMS'];
  Data.DefaultNumberOfCopiesToPrint := Query.FieldAsInteger['DEFAULT_NUM_OF_COPIES_TO_PRINT'];
end;

procedure TVisSystemValues_Read.SetupParams;
begin
  // Do nothing
end;

{ TVisSystemValues_Update }

function TVisSystemValues_Update.AcceptVisitor: boolean;
begin
  Result := (Visited is TSystemValues) and
    (Visited.ObjectState = posUpdate);
end;

procedure TVisSystemValues_Update.Init;
begin
  Query.SQL.Text :=
    'UPDATE SYSTEM ' +
    '  CONTRACT_PERIOD = :CONTRACT_PERIOD, ' +
    '  CONTRACT_FEE = :CONTRACT_FEE, ' +
    '  CONTRACT_INTEREST_RATE = :CONTRACT_INTEREST_RATE, ' +
    '  DEFAULT_PAYMENT_TYPE_OID = :DEFAULT_PAYMENT_TYPE_OID, ' +
    '  CONTRACT_TERMS = :CONTRACT_TERMS ';
end;

procedure TVisSystemValues_Update.SetupParams;
var
  Data: TSystemValues;
begin
  Data := Visited as TSystemValues;
  Query.ParamAsInteger['CONTRACT_PERIOD'] := Data.ContractPeriod;
  Query.ParamAsFloat['CONTRACT_FEE'] := Data.ContractFee;
  Query.ParamAsFloat['CONTRACT_INTEREST_RATE'] := Data.ContractInterestRate;
  Data.DefaultContractPaymentType.OID.AssignToTiQuery('DEFAULT_PAYMENT_TYPE_OID', Query);
  Query.ParamAsString['CONTRACT_TERMS'] := Data.ContractTerms;
end;

{ TVisClient_Create }

function TVisClient_Create.AcceptVisitor: boolean;
begin
  Result := (Visited is TClient) and
    (Visited.ObjectState = posCreate);
end;

procedure TVisClient_Create.Init;
begin
  Query.SQL.Text :=
    'INSERT INTO CLIENT ' +
    '( ' +
    '  OID, ' +
    '  CLIENT_NUMBER, ' +
    '  EMAIL_ADDRESS , ' +
    '  FAMILY_NAME , ' +
    '  GIVEN_NAMES , ' +
    '  DATE_OF_BIRTH, ' +
    '  PHONE_HOME, ' +
    '  PHONE_MOBILE, ' +
    '  PHONE_WORK, ' +
    '  NOTES , ' +
//    '  PHOTO, ' +
    '  CURRENT_ADDRESS_OID, ' +
    '  UNDESIRABLE, ' +
    '  UNDESIRABLE_NOTES, ' +
    '  UNDESIRABLE_CODE ' +
    ') ' +
    'VALUES ' +
    '( ' +
    '  :OID, ' +
    '  :CLIENT_NUMBER, ' +
    '  :EMAIL_ADDRESS , ' +
    '  :FAMILY_NAME , ' +
    '  :GIVEN_NAMES , ' +
    '  :DATE_OF_BIRTH, ' +
    '  :PHONE_HOME, ' +
    '  :PHONE_MOBILE, ' +
    '  :PHONE_WORK, ' +
    '  :NOTES , ' +
  //  '  :PHOTO, ' +
    '  :CURRENT_ADDRESS_OID, ' +
    '  :UNDESIRABLE, ' +
    '  :UNDESIRABLE_NOTES, ' +
    '  :UNDESIRABLE_CODE ' +
    ') ';
end;

procedure TVisClient_Create.SetupParams;
var
  Data: TClient;
begin
  Data := TClient(Visited);
  Data.OID.AssignToTiQuery(Query);
  Query.ParamAsInteger['CLIENT_NUMBER'] := Data.ClientNumber;
  Query.ParamAsString['EMAIL_ADDRESS '] := Data.EmailAddress;
  Query.ParamAsString['FAMILY_NAME '] := Data.FamilyName;
  Query.ParamAsString['GIVEN_NAMES '] := Data.GivenNames;
  Query.ParamAsDateTime['DATE_OF_BIRTH'] := Data.DateOfBirth;
  Query.ParamAsString['PHONE_HOME'] := Data.PhoneHome;
  Query.ParamAsString['PHONE_MOBILE'] := Data.PhoneMobile;
  Query.ParamAsString['PHONE_WORK'] := Data.PhoneWork;
  Query.ParamAsString['NOTES '] := Data.Notes;
  // ['PHOTO'] := Data.
  if Data.CurrentAddress = nil then
    Query.ParamAsInteger['CURRENT_ADDRESS_OID'] := -1
  else
    Data.CurrentAddress.OID.AssignToTiQuery('CURRENT_ADDRESS_OID', Query);
  Query.ParamAsBoolean['UNDESIRABLE'] := Data.Undesirable;
  Query.ParamAsString['UNDESIRABLE_CODE'] := Data.UndesirableCode;
  Query.ParamAsString['UNDESIRABLE_NOTES'] := Data.UndesirableReason;
end;

{ TVisClient_DefaultValues }

function TVisClient_DefaultValues.AcceptVisitor: boolean;
begin
  Result := (Visited is TClient) and
            (Visited.ObjectState = posCreate);
end;

procedure TVisClient_DefaultValues.ReadData;
var
  Data: TClient;
begin
  Data := Visited as TClient;
  Data.DateOfBirth := EncodeDate(1970, 1, 1); 
end;

{ TVisClient_Read }

function TVisClient_Read.AcceptVisitor: boolean;
begin
  Result := (Visited is TClient) and
            (Visited.ObjectState = posPK);
end;

procedure TVisClient_Read.Init;
begin
  Query.SQL.Text :=
    'SELECT ' +
    '  OID, ' +
    '  CLIENT_NUMBER, ' +
    '  EMAIL_ADDRESS , ' +
    '  FAMILY_NAME , ' +
    '  GIVEN_NAMES , ' +
    '  DATE_OF_BIRTH, ' +
    '  PHONE_HOME, ' +
    '  PHONE_MOBILE, ' +
    '  PHONE_WORK, ' +
    '  NOTES , ' +
    '  PHOTO, ' +
    '  UNDESIRABLE, ' +
    '  UNDESIRABLE_CODE, ' +
    '  UNDESIRABLE_NOTES ' +
    'FROM ' +
    '  CLIENT ' +
    'WHERE ' +
    '  OID = :OID'
end;

procedure TVisClient_Read.MapRowToObject;
var
  Data: TClient;
begin
  Data := Visited as TClient;
  Data.OID.AssignFromTiQuery(Query);
  Data.ClientNumber := Query.FieldAsInteger['CLIENT_NUMBER'];
  Data.EmailAddress := Query.FieldAsString['EMAIL_ADDRESS'];
  Data.FamilyName := Query.FieldAsString['FAMILY_NAME'];
  Data.GivenNames := Query.FieldAsString['GIVEN_NAMES'];
  Data.DateOfBirth := Query.FieldAsDateTime['DATE_OF_BIRTH'];
  Data.PhoneHome := Query.FieldAsString['PHONE_HOME'];
  Data.PhoneMobile := Query.FieldAsString['PHONE_MOBILE'];
  Data.PhoneWork := Query.FieldAsString['PHONE_WORK'];
  Data.Notes := Query.FieldAsString['NOTES'];
{ TODO -oTT -cIncomplete : Not sure how to read in graphic files }
//  Data.Photo := TGraphic(Query.FieldAsVariant);
  Data.Undesirable := Query.FieldAsBoolean['UNDESIRABLE'];
  Data.UndesirableCode := Query.FieldAsString['UNDESIRABLE_CODE'];
  Data.UndesirableReason := Query.FieldAsString['UNDESIRABLE_NOTES'];
  Data.ObjectState := posClean;
end;

procedure TVisClient_Read.SetupParams;
begin
  Visited.OID.AssignToTiQuery(Query);
end;

{ TVisClient_Refresh }

function TVisClient_Refresh.AcceptVisitor: boolean;
begin
  Result := (Visited is TClient) and
            (Visited.ObjectState = posClean);
end;

procedure TVisClient_Refresh.Init;
begin
  Query.SQL.Text :=
    'SELECT ' +
    '  OID, ' +
    '  CLIENT_NUMBER, ' +
    '  EMAIL_ADDRESS , ' +
    '  FAMILY_NAME , ' +
    '  GIVEN_NAMES , ' +
    '  DATE_OF_BIRTH, ' +
    '  PHONE_HOME, ' +
    '  PHONE_MOBILE, ' +
    '  PHONE_WORK, ' +
    '  NOTES , ' +
    '  PHOTO, ' +
    '  UNDESIRABLE, ' +
    '  UNDESIRABLE_CODE, ' +
    '  UNDESIRABLE_NOTES ' +
    'FROM ' +
    '  CLIENT ' +
    'WHERE ' +
    '  OID = :OID'

end;

procedure TVisClient_Refresh.MapRowToObject;
var
  Data: TClient;
begin
  Data := Visited as TClient;
  Data.OID.AssignFromTiQuery(Query);
  Data.ClientNumber := Query.FieldAsInteger['CLIENT_NUMBER'];
  Data.EmailAddress := Query.FieldAsString['EMAIL_ADDRESS'];
  Data.FamilyName := Query.FieldAsString['FAMILY_NAME'];
  Data.GivenNames := Query.FieldAsString['GIVEN_NAMES'];
  Data.DateOfBirth := Query.FieldAsDateTime['DATE_OF_BIRTH'];
  Data.PhoneHome := Query.FieldAsString['PHONE_HOME'];
  Data.PhoneMobile := Query.FieldAsString['PHONE_MOBILE'];
  Data.PhoneWork := Query.FieldAsString['PHONE_WORK'];
  Data.Notes := Query.FieldAsString['NOTES'];
{ TODO -oTT -cIncomplete : Not sure how to read in graphic files }
//  Data.Photo := TGraphic(Query.FieldAsVariant);
  Data.Undesirable := Query.FieldAsBoolean['UNDESIRABLE'];
  Data.UndesirableCode := Query.FieldAsString['UNDESIRABLE_CODE'];
  Data.UndesirableReason := Query.FieldAsString['UNDESIRABLE_NOTES'];
  Data.ObjectState := posClean;
end;

procedure TVisClient_Refresh.SetupParams;
begin
  Visited.OID.AssignToTiQuery(Query);
end;

{ TVisClientAndContract_ReadPK }

function TVisClientAndContract_ReadPK.AcceptVisitor: boolean;
begin
  Result := (Visited = PawnBroker.Clients) and
            (Visited.ObjectState = posEmpty) and
            (PawnBroker.Contracts.ObjectState = posEmpty);
end;

procedure TVisClientAndContract_ReadPK.Init;
begin
  FLastClient := nil;
  Query.SQL.Text :=
    'SELECT ' +
    '  CLIENT.OID AS CLIENT_OID, ' +
    '  CLIENT.CLIENT_NUMBER, ' +
//    '  CLIENT.EMAIL_ADDRESS , ' +
    '  CLIENT.FAMILY_NAME , ' +
    '  CLIENT.GIVEN_NAMES , ' +
//    '  CLIENT.DATE_OF_BIRTH, ' +
//    '  CLIENT.PHONE_HOME, ' +
//    '  CLIENT.PHONE_MOBILE, ' +
//    '  CLIENT.PHONE_WORK, ' +
//    '  CLIENT.NOTES , ' +
//    '  CLIENT.PHOTO, ' +
    '  CLIENT.UNDESIRABLE, ' +
    '  CLIENT.UNDESIRABLE_CODE, ' +
    '  CONTRACT.OID AS CONTRACT_OID, ' +
    '  CONTRACT.CONTRACT_FEE, ' +
    '  CONTRACT.CONTRACT_NUMBER, ' +
    '  CONTRACT.END_DATE, ' +
    '  CONTRACT.EXTENSION_NUMBER, ' +
    '  CONTRACT.INTEREST_RATE, ' +
    '  CONTRACT.CONTRACT_STATE, ' +
    '  CONTRACT.START_DATE, ' +
    '  CONTRACT.CLIENT_ADDRESS_OID ' +
    'FROM CLIENT ' +
    '  LEFT OUTER JOIN CONTRACT ON CLIENT.OID = CONTRACT.CLIENT_OID ' +
    'ORDER BY CLIENT.CLIENT_NUMBER';
//    'WHERE ' +
//    '  CONTRACT.CONTRACT_STATE = :CONTRACT_STATE';

end;

procedure TVisClientAndContract_ReadPK.MapRowToClient;
var
  Data: TClient;
begin
  Data := TClient.Create;
  Data.OID.AssignFromTiQuery('CLIENT_OID', Query);
  Data.ClientNumber := Query.FieldAsInteger['CLIENT_NUMBER'];
//  Data.EmailAddress := Query.FieldAsString['EMAIL_ADDRESS'];
  Data.FamilyName := Query.FieldAsString['FAMILY_NAME'];
  Data.GivenNames := Query.FieldAsString['GIVEN_NAMES'];
//  Data.DateOfBirth := Query.FieldAsDateTime['DATE_OF_BIRTH'];
//  Data.PhoneHome := Query.FieldAsString['PHONE_HOME'];
//  Data.PhoneMobile := Query.FieldAsString['PHONE_MOBILE'];
//  Data.PhoneWork := Query.FieldAsString['PHONE_WORK'];
//  Data.Notes := Query.FieldAsString['NOTES'];
{ TODO -oTT -cIncomplete : Not sure how to read in graphic files }
//  Data.Photo := TGraphic(Query.FieldAsVariant);
  Data.Undesirable := Query.FieldAsBoolean['UNDESIRABLE'];
  Data.UndesirableCode := Query.FieldAsString['UNDESIRABLE_CODE'];
  Data.ObjectState := posPK;

  PawnBroker.Clients.Add(Data);
  FLastClient := Data;
end;

procedure TVisClientAndContract_ReadPK.MapRowToContract;
var
  Data: TContract;
begin
  Data := TContract.Create;

  Data.OID.AssignFromTiQuery('CONTRACT_OID', Query);
  Data.Client := FLastClient;
  Data.ContractFee := Query.FieldAsFloat['CONTRACT_FEE'];
  Data.ContractNumber := Query.FieldAsInteger['CONTRACT_NUMBER'];
  Data.ContractState := TContractState(Query.FieldAsInteger['CONTRACT_STATE']);
  Data.ExtensionNumber := Query.FieldAsInteger['EXTENSION_NUMBER'];
  Data.EndDate := Query.FieldAsDateTime['END_DATE'];
  Data.InterestRate := Query.FieldAsFloat['INTEREST_RATE'];
  Data.StartDate := Query.FieldAsDateTime['START_DATE'];
//  Data.ClientAddressOID.AssignFromTiQuery('CLIENT_ADDRESS_OID', Query);
  Data.ObjectState := posPK;

  Pawnbroker.Contracts.Add(Data);
end;

procedure TVisClientAndContract_ReadPK.MapRowToObject;
begin
  if (FLastClient = nil) or (FLastClient.OID.AsString <> Query.FieldAsString['CLIENT_OID']) then
    MapRowToClient;
  Assert(FLastClient <> nil);
  if not Query.FieldIsNull['CONTRACT_OID'] then
    MapRowToContract;
end;

procedure TVisClientAndContract_ReadPK.SetupParams;
begin
//  Query.ParamAsInteger['CONTRACT_STATE'] := ord(csActive);
end;

{ TVisClient_Update }

function TVisClient_Update.AcceptVisitor: boolean;
begin
  Result := (Visited is TClient) and
    (Visited.ObjectState = posUpdate);
end;

procedure TVisClient_Update.Init;
begin
  Query.SQL.Text :=
    'UPDATE CLIENT SET ' +
    '  CLIENT_NUMBER = :CLIENT_NUMBER, ' +
    '  EMAIL_ADDRESS = :EMAIL_ADDRESS, ' +
    '  FAMILY_NAME = :FAMILY_NAME, ' +
    '  GIVEN_NAMES = :GIVEN_NAMES, ' +
    '  DATE_OF_BIRTH = :DATE_OF_BIRTH, ' +
    '  PHONE_HOME = :PHONE_HOME, ' +
    '  PHONE_MOBILE = :PHONE_MOBILE, ' +
    '  PHONE_WORK = :PHONE_WORK, ' +
    '  NOTES = :NOTES, ' +
    '  CURRENT_ADDRESS_OID = :CURRENT_ADDRESS_OID, ' +
//    '  PHOTO, ' +
  '  UNDESIRABLE = :UNDESIRABLE, ' +
    '  UNDESIRABLE_CODE = :UNDESIRABLE_CODE, ' +
    '  UNDESIRABLE_NOTES = :UNDESIRABLE_NOTES ' +
    'WHERE  ' +
    '  OID = :OID ';
end;

procedure TVisClient_Update.SetupParams;
var
  Data: TClient;
begin
  Data := TClient(Visited);
  Data.OID.AssignToTiQuery(Query);
  Query.ParamAsInteger['CLIENT_NUMBER'] := Data.ClientNumber;
  Query.ParamAsString['EMAIL_ADDRESS '] := Data.EmailAddress;
  Query.ParamAsString['FAMILY_NAME '] := Data.FamilyName;
  Query.ParamAsString['GIVEN_NAMES '] := Data.GivenNames;
  Query.ParamAsDateTime['DATE_OF_BIRTH'] := Data.DateOfBirth;
  Query.ParamAsString['PHONE_HOME'] := Data.PhoneHome;
  Query.ParamAsString['PHONE_MOBILE'] := Data.PhoneMobile;
  Query.ParamAsString['PHONE_WORK'] := Data.PhoneWork;
  Query.ParamAsString['NOTES '] := Data.Notes;
  // ['PHOTO'] := Data.
  if Data.CurrentAddress = nil then
    Query.ParamAsInteger['CURRENT_ADDRESS_OID'] := -1
  else
    Data.CurrentAddress.OID.AssignToTIQuery('CURRENT_ADDRESS_OID', Query);
  Query.ParamAsBoolean['UNDESIRABLE'] := Data.Undesirable;
  Query.ParamAsString['UNDESIRABLE_CODE'] := Data.UndesirableCode;
  Query.ParamAsString['UNDESIRABLE_NOTES'] := Data.UndesirableReason;
end;

{ TVisClientAddress_Create }

function TVisClientAddress_Create.AcceptVisitor: boolean;
begin
  Result := (Visited is TClientAddress) and
            (Visited.ObjectState = posCreate);
end;

procedure TVisClientAddress_Create.Init;
begin
  Query.SQL.Text :=
    'INSERT INTO CLIENT_ADDRESS ' +
    '  ( ' +
    '    OID, ' +
    '    OWNER_OID, ' +
    '    STREET, ' +
    '    SUBURB, ' +
    '    STATE, ' +
    '    POSTCODE, ' +
    '    DELETED ' +
    '  ) ' +
    'VALUES ' +
    '  ( ' +
    '    :OID, ' +
    '    :OWNER_OID, ' +
    '    :STREET, ' +
    '    :SUBURB, ' +
    '    :STATE, ' +
    '    :POSTCODE, ' +
    '    :DELETED ' +
    '  ) ';
end;

procedure TVisClientAddress_Create.SetupParams;
var
  Data: TClientAddress;
begin
  Data := Visited as TClientAddress;
  Data.OID.AssignToTiQuery(Query);
  Data.Owner.OID.AssignToTiQuery('OWNER_OID', Query);
  Query.ParamAsString['STREET'] := Data.Street;
  Query.ParamAsString['SUBURB'] := Data.Suburb;
  Query.ParamAsString['STATE'] := Data.State;
  Query.ParamAsString['POSTCODE'] := Data.PostCode;
  Query.ParamAsBoolean['DELETED'] := False;
end;

{ TVisClientAddress_Read }

function TVisClientAddress_Read.AcceptVisitor: boolean;
begin
  Result := (Visited is TClientAddressList) and
            (Visited.ObjectState = posEmpty) and
            (Visited.Owner <> nil);
end;

procedure TVisClientAddress_Read.Init;
begin
  Query.SQL.Text :=
    'SELECT ' +
    '  OID, ' +
    '  STREET, ' +
    '  SUBURB, ' +
    '  STATE, ' +
    '  POSTCODE ' +
    'FROM ' +
    '  CLIENT_ADDRESS ' +
    'WHERE ' +
    '  OWNER_OID = :OWNER_OID AND' +
    '  DELETED = :DELETED ';
end;

procedure TVisClientAddress_Read.MapRowToObject;
var
  Data: TClientAddress;
begin
  Data := TClientAddress.Create;
  Data.OID.AssignFromTiQuery(Query);
  Data.Street := Query.FieldAsString['STREET'];
  Data.Suburb := Query.FieldAsString['SUBURB'];
  Data.State := Query.FieldAsString['STATE'];
  Data.PostCode := Query.FieldAsString['POSTCODE'];
  Data.ObjectState := posClean;

  (Visited as TClientAddressList).Add(Data);
end;

procedure TVisClientAddress_Read.SetupParams;
begin
  (Visited as TClientAddressList).Owner.OID.AssignToTiQuery('OWNER_OID', Query);
  Query.ParamAsBoolean['DELETED'] := False;
end;

{ TVisClientAddress_Read_InUse }

function TVisClientAddress_Read_InUse.AcceptVisitor: boolean;
begin
  Result := (Visited is TClientAddress);
end;

procedure TVisClientAddress_Read_InUse.Init;
begin
  Query.SQL.Text :=
    'SELECT COUNT(OID) AS USAGE_COUNT ' +
    '  FROM CONTRACT ' +
    'WHERE ' +
    '  CLIENT_ADDRESS_OID = :CLIENT_ADDRESS_OID';
end;

procedure TVisClientAddress_Read_InUse.MapRowToObject;
var
  Data: TClientAddress;
begin
  Data := Visited as TClientAddress;
  Data.InUse := Query.FieldAsInteger['USAGE_COUNT'] > 0;
end;

procedure TVisClientAddress_Read_InUse.SetupParams;
begin
  Visited.OID.AssignToTIQuery('CLIENT_ADDRESS_OID', Query);
end;

{ TVisClientAddress_Refresh }

function TVisClientAddress_Refresh.AcceptVisitor: boolean;
begin
  Result := (Visited is TClientAddressList) and
            (Visited.Owner <> nil);
end;

procedure TVisClientAddress_Refresh.Init;
begin
  Query.SQL.Text :=
    'SELECT ' +
    '  OID, ' +
    '  STREET, ' +
    '  SUBURB, ' +
    '  STATE, ' +
    '  POSTCODE ' +
    'FROM ' +
    '  CLIENT_ADDRESS ' +
    'WHERE ' +
    '  OWNER_OID = :OWNER_OID AND' +
    '  DELETED = :DELETED ';
end;

procedure TVisClientAddress_Refresh.MapRowToObject;
var
  Data: TClientAddress;
  List: TClientAddressList;
begin
  List := Visited as TClientAddressList;
  Data := List.Find(Query.FieldAsString['OID']) as TClientAddress;

  if Data = nil then
  begin
    Data := TClientAddress.Create;
    List.Add(Data);
    Data.OID.AssignFromTiQuery(Query);
  end;

  Data.Street := Query.FieldAsString['STREET'];
  Data.Suburb := Query.FieldAsString['SUBURB'];
  Data.State := Query.FieldAsString['STATE'];
  Data.PostCode := Query.FieldAsString['POSTCODE'];
  Data.ObjectState := posClean;
end;

procedure TVisClientAddress_Refresh.SetupParams;
begin
  (Visited as TClientAddressList).Owner.OID.AssignToTIQuery('OWNER_OID', Query);
  Query.ParamAsBoolean['DELETED'] := False;
end;

{ TVisClientAddress_Refresh_RemoveUnusedObjects }

function TVisClientAddress_Refresh_RemoveUnusedObjects.AcceptVisitor: boolean;
begin
  Result := (Visited is TClientAddressList);
end;

procedure TVisClientAddress_Refresh_RemoveUnusedObjects.Execute(
  const pVisited: TVisitedAbs);
var
  Address: TClientAddress;
  Counter: Integer;
  Data: TClientAddressList;
begin
  inherited;
  if AcceptVisitor then
  begin
    Data := pVisited as TClientAddressList;
    for Counter := Data.Count -1 downto 0 do
    begin
      Address := Data.Items[Counter];
      if Address.ObjectState = posCreate then
        Data.Remove(Address);
    end;
  end;
end;

{ TVisClientAddress_Update }

function TVisClientAddress_Update.AcceptVisitor: boolean;
begin
  Result := (Visited is TClientAddress) and
            (Visited.ObjectState = posUpdate);
end;

procedure TVisClientAddress_Update.Init;
begin
  Query.SQL.Text :=
    'UPDATE CLIENT_ADDRESS SET' +
    '    OWNER_OID = :OWNER_OID, ' +
    '    STREET = :STREET, ' +
    '    SUBURB = :SUBURB, ' +
    '    STATE = :STATE, ' +
    '    POSTCODE = :POSTCODE ' +
    'WHERE ' +
    '  OID = :OID';
end;

procedure TVisClientAddress_Update.SetupParams;
var
  Data: TClientAddress;
begin
  Data := Visited as TClientAddress;
  Data.OID.AssignToTiQuery(Query);
  Data.Owner.OID.AssignToTiQuery('OWNER_OID', Query);
  Query.ParamAsString['STREET'] := Data.Street;
  Query.ParamAsString['SUBURB'] := Data.Suburb;
  Query.ParamAsString['STATE'] := Data.State;
  Query.ParamAsString['POSTCODE'] := Data.PostCode;
end;

{ TVisClientAddress_Delete }

function TVisClientAddress_Delete.AcceptVisitor: boolean;
begin
  Result := (Visited is TClientAddress) and
            (Visited.ObjectState = posDelete);
end;

procedure TVisClientAddress_Delete.Init;
begin
  Query.SQL.Text :=
    'UPDATE CLIENT_ADDRESS SET' +
    '    DELETED = :DELETED ' +
    'WHERE ' +
    '  OID = :OID';
end;

procedure TVisClientAddress_Delete.SetupParams;
var
  Data: TClientAddress;
begin
  Data := Visited as TClientAddress;
  Data.OID.AssignToTiQuery(Query);
  Query.ParamAsBoolean['DELETED'] := True;
end;












initialization
  RegisterVisitors;



end.


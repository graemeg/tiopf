{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

    This file is part of the PBPro Pawnbroking System
    Copyright (c) 2003 Eventide Systems Pty Ltd.

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
unit pbpBusinessClasses;

interface

uses
  SysUtils, Graphics, Classes, Controls, Dialogs,

  tiPersist, tiPtnVisPerObj, tiPerObjOIDAbs,

  pbpClasses, pbpEvents, pbpTypes;

const
  BOTTOM_MARGIN: Integer = 4;

type
  TContractState = (csUnknown, csNew, csActive, csRedeemed, csExpired, csDeleted);
  TContractStates = set of TContractState;

{ TODO -oTT:2002-07-19 -cDeprecate :
cttPurchase is a legacy transaction from the days when PBLite was a
point of purchase tool }
  TContractTransactionType = (cttUnknown, cttActivate, cttExtend, cttExpire, cttPartPayment, cttRedeem,
    cttPurchase, cttAdditionalAmount);

  TClient = class;
  TClientAddress = class;
  TClientAddressList = class;
  TClientIdentityRecordList = class;
  TClientIdentityRecordType = class;
  TContractItemCategory = class;
  TContractItemList = class;
  TContractList = class;
  TContractPaymentType = class;
  TContractTransactionList = class;
  TManufacturer = class;
  TSystemValues = class;

  IClientListener = interface(IEventListener)
    ['{991E58F1-6CB3-444E-8695-5A9118624AC0}']
    procedure Changed(Sender: TClient);
  end;

  TCustomBusinessObject = class(TInterfacedPerObjAbs)
  private
    FInUse: Boolean;
    FListeners: TEventListenerList;
    procedure EnumDoChanged(Listener: IEventListener);
  public
    procedure AddListener(Listener: IPerObjAbsListener); virtual;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Changed; virtual;
    procedure RemoveListener(Listener: IPerObjAbsListener); virtual;
  published
    property InUse: Boolean read FInUse write FInUse;
  end;

  TCustomBusinessObjectList = class(TPerObjList)
  private
    FLastItemAdded: TPerObjAbs;
    FListeners: TEventListenerList;
    procedure EnumDoAdd(Listener: IEventListener);
  public
    procedure AfterConstruction; override;
    procedure Add(pObject: TPerObjAbs; pDefDispOrdr: Boolean = True); override;
    procedure AddListener(Listener: IPerObjListListener); virtual;
    procedure BeforeDestruction; override;
    procedure RemoveListener(Listener: IPerObjListListener); virtual;
  end;

  TClient = class(TCustomBusinessObject, IEventListenerList)
  private
    FEventListeners: TEventListenerList;
    FUndesirable: Boolean;
    FClientNumber: Integer;
    FGivenNames: string;
    FUndesirableReason: string;
    FFamilyName: string;
    FNotes: string;
    FIdentityRecords: TClientIdentityRecordList;
    FContracts: TContractList;
    FDateOfBirth: TDateTime;
    FPhoto: TPicture;
    FPhoneMobile: string;
    FPhoneHome: string;
    FPhoneWork: string;
    FEmailAddress: string;
    FUndesirableCode: string;
    FAddresses: TClientAddressList;
    FCurrentAddress: TClientAddress;
    procedure SetPhoto(const Value: TPicture);
    function GetCurrentAddress: TClientAddress;
  protected
    function GetCaption: string; override;
  public
    constructor CreateNew( pOwner : TPerObjAbs = nil ); override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Refresh;
    property CurrentAddress: TClientAddress read GetCurrentAddress write FCurrentAddress;
    property EventListeners: TEventListenerList read FEventListeners implements IEventListenerList;
  published
    property Addresses: TClientAddressList read FAddresses;
    property ClientNumber: Integer read FClientNumber write FClientNumber;
    property Contracts: TContractList read FContracts;
    property EmailAddress: string read FEmailAddress write FEmailAddress;
    property FamilyName: string read FFamilyName write FFamilyName;
    property GivenNames: string read FGivenNames write FGivenNames;
    property IdentityRecords: TClientIdentityRecordList read FIdentityRecords;
    property Notes: string read FNotes write FNotes;
    property DateOfBirth: TDateTime read FDateOfBirth write FDateOfBirth;
    property PhoneHome: string read FPhoneHome write FPhoneHome;
    property PhoneMobile: string read FPhoneMobile write FPhoneMobile;
    property PhoneWork: string read FPhoneWork write FPhoneWork;
    property Photo: TPicture read FPhoto write SetPhoto;
    property Undesirable: Boolean read FUndesirable write FUndesirable;
    property UndesirableCode: string read FUndesirableCode write FUndesirableCode;
    property UndesirableReason: string read FUndesirableReason write FUndesirableReason;
  end;

  TClientList = class(TCustomBusinessObjectList)
  protected
    function GetCaption: string; override;
    function GetItems(i: integer): TClient; reintroduce;
    procedure SetItems(i: integer; const Value: TClient); reintroduce;
  protected
    function GetOID: TOID; override;
  public
    procedure Add(pObject: TClient; pbDefaultDispOrder: boolean = true); reintroduce;
    function FindByProps(const pProps: array of string; const pVals: array of variant;
      pCaseSensitive : boolean = true ): TClient; reintroduce;
    property Items[i: integer]: TClient read GetItems write SetItems;
  end;

  TClientAddress = class(TPerObjAbs)
  private
    FPostCode: string;
    FSuburb: string;
    FStreet: string;
    FState: string;
    FInUse: Boolean;
  public
    property InUse: Boolean read FInUse write FInUse;
  published
    property Street: string read FStreet write FStreet;
    property Suburb: string read FSuburb write FSuburb;
    property State: string read FState write FState;
    property PostCode: string read FPostCode write FPostCode;

  end;

  TClientAddressList = class(TPerObjList)
  protected
    function GetItems(i: integer): TClientAddress; reintroduce;
    function GetOID: TOID; override;
    procedure SetItems(i: integer; const Value: TClientAddress); reintroduce;
  public
    procedure Add(pObject: TClientAddress; pbDefaultDispOrder: boolean = true); reintroduce;
    function Find(pOIDAsString: string): TClientAddress; reintroduce;
    property Items[i: integer]: TClientAddress read GetItems write SetItems;
  end;

  TClientIdentityRecord = class(TPerObjAbs)
  private
    FDetails: string;
    FIdentityRecordType: TClientIdentityRecordType;
    FSelected: Boolean;
    FInUse: Boolean;
  public
{ TODO -oTT -cComment :
Selected property used in contract form when persisting transactions. May not be
in use anymore, in which case deprecate }
    property Selected: Boolean read FSelected write FSelected;
  published
    property IdentityRecordType: TClientIdentityRecordType read FIdentityRecordType write FIdentityRecordType;
    property InUse: Boolean read FInUse write FInUse;
    property Details: string read FDetails write FDetails;
  end;

  TClientIdentityRecordProxy = class(TClientIdentityRecord)
  private
    FClientIdentityRecord: TClientIdentityRecord;
    function GetDetails: string;
    function GetIdentityRecordType: TClientIdentityRecordType;
    function GetIdentityRecordTypeName: string;
    function GetInUse: Boolean;
    procedure SetInUse(const Value: Boolean);
  protected
    function GetOID: TOID; override;
  public
    constructor Create(ClientIdentityRecord: TClientIdentityRecord); overload;
    property InUse: Boolean read GetInUse write SetInUse;
  published
{ TODO -oTT -cReview : These read-only properties are only for reporting purposes only. }
    property IdentityRecordTypeName: string read GetIdentityRecordTypeName;

    property IdentityRecordType read GetIdentityRecordType;
    property Details read GetDetails;
  end;

  TClientIdentityRecordList = class(TPerObjList)
  protected
    function GetItems(i: integer): TClientIdentityRecord; reintroduce;
    procedure SetItems(i: integer; const Value: TClientIdentityRecord); reintroduce;
  public
    function Find(pOIDAsString: string): TClientIdentityRecord; reintroduce;
    property Items[i: integer]: TClientIdentityRecord read GetItems write SetItems;
    procedure Add(pObject: TClientIdentityRecord; pbDefaultDispOrder: boolean = true); reintroduce;
  end;

  TClientIdentityRecordType = class(TPerObjAbs)
  private
    FName: string;
  protected
    function GetCaption: string; override;
  published
    property Name: string read FName write FName;
  end;

  TClientIdentityRecordTypeList = class(TPerObjList)
  private
  protected
    function GetCaption: string; override;
    function GetItems(i: integer): TClientIdentityRecordType; reintroduce;
    procedure SetItems(i: integer; const Value: TClientIdentityRecordType); reintroduce;
  public
    function Find(pOIDAsString: string): TClientIdentityRecordType; reintroduce;
    function FindByProps(const pProps: array of string; const pVals: array of variant;
      pCaseSensitive : boolean = true ): TClientIdentityRecordType; reintroduce;
    function FindOrCreateByName(AName: string): TClientIdentityRecordType;  
    procedure Add(pObject: TClientIdentityRecordType; pbDefaultDispOrder: boolean = true); reintroduce;
    property Items[i: integer]: TClientIdentityRecordType read GetItems write SetItems;
  end;

  TContract = class(TCustomBusinessObject, IClientListener)
  private
    FClientOID: TOID;
    FClientAddress: TClientAddress;
    FContractFee: Currency;
    FContractNumber: Integer;
    FEndDate: TDateTime;
    FExtensionNumber: Integer;
    FInterestRate: Double;
    FItems: TContractItemList;
    FTransactions: TContractTransactionList;
    FClientIdentityRecordProxies: TClientIdentityRecordList;
    FContractState: TContractState;
    FStartDate: TDateTime;
    FClient: TClient;
    FNotes: string;
    function GetClientFamilyName: string;
    function GetClientGivenNames: string;
    function GetInterestValue: Currency;
    function GetRedemptionValue: Currency;
    function GetLoanValue: Currency;
    function GetPreviousPaymentsTotal: Currency;
    function GetCurrentPaymentsTotal: Currency;
    procedure SetClient(const Value: TClient);
    function GetItemsSubtotal: Currency;
    function GetContractStateAsString: string;
    function GetContractNumberAsString: string;
    function GetContractTerms: string;
  protected
    function GetCaption: string; override;
    procedure ClientListener_Changed(Sender: TClient);
    procedure IClientListener.Changed = ClientListener_Changed;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property Client: TClient read FClient write SetClient;
    property ClientAddress: TClientAddress read FClientAddress write FClientAddress;
  published
    property ClientOID: TOID read FClientOID write FClientOID;
    property LoanValue: Currency read GetLoanValue;
    property PreviousPaymentsTotal: Currency read GetPreviousPaymentsTotal;
    property CurrentPaymentsTotal: Currency read GetCurrentPaymentsTotal;
    property ClientIdentityRecordProxies: TClientIdentityRecordList read FClientIdentityRecordProxies;
    property ContractFee: Currency read FContractFee write FContractFee;
    property ContractNumber: Integer read FContractNumber write FContractNumber;
    property EndDate: TDateTime read FEndDate write FEndDate;
    property ExtensionNumber: Integer read FExtensionNumber write FExtensionNumber;
    property InterestRate: Double read FInterestRate write FInterestRate;
    property InterestValue: Currency read GetInterestValue;
    property Notes: string read FNotes write FNotes;
    property RedemptionValue: Currency read GetRedemptionValue;
    property ContractState: TContractState read FContractState write FContractState;
    property StartDate: TDateTime read FStartDate write FStartDate;
    property Items: TContractItemList read FItems;
    property Transactions: TContractTransactionList read FTransactions;
{ TODO -oTT -cReview :
These derived properties are used in the Contract Finder, enables the
sorting of contracts by client name. They may be uneccessary now, as the
new techinsite framework may very well sort on derived columns. }
    property ClientGivenNames: string read GetClientGivenNames;
    property ClientFamilyName: string read GetClientFamilyName;
{ TODO -oTT -cReview :
These read-only properties are only for reporting purposes only. Possibly move them
out to a helper class.  }
    property ContractStateAsString: string read GetContractStateAsString;
    property ContractNumberAsString: string read GetContractNumberAsString;
    property ContractTerms: string read GetContractTerms;
  end;

  TContractItem = class(TPerObjAbs)
  private
    FCategory: TContractItemCategory;
    FDescription: string;
    FManufacturer: TManufacturer;
    FModelNumber: string;
    FNotes: string;
    FPhoto: TPicture;
    FQuantity: Integer;
    FSerialNumber: string;
    FValue: Currency;
    procedure SetPhoto(const Value: TPicture);
    function GetManufacturerName: string;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

  published
    property Category: TContractItemCategory read FCategory write FCategory;
    property Description: string read FDescription write FDescription;
    property Manufacturer: TManufacturer read FManufacturer write FManufacturer;

    property ModelNumber: string read FModelNumber write FModelNumber;
    property Notes: string read FNotes write FNotes;
    property Photo: TPicture read FPhoto write SetPhoto;
    property Quantity: Integer read FQuantity write FQuantity;
    property SerialNumber: string read FSerialNumber write FSerialNumber;
    property Value: Currency read FValue write FValue;
{ TODO -oTT -cRefactor : This is only to support report printing }
    property ManufacturerName: string read GetManufacturerName;    
  end;

  TContractItemList = class(TPerObjList)
  protected
    function GetCaption: string; override;
    function GetOID: TOID; override;
    function GetSubtotal: Currency;
    function GetItems(i: integer): TContractItem; reintroduce;
    procedure SetItems(i: integer; const Value: TContractItem); reintroduce;
  public
    property Items[i: integer]: TContractItem read GetItems write SetItems;
    procedure Add(pObject: TContractItem; pbDefaultDispOrder: boolean = true); reintroduce;
  published
    property SubTotal: Currency read GetSubtotal;
  end;

  TContractItemCategory = class(TPerObjAbs)
  private
    FName: string;
  protected
    function GetCaption: string; override;
  published
    property Name: string read FName write FName;
  end;

  TContractItemCategoryList = class(TPerObjList)
  protected
    function GetCaption: string; override;
    function GetItems(i: integer): TContractItemCategory; reintroduce;
    procedure SetItems(i: integer; const Value: TContractItemCategory); reintroduce;
  public
    procedure Add(pObject: TContractItemCategory; pbDefaultDispOrder: boolean = true); reintroduce;
    function Find(pOIDAsString: string): TContractItemCategory; reintroduce;
    function FindByProps( const pProps : array of string; const pVals  : array of variant;
      pCaseSensitive : boolean = true): TContractItemCategory ; reintroduce;
    property Items[i: integer]: TContractItemCategory read GetItems write SetItems;
  end;

  TContractList = class(TCustomBusinessObjectList)
  protected
    function GetCaption: string; override;
    function GetItems(i: integer): TContract; reintroduce;
    procedure SetItems(i: integer; const Value: TContract); reintroduce;
  public
    procedure Add(pObject: TContract; pbDefaultDispOrder: boolean = true); reintroduce;
    function FindByProps( const pProps : array of string; const pVals  : array of variant;
      pCaseSensitive : boolean = true): TContract ; reintroduce;
    property Items[i: integer]: TContract read GetItems write SetItems;
  end;

  TContractPaymentType = class(TPerObjAbs)
  private
    FDescription: string;
    FName: string;
    FRequiresDetails: Boolean;
  published
    property Description: string read FDescription write FDescription;
    property Name: string read FName write FName;
    property RequiresDetails: Boolean read FRequiresDetails write FRequiresDetails;
  end;

  TContractPaymentTypeList = class(TPerObjList)
  protected
    function GetItems(i: integer): TContractPaymentType; reintroduce;
    procedure SetItems(i: integer; const Value: TContractPaymentType); reintroduce;
  public
    procedure Add(pObject: TContractPaymentType; pbDefaultDispOrder: boolean = true); reintroduce;
    function Find(pOIDAsString: string): TContractPaymentType; reintroduce;
    property Items[i: integer]: TContractPaymentType read GetItems write SetItems;

  end;

  TContractTransaction = class(TPerObjAbs)
  private
    FDetails: string;
    FTimeStamp: TDateTime;
    FTransactionType: TContractTransactionType;
    FExtensionNumber: Integer;
    FValue: Currency;
    FPaymentType: TContractPaymentType;
  published
    property Details: string read FDetails write FDetails;
    property ExtensionNumber: Integer read FExtensionNumber write FExtensionNumber;
    property Value: Currency read FValue write FValue;
    property TimeStamp: TDateTime read FTimeStamp write FTimeStamp;
    property TransactionType: TContractTransactionType read FTransactionType write FTransactionType;
    property PaymentType: TContractPaymentType read FPaymentType write FPaymentType;
  end;

  TContractTransactionGateway = class(TObject)
  public
{ TODO -oTT -cRefactor : Will want to make this method private eventually }
    class procedure CommitTransaction(Contract: TContract; Value: Currency; Details: string;
      TransactionType: TContractTransactionType; PaymentType: TContractPaymentType = nil);
  public
    class procedure AdditionalAmount(Contract: TContract; Value: Currency; Details: string);
    class procedure ExpireContract(Contract: TContract; Details: string);
    class procedure ExtendContract(Contract: TContract; Value: Currency; Details: string);
    class procedure RedeemContract(Contract: TContract; Value: Currency; Details: string);
  end;

  TContractTransactionList = class(TPerObjList)
  protected
    function GetItems(i: integer): TContractTransaction; reintroduce;
    procedure SetItems(i: integer; const Value: TContractTransaction); reintroduce;
  public
    property Items[i: integer]: TContractTransaction read GetItems write SetItems;
    procedure Add(pObject: TContractTransaction; pbDefaultDispOrder: boolean = true); reintroduce;
  end;

{ TODO -oTT -cReview : There has got to be a better way of doing this }
  TContractTransactionListCashFlowReportQuery = class(TContractTransactionList)
  private
    FCashIn: Boolean;
    FStartDate: TDate;
    FEndDate: TDate;
  public
    property StartDate: TDate read FStartDate write FStartDate;
    property EndDate: TDate read FEndDate write FEndDate;
    property CashIn: Boolean read FCashIn write FCashIn;
  end;

  TManufacturer = class(TPerObjAbs)
  private
    FName: string;
  protected
    function GetCaption: string; override;
  published
    property Name: string read FName write FName;
  end;

  TManufacturerList = class(TPerObjList)
  protected
    function GetItems(i: integer): TManufacturer; reintroduce;
    procedure SetItems(i: integer; const Value: TManufacturer); reintroduce;
  protected
    function GetCaption: string; override;
  public
    procedure Add(pObject: TManufacturer; pbDefaultDispOrder: boolean = true); reintroduce;
    function Find(pOIDAsString: string): TManufacturer; reintroduce;
    function FindByProps( const pProps : array of string; const pVals  : array of variant;
      pCaseSensitive : boolean = true): TManufacturer ; reintroduce;
    property Items[i: integer]: TManufacturer read GetItems write SetItems;
  end;

  TPawnBroker = class(TPerObjAbs)
  private
    FClients: TClientList;
    FContracts: TContractList;
    FClientIdentityRecordTypes: TClientIdentityRecordTypeList;
    FContractItemCategories: TContractItemCategoryList;
    FName: string;
    FSystemValues: TSystemValues;
    FManufacturers: TManufacturerList;
    FContractPaymentTypes: TContractPaymentTypeList;
    FContactDetails: string;
  protected
    function GetCaption: string; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  published
    property Contracts: TContractList read FContracts;
    property ContactDetails: string read FContactDetails write FContactDetails;
    property Clients: TClientList read FClients;
    property ClientIdentityRecordTypes: TClientIdentityRecordTypeList read FClientIdentityRecordTypes;
    property ContractItemCategories: TContractItemCategoryList read FContractItemCategories;
    property ContractPaymentTypes: TContractPaymentTypeList read FContractPaymentTypes;
    property Manufacturers: TManufacturerList read FManufacturers;
    property Name: string read FName write FName;
    property SystemValues: TSystemValues read FSystemValues;
  end;

  TSystemValues = class(TPerObjAbs)
  private
    FContractPeriod: Integer;
    FContractFee: Currency;
    FContractInterestRate: Double;
    FDefaultContractPaymentType: TContractPaymentType;
    FContractTerms: string;
    FDefaultNumberOfCopiesToPrint: Integer;
  published
    property ContractPeriod: Integer read FContractPeriod write FContractPeriod;
    property ContractFee: Currency read FContractFee write FContractFee;
    property ContractInterestRate: Double read FContractInterestRate write FContractInterestRate;
    property DefaultContractPaymentType: TContractPaymentType read FDefaultContractPaymentType write FDefaultContractPaymentType;
    property ContractTerms: string read FContractTerms write FContractTerms;
    property DefaultNumberOfCopiesToPrint: Integer read FDefaultNumberOfCopiesToPrint write FDefaultNumberOfCopiesToPrint;
  end;

function ContractStateToStr(Status: TContractState): string;
function ContractTransactionTypeToStr(TransactionType: TContractTransactionType): string;
//  procedure NewClient;
//  procedure NewContract;

var
  PawnBroker: TPawnBroker;

implementation

function ContractStateToStr(Status: TContractState): string;
begin
  case Status of
    csUnknown: Result := 'Unknown';
    csNew: Result := '<new>';
    csActive: Result := 'Active';
    csRedeemed: Result := 'Redeemed';
    csExpired: Result := 'Expired';
    csDeleted: Result := 'Deleted';
  end;
end;

function ContractTransactionTypeToStr(TransactionType: TContractTransactionType): string;
begin
  case TransactionType of
    cttUnknown: Result := '<unknown>';
    cttActivate: Result := 'Activate';
    cttExtend: Result := 'Extend';
    cttExpire: Result := 'Expire';
    cttPartPayment: Result := 'Part payment';
    cttRedeem: Result := 'Redeem';
    cttPurchase: Result := 'Purchase';
    cttAdditionalAmount: Result := 'Additional amount';
  end;
end;

{ TCustomBusinessObject }

procedure TCustomBusinessObject.AddListener(Listener: IPerObjAbsListener);
begin
  FListeners.Add(Listener);
end;

procedure TCustomBusinessObject.AfterConstruction;
begin
  inherited;
  FListeners := TEventListenerList.Create;
end;

procedure TCustomBusinessObject.BeforeDestruction;
begin
  FListeners.Free;
  inherited;
end;

procedure TCustomBusinessObject.Changed;
begin
  FListeners.EnumListeners(EnumDoChanged);
end;

procedure TCustomBusinessObject.EnumDoChanged(Listener: IEventListener);
var
  PerObjAbsListener: IPerObjAbsListener;
begin
  if Supports(Listener, IID_PerObjAbsListener, PerObjAbsListener) then
  begin
    PerObjAbsListener.Changed(Self);
  end;
end;

procedure TCustomBusinessObject.RemoveListener(
  Listener: IPerObjAbsListener);
begin
  FListeners.Remove(Listener);
end;

{ TCustomBusinessObjectList }

procedure TCustomBusinessObjectList.Add(pObject: TPerObjAbs;
  pDefDispOrdr: Boolean);
begin
  inherited;
  FLastItemAdded := pObject;
  FListeners.EnumListeners(EnumDoAdd);
end;

procedure TCustomBusinessObjectList.AddListener(
  Listener: IPerObjListListener);
begin
  FListeners.Add(Listener);
end;

procedure TCustomBusinessObjectList.AfterConstruction;
begin
  inherited;
  FListeners := TEventListenerList.Create;
end;

procedure TCustomBusinessObjectList.BeforeDestruction;
begin
  FListeners.Free;
  inherited;
end;

procedure TCustomBusinessObjectList.EnumDoAdd(Listener: IEventListener);
var
  PerObjListListener: IPerObjListListener;
begin
  if Supports(Listener, IID_PerObjListListener, PerObjListListener) then
  begin
    PerObjListListener.ItemAdded(FLastItemAdded);
  end;
end;

procedure TCustomBusinessObjectList.RemoveListener(Listener: IPerObjListListener);
begin
  FListeners.Remove(Listener);
end;

{ TClient }

procedure TClient.AfterConstruction;
begin
  inherited;
  FEventListeners := TEventListenerList.Create;

  FIdentityRecords := TClientIdentityRecordList.Create;
  FIdentityRecords.Owner := Self;

  FAddresses := TClientAddressList.Create;
  FAddresses.Owner := Self;
end;

procedure TClient.BeforeDestruction;
begin
  FEventListeners.Free;
  FIdentityRecords.Free;
  FAddresses.Free;
  inherited;
end;

constructor TClient.CreateNew(pOwner: TPerObjAbs);
begin
  inherited;
  gTiPerMgr.VisMgr.Execute('read.defaultValues', Self);
  Self.ObjectState := posCreate;
end;

function TClient.GetCaption: string;
begin
  Result := '';
  if Self.FamilyName <> '' then
  begin
    Result := Self.FamilyName;
  end;

  if Self.GivenNames <> '' then
  begin
    if Result = '' then
      Result := Self.GivenNames
    else
      Result := Result + ', ' + Self.GivenNames;
  end;
end;

function TClient.GetCurrentAddress: TClientAddress;
begin
  Result := FCurrentAddress;
  if Result = nil then
    Result := Addresses.Last as TClientAddress;
end;

procedure TClient.Refresh;
begin
  gTiPerMgr.VisMgr.Execute('read.refresh', Self);
end;

procedure TClient.SetPhoto(const Value: TPicture);
begin
  FPhoto.Assign(Value);
end;

{ TClientList }

procedure TClientList.Add(pObject: TClient; pbDefaultDispOrder: boolean);
begin
  inherited Add(pObject, pbDefaultDispOrder);
end;

function TClientList.FindByProps(const pProps: array of string;
  const pVals: array of variant; pCaseSensitive: boolean): TClient;
begin
  Result := inherited FindByProps(pProps, pVals, pCaseSensitive) as TClient;
end;

function TClientList.GetCaption: string;
begin
  Result := 'Clients';
end;

function TClientList.GetItems(i: integer): TClient;
begin
  Result := TClient(inherited GetItems(i));
end;

function TClientList.GetOID: TOID;
begin
  if Owner <> nil then
    Result := Owner.OID
  else
    Result := nil;
end;

procedure TClientList.SetItems(i: integer; const Value: TClient);
begin
  inherited SetItems(i, Value);
end;

{ TClientAddressList }

procedure TClientAddressList.Add(pObject: TClientAddress;
  pbDefaultDispOrder: boolean);
begin
  inherited Add(pObject, pbDefaultDispOrder);
end;

function TClientAddressList.Find(pOIDAsString: string): TClientAddress;
begin
  Result := ( inherited Find( pOIDAsString )) as TClientAddress ;
end;

function TClientAddressList.GetItems(i: integer): TClientAddress;
begin
  Result := TClientAddress(inherited GetItems(i));
end;

function TClientAddressList.GetOID: TOID;
begin
  if Owner <> nil then
    Result := Owner.OID
  else
    Result := inherited GetOID;
end;

procedure TClientAddressList.SetItems(i: integer;
  const Value: TClientAddress);
begin
  inherited SetItems(i, Value);
end;

{ TClientIdentityRecordList }

procedure TClientIdentityRecordList.Add(pObject: TClientIdentityRecord;
  pbDefaultDispOrder: boolean);
begin
  inherited Add(pObject, pbDefaultDispOrder);
end;

function TClientIdentityRecordList.Find(
  pOIDAsString: string): TClientIdentityRecord;
begin
  Result := ( inherited Find( pOIDAsString )) as TClientIdentityRecord;
end;

function TClientIdentityRecordList.GetItems(
  i: integer): TClientIdentityRecord;
begin
  Result := TClientIdentityRecord(inherited GetItems(i));
end;

procedure TClientIdentityRecordList.SetItems(i: integer;
  const Value: TClientIdentityRecord);
begin
  inherited SetItems(i, Value);
end;

{ TClientIdentityRecordProxy }

function TClientIdentityRecordProxy.GetIdentityRecordTypeName: string;
begin
  Assert(IdentityRecordType <> nil);

  Result := IdentityRecordType.Name;
end;


{ TClientIdentityRecordType }

function TClientIdentityRecordType.GetCaption: string;
begin
  Result := FName;
end;

{ TClientIdentityRecordTypeList }

procedure TClientIdentityRecordTypeList.Add(
  pObject: TClientIdentityRecordType; pbDefaultDispOrder: boolean);
begin
  inherited Add(pObject, pbDefaultDispOrder);
end;

function TClientIdentityRecordTypeList.Find(
  pOIDAsString: string): TClientIdentityRecordType;
begin
  Result := ( inherited Find( pOIDAsString )) as TClientIdentityRecordType ;
end;

function TClientIdentityRecordTypeList.FindByProps(
  const pProps: array of string; const pVals: array of variant;
  pCaseSensitive: boolean): TClientIdentityRecordType;
begin
  Result := inherited FindByProps(pProps, pVals, pCaseSensitive) as TClientIdentityRecordType;
end;

function TClientIdentityRecordTypeList.FindOrCreateByName(
  AName: string): TClientIdentityRecordType;
begin
  Result := nil;

  if AName <> '' then
  begin
    Result := Self.FindByProps(['Name'], [AName], False);

    if not Assigned(Result) then
    begin
      Result := TClientIdentityRecordType.CreateNew;
      Result.Name := AName;
      Self.Add(Result);
      Self.Dirty := True;
    end;
  end;
end;

function TClientIdentityRecordTypeList.GetCaption: string;
begin
  Result := 'Client IdentityRecord Types';
end;

function TClientIdentityRecordTypeList.GetItems(
  i: integer): TClientIdentityRecordType;
begin
  Result := TClientIdentityRecordType(inherited GetItems(i));
end;

procedure TClientIdentityRecordTypeList.SetItems(i: integer;
  const Value: TClientIdentityRecordType);
begin
  inherited SetItems(i, Value);
end;

{ TContract }

procedure TContract.AfterConstruction;
begin
  inherited;
  FItems := TContractItemList.Create;
  FItems.Owner := Self;

  FClientIdentityRecordProxies := TClientIdentityRecordList.Create;
  FClientIdentityRecordProxies.Owner := Self;

  FTransactions := TContractTransactionList.Create;
  FTransactions.Owner := Self;
end;

procedure TContract.BeforeDestruction;
begin
  FClientIdentityRecordProxies.Free;
  FTransactions.Free;
  FItems.Free;
  inherited;
end;

procedure TContract.ClientListener_Changed(Sender: TClient);
begin
  ShowMessage('ClientChanged');
end;

function TContract.GetCaption: string;
begin
  Result := 'Contract #' + IntToStr(FContractNumber);
end;

function TContract.GetClientFamilyName: string;
begin
  Assert(Self.Client <> nil);

  Result := Self.Client.FamilyName;
end;

function TContract.GetClientGivenNames: string;
begin
  Assert(Self.Client <> nil);

  Result := Self.Client.GivenNames;
end;

function TContract.GetContractNumberAsString: string;
begin
  Result := IntToStr(Self.ContractNumber);
  if Self.ExtensionNumber > 0 then
  begin
    Result := Result + ' E' + IntToStr(Self.ExtensionNumber);
  end;
end;

function TContract.GetContractStateAsString: string;
begin
  Result := ContractStateToStr(Self.ContractState);
end;

function TContract.GetContractTerms: string;
begin
  Result := PawnBroker.SystemValues.ContractTerms;
end;

function TContract.GetCurrentPaymentsTotal: Currency;
var
  Counter: Integer;
  Transaction: TContractTransaction;
begin
{ TODO -oTT:2002-07-16 -cComment : Returns all part payments that have occured in current extensions }
  Result := 0;
  for Counter := 0 to FTransactions.Count - 1 do
  begin
    Transaction := FTransactions.Items[Counter];
    if (Transaction.TransactionType = cttPartPayment) and (Transaction.ExtensionNumber = Self.ExtensionNumber) then
      Result := Result + Transaction.Value;
  end;
end;

function TContract.GetInterestValue: Currency;
begin
  Result := (Self.InterestRate / 100) * Self.LoanValue;
end;

function TContract.GetItemsSubtotal: Currency;
begin
  Result := FItems.SubTotal;
end;

function TContract.GetLoanValue: Currency;
begin
  Result := FItems.SubTotal - PreviousPaymentsTotal;
end;

function TContract.GetPreviousPaymentsTotal: Currency;
var
  Counter: Integer;
  Transaction: TContractTransaction;
begin
{ TODO -oTT:2002-07-16 -cComment : Returns all part payments that have occured in previous extensions }
  Result := 0;
  for Counter := 0 to FTransactions.Count - 1 do
  begin
    Transaction := FTransactions.Items[Counter];
    if (Transaction.TransactionType = cttPartPayment) and (Transaction.ExtensionNumber < Self.ExtensionNumber) then
      Result := Result + Transaction.Value;
  end;
end;

function TContract.GetRedemptionValue: Currency;
begin
  Result := Self.LoanValue + Self.ContractFee + Self.InterestValue - Self.CurrentPaymentsTotal;
end;

procedure TContract.SetClient(const Value: TClient);
begin
  FClient := Value;

  if FClient <> nil then
    FClientOID := FClient.OID
  else
    FClientOID := nil;
end;

{ TContractItem }

procedure TContractItem.AfterConstruction;
begin
  inherited;
  FPhoto := TPicture.Create;
end;

procedure TContractItem.BeforeDestruction;
begin
  FPhoto.Free;
  inherited;
end;

function TContractItem.GetManufacturerName: string;
begin
  if FManufacturer = nil then
    Result := ''
  else
    Result := FManufacturer.Name;
end;

procedure TContractItem.SetPhoto(const Value: TPicture);
begin
  if Value <> nil then
    FPhoto.Assign(Value)
  else
    FPhoto.Assign(nil);
end;

{ TContractList }

procedure TContractList.Add(pObject: TContract;
  pbDefaultDispOrder: boolean);
begin
  inherited Add(pObject, pbDefaultDispOrder);
end;

function TContractList.FindByProps(const pProps: array of string;
  const pVals: array of variant; pCaseSensitive: boolean): TContract;
begin
  Result := (inherited FindByProps(pProps, pVals, pCaseSensitive)) as TContract;
end;

function TContractList.GetCaption: string;
begin
  Result := 'Contracts';
end;

function TContractList.GetItems(i: integer): TContract;
begin
  Result := TContract(inherited GetItems(i));
end;

procedure TContractList.SetItems(i: integer; const Value: TContract);
begin
  inherited SetItems(i, Value);
end;

{ TContractItem }

{ TContractItemCategory }

function TContractItemCategory.GetCaption: string;
begin
  Result := FName;
end;

{ TContractItemCategoryList }

procedure TContractItemCategoryList.Add(pObject: TContractItemCategory;
  pbDefaultDispOrder: boolean);
begin
  inherited Add(pObject, pbDefaultDispOrder);
end;

function TContractItemCategoryList.Find(pOIDAsString: string): TContractItemCategory;
begin
  Result := (inherited Find(pOIDAsString)) as TContractItemCategory;
end;

function TContractItemCategoryList.FindByProps(
  const pProps: array of string; const pVals: array of variant;
  pCaseSensitive: boolean): TContractItemCategory;
begin
  Result := (inherited FindByProps(pProps, pVals, pCaseSensitive)) as TContractItemCategory;
end;

function TContractItemCategoryList.GetCaption: string;
begin
  Result := 'Contract item categories';
end;

function TContractItemCategoryList.GetItems(
  i: integer): TContractItemCategory;
begin
  Result := TContractItemCategory(inherited GetItems(i));
end;

procedure TContractItemCategoryList.SetItems(i: integer;
  const Value: TContractItemCategory);
begin
  inherited SetItems(i, Value);
end;

{ TContractItemList }

procedure TContractItemList.Add(pObject: TContractItem;
  pbDefaultDispOrder: boolean);
begin
  inherited Add(pObject, pbDefaultDispOrder);
end;

function TContractItemList.GetCaption: string;
begin
  Result := 'Contract Items';
end;

function TContractItemList.GetItems(i: integer): TContractItem;
begin
  Result := TContractItem(inherited GetItems(i));
end;

function TContractItemList.GetOID: TOID;
begin
  if Owner <> nil then
    Result := Owner.OID
  else
    Result := nil;
end;

function TContractItemList.GetSubtotal: Currency;
var
  Counter: Integer;
begin
  Result := 0;
  for Counter := 0 to Count - 1 do
  begin
    if not (Items[Counter].ObjectState in [posDelete, posDeleted]) then
      Result := Result + Items[Counter].Value;
  end;
end;

procedure TContractItemList.SetItems(i: integer; const Value: TContractItem);
begin
  inherited SetItems(i, Value);
end;

{ TContractPaymentTypeList }

procedure TContractPaymentTypeList.Add(pObject: TContractPaymentType;
  pbDefaultDispOrder: boolean);
begin
  inherited Add(pObject, pbDefaultDispOrder);
end;

function TContractPaymentTypeList.Find(
  pOIDAsString: string): TContractPaymentType;
begin
  Result := (inherited Find(pOIDAsString)) as TContractPaymentType;
end;

function TContractPaymentTypeList.GetItems(
  i: integer): TContractPaymentType;
begin
  Result := inherited GetItems(i) as TContractPaymentType;
end;

procedure TContractPaymentTypeList.SetItems(i: integer;
  const Value: TContractPaymentType);
begin
  inherited SetItems(i, Value);
end;

{ TContractTransactionList }

procedure TContractTransactionList.Add(pObject: TContractTransaction;
  pbDefaultDispOrder: boolean);
begin
  inherited Add(pObject, pbDefaultDispOrder);
end;

function TContractTransactionList.GetItems(
  i: integer): TContractTransaction;
begin
  Result := TContractTransaction(inherited GetItems(i));
end;

procedure TContractTransactionList.SetItems(i: integer;
  const Value: TContractTransaction);
begin
  inherited SetItems(i, Value);
end;

{ TManufacturer }

function TManufacturer.GetCaption: string;
begin
  Result := FName;
end;

{ TManufacturerList }

procedure TManufacturerList.Add(pObject: TManufacturer;
  pbDefaultDispOrder: boolean);
begin
  inherited Add(pObject, pbDefaultDispOrder);
end;

function TManufacturerList.Find(pOIDAsString: string): TManufacturer;
begin
  Result := (inherited Find(pOIDAsString)) as TManufacturer;
end;

function TManufacturerList.FindByProps(const pProps: array of string;
  const pVals: array of variant; pCaseSensitive: boolean): TManufacturer;
begin
  Result := (inherited FindByProps(pProps, pVals, pCaseSensitive)) as TManufacturer;
end;

function TManufacturerList.GetCaption: string;
begin
  Result := 'Manufacturers';
end;

function TManufacturerList.GetItems(i: integer): TManufacturer;
begin
  Result := TManufacturer(inherited GetItems(i));
end;

procedure TManufacturerList.SetItems(i: integer;
  const Value: TManufacturer);
begin
  inherited SetItems(i, Value);
end;

{ TPawnBroker }

procedure TPawnBroker.AfterConstruction;
begin
  inherited;
  FClientIdentityRecordTypes := TClientIdentityRecordTypeList.Create;
  FClients := TClientList.Create;
  FClients.Owner := Self;

  FContracts := TContractList.Create;
  FContracts.Owner := Self;

  FContractItemCategories := TContractItemCategoryList.Create;
  FContractItemCategories.Owner := Self;

  FContractPaymentTypes := TContractPaymentTypeList.Create;
  FContractPaymentTypes.Owner := Self;

  FManufacturers := TManufacturerList.Create;
  FManufacturers.Owner := Self;

  FSystemValues := TSystemValues.Create;
  FSystemValues.Owner := Self;
end;

procedure TPawnBroker.BeforeDestruction;
begin
  FClients.Free;
  FContracts.Free;
  FContractItemCategories.Free;
  FClientIdentityRecordTypes.Free;
  FManufacturers.Free;
  FSystemValues.Free;
  FContractPaymentTypes.Free;
  inherited;
end;

function TPawnBroker.GetCaption: string;
begin
  Result := FName;
end;

{ TClientIdentityRecordProxy }

constructor TClientIdentityRecordProxy.Create(
  ClientIdentityRecord: TClientIdentityRecord);
begin
  inherited Create;
  FClientIdentityRecord := ClientIdentityRecord;
  Assert(FClientIdentityRecord <> nil);
end;

function TClientIdentityRecordProxy.GetDetails: string;
begin
  Result := FClientIdentityRecord.Details;
end;

function TClientIdentityRecordProxy.GetIdentityRecordType: TClientIdentityRecordType;
begin
  Result := FClientIdentityRecord.IdentityRecordType;
end;

function TClientIdentityRecordProxy.GetOID: TOID;
begin
  Result := FClientIdentityRecord.OID;
end;

function TClientIdentityRecordProxy.GetInUse: Boolean;
begin
  Result := FClientIdentityRecord.InUse;
end;

procedure TClientIdentityRecordProxy.SetInUse(const Value: Boolean);
begin
  FClientIdentityRecord.InUse := Value;
end;

{ TContractTransactionGateway }

class procedure TContractTransactionGateway.AdditionalAmount(
  Contract: TContract; Value: Currency; Details: string);
var
  ContractItem: TContractItem;
begin
  Assert(Contract.ContractState = csActive);

  CommitTransaction(Contract, Value, Details, cttAdditionalAmount, nil);
  ContractItem := TContractItem.CreateNew;
  ContractItem.Description := '<additional amount>';
  ContractItem.Value := - Value;
  Contract.Items.Add(ContractItem);
  Contract.Dirty := True;
  Contract.Save;
end;

class procedure TContractTransactionGateway.CommitTransaction(Contract: TContract; Value: Currency; Details: string;
  TransactionType: TContractTransactionType; PaymentType: TContractPaymentType);
var
  NewTransaction: TContractTransaction;
begin
{ TODO -oTT -cRefactor : Split this up into a transaction gateway }
  Assert(Contract <> nil, 'Contract cannot be nil');
  Assert(Contract.Client <> nil, 'Contract.Client cannot be nil');

  NewTransaction := TContractTransaction.CreateNew;

  NewTransaction.Details := Details;
  NewTransaction.ExtensionNumber := Contract.ExtensionNumber;
  NewTransaction.PaymentType := PaymentType;
  NewTransaction.Value := Value;
  NewTransaction.TimeStamp := Now;
  NewTransaction.TransactionType := TransactionType;

  Contract.ClientAddress := Contract.Client.CurrentAddress;

  Contract.Transactions.Add(NewTransaction);
  NewTransaction.Save;
end;


class procedure TContractTransactionGateway.ExpireContract(
  Contract: TContract; Details: string);
begin
  Assert(Contract.ContractState = csActive);

  CommitTransaction(Contract, 0, Details, cttExpire, nil);
  Contract.ContractState := csExpired;
  Contract.Dirty := True;
  Contract.Save;
end;

class procedure TContractTransactionGateway.ExtendContract(
  Contract: TContract; Value: Currency; Details: string);
begin
  Assert(Contract.ContractState = csActive);

  CommitTransaction(Contract, Value, Details, cttExtend, nil);
  Contract.ExtensionNumber := Contract.ExtensionNumber + 1;
  Contract.StartDate := Contract.EndDate + 1;
  Contract.EndDate := Contract.StartDate + PawnBroker.SystemValues.ContractPeriod;
  Contract.Dirty := True;
  Contract.Save;
end;

class procedure TContractTransactionGateway.RedeemContract(
  Contract: TContract; Value: Currency; Details: string);
begin
  Assert(Contract.ContractState = csActive);

  CommitTransaction(Contract, Value, Details, cttRedeem, nil);
  Contract.ContractState := csRedeemed;
  Contract.Dirty := True;
  Contract.Save;
end;






initialization
  PawnBroker := TPawnBroker.Create;
  RegisterClasses([ TClient,
                    TClientAddress,
                    TClientIdentityRecord,
                    TClientIdentityRecordProxy,
                    TContract,
                    TContractItem,
                    TContractTransaction,
                    TPawnbroker,
                    TSystemValues
                    ]);

finalization
  PawnBroker.Free;

end.


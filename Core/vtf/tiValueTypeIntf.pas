{
  Main unit containing interfaces for the Value Type Framework.
  
  NOTE:  All still work in progress!
}
unit tiValueTypeIntf;

{$i vtfDefines.inc}

interface

uses
  Classes, SysUtils, tiTypes, tiSubjectIntf, fpchelper;
  
  
type

  ItiState = interface(IInterface)
    ['{FFDAF6E0-F764-4511-8BEC-36772D90D5A2}']
    function  GetLoaded: boolean;
    function  GetDeleting: boolean;
    function  GetDeleted: boolean;
    function  GetModified: boolean;
    function  GetPersisted: boolean;
    procedure Assign(const Source: ItiState);
    procedure LoadFromStream(const Reader: TReader);
    procedure SaveToStream(const Writer: TWriter);
    procedure StateChanged(const Sender: IInterface; const NotifyType: TtiNotifyType);
    property  Loaded: boolean read GetLoaded;
    property  Deleting: boolean read GetDeleting;
    property  Deleted: boolean read GetDeleted;
    property  Modified: boolean read GetModified;
    property  Persisted: boolean read GetPersisted;
  end;


  { Object owned by other object }
  ItiOwnedObject = interface(IInterface)
    ['{3693351D-5C26-461E-9150-479D99D613F7}']
    function  Implementor: TObject;
    function  ClassName: string;
    function  GetOwner: IInterface;
    procedure SetOwner(const Value: IInterface);
    property  Owner: IInterface read GetOwner write SetOwner;
  end;


  { Basic attribute/value type interface }
  ItiValueType = interface(ItiOwnedObject)
    ['{E48E7D5B-AE6E-445F-AD94-D2BE83F7E34A}']
    function  Clone: ItiValueType;
    function  GetState: ItiState;
    function  GetSubject: ItiSubject;
    procedure Assign(const Source: ItiValueType);
    procedure Notify(const Sender: IInterface; const NotifyType: TtiNotifyType);
    property  State: ItiState read GetState;
    property  Subject: ItiSubject read GetSubject;
  end;


  { Interface for accessing members (attributes) }
  ItiMemberType = interface(ItiValueType)
    ['{D9818C94-3C8E-4DA1-B9AF-4A61302120ED}']
    function  GetAsString: string;
    function  GetName: string;
    function  GetState: ItiState;
    procedure SetAsString(const Value: string);
    function  IsNull: boolean;
    procedure Changed;
    procedure Clear;
    function  LoadNullFromStream(const Reader: TReader): boolean;
    function  SaveNullToStream(const Writer: TWriter): boolean;
    procedure LoadFromStream(const Reader: TReader);
    procedure SaveToStream(const Writer: TWriter);
    property  AsString: string read GetAsString write SetAsString;
    property  Name: string read GetName;
    property  State: ItiState read GetState;
  end;


  ItiBlobType = interface(ItiMemberType)
    ['{67437B35-2361-4403-823F-364C6C37900F}']
    function  GetValue: TStream;
    procedure SetAsString(const Value: string);
    procedure SetValue(const Value: TStream);
    property  Value: TStream read GetValue write SetValue;
  end;


  ItiBooleanType = interface(ItiMemberType)
    ['{13400ADD-A3EE-4235-B2F2-285D6B833A99}']
    function  GetValue: boolean;
    procedure SetValue(const Value: boolean);
    property  Value: boolean read GetValue write SetValue;
  end;


  ItiCharType = interface(ItiMemberType)
    ['{B8D024F7-4A83-4A31-A3F0-1159648C6019}']
    function  GetValue: Char;
    procedure SetValue(const Value: Char);
    property  Value: Char read GetValue write SetValue;
  end;


  ItiDateType = interface(ItiMemberType)
    ['{EFEE099D-2938-4124-BD32-4907F7084000}']
    function  AsISOTimeStamp: TISOTimeStamp;
    function  AsTimeStampString: string;
    function  AsTimeStamp: TTimeStamp;
    function  GetValue: TDateTime;
    procedure SetValue(const Value: TDateTime);
    property  Value: TDateTime read GetValue write SetValue;
  end;


  ItiCurrencyType = interface(ItiMemberType)
    ['{D5752765-63B0-4470-ABF6-540A589EFDCD}']
    function  AsInteger: Integer;
    function  GetValue: Currency;
    procedure SetValue(const Value: Currency);
    property  Value: Currency read GetValue write SetValue;
  end;


  ItiFloatType = interface(ItiMemberType)
    ['{2A49004F-9A59-4A7E-B8C7-3EB0A865AEA5}']
    function  AsInteger: Integer;
    function  GetValue: Double;
    procedure SetValue(const Value: Double);
    property  Value: Double read GetValue write SetValue;
  end;


  ItiIntegerType = interface(ItiMemberType)
    ['{776866D9-C9BD-41E5-879B-A739EBFCB724}']
    function  GetValue: Integer;
    procedure SetValue(const Value: Integer);
    property  Value: Integer read GetValue write SetValue;
  end;


  ItiLongIntType = interface(ItiMemberType)
    ['{6374F963-CEA9-46D2-A257-83ADC88D550E}']
    function  GetValue: LongInt;
    procedure SetValue(const Value: LongInt);
    property  Value: LongInt read GetValue write SetValue;
  end;


  ItiMemoType = interface(ItiMemberType)
    ['{C3184C71-F9B4-40EF-9365-1513574D7C5D}']
    function  GetValue: TStrings;
    procedure SetValue(const Value: TStrings);
    procedure MemoChanged(Sender: TObject);
    property  Value: TStrings read GetValue write SetValue;
  end;


  ItiSmallIntType = interface(ItiMemberType)
    ['{DF713258-D3B2-4DF0-B018-2F671C0A8192}']
    function  GetValue: SmallInt;
    procedure SetValue(const Value: SmallInt);
    property  Value: SmallInt read GetValue write SetValue;
  end;


  ItiStringType = interface(ItiMemberType)
    ['{EE8EEA90-35ED-410D-B0B8-EA2500D8E3A1}']
    function  GetValue: string;
    procedure SetValue(const Value: string);
    property  Value: string read GetValue write SetValue;
  end;


  ItiWideStringType = interface(ItiMemberType)
    ['{56D3819F-54E0-447F-A756-FAF86AF7AF46}']
    function  GetValue: WideString;
    procedure SetValue(const Value: WideString);
    property  Value: WideString read GetValue write SetValue;
  end;


  TtiMemberType = (mtMember, mtObject, mtList);


implementation

end.


unit tiNullField;

//Example of how to introduce NULL FieldList to the TechInsite business classes.
//I use this method for my own (simple) framework to "reduce" network traffic.

interface

uses
  Windows, Classes, SysUtils, Forms, ContNrs, Db, TypInfo, Graphics, Controls,
  Dialogs, tiPtnVisPerObj, Variants;

type
  PtiFieldState = ^TtiFieldState;
  TtiFieldState = set of(ofModified, ofNull);

  TPerObjAbsNull = class(TPerObjAbs)
  private
    //A small array to include the field statusses
    FFieldList: array of TtiFieldState;
    function GetFieldCount: integer;
    property FieldCount: integer read GetFieldCount;
    function GetFieldState(Index: integer): TtiFieldState;
    function GetField(AProp: string): PtiFieldState;overload;
    function GetField(AProp: PPropInfo): PtiFieldState;overload;
    procedure SetFieldState(Index: integer;const Value: TtiFieldState);
  public
    constructor Create;override;

    //This will make the record dirty & will mark the "field" as modified
    procedure CheckOutValue(AProp: string);overload;
    procedure CheckOutValue(AProp: string;var AStore: byte;AValue: byte);overload;
    procedure CheckOutValue(AProp: string;var AStore: char;AValue: char);overload;
    procedure CheckOutValue(AProp: string;var AStore: word;AValue: word);overload;
    procedure CheckOutValue(AProp: string;var AStore: TDate;AValue: TDate);overload;
    procedure CheckOutValue(AProp: string;var AStore: TTime;AValue: TTime);overload;
    procedure CheckOutValue(AProp: string;var AStore: int64;AValue: int64);overload;
    procedure CheckOutValue(AProp: string;var AStore: Double;AValue: Double);overload;
    procedure CheckOutValue(AProp: string;var AStore: string;AValue: string);overload;
    procedure CheckOutValue(AProp: string;var AStore: integer;AValue: integer);overload;
    procedure CheckOutValue(AProp: string;var AStore: boolean;AValue: boolean);overload;
    procedure CheckOutValue(AProp: string;var AStore: Extended;AValue: Extended);overload;
    procedure CheckOutValue(AProp: string;var AStore: shortint;AValue: shortint);overload;
    procedure CheckOutValue(AProp: string;var AStore: smallint;AValue: smallint);overload;
    procedure CheckOutValue(AProp: string;var AStore: Currency;AValue: Currency);overload;
    procedure CheckOutValue(AProp: string;var AStore: TDateTime;AValue: TDateTime);overload;
    procedure CheckOutValue(AProp: string;var AStore: TTimeStamp;AValue: TTimeStamp);overload;

    //Support routines
    procedure SetNull(AProp: string);
    function IsNull(AProp: string): boolean;
    property FieldList[Index: integer]: TtiFieldState read GetFieldState write SetFieldState;
  end;


  //Person class (Not all properties & methods are included for this demo)
  TPerson = class(TPerObjAbsNull)
  private
    FsFirstname: string;
    FsLastName: string;
    FsInitials: string;
    FsTitle: string;
    FsNotes: string;
    procedure SetFirstName(const Value: string);
    procedure SetInitials(const Value: string);
    procedure SetLastName(const Value: string);
    procedure SetNotes(const Value: string);
    procedure SetTitle(const Value: string);
  published
    //Just introduce a setter for each field property
    property LastName: string read FsLastName write SetLastName;
    property FirstName: string read FsFirstname write SetFirstName;
    property Title: string read FsTitle write SetTitle;
    property Initials: string read FsInitials write SetInitials;
    property Notes: string read FsNotes write SetNotes;
  end;


implementation


{ TPerObjAbsNull }


constructor TPerObjAbsNull.Create;
var Data: PTypeData;
  i: integer;
begin
  inherited;
  Data := GetTypeData(ClassInfo);
  SetLength(FFieldList, Data.PropCount);
  for i := 0 to FieldCount - 1 do FieldList[i] := [ofNull];
end;

function TPerObjAbsNull.GetFieldCount: integer;
begin
  //+1 so we can use FieldCount-1
  Result := High(FFieldList) + 1;
end;

function TPerObjAbsNull.GetFieldState(Index: integer): TtiFieldState;
begin
  Result := FFieldList[Index];
end;

procedure TPerObjAbsNull.SetFieldState(Index: integer;const Value: TtiFieldState);
begin
  FFieldList[Index] := Value;
end;

function TPerObjAbsNull.GetField(AProp: PPropInfo): PtiFieldState;
begin
  //The name index is simply used as an index in the array!
  Result := @FFieldList[AProp^.NameIndex];
end;

function TPerObjAbsNull.GetField(AProp: string): PtiFieldState;
var oProp:PPropInfo;
begin
  oProp:=GetPropInfo(ClassInfo, AProp);
  if not Assigned(oProp)
  then raise Exception.CreateFmt('Property %s.%s does not exist',[ClassName, AProp]);
  Result := GetField(oProp);
end;

procedure TPerObjAbsNull.SetNull(AProp: string);
var FieldState: PtiFieldState;
begin
  FieldState := GetField(AProp);
  FieldState^ :=[ofNull];
end;

function TPerObjAbsNull.IsNull(AProp: string): boolean;
var FieldState: PtiFieldState;
begin
  FieldState := GetField(AProp);
  Result := ofNull in FieldState^;
end;

procedure TPerObjAbsNull.CheckOutValue(AProp: string;var AStore: string;AValue: string);
var FieldState: PtiFieldState;
begin
  FieldState := GetField(AProp);
  if(ofNull in FieldState^)or(AStore <> AValue) then begin
    AStore := AValue;
    FieldState^ :=[ofModified];
    Dirty := true
  end;
end;

procedure TPerObjAbsNull.CheckOutValue(AProp: string);
var FieldState: PtiFieldState;
begin
  FieldState := GetField(AProp);
  if ofNull in FieldState^ then begin
    FieldState^ :=[ofModified];
    Dirty := true
  end;
end;

procedure TPerObjAbsNull.CheckOutValue(AProp: string;var AStore: TDateTime;AValue: TDateTime);
var FieldState: PtiFieldState;
begin
  FieldState := GetField(AProp);
  if(ofNull in FieldState^)or(AStore <> AValue) then begin
    AStore := AValue;
    if AValue<>0
    then FieldState^ :=[ofModified]
    else FieldState^ :=[ofModified,ofNull];
    Dirty := true
  end;
end;

procedure TPerObjAbsNull.CheckOutValue(AProp: string;var AStore: TDate;AValue: TDate);
var FieldState: PtiFieldState;
begin
  FieldState := GetField(AProp);
  if(ofNull in FieldState^)or(AStore <> AValue) then begin
    AStore := AValue;
    if AValue<>0
    then FieldState^ :=[ofModified]
    else FieldState^ :=[ofModified,ofNull];
    Dirty := true
  end;
end;

procedure TPerObjAbsNull.CheckOutValue(AProp: string;var AStore: TTime;AValue: TTime);
var FieldState: PtiFieldState;
begin
  FieldState := GetField(AProp);
  if(ofNull in FieldState^)or(AStore <> AValue) then begin
    AStore := AValue;
    if AValue<>0
    then FieldState^ :=[ofModified]
    else FieldState^ :=[ofModified,ofNull];
    Dirty := true
  end;
end;

procedure TPerObjAbsNull.CheckOutValue(AProp: string;var AStore: char;AValue: char);
var FieldState: PtiFieldState;
begin
  FieldState := GetField(AProp);
  if(ofNull in FieldState^)or(AStore <> AValue) then begin
    AStore := AValue;
    FieldState^ :=[ofModified];
    Dirty := true
  end;
end;

procedure TPerObjAbsNull.CheckOutValue(AProp: string;var AStore: integer;AValue: integer);
var FieldState: PtiFieldState;
begin
  FieldState := GetField(AProp);
  if(ofNull in FieldState^)or(AStore <> AValue) then begin
    AStore := AValue;
    FieldState^ :=[ofModified];
    Dirty := true
  end;
end;

procedure TPerObjAbsNull.CheckOutValue(AProp: string;var AStore: Double;AValue: Double);
var FieldState: PtiFieldState;
begin
  FieldState := GetField(AProp);
  if(ofNull in FieldState^)or(AStore <> AValue) then begin
    AStore := AValue;
    FieldState^ :=[ofModified];
    Dirty := true
  end;
end;

procedure TPerObjAbsNull.CheckOutValue(AProp: string;var AStore: Extended;AValue: Extended);
var FieldState: PtiFieldState;
begin
  FieldState := GetField(AProp);
  if(ofNull in FieldState^)or(AStore <> AValue) then begin
    AStore := AValue;
    FieldState^ :=[ofModified];
    Dirty := true
  end;
end;

procedure TPerObjAbsNull.CheckOutValue(AProp: string;var AStore: boolean;AValue: boolean);
var FieldState: PtiFieldState;
begin
  FieldState := GetField(AProp);
  if(ofNull in FieldState^)or(AStore <> AValue) then begin
    AStore := AValue;
    FieldState^ :=[ofModified];
    Dirty := true
  end;
end;

procedure TPerObjAbsNull.CheckOutValue(AProp: string;var AStore: smallint;AValue: smallint);
var FieldState: PtiFieldState;
begin
  FieldState := GetField(AProp);
  if(ofNull in FieldState^)or(AStore <> AValue) then begin
    AStore := AValue;
    FieldState^ :=[ofModified];
    Dirty := true
  end;
end;

procedure TPerObjAbsNull.CheckOutValue(AProp: string;var AStore: byte;AValue: byte);
var FieldState: PtiFieldState;
begin
  FieldState := GetField(AProp);
  if(ofNull in FieldState^)or(AStore <> AValue) then begin
    AStore := AValue;
    FieldState^ :=[ofModified];
    Dirty := true
  end;
end;

procedure TPerObjAbsNull.CheckOutValue(AProp: string;var AStore: word;AValue: word);
var FieldState: PtiFieldState;
begin
  FieldState := GetField(AProp);
  if(ofNull in FieldState^)or(AStore <> AValue) then begin
    AStore := AValue;
    FieldState^ :=[ofModified];
    Dirty := true
  end;
end;

procedure TPerObjAbsNull.CheckOutValue(AProp: string;var AStore: int64;AValue: int64);
var FieldState: PtiFieldState;
begin
  FieldState := GetField(AProp);
  if(ofNull in FieldState^)or(AStore <> AValue) then begin
    AStore := AValue;
    FieldState^ :=[ofModified];
    Dirty := true
  end;
end;

procedure TPerObjAbsNull.CheckOutValue(AProp: string;var AStore: TTimeStamp;AValue: TTimeStamp);
var FieldState: PtiFieldState;
begin
  FieldState := GetField(AProp);
  if(ofNull in FieldState^)or(int64(AStore) <> int64(AValue)) then begin
    AStore := AValue;
    FieldState^ :=[ofModified];
    Dirty := true
  end;
end;

procedure TPerObjAbsNull.CheckOutValue(AProp: string;var AStore: shortint;AValue: shortint);
var FieldState: PtiFieldState;
begin
  FieldState := GetField(AProp);
  if(ofNull in FieldState^)or(AStore <> AValue) then begin
    AStore := AValue;
    FieldState^ :=[ofModified];
    Dirty := true
  end;
end;

procedure TPerObjAbsNull.CheckOutValue(AProp: string;var AStore: Currency;AValue: Currency);
var FieldState: PtiFieldState;
begin
  FieldState := GetField(AProp);
  if(ofNull in FieldState^)or(AStore <> AValue) then begin
    AStore := AValue;
    FieldState^ :=[ofModified];
    Dirty := true
  end;
end;

{ TPerson }

procedure TPerson.SetFirstName(const Value: string);
begin
  //This will make the record dirty & will mark the "field" as modified
  CheckOutValue('Firstname',FsFirstname,Value);
end;

procedure TPerson.SetInitials(const Value: string);
begin
  CheckOutValue('Initials',FsInitials,Value);
end;

procedure TPerson.SetLastName(const Value: string);
begin
  CheckOutValue('LastName',FsLastName,Value);
end;

procedure TPerson.SetNotes(const Value: string);
begin
  CheckOutValue('Notes',FsNotes,Value);
end;

procedure TPerson.SetTitle(const Value: string);
begin
  CheckOutValue('Title',FsTitle,Value);
end;

end.


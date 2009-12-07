
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
The contents of this file are subject to the Mozilla Public
License Version 1.1 (the "License"); you may not use this file
except in compliance with the License. You may obtain a copy of
the License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS
IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
implied. See the License for the specific language governing
rights and limitations under the License.

If you make any changes or enhancements, which you think will
benefit other developers and will not break any existing code,
please forward your changes (well commented) to mariusellen@home.nl
and I will make them permanent.

Revision history:

  20-07-2003:
    Complete rewrite of the dataset components based on other
    existing datasets. All components have been renamed so the existing
    older TOpfDataset and TOpfNestedDataset are not affected.
    TtiRecordDataset is new in this release.

  24-07-2003:
    Fixed the BookMarkValid function, which failed in combination with
    filters & ShowDeleted. It could cause "Record not found" messages.
    Also some other (minor) changes which could cause AV's.

  22-05-2005:
    Added the TTiNestedRecordDataset. And some other stuff i have used
    for a while now in my own Opf. + Corrections to the faulty BlobStream.

  10-06-2007
    Altered for tiOpf2
    Removed ObjectClassName as it is of no use.  TtiObject no longer descends from TPersistant
    Altered Delete and RecordCount

    4-11-2008
    Altered Delphi 2009 (Sean Cross)
Purpose:
  Browse, edit and Create business objects as easy as the normal RAD
  approach. (The populair DB components & reportbuilders can be used)

ToDo:
  Well, if anybody got ideas for this section, please email them!

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$I tiDefines.inc}
unit tiDataset;

interface

uses
  Classes, SysUtils, Forms, Db, TypInfo, Graphics, Controls,
  {$IFDEF DELPHI6ORABOVE} Variants, {$ENDIF} {$ifndef fpc}SqlTimSt,{$endif} 
  tiObject {$ifdef UNICODE}, WideStrUtils {$endif}, tiUtils;

type
{$IFDEF UNICODE}
  PByteOrChar = PByte;
{$ELSE}
  PByteOrChar = PChar;
{$ENDIF}

  TTiListSortCompare = function(Item1, Item2: TObject): integer of object;

  PtiObjectBookmark = ^TTiObjectBookmark;
  TTiObjectBookmark = record
    BookMarkFlag: TBookmarkFlag;
    BookMarkObject: TtiObject; //Current object
    BookMarkBackup: TtiObject; //Backup of object in case of edit&insert mode
  end;


  TTiCustomDataset = class(TDataset)
  private
    FParentDataSet: TTiCustomDataset;
    FCursorOpen: boolean;
    FFilterBuffer: PByteOrChar;
    FStringWidth: integer;
//    FObjectClassName: string;
    FStartCalculated: integer;
    FObjectFields: TStringlist;
    FObjectClass: TtiObjectClass;
    FLastParentPos, FObjectIndex: integer;
    FSortColumn: string;
    FSortAscending: boolean;
    FObjectDepth: integer;
    function SortIt(Item1, Item2: TObject): integer;
//    procedure SetObjectClassName(const Value: string);
    procedure SetObjectClass(const Value: TtiObjectClass);
    function MatchFieldValue(Field: TField;Value: Variant;Options: TLocateOptions): boolean;
    procedure QuickSort(SortList: TList;l, R: integer;SCompare: TTiListSortCompare);
    function GetPropertyInfo(var oObject: TClass;var PropInfo: PPropInfo;cFieldName: string): boolean;overload;
    function GetPropertyInfo(var oObject: TObject;var PropInfo: PPropInfo;cFieldName: string): boolean;overload;
    function LocateRecord(const KeyFields: string;const KeyValues: Variant;Options: TLocateOptions;SyncCursor: boolean): boolean;
    procedure SetSortColumn(const Value: string);
    function GetObjectClass: TtiObjectClass;
    procedure SetObjectDepth(const Value: integer);
    procedure SetSortAscending(const Value: boolean);
  protected
    //Observer: TOpfFieldObserver;
    procedure CheckObjectClass;virtual;
    function NewRecord: TtiObject;virtual;abstract;
    procedure DelRecord(oObject: TtiObject);virtual;abstract;

    procedure InternalClose;override;
    procedure InternalOpen;override;
    procedure InternalEdit;override;
    procedure InternalCancel;override;
    procedure InternalDelete;override;
    procedure InternalInsert;override;
    procedure InternalPost;override;
    procedure InternalInitFieldDefs;override;
    procedure InternalHandleException;override;
    procedure InternalInitRecord(Buffer: PByteOrChar);override;
    procedure InternalSetToRecord(Buffer: PByteOrChar);override;
    procedure InternalAddRecord(Buffer: pointer;Append: boolean);override;
    procedure Notification(AComponent: TComponent;Operation: TOperation);override;


    procedure DoAfterOpen;override;
    procedure DoAfterInsert;override;
    function IsCursorOpen: boolean;override;
    function GetRecordSize: word;override;
    function AllocRecordBuffer: PByteOrChar;override;
    function GetActiveRecordBuffer: PByteOrChar;virtual;
    procedure ClearCalcFields(Buffer: PByteOrChar);override;
    procedure FreeRecordBuffer(var Buffer: PByteOrChar);override;
    procedure Sort(Compare: TTiListSortCompare);virtual;abstract;
    function GetBookmarkIndex(Bookmark: pointer): integer;virtual;
    procedure SetFieldData(Field: TField;Buffer: pointer);override;
    procedure GetBookmarkData(Buffer: PByteOrChar;Data: pointer);override;
    procedure SetBookmarkData(Buffer: PByteOrChar;Data: pointer);override;
    function GetBookmarkFlag(Buffer: PByteOrChar): TBookmarkFlag;override;
    procedure SetBookmarkFlag(Buffer: PByteOrChar;Value: TBookmarkFlag);override;
    property SortColumn: string read FSortColumn write SetSortColumn;
    property SortAscending: boolean read FSortAscending write SetSortAscending;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    function GetActiveItem: TtiObject;virtual;
    procedure ClearFields;reintroduce;
    procedure InsertRecord(const Values: array of const);
    procedure AppendRecord(const Values: array of const);
    //procedure ObserverFieldChanged(AObject: TtiObject;AFieldName: string);
    procedure SetSortOrder(AColumn: string;AAscending: boolean);

    function GetBookMarkPtr(Buffer: PByteOrChar): PtiObjectBookmark;
    function BookmarkValid(Bookmark: TBookmark): boolean;override;
    function GetFieldData(Field: TField;Buffer: pointer): boolean;override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): integer;override;
    function CreateBlobStream(Field: TField;Mode: TBlobStreamMode): TStream;override;
    function Locate(const KeyFields: string;const KeyValues: Variant;Options: TLocateOptions): boolean;override;
    function Lookup(const KeyFields: string;const KeyValues: Variant;const ResultFields: string): Variant;override;
    procedure DataEvent(Event: TDataEvent;Info: ptrint);override;
    property ObjectClass: TtiObjectClass read GetObjectClass write SetObjectClass;
  published
    property StringWidth: integer read FStringWidth write FStringWidth;
 //   property ObjectClassName: string read FObjectClassName write SetObjectClassName;  // removed at the moment as TtiObject does not descent from Tpersistant
    property ObjectDepth: integer read FObjectDepth write SetObjectDepth;

    property Active;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnNewRecord;
    property OnPostError;
{$ifndef fpc}
    property ObjectView;
{$endif}
  end;


  TTiDataset = class(TTiCustomDataset)
  private
    FOwnsObjects: boolean;
    FObjectList: TtiObjectList;
    FShowDeleted: boolean;
    function InternalGetRecord(Buffer: PByteOrChar;GetMode: TGetMode;DoCheck: boolean): TGetResult;
    procedure SetObjectList(const Value: TtiObjectList);
  protected
    function NewRecord: TtiObject;override;
    procedure DelRecord(oObject: TtiObject);override;

    function GetObjectList: TtiObjectList;virtual;
    procedure InternalGotoBookmark(Bookmark: pointer);override;
    function GetObjectListIndex(oObject: TtiObject): integer;
    function GetBookmarkIndex(Bookmark: pointer): integer;override;

    procedure SetFiltered(Value: boolean);override;
    procedure Sort(Compare: TTiListSortCompare);override;

    function GetRecord(Buffer: PByteOrChar;GetMode: TGetMode;DoCheck: boolean): TGetResult;override;
    function GetRecNo: integer;override;
    function GetRecordCount: integer;override;
    procedure InternalFirst;override;
    procedure InternalLast;override;
  public
    procedure LinkObject(AObjectList: TtiObjectList; AClass: TtiObjectClass);  // src addition

    property ObjectList: TtiObjectList read GetObjectList write SetObjectList;
    property SortColumn;
    property SortAscending;
  published
    property Filtered; //Only working with the OnFilterEvent
    property OnFilterRecord;
    property ShowDeleted: boolean read FShowDeleted write FShowDeleted;
    property OwnsObjects: boolean read FOwnsObjects write FOwnsObjects; //dummy for previous versions
  end;


  //Nested datasets maintain a list of classes held in a TList (or successor)
{$ifndef fpc}
  TTiNestedDataset = class(TTiDataset)
  protected
    function GetObjectList: TtiObjectList;override;
    procedure DoBeforeInsert;override;
    procedure OpenCursor(InfoQuery: boolean);override;
  public
    procedure DataEvent(Event: TDataEvent;Info: ptrint);override;
  published
    property DataSetField;
  end;
{$endif fpc}


  TTiRecordDataset = class(TTiCustomDataset)
  private
    FRecord: TtiObject;
  protected
    procedure DoBeforeDelete;override;
    procedure Sort(Compare: TTiListSortCompare);override;
    procedure InternalGotoBookmark(Bookmark: pointer);override;

    function GetRecord(Buffer: PByteOrChar;GetMode: TGetMode;DoCheck: boolean): TGetResult;override;
    function GetRecordCount: integer;override;
    procedure InternalFirst;override;
    procedure InternalLast;override;
    function NewRecord: TtiObject;override;
    procedure DelRecord(oObject: TtiObject);override;
    function GetObjectRecord: TtiObject;virtual;
    procedure SetObjectRecord(const Value: TtiObject);virtual;
  public
    property oRecord: TtiObject read GetObjectRecord write SetObjectRecord;
    function GetActiveItem: TtiObject;override;
  end;


{$ifndef fpc}
  TTiNestedRecordDataset = class(TTiRecordDataset)
  protected
    procedure OpenCursor(InfoQuery: boolean);override;
    function GetObjectRecord: TtiObject;override;
  public
    procedure DataEvent(Event: TDataEvent;Info: Ptrint);override;
  published
    property DataSetField;
  end;
{$endif}


  TTiDatasetBlobStream = class(TMemoryStream)
  private
    FField: TBlobField;
    FDataSet: TTiCustomDataset;
    FMode: TBlobStreamMode;
    FModified: boolean;
    FOpened: boolean;
    procedure LoadBlobData;
    procedure SaveBlobData;
    procedure Truncate;
  public
    constructor Create(Field: TBlobField;Mode: TBlobStreamMode);
    destructor Destroy;override;
    function Read(var Buffer;Count: longint): longint;override;
    function Write(const Buffer;Count: longint): longint;override;
  end;

// procedure Register;

implementation
{$ifdef MSWINDOWS}
  uses
    Windows; // To inline function AnsisCompareText 
{$endif}

// moved to separate unit
//procedure Register;
//begin
//  RegisterComponents('Data Access',[TTiDataset, TTiNestedDataset, TTiRecordDataset, TTiNestedRecordDataset]);
//end;

{
Internal buffer layout:
+-----------------+-------------------+
| Bookmark buffer | Calculated Fields |
|  BookmarkSize   |  CalcFieldsSize   |
+-----------------+-------------------+
                  ^
                  |
             FStartCalculated

<-----RecordSize--------------------->
}

{ TTiDatasetBlobStream }

constructor TTiDatasetBlobStream.Create(Field: TBlobField;Mode: TBlobStreamMode);
begin
  inherited Create;
  FField := Field;
  FMode := Mode;
  FDataSet := FField.Dataset as TTiCustomDataset;
  if Mode <> bmWrite
  then LoadBlobData
  else Truncate;
end;

destructor TTiDatasetBlobStream.Destroy;
begin
  if FModified
  then SaveBlobData;
  inherited Destroy;
end;

function TTiDatasetBlobStream.Read(var Buffer;Count: longint): longint;
begin
  Result := inherited Read(Buffer, Count);
  FOpened := true;
end;

function TTiDatasetBlobStream.Write(const Buffer;Count: longint): longint;
begin
  Result := inherited Write(Buffer, Count);
  FModified := true;
end;

procedure TTiDatasetBlobStream.Truncate;
begin
  Clear;
  FModified := true;
end;

procedure TTiDatasetBlobStream.LoadBlobData;
//Load data from object into BlobField
var Buffer: PByteOrChar;
  TempStr: string;
  PropInfo: PPropInfo;
  Obj, oObject: TObject;
begin
  FModified := false;
  Self.Size := 0;
  Buffer := FDataset.GetActiveRecordBuffer;
  if Buffer = nil
  then exit;
  oObject := FDataset.GetBookmarkPtr(Buffer)^.BookMarkObject;
  if not FDataset.GetPropertyInfo(oObject, PropInfo, FField.FieldName)
  then exit;

  case PropInfo^.PropType^.Kind of
    tkString, tkLString, tkWString: begin
      TempStr := GetStrProp(oObject, PropInfo);
      if TempStr <> ''
      then inherited Write(TempStr[1], Length(TempStr));
    end;
    tkClass: begin
      Obj := GetObjectProp(oObject, FField.FieldName);
      if Obj is TPicture then begin
        if(Obj as TPicture).Graphic <> nil
        then(Obj as TPicture).Graphic.SaveToStream(Self);
      end
      else if Obj is TStrings
      then(Obj as TStrings).SaveToStream(Self)
    end;
  end;
  Position := 0;
end;

procedure TTiDatasetBlobStream.SaveBlobData;
//Save data from BlobField into object
var ABuffer: PByteOrChar;
  TempStr: string;
  PropInfo: PPropInfo;
  Obj, AObject: TObject;
begin
  ABuffer := FDataset.GetActiveRecordBuffer;
  if ABuffer = nil
  then exit;
  AObject := FDataset.GetBookmarkPtr(ABuffer)^.BookMarkObject;
  if not FDataset.GetPropertyInfo(AObject, PropInfo, FField.FieldName)
  then exit;
  (AObject as TtiObject).Dirty := true;

  Position := 0;
  case PropInfo^.PropType^.Kind of
    tkString, tkLString, tkWString: begin
      if Size = 0 then begin
        SetStrProp(AObject, PropInfo, '');
        //(oObject as TtiObject).SetNull(PropInfo^.Name);
      end else begin
        SetLength(TempStr, Size);
        inherited Read(TempStr[1], Size);
        SetStrProp(AObject, PropInfo, TempStr);
      end;
    end;
    tkClass: begin
      Obj := GetObjectProp(AObject, FField.FieldName);
      if Obj is TPicture then begin
        if(Obj as TPicture).Graphic <> nil
        then(Obj as TPicture).Graphic.LoadFromStream(Self);
      end
      else if Obj is TStrings
      then(Obj as TStrings).LoadFromStream(Self)
    end;
  end;
  FModified := false;
end;



{ TTiCustomDataset }

constructor TTiCustomDataset.Create(AOwner: TComponent);
begin
  inherited;
  FStringWidth := 255;
  FObjectFields := TStringlist.Create;
{$ifndef fpc}
  NestedDataSetClass := TTiCustomDataset;
{$endif}
  BookmarkSize := SizeOf(TTiObjectBookmark);
  FStartCalculated := BookmarkSize;
  //Observer := TOpfFieldObserver.Create(nil);
  //Observer.Enabled := true;
  //Observer.OnChange := ObserverFieldChanged;
end;

destructor TTiCustomDataset.Destroy;
begin
  //FreeAndNil(Observer);
  FreeAndNil(FObjectFields);
  inherited;
end;

procedure TTiCustomDataset.ClearCalcFields(Buffer: PByteOrChar);
begin
  FillChar(Buffer[FStartCalculated], CalcFieldsSize, 0);
end;

function TTiCustomDataset.GetActiveRecordBuffer: PByteOrChar;
begin
  case State of
    dsBrowse: begin
      if isEmpty
      then Result := nil
      else Result := ActiveBuffer;
    end;
    dsCalcFields: Result := CalcBuffer;
    dsFilter: Result := TempBuffer; //FFilterBuffer
    dsEdit, dsInsert, dsNewValue: Result := ActiveBuffer;
    else Result := nil;
  end;
end;

function TTiCustomDataset.GetObjectClass: TtiObjectClass;
begin
  if not Assigned(FObjectClass)
  then CheckObjectClass;
  Result := FObjectClass;
end;

procedure TTiCustomDataset.SetObjectClass(const Value: TtiObjectClass);
begin
  CheckInactive;
  FObjectClass := Value;
  if Value <> nil then begin
    if(Value.ClassInfo = nil)or not(Value.InheritsFrom(TtiObject))
    then DatabaseErrorFmt('Class %s not derived from TtiObject',[Value.ClassName]);
//    FObjectClassName := FObjectClass.ClassName;
  end;
end;

//procedure TTiCustomDataset.SetObjectClassName(const Value: string);
//begin
//  CheckInactive;
//  FObjectClass := nil;
//  FObjectClassName := Value;
//end;

function TTiCustomDataset.GetBookMarkPtr(Buffer: PByteOrChar): PtiObjectBookmark;
begin
  Result := PtiObjectBookmark(Buffer);
end;

procedure TTiCustomDataset.GetBookmarkData(Buffer: PByteOrChar;Data: pointer);
begin
  if Assigned(Data)
  then Move(GetBookMarkPtr(Buffer)^, Data^, BookmarkSize);
end;

procedure TTiCustomDataset.SetBookmarkData(Buffer: PByteOrChar;Data: pointer);
const bfNA = TBookmarkFlag(ord(High(TBookmarkFlag)) + 1);
begin
  if Assigned(Data)
  then Move(Data^, GetBookMarkPtr(Buffer)^, BookmarkSize)
  else SetBookmarkFlag(Buffer, bfNA);
end;

function TTiCustomDataset.GetBookmarkFlag(Buffer: PByteOrChar): TBookmarkFlag;
begin
  Result := GetBookMarkPtr(Buffer)^.BookMarkFlag;
end;

procedure TTiCustomDataset.SetBookmarkFlag(Buffer: PByteOrChar;Value: TBookmarkFlag);
begin
  GetBookMarkPtr(Buffer)^.BookMarkFlag := Value;
end;

procedure TTiCustomDataset.InternalSetToRecord(Buffer: PByteOrChar);
begin
  InternalGotoBookmark(GetBookMarkPtr(Buffer));
end;

function TTiCustomDataset.AllocRecordBuffer: PByteOrChar;
begin
  Result := AllocMem(RecordSize);
end;

procedure TTiCustomDataset.FreeRecordBuffer(var Buffer: PByteOrChar);
begin
  FreeMem(Buffer);
end;

function TTiCustomDataset.GetRecordSize: word;
begin
  Result := BookmarkSize + CalcFieldsSize;
end;


procedure TTiCustomDataset.CheckObjectClass;
begin
  if not Assigned(FObjectClass) then
    DatabaseErrorFmt('%s.ObjectClass is not assigned',[Name]);

// removed src coz we can't use ObjectClassName
//  if not Assigned(FObjectClass) then begin
//    if Trim(ObjectClassName) = ''
//    then DatabaseErrorFmt('%s.ObjectClassname is empty',[Name]);
//    ObjectClass := TtiObjectClass(GetClass(ObjectClassName));
//  end;
//  if not Assigned(FObjectClass)
//  then DatabaseErrorFmt('Class %s in %s.ObjectClassname is not registered',[ObjectClassName, Name]);
end;

{procedure TTiCustomDataset.InitializeFields;
//If not persistent fields patch the minimal&maximum values.
var i: integer;
  Field: TField;
  Typedata: PTypeData;
  PropInfo: PPropInfo;
  oObject: TClass;
begin
  exit; //Some problems here
  if DefaultFields then begin
    for i := 0 to Fields.Count - 1 do begin
      Field := Fields[i];
      if Field.FieldNo >= 0 then begin //Skip calculated fields
        oObject := ObjectClass;
        if GetPropertyInfo(oObject, PropInfo, Field.FieldName) then begin
          if Field is TStringField then begin
            (Field as TStringField).DisplayWidth := 9
          end;
        end;
      end;
    end;
  end;

  exit; //Some problems here
  if DefaultFields then begin
    for i := 0 to Fields.Count - 1 do begin
      Field := Fields[i];
      if Field.FieldNo >= 0 then begin //Skip calculated fields
        oObject := ObjectClass;
        if GetPropertyInfo(oObject, PropInfo, Field.Fieldname) then begin
          if PropInfo^.PropType^.Kind in[tkInteger, tkEnumeration] then begin
            TypeData := GetTypeData(PropInfo^.PropType^);
            if Field is TIntegerField then begin
              if(TypeData^.MinValue <> - Maxint)and(TIntegerField(Field).MinValue = 0)
              then TIntegerField(Field).MinValue := TypeData^.MinValue;
              if(TypeData^.MaxValue <> Maxint)and(TIntegerField(Field).MaxValue = 0)
              then TIntegerField(Field).MaxValue := TypeData^.MaxValue;
            end
            else if Field is TWordField then begin
              if TWordField(Field).MinValue = 0
              then TWordField(Field).MinValue := TypeData^.MinValue;
              if TWordField(Field).MaxValue = 0
              then TWordField(Field).MaxValue := TypeData^.MaxValue;
            end
            else if Field is TSmallintField then begin
              if TSmallintField(Field).MinValue = 0
              then TSmallintField(Field).MinValue := TypeData^.MinValue;
              if TSmallintField(Field).MaxValue = 0
              then TSmallintField(Field).MaxValue := TypeData^.MaxValue;
            end;
          end
          else if PropInfo^.PropType^.Kind in[tkInt64] then begin
            if Field is TLargeIntField then begin
              TypeData := GetTypeData(PropInfo^.PropType^);
              if TLargeIntField(Field).MinValue = 0
              then TLargeIntField(Field).MinValue := TypeData^.MinInt64Value;
              if TLargeIntField(Field).MaxValue = 0
              then TLargeIntField(Field).MaxValue := TypeData^.MaxInt64Value;
            end;
          end
          else if PropInfo^.PropType^.Kind in[tkString, tkLString, tkWString] then begin
            if Field is TStringField then begin
              if(Field as TStringField).DisplayWidth = StringWidth
              then(Field as TStringField).DisplayWidth := 25
            end;
          end;
        end;
      end;
    end;
  end;
end;}

function TTiCustomDataset.GetPropertyInfo(var oObject: TObject;var PropInfo: PPropInfo;cFieldName: string): boolean;
//Returns the right Object and PropInfo in case of ObjectView (Recursive class fields)
var i, nLen, nParent: integer;
begin
  Result := false;
  if Assigned(oObject)then try
    PropInfo := GetPropInfo(oObject, cFieldName);
    if not Assigned(PropInfo) then begin //Its a ObjectView (a nested property)
      with TStringlist.Create do try
        //First walk upward
        nParent := FObjectFields.IndexOf(cFieldName);
        if nParent >= 0 then begin
          AddObject(cFieldName, TObject(0));
          while true do begin
            nParent := integer(FObjectFields.Objects[nParent]);
            if nParent < 0
            then break;
            cFieldName := FObjectFields[nParent];
            AddObject(cFieldName, TObject(1));
          end;
        end;

        //Boring, walk downward again
        nLen := 0;
        for i := Count - 1 downto 0 do begin
          cFieldName := Strings[i];
          PropInfo := GetPropInfo(oObject, Copy(cFieldName, nLen + 1, Maxint));
          if not Assigned(PropInfo)
          then break;
          if integer(Objects[i]) > 0
          then oObject := GetObjectProp(oObject, PropInfo);
          if not Assigned(oObject)
          then break;
          nLen := Length(cFieldName);
        end;
      finally
        Free;
      end;
    end;
    Result := Assigned(oObject)and Assigned(PropInfo);
  except
    Result := false;
    PropInfo := nil;
  end
  else PropInfo := nil;
end;

function TTiCustomDataset.GetPropertyInfo(var oObject: TClass;var PropInfo: PPropInfo;cFieldName: string): boolean;
//Returns the right Object and PropInfo in case of ObjectView (Recursive class fields)
var i, nLen, nParent: integer;
begin
  Result := false;
  if Assigned(oObject) then begin
    PropInfo := GetPropInfo(oObject, cFieldName);
    if not Assigned(PropInfo) then begin //Its a ObjectView (a nested property)
      with TStringlist.Create do try
        //First walk upward
        nParent := FObjectFields.IndexOf(cFieldName);
        if nParent >= 0 then begin
          AddObject(cFieldName, TObject(0));
          while true do begin
            nParent := integer(FObjectFields.Objects[nParent]);
            if nParent < 0
            then break;
            AddObject(FObjectFields[nParent], TObject(1));
          end;
        end;

        //Boring, walk downward again
        nLen := 0;
        for i := Count - 1 downto 0 do begin
          cFieldName := Strings[i];
          PropInfo := GetPropInfo(oObject, Copy(cFieldName, nLen + 1, Maxint));
          if integer(Objects[i]) > 0 then
{$ifdef fpc}
          oObject := GetTypeData(PropInfo^.PropType).ClassType;
{$else fpc}
          oObject := GetTypeData(PropInfo^.PropType^).ClassType;
{$endif fpc}
          nLen := Length(cFieldName);
        end;

      finally
        Free;
      end;
    end;
    Result := Assigned(oObject)and Assigned(PropInfo);
    if not Result
    then DatabaseErrorFmt('Field %s.%s not found',[ObjectClass.ClassName, cFieldName], Self);
  end
  else PropInfo := nil;
end;

procedure TTiCustomDataset.InternalInitFieldDefs;
//Build the TFieldDefs from the object RTTi..


  procedure GetFieldDefsRtti(AObjectClass: TClass;cPrefix: string;nParent, nDepth: integer);
  //Create fields from the published properties of the ObjectClass
  var PropList: PPropList;
    PropInfo: PPropInfo;
    Typedata: PTypeData;
    Data: PTypeData;
    cParent: string;
    oClass: TClass;
    i, nIndex: integer;


    procedure InValidFieldMapping(PropInfo: PPropInfo);
    begin
      DatabaseErrorFmt('%s.%s (%s) could not be mapped into a dataset field',
        [cPrefix + AObjectClass.ClassName, PropInfo^.Name, PropInfo^.PropType^.Name]);
    end;


    procedure AddFieldDef(PropInfo: PPropInfo;FieldType: TFieldType;FieldSize: integer = 0);
    var cFieldName: string;
    begin
      cFieldName := cPrefix + PropInfo^.Name;
      FieldDefs.Add(cFieldName, FieldType, FieldSize, false);
      if cPrefix <> ''
      then FObjectFields.AddObject(Uppercase(cFieldName), TObject(nParent));
    end;


  begin
    Data := GetTypeData(AObjectClass.ClassInfo);
    GetMem(PropList, Data^.PropCount * SizeOf(PPropInfo));
    try
      GetPropInfos(AObjectClass.ClassInfo, PropList);
      for i := 0 to Data^.propCount - 1 do begin
        PropInfo := PropList^[i];
        if Assigned(PropInfo^.GetProc) then begin
{$ifdef fpc}
          TypeData := GetTypeData(PropInfo^.PropType);
{$else fpc}
          TypeData := GetTypeData(PropInfo^.PropType^);
{$endif fpc}
          case PropInfo^.PropType^.Kind of
            //Long strings and Widestrings with no real limit (except available
            //memory). We use the given StringWidth to limit the length of the
            //string and increase the buffersize with 1 for a #0 character.
            {$ifdef fpc}
            tkAString,
            {$endif}
            tkLString, tkWString: AddFieldDef(PropInfo, ftString, StringWidth);
{$ifdef UNICODE}
            tkUString: AddFieldDef(PropInfo, ftString, StringWidth);
{$endif}
            //Other strings like type string[50] and shortstrings have a length
            tkString: AddFieldDef(PropInfo, ftString, TypeData^.MaxLength);
            tkChar: AddFieldDef(PropInfo, ftString, 1);
            tkSet: AddFieldDef(PropInfo, ftInteger);
            tkInt64: AddFieldDef(PropInfo, ftLargeint);
            tkEnumeration: begin
              if {$ifdef fpc}TypeData^.BaseType{$else}TypeData^.BaseType^{$endif} = TypeInfo(boolean)
              then AddFieldDef(PropInfo, ftBoolean)
              else AddFieldDef(PropInfo, ftInteger);
            end;
            tkInteger: begin
              case TypeData^.OrdType of
                otSByte: AddFieldDef(PropInfo, ftSmallint);
                otUByte: AddFieldDef(PropInfo, ftWord);
                otSWord: AddFieldDef(PropInfo, ftSmallint);
                otUWord: AddFieldDef(PropInfo, ftWord);
                otSLong: AddFieldDef(PropInfo, ftInteger);
                otULong: AddFieldDef(PropInfo, ftLargeint);
              end;
            end;
            tkFloat: begin
              if SameText(PropInfo^.PropType^.Name, 'TDate')
              then AddFieldDef(PropInfo, ftDate)
              else if SameText(PropInfo^.PropType^.Name, 'TTime')
              then AddFieldDef(PropInfo, ftTime)
              else if SameText(PropInfo^.PropType^.Name, 'TDateTime')
              then AddFieldDef(PropInfo, ftDateTime)
              else begin
                case TypeData^.FloatType of
                  ftDouble: AddFieldDef(PropInfo, ftFloat);
                  ftExtended: AddFieldDef(PropInfo, ftFloat);
                  ftCurr: AddFieldDef(PropInfo, ftCurrency);
                  else InValidFieldMapping(PropInfo);
                end;
              end;
            end;
            tkClass: begin
              oClass := TypeData.ClassType;
              if oClass.InheritsFrom(TStrings)
              then AddFieldDef(PropInfo, ftMemo)
              else if oClass.InheritsFrom(TPicture)
              then AddFieldDef(PropInfo, ftGraphic)
              else if oClass.InheritsFrom(TtiObject) then begin
                if oClass.InheritsFrom(TtiObjectList)
                then AddFieldDef(PropInfo, ftDataSet);
                if(nDepth > 0)and oClass.InheritsFrom(TtiObject) then begin
                  cParent := UpperCase(cPrefix + PropInfo^.Name);
                  nIndex := FObjectFields.AddObject(cParent, TObject(nParent));
                  GetFieldDefsRtti(oClass, cParent, nIndex, nDepth - 1);
                end;
              end;
            end;
            tkMethod:; //Ignore since we don't support it.
            else InValidFieldMapping(PropInfo);
          end;
        end;
      end;
    finally
      FreeMem(PropList, Data^.PropCount * SizeOf(PPropInfo));
    end;
  end;
begin
  CheckObjectClass;
  FieldDefs.BeginUpdate;
  try
    FieldDefs.Clear;
    FObjectFields.Clear;
    GetFieldDefsRtti(FObjectClass, '', - 1, FObjectDepth);
  finally
    FieldDefs.EndUpdate;
  end;
end;

function TTiCustomDataset.GetFieldData(Field: TField;Buffer: pointer): boolean;
//"Load"; Copy the object property value to the field value
var RecBuffer, pDst: PByteOrChar;
  oObject: TObject;
  TempStr: string;
  TempInt: integer;
  TempSmallInt: smallint;
  TempSmallWord: word;
  TempInt64: int64;
  TempDouble: Double;
  TimeStamp: TTimeStamp;
{$ifndef fpc}
  SQLTimeStamp: TSQLTimeStamp;
{$endif}
  Data: TDateTimeRec;
  TempBool: LongBool;
  PropInfo: PPropInfo;
  Stream: TStream;
  Obj: TObject;
begin
  Result := false;

  RecBuffer := GetActiveRecordBuffer;
  //Can't get the BookMarkObject
  if RecBuffer = nil
  then exit;
  oObject := GetBookmarkPtr(RecBuffer)^.BookMarkObject;
  //Buffer is really screwed up, there's no Object
  if oObject = nil
  then exit;

  if Buffer = nil then begin
    //Dataset checks if the field is NULL by passing a nil buffer
    //Tell it is not NULL by passing back True.
    if Field.DataType = ftBoolean
    then Result := true //Boolean's zijn nooit NULL (ter presentatie)
    else begin
      if not GetPropertyInfo(oObject, PropInfo, Field.FieldName)
      then exit;
      {if Assigned(PropInfo^.SetProc)
      then Result := not(ofNull in(oObject as TtiObject).GetField(PropInfo)^)
      else} Result := true;
    end;
    exit;
  end;

  if Field.FieldNo < 0 then begin
    inc(RecBuffer, FStartCalculated + Field.Offset);
    Result := boolean(RecBuffer[0]);
    if Result and LongBool(Buffer)
    then Move(RecBuffer[1], Buffer^, Field.DataSize);
  end else begin

    if not GetPropertyInfo(oObject, PropInfo, Field.FieldName)
    then exit;

    //if the object property value is NULL just return false
    if not(Field.DataType in[ftDataSet, ftBoolean]) then begin
      if Field.FieldKind = fkInternalCalc
      then else if not Assigned(PropInfo^.SetProc)
      then else if false {ofNull in(oObject as TtiObject).GetField(PropInfo)^}
      then exit;
    end;

    pDst := PByteOrChar(Buffer);
    case Field.DataType of
      ftString: begin
        TempStr := GetStrProp(oObject, PropInfo);
{$ifdef UNICODE}
            PAnsiChar(Buffer)[Field.Size] := #0;
            WideCharToMultiByte(0, 0, pwidechar(TempStr), Length(TempStr)+1,
              Buffer, Field.Size, nil, nil);
{$else}
        StrLCopy(PChar(Buffer), PChar(TempStr), Field.DataSize);
{$endif}
      end;
      ftInteger: begin
        TempInt := GetOrdProp(oObject, PropInfo);
        Move(TempInt, pDst^, SizeOf(TempInt));
      end;
      ftSmallint: begin
        TempSmallInt := GetOrdProp(oObject, PropInfo);
        Move(TempSmallInt, pDst^, SizeOf(TempSmallInt));
      end;
      ftWord: begin
        TempSmallWord := GetOrdProp(oObject, PropInfo);
        Move(TempSmallWord, pDst^, SizeOf(TempSmallWord));
      end;
      ftLargeint: begin //Variants do not support int64 (<Delphi6)
      //Problem:Cardinals are edited as int64's
        if PropInfo^.PropType^.Kind = tkInt64
        then TempInt64 := GetInt64Prop(oObject, PropInfo)
        else TempInt64 := GetOrdProp(oObject, PropInfo);
        Move(TempInt64, pDst^, SizeOf(int64));
      end;
      ftBoolean: begin
        TempBool := LongBool(GetOrdProp(oObject, PropInfo));
        Move(TempBool, pDst^, SizeOf(LongBool));
      end;
      ftFloat, ftBCD, ftCurrency: begin
        TempDouble := GetFloatProp(oObject, PropInfo);
        Move(TempDouble, pDst^, SizeOf(TempDouble));
      end;
      ftTimeStamp: begin
        TempDouble := GetFloatProp(oObject, PropInfo);
        {$ifndef fpc}
        SQLTimeStamp := DateTimeToSQLTimeStamp(TempDouble);
        Move(SQLTimeStamp, pDst^, SizeOf(SQLTimeStamp));
        {$else}
        Move(TempDouble, pDst^, SizeOf(Double));
        {$endif}
      end;
      ftDateTime: begin
        TempDouble := GetFloatProp(oObject, PropInfo);
        TimeStamp := DateTimeToTimeStamp(TempDouble);
        Data.DateTime := TimeStampToMSecs(TimeStamp);
        Move(Data, pDst^, SizeOf(TDateTimeRec));
      end;
      ftTime: begin
        TempDouble := GetFloatProp(oObject, PropInfo);
        TimeStamp := DateTimeToTimeStamp(TempDouble);
        Move(TimeStamp.Time, pDst^, SizeOf(integer));
      end;
      ftDate: begin
        TempDouble := GetFloatProp(oObject, PropInfo);
        TimeStamp := DateTimeToTimeStamp(TempDouble);
        Move(TimeStamp.Date, pDst^, SizeOf(integer));
      end;
      ftMemo: begin
        Move(pDst^, pointer(Stream), SizeOf(pointer));
        Stream.Size := 0;
        Stream.Position := 0;
        Obj := TObject(GetOrdProp(oObject, PropInfo));
        (Obj as TStrings).SaveToStream(Stream);
      end;
      ftGraphic: begin
        Move(pDst^, pointer(Stream), SizeOf(pointer));
        Stream.Size := 0;
        Stream.Position := 0;
        Obj := TObject(GetOrdProp(oObject, PropInfo));
        if(Obj as TPicture).Graphic <> nil
        then(Obj as TPicture).Graphic.SaveToStream(Stream);
      end;
      ftDataSet: begin
        Obj := GetObjectProp(oObject, PropInfo);
        Move(Obj, pDst^, SizeOf(TObject));
      end;
      else raise Exception.CreateFmt('Onbekend veldtype',[{$ifndef fpc}Field.FullName{$else}Field.DisplayName{$endif fpc}]);
    end;
    Result := true;
  end;
end;

procedure TTiCustomDataset.SetFieldData(Field: TField;Buffer: pointer);
//"Save"; Copy the field value to the object property value
var RecBuffer, pSrc: PByteOrChar;
  oObject: TObject;
  Obj: TObject;
  TempInt: integer;
  TempDouble: Double;
  TempSmallInt: smallint;
  TempSmallWord: word;
  TempBool: LongBool;
{$ifndef fpc}
  SQLTimeStamp: TSQLTimeStamp;
{$endif}
  Stream: TStream;
  TempInt64: int64;
  TimeStamp: TTimeStamp;
  Data: TDateTimeRec;
  PropInfo: PPropInfo;
begin
  RecBuffer := GetActiveRecordBuffer;
  //Can't get the BookMarkObject
  if RecBuffer = nil
  then exit;

  if Field.FieldNo < 0 then begin //Calculated fields
    if Buffer = nil
    then exit;
    Inc(RecBuffer, FStartCalculated + Field.Offset);
    boolean(RecBuffer[0]) := LongBool(Buffer);
    if LongBool(Buffer)
    then Move(Buffer^, RecBuffer[1], Field.DataSize);
  end else begin
    oObject := GetBookmarkPtr(RecBuffer)^.BookMarkObject;
    //Buffer is really screwed up, there's no Object
    if oObject = nil
    then exit;

    if not GetPropertyInfo(oObject, PropInfo, Field.FieldName)
    then exit;
    //Skip the readonly properties (no deFieldChange event)
    if not Assigned(PropInfo^.SetProc)
    then exit;

    (oObject as TtiObject).Dirty := true;
    pSrc := PByteOrChar(Buffer);
    case Field.DataType of
      ftString:
      begin
{$ifdef UNICODE}
        SetStrProp(oObject, PropInfo, AnsiString(PAnsiChar(Buffer)));
{$else}
        SetStrProp(oObject, PropInfo, PChar(Buffer));
{$endif}
      end;
      ftInteger: begin
        if Buffer = nil
        then TempInt := 0
        else Move(pSrc^, TempInt, SizeOf(integer));
        SetOrdProp(oObject, PropInfo, TempInt);
      end;
      ftSmallint: begin
        if Buffer = nil
        then TempSmallInt := 0
        else Move(pSrc^, TempSmallInt, SizeOf(TempSmallInt));
        SetOrdProp(oObject, PropInfo, TempSmallInt);
      end;
      ftWord: begin
        if Buffer = nil
        then TempSmallWord := 0
        else Move(pSrc^, TempSmallWord, SizeOf(TempSmallWord));
        SetOrdProp(oObject, PropInfo, TempSmallWord);
      end;
      ftBoolean: begin
        if Buffer = nil
        then TempBool := false
        else Move(pSrc^, TempBool, SizeOf(LongBool));
        SetOrdProp(oObject, PropInfo, ord(TempBool));
      end;
      ftFloat, ftBCD, ftCurrency: begin
        if Buffer = nil
        then TempDouble := 0.0
        else Move(pSrc^, TempDouble, SizeOf(Double));
        SetFloatProp(oObject, PropInfo, TempDouble);
      end;
      ftLargeint: begin //Variants do not support int64 (<Delphi6)
        if Buffer = nil
        then TempInt64 := 0
        else Move(pSrc^, TempInt64, SizeOf(int64));
        //Problem:Cardinal is edited as int64
        if PropInfo^.PropType^.Kind = tkInt64
        then SetInt64Prop(oObject, PropInfo, TempInt64)
        else SetOrdProp(oObject, PropInfo, TempInt64);
      end;
      ftTimeStamp: begin
        if Buffer = nil
        then TempDouble := 0
        else begin
{$ifndef fpc}
          Move(pSrc^, SQLTimeStamp, SizeOf(SQLTimeStamp));
          TempDouble := SQLTimeStampToDateTime(SQLTimeStamp);
{$else}
          Move(pSrc^, TempDouble, SizeOf(Double));
{$endif}
        end;
        SetFloatProp(oObject, PropInfo, TempDouble);
      end;
      ftDateTime: begin
        if Buffer = nil
        then Data.DateTime := 0.0
        else Move(pSrc^, Data, SizeOf(Double));
        TimeStamp := MSecsToTimeStamp(Data.DateTime);
        if Buffer = nil
        then TempDouble := 0.0
        else TempDouble := TimeStampToDateTime(TimeStamp);
        SetFloatProp(oObject, PropInfo, TempDouble);
      end;
      ftDate: begin
        TimeStamp.Date := 0;
        TimeStamp.Time := 0;
        if Buffer <> nil
        then Move(pSrc^, TimeStamp.Date, SizeOf(integer));
        if Buffer = nil
        then TempDouble := 0.0
        else TempDouble := TimeStampToDateTime(TimeStamp);
        SetFloatProp(oObject, PropInfo, TempDouble);
      end;
      ftTime: begin
        TimeStamp.Date := 1; //TimeStampToDateTime needs a valid date
        TimeStamp.Time := 0;
        if Buffer <> nil
        then Move(pSrc^, TimeStamp.Time, SizeOf(integer));
        if Buffer = nil
        then TempDouble := 0.0
        else TempDouble := TimeStampToDateTime(TimeStamp);
        SetFloatProp(oObject, PropInfo, Frac(TempDouble));
      end;
      ftGraphic: begin //Buffer can't be nil (?)
        Move(pSrc^, pointer(Stream), SizeOf(pointer));
        Stream.Position := 0;
        Obj := GetObjectProp(oObject, PropInfo);
        if(Obj as TPicture).Graphic <> nil
        then(Obj as TPicture).Graphic.LoadFromStream(Stream);
      end;
      ftMemo: begin //Buffer can't be nil (?)
        Move(pSrc^, pointer(Stream), SizeOf(pointer));
        Stream.Position := 0;
        Obj := GetObjectProp(oObject, PropInfo);
        (Obj as TStrings).LoadFromStream(Stream)
      end;
      ftDataSet: begin //Buffer can't be nil (?)
        Move(pSrc^, Obj, SizeOf(TObject));
        SetObjectProp(oObject, PropInfo, Obj);
      end;
      else begin
        raise Exception.CreateFmt('Onbekend veldtype',[{$ifndef fpc}Field.FullName{$else}Field.DisplayName{$endif fpc}]);
      end;
    end;

    //if Buffer = nil
    //then(oObject as TtiObject).SetNull(PropInfo^.Name);
  end;
  if not(State in[dsCalcFields, dsFilter, dsNewValue])
  then DataEvent(deFieldChange, longint(Field));
end;

procedure TTiCustomDataset.InternalInitRecord(Buffer: PByteOrChar);
//This is called by the TDataset to initialize an already existing buffer.
//We cannot just fill the buffer with 0s since that would overwrite our BLOB
//pointers. Therefore we free the blob pointers first, then fill the buffer
//with zeros, then reallocate the blob pointers
var Data: TTiObjectBookmark;
begin
  GetBookmarkdata(Buffer, @Data);
  FillChar(Buffer^, RecordSize, 0);
  SetBookmarkdata(Buffer, @Data);
end;

procedure TTiCustomDataset.InternalHandleException;
begin
  Application.HandleException(Self);
end;

function TTiCustomDataset.GetBookmarkIndex(Bookmark: pointer): integer;
//Abstract bookmark index
begin
  Result := 0;
end;

function TTiCustomDataset.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): integer;
//Compare bookmark pointers
const NilResults: array[boolean, boolean]of integer =((2, - 1),(1, 0));
var Index1, Index2: integer;
begin
  Result := NilResults[Bookmark1 = nil, Bookmark2 = nil];
  if Result = 2 then begin
    //get object indexes for position comparison
    Index1 := GetBookmarkIndex(Bookmark1);
    Index2 := GetBookmarkIndex(Bookmark2);
    //Compare indexes
    if Index1 < Index2
    then Result := - 1
    else if Index1 = Index2
    then Result := 0
    else Result := 1;
  end;
end;

procedure TTiCustomDataset.InternalOpen;
begin
  FObjectIndex := - 1;
  FLastParentPos := 0;
  CheckObjectClass;
  FCursorOpen := true;
  InternalInitFieldDefs;
  if DefaultFields
  then CreateFields;
  BindFields(true);
end;

procedure TTiCustomDataset.InternalClose;
begin
  BindFields(false);
  if DefaultFields
  then DestroyFields;
  FParentDataSet := nil;
  FCursorOpen := false;
end;

function TTiCustomDataset.IsCursorOpen: boolean;
begin
  Result := FCursorOpen;
end;

procedure TTiCustomDataset.InternalAddRecord(Buffer: pointer;Append: boolean);
begin
  if Append
  then SetBookmarkFlag(Buffer, bfEOF);
  InternalPost;
end;

function TTiCustomDataset.CreateBlobStream(Field: TField;Mode: TBlobStreamMode): TStream;
begin
  Result := TTiDatasetBlobStream.Create(Field as TBlobField, Mode);
end;

procedure TTiCustomDataset.InternalEdit;
var Buffer: PByteOrChar;
begin
  Buffer := GetActiveRecordBuffer;
  if Buffer <> nil then begin
    //InternalSetToRecord(Buffer);
    with GetBookMarkPtr(Buffer)^ do begin
      BookMarkBackup := BookMarkObject.Clone; //Create a backup (for canceling)
      //BookMarkObject.Observers.RegisterObserver(Observer);
    end;
  end;
end;

procedure TTiCustomDataset.InternalCancel;
var Buffer: PByteOrChar;
begin
  Buffer := GetActiveRecordBuffer;
  with GetBookMarkPtr(Buffer)^ do begin
    //BookMarkObject.Observers.UnRegisterObserver(Observer);
    if State = dsInsert then begin
      //Delete temp object
      DelRecord(BookMarkObject);
      BookMarkObject := nil; //Expliciet op null zetten, anders AV (20040916)
    end
    else BookMarkObject.Assign(BookMarkBackup); //Restore backuped
    FreeAndNil(BookMarkBackup); //Delete backup
  end;
end;

procedure TTiCustomDataset.InternalInsert;
//Create a new record (no backup needed)
var Buffer: PByteOrChar;
begin
  Buffer := GetActiveRecordBuffer;
  with GetBookMarkPtr(Buffer)^ do begin
    BookMarkFlag := bfInserted;
    BookMarkObject := NewRecord; //Temp object
    //BookMarkObject.Observers.RegisterObserver(Observer);
  end;
end;

procedure TTiCustomDataset.InternalPost;
//Flush the edited or inserted record
var Buffer: PByteOrChar;
begin
  inherited;
  Buffer := GetActiveRecordBuffer;
  with GetBookMarkPtr(Buffer)^ do begin
    if BookMarkObject.ObjectState = posClean
    then BookMarkObject.ObjectState := posUpdate;
    FreeAndNil(BookMarkBackup); //Delete the backup
    //BookMarkObject.Observers.UnRegisterObserver(Observer);
  end;
end;

procedure TTiCustomDataset.InternalDelete;
begin
  with GetBookMarkPtr(GetActiveRecordBuffer)^ do begin
    //BookMarkObject.Observers.UnRegisterObserver(Observer);
    DelRecord(BookMarkObject);
    BookMarkObject := nil;
  end;
end;

procedure TTiCustomDataset.DoAfterOpen;
//This is a silly trick to update the nested datasets, since some items were
//not present at runtime;
begin
  DataEvent(deParentScroll, 0);
  inherited;
end;

procedure TTiCustomDataset.DoAfterInsert;
begin
  try
    inherited;
  except
    if State in dsEditModes
    then Cancel;
    raise;
  end;
end;

function TTiCustomDataset.GetActiveItem: TtiObject;
begin
  if GetActiveRecordBuffer <> nil
  then Result := GetBookmarkPtr(GetActiveRecordBuffer)^.BookMarkObject
  else Result := nil;
end;

function TTiCustomDataset.BookmarkValid(Bookmark: TBookmark): boolean;
//Bookmark is valid if the object is found in the list (and not filtered)
var SaveState: TDataSetState;
begin
  Result := Active and (GetBookmarkIndex(Bookmark) >= 0);
  if Result then begin
    //Object is still in memory
    FObjectIndex := GetBookmarkIndex(Bookmark);
    //Is it still valid with filters?
    if Filtered And Assigned(OnFilterRecord) then begin
      SaveState := SetTempState(dsFilter);
      try
        Result := GetRecord(FFilterBuffer, gmCurrent, true) = grOK;
        OnFilterRecord(Self, Result);
      except
        Application.HandleException(Self);
      end;
      RestoreState(SaveState);
    end;
  end;
end;

procedure TTiCustomDataset.DataEvent(Event: TDataEvent;Info: ptrint);


  procedure CheckIfParentScrolled;
  var ParentPosition: integer;
  begin
    if Assigned(FParentDataSet)
    then ParentPosition := TTiCustomDataset(FParentDataSet).FObjectIndex
    else ParentPosition := - 1;
    if ParentPosition <> FLastParentPos then begin
      First;
      FLastParentPos := ParentPosition;
    end else begin
      UpdateCursorPos;
      Resync([]);
    end;
  end;
begin
  if Event = deParentScroll
  then CheckIfParentScrolled;
  inherited DataEvent(Event, Info);
end;

function TTiCustomDataset.MatchFieldValue(Field: TField;Value: Variant;Options: TLocateOptions): boolean;
var FieldValue: string;
begin
  if Field.DataType = ftString then begin
    if Field.IsNull
    then FieldValue := ''
    else FieldValue := Field.Value;

    //Trim field string to partial string length to simplify compare
    if loPartialKey in Options then begin
      if Length(FieldValue) > Length(Value)
      then SetLength(FieldValue, Length(Value));
    end;

    if loCaseInsensitive in Options
    then Result :=(AnsiCompareText(Value, FieldValue) = 0)
    else Result :=(AnsiCompareStr(Value, FieldValue) = 0);
  end
  else Result :=(Field.Value = Value);
end;

function TTiCustomDataset.LocateRecord(const KeyFields: string;const KeyValues: Variant;Options: TLocateOptions;SyncCursor: boolean): boolean;
var Fields: TList;
  FieldCount: integer;
  OldObject: integer;
  Index: integer;
  Buffer: PByteOrChar;
begin
  //Variants do not support int64 (<Delphi6)
  CheckBrowseMode;
  CursorPosChanged;
  Buffer := TempBuffer;

  Result := false;
  Fields := TList.Create;
  try
    GetFieldList(Fields, KeyFields);
    FieldCount := Fields.Count;

    //Use filter state to provide temporary buffer for record matching
    OldObject := FObjectIndex;
    FFilterBuffer := Buffer;
    SetTempState(dsFilter);
    try
      InternalFirst;

      while GetRecord(FFilterBuffer, gmNext, true) = grOk do begin
        for Index := 0 to(FieldCount - 1)do begin
          if FieldCount = 1
          then Result := MatchFieldValue(TField(Fields[Index]), KeyValues, Options)
          else Result := MatchFieldValue(TField(Fields[Index]), KeyValues[Index], Options);
          if not Result
          then Break;
        end;
        if Result
        then Break;
      end;

      if not(Result and SyncCursor)
      then FObjectIndex := OldObject;
    finally
      RestoreState(dsBrowse);
    end;
  finally
    Fields.Free;
  end;
end;


function TTiCustomDataset.Locate(const KeyFields: string;const KeyValues: Variant;Options: TLocateOptions): boolean;
begin
  DoBeforeScroll;
  //Variants do not support int64 (<Delphi6)
  Result := LocateRecord(KeyFields, KeyValues, Options, true);
  if Result then begin
    Resync([rmExact, rmCenter]);
    DoAfterScroll;
  end;
end;

function TTiCustomDataset.Lookup(const KeyFields: string;const KeyValues: Variant;const ResultFields: string): Variant;
begin
  Result := Null;
  //Variants do not support int64 (<Delphi6)
  if LocateRecord(KeyFields, KeyValues,[], false) then begin
    //Use filter state as the data is stored in the filter buffer in LocateRecord
    SetTempState(dsFilter);
    try
      Result := FieldValues[ResultFields];
    finally
      RestoreState(dsBrowse);
    end;
  end;
end;

function TTiCustomDataset.SortIt(Item1, Item2: TObject): integer;
var Float1, Float2: extended;
  PropInfo1, PropInfo2: PPropInfo;
begin
  Result := 0;
  if GetPropertyInfo(Item1, PropInfo1, SortColumn)and GetPropertyInfo(Item2, PropInfo2, SortColumn) then begin
    case PropInfo1^.PropType^.Kind of
    {$ifdef fpc}tkAstring,{$endif}
      tkWString, tkLString, tkString: Result := CompareText(GetStrProp(Item1, PropInfo1), GetStrProp(Item2, PropInfo1));
      tkEnumeration, tkInteger: Result := GetOrdProp(Item1, PropInfo1) - GetOrdProp(Item2, PropInfo2);
      tkFloat: begin
        Float1 := GetFloatProp(Item1, PropInfo1);
        Float2 := GetFloatProp(Item2, PropInfo2);
        if Float1 = Float2
        then Result := 0
        else if Float1 < Float2
        then Result := - 1
        else Result := 1;
      end;
    end;
    if not FSortAscending
    then Result := Result * - 1;
  end;
end;

procedure TTiCustomDataset.QuickSort(SortList: TList;l, R: integer;SCompare: TTiListSortCompare);
var i, j: integer;
  p: pointer;
begin
  repeat
    i := l;
    j := R;
    p := SortList[(l + R)shr 1];
    repeat
      while SCompare(SortList[i], p) < 0 do Inc(i);
      while SCompare(SortList[j], p) > 0 do Dec(j);
      if i <= j then begin
        SortList.Exchange(i, j);
        Inc(i);
        Dec(j);
      end;
    until i > j;
    if l < J
    then QuickSort(SortList, l, j, SCompare);
    l := i;
  until i >= R;
end;

procedure TTiCustomDataset.SetSortOrder(AColumn: string;AAscending: boolean);
var SavedCursor: TCursor;
begin
  SavedCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    FSortColumn := AColumn;
    FSortAscending := AAscending;
    Sort(SortIt);
    if Active
    then Refresh;
  finally
    Screen.Cursor := SavedCursor;
  end;
end;

procedure TTiCustomDataset.SetSortAscending(const Value: boolean);
var SavedCursor: TCursor;
begin
  SavedCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    if FSortAscending <> Value then begin
      FSortAscending := Value;
      if FSortColumn <> '' then begin
        Sort(SortIt);
        if Active
        then Refresh;
      end;
    end;
  finally
    Screen.Cursor := SavedCursor;
  end;
end;

procedure TTiCustomDataset.SetSortColumn(const Value: string);
var SavedCursor: TCursor;
begin
  SavedCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    if SameText(FSortColumn, Value)
    then FSortAscending := not FSortAscending
    else begin
      FSortColumn := Value;
      FSortAscending := true;
    end;
    Sort(SortIt);
    if Active
    then Refresh;
  finally
    Screen.Cursor := SavedCursor;
  end;
end;

procedure TTiCustomDataset.SetObjectDepth(const Value: integer);
begin
  if FObjectDepth <> Value then begin
    CheckInactive;
    FObjectDepth := Value;
    if not(csLoading in ComponentState)
    then InternalInitFieldDefs;
  end;
end;

procedure TTiCustomDataset.ClearFields;
begin
  raise Exception.Create('InsertRecord & AppendRecord or ClearFields are not supported');
end;

procedure TTiCustomDataset.AppendRecord(const Values: array of const);
//Internal problem with InternalInitRecord & creating the fysical component
begin
  raise Exception.Create('InsertRecord & AppendRecord or ClearFields are not supported');
end;

procedure TTiCustomDataset.InsertRecord(const Values: array of const);
begin
  raise Exception.Create('InsertRecord & AppendRecord or ClearFields are not supported');
end;

{procedure TTiCustomDataset.ObserverFieldChanged(AObject: TtiObject;AFieldName: string);
//Field event refresh in the dataset
var ADataset: TTiCustomDataset;
  AParent: TtiObject;
  cField: string;
  oField: TField;
  i, j: integer;
begin
  cField := AFieldName;

  while Assigned(AObject)do begin
    if Assigned(AObject.Table.Dataset)and(AObject.Table.Dataset is TTiCustomDataset) then begin
      ADataset := AObject.Table.Dataset as TTiCustomDataset;
      oField := ADataset.FindField(cField);
      if Assigned(oField) then begin
        ADataset.DataEvent(deFieldChange, longint(oField));
        //CodeSite.SendString('Changed field', ADataset.Name + ' ' + cField);
      end;
    end;

    AParent := AObject.Owner;
    if Assigned(AParent)and(AParent is TtiObject) then begin
      for i := 0 to AParent.Table.Childs.Count - 1 do begin
        oChild := AParent.Table.Childs[i];
        with oChild do begin
          if coCalculated in Options
          then continue;
          if GetObjectProp(AParent, ObjectProp) = AObject then begin
            cField := oChild.Objectname + cField;
            if Assigned(AParent.Table.Dataset)and(AParent.Table.Dataset is TTiCustomDataset) then begin
              ADataset := AParent.Table.Dataset as TTiCustomDataset;
              oField := ADataset.FindField(cField);
              if Assigned(oField) then begin
                ADataset.DataEvent(deFieldChange, longint(oField));

                //CodeSite.SendString('Changed subfield', ADataset.ObjectClassname+'.'+ADataset.Name + ' ' + cField);
              end;

              for j := 0 to AObject.Table.Fields.Count - 1 do begin
                with AObject.Table.Fields[j]do begin
                  if foCalculated in Options then begin
                    cField := oChild.Objectname + FieldName;
                    oField := ADataset.FindField(cField);
                    if Assigned(oField) then begin
                      ADataset.DataEvent(deFieldChange, longint(oField));
                      //CodeSite.SendString('Changed calculated field', ADataset.Name+' '+FieldName);
                    end;
                  end;
                end;
              end;
            end;
            break;
          end;
        end;
      end;
    end;

    //De link record houdt op bij een lijst
    AObject := AParent;
    if AObject is TtiObjectList
    then break;
  end;
end;}

procedure TTiCustomDataset.Notification(AComponent: TComponent;Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then begin
    if AComponent is TTiCustomDataset then begin
      if FParentDataSet = AComponent
      then FParentDataSet := nil
{$ifndef fpc}
      else if DataSetField = AComponent
      then DataSetField := nil
{$endif}
    end;
  end;
end;


{ TTiRecordDataset }

function TTiRecordDataset.GetRecord(Buffer: PByteOrChar;GetMode: TGetMode;DoCheck: boolean): TGetResult;
begin
  Result := grError;
  try
    if Assigned(oRecord) then begin
      Result := grOK;
      case GetMode of
        gmNext: begin
          if FObjectIndex + 1 < 1
          then FObjectIndex := FObjectIndex + 1
          else Result := grEOF;
        end;
        gmPrior: begin
          if FObjectIndex > 0
          then FObjectIndex := FObjectIndex - 1
          else Result := grBOF;
        end;
        gmCurrent: begin
          if FObjectIndex < 0
          then Result := grBOF;
          if FObjectIndex > 0
          then Result := grEOF;
        end;
      end;
    end;

    if Result = grOK then begin
      with GetBookMarkPtr(Buffer)^ do begin
        BookMarkFlag := bfCurrent;
        BookMarkObject := oRecord;
      end;
      if CalcFieldsSize > 0
      then GetCalcFields(Buffer);
    end;
  except
    if DoCheck
    then raise;
    Result := grError;
  end;
end;

procedure TTiRecordDataset.InternalFirst;
//set position before the first record
begin
  //Nothing (just 1 record available)
  FObjectIndex := - 1;
end;

procedure TTiRecordDataset.InternalLast;
//set position after the last record
begin
  //Nothing (just 1 record available)
  FObjectIndex := 1;
end;

procedure TTiRecordDataset.InternalGotoBookmark(Bookmark: pointer);
begin
  //Nothing (just 1 record available) stay on the current record
end;

function TTiRecordDataset.GetRecordCount: integer;
begin
  if Assigned(oRecord)
  then Result := 1
  else Result := 0;
end;

procedure TTiRecordDataset.DoBeforeDelete;
begin
  DatabaseError('Record cannot be removed in a RecordDataset!');
end;

procedure TTiRecordDataset.DelRecord(oObject: TtiObject);
begin
  DatabaseError('Record cannot be removed in a RecordDataset!');
end;

function TTiRecordDataset.NewRecord: TtiObject;
begin
  DatabaseError('Record should be set manually!');
  Result := nil;
end;

procedure TTiRecordDataset.SetObjectRecord(const Value: TtiObject);
begin
  if FRecord <> Value then begin
    //Flush previous record
    if Assigned(FRecord) then begin
      if State in dsEditModes
      then Post;
    end;
    FRecord := Value;
    if Active
    then Refresh;
  end;
end;

function TTiRecordDataset.GetObjectRecord: TtiObject;
begin
  Result := FRecord;
end;

procedure TTiRecordDataset.Sort(Compare: TTiListSortCompare);
begin
  //Nothing
end;

function TTiRecordDataset.GetActiveItem: TtiObject;
begin
  Result := oRecord;
end;

{ TTiDataset }

function TTiDataset.InternalGetRecord(Buffer: PByteOrChar;GetMode: TGetMode;DoCheck: boolean): TGetResult;
begin
  if Assigned(FParentDataSet)and FParentDataSet.Active and(FParentDataSet.IsEmpty or(FParentDataset.State = dsInsert)) then begin
    Result := grEOF;
    Exit;
  end;

  if Assigned(ObjectList) then begin
    if FObjectIndex > Objectlist.Count
    then FObjectIndex := Objectlist.Count
    else if FObjectIndex < - 1
    then FObjectIndex := - 1;
  end;
  Result := grError;
  try
    if Assigned(ObjectList)and(Objectlist.Count > 0) then begin
      Result := grOK;
      case GetMode of
        gmNext: begin
          if FObjectIndex + 1 < Objectlist.Count
          then FObjectIndex := FObjectIndex + 1
          else Result := grEOF;
        end;
        gmPrior: begin
          if FObjectIndex > 0
          then FObjectIndex := FObjectIndex - 1
          else Result := grBOF;
        end;
        gmCurrent: begin
          if FObjectIndex < 0
          then Result := grBOF;
          if FObjectIndex > Objectlist.Count - 1
          then Result := grEOF;
        end;
      end;
    end;

    if Result = grOK then begin
      with GetBookMarkPtr(Buffer)^ do begin
        BookMarkFlag := bfCurrent;
        BookMarkObject := ObjectList.Items[FObjectIndex];
      end;
      if CalcFieldsSize > 0
      then GetCalcFields(Buffer);
    end;
  except
    if DoCheck
    then raise;
    Result := grError;
  end;
end;

function TTiDataset.GetRecord(Buffer: PByteOrChar;GetMode: TGetMode;DoCheck: boolean): TGetResult;
var Accept: boolean;
  SaveState: TDataSetState;
  oObject: TtiObject;
begin
  if not FShowDeleted or(Filtered and Assigned(OnFilterRecord)) then begin
    SaveState := SetTempState(dsFilter);
    try
      Accept := true;
      repeat
        Result := InternalGetRecord(Buffer, GetMode, DoCheck);
        if Result = grOK then begin
          if not ShowDeleted then begin
            oObject := GetBookMarkPtr(Buffer)^.BookMarkObject;
            Accept := not(oObject.ObjectState in[posDelete, posDeleted]);
          end;
          if Accept and Assigned(OnFilterRecord)
          then OnFilterRecord(Self, Accept);
          if not Accept and(GetMode = gmCurrent)
          then Result := grError;
        end;
      until Accept or(Result <> grOK);
    except
      Application.HandleException(Self);
      Result := grError;
    end;
    RestoreState(SaveState);
  end
  else Result := InternalGetRecord(Buffer, GetMode, DoCheck)
end;

procedure TTiDataset.InternalFirst;
//set position before the first record
begin
  FObjectIndex := - 1;
end;

procedure TTiDataset.InternalLast;
//set position after the last record
begin
  FObjectIndex := ObjectList.Count;
end;

procedure TTiDataset.LinkObject(AObjectList: TtiObjectList;
  AClass: TtiObjectClass);
begin
  ObjectClass:= AClass; 
  ObjectList:= AObjectList;
end;

function TTiDataset.GetObjectListIndex(oObject: TtiObject): integer;
begin
  Result := - 1;
  if Assigned(ObjectList)
  then Result := ObjectList.List.IndexOf(oObject);
end;

function TTiDataset.GetBookmarkIndex(Bookmark: pointer): integer;
begin
  Result := GetObjectListIndex(PtiObjectBookmark(Bookmark)^.BookMarkObject);
end;

procedure TTiDataset.InternalGotoBookmark(Bookmark: pointer);
begin
  FObjectIndex := GetBookmarkIndex(Bookmark);
end;

procedure TTiDataset.SetFiltered(Value: boolean);
begin
  if Active then
  begin
    if Filtered <> Value then
      inherited;
    If Value Then
      GetRecordCount; // Prevents AV when filtering.
    First;
    CursorPosChanged;
  end
  else
    inherited;
end;

function TTiDataset.GetRecordCount: integer;
var
  oList: TtiObjectList;
  lPassesFilter : Boolean;
  Buffer : PByteOrChar;
  OldObject : Integer;
  lTempResult : Integer;
begin
  oList := ObjectList;
  If Assigned(oList) Then
  Begin
    If Filtered And Assigned(OnFilterRecord) Then
    Begin
      Buffer := TempBuffer;
      OldObject := FObjectIndex;
      FFilterBuffer := Buffer;
      SetTempState(dsFilter);
      lTempResult := 0;
      Try
        InternalFirst;
        While (GetRecord(FFilterBuffer, gmNext, True) = grOk) Do
        Begin
          OnFilterRecord(Self, lPassesFilter);
          If lPassesFilter Then
            Inc(lTempResult);
        End;
      Finally
        SetTempState(dsBrowse);
      End;
      Result := lTempResult;
      FObjectIndex := OldObject;
    End
    Else
      Result := oList.CountNotDeleted
  End
  Else
    Result := -1;
end;

procedure TTiDataset.DelRecord(oObject: TtiObject);
begin
  if oObject.ObjectState = posCreate then
  begin
    ObjectList.Extract(oObject);
    oObject.Free;
  end
  else
    oObject.ObjectState := posDelete;

end;

function TTiDataset.NewRecord: TtiObject;
//Add new record
begin
  Result := FObjectClass.CreateNew(ObjectList);
  ObjectList.Add(Result);
  FObjectIndex := ObjectList.IndexOf(Result); //Grumbl, Add should be a function
end;

procedure TTiDataset.Sort(Compare: TTiListSortCompare);
begin
  if(ObjectList <> nil)and(ObjectList.Count > 1)and(SortColumn <> '')
  then QuickSort(ObjectList.List, 0, ObjectList.Count - 1, Compare);
  if Active
  then First;
end;

function TTiDataset.GetObjectList: TtiObjectList;
begin
  Result := FObjectList;
end;

procedure TTiDataset.SetObjectList(const Value: TtiObjectList);
begin
  FObjectList := Value;
  //if Assigned(Value)
  //then ObjectClass := Value.ObjectClass
  //else ObjectClass := nil;
end;

function TTiDataset.GetRecNo: integer;
var ABuffer: PByteOrChar;
  AObjList: TtiObjectList;
begin
  AObjList := ObjectList;
  ABuffer := GetActiveRecordBuffer;
  if Assigned(AObjList)and Assigned(ABuffer)
  then Result := AObjList.List.IndexOf(GetBookmarkPtr(ABuffer)^.BookMarkObject) + 1
  else Result := - 1;
end;

{ TTiNestedDataset }
 {$ifndef fpc}
procedure TTiNestedDataset.DoBeforeInsert;
begin
  inherited;
  if(DataSetField.DataType = ftDataSet)and(FParentDataSet.State = dsInsert)
  then FParentDataSet.Post;
end;

procedure TTiNestedDataset.OpenCursor(InfoQuery: boolean);
begin
  if not Assigned(DataSetField)
  then DatabaseError('Missing DataSetField property', Self);
  FParentDataSet := DataSetField.Dataset as TTiCustomDataset;
  OpenParentDataSet(FParentDataSet);
  inherited;
end;

function TTiNestedDataset.GetObjectList: TtiObjectList;
//We need a List from the parent object.. Problems because these items are
//"not present" because the list could not be read (see DoAfterOpen)
//It only occurs during opening fase of the Dataset.
begin
  if Assigned(DataSetField)and not FParentDataSet.IsEmpty then begin
    if not DataSetField.GetData(@Result)
    then Result := nil
    else if not(Result is TtiObjectList)
    then DatabaseErrorFmt('%s is not a TtiObject',[DataSetField.FieldName], Self);
  end
  else Result := nil;
end;

procedure TTiNestedDataset.DataEvent(Event: TDataEvent;Info: ptrint);
begin
  inherited;
  if Assigned(DataSetField) then begin
    //Force parent dataset to Modified (otherwise it could be cancelled)
    if(Event = deFieldChange)and not(DataSetField.Dataset.Modified)
    then TTiCustomDataset(DataSetField.Dataset).DataEvent(Event, ptrint(DataSetField));
  end;
end;

{ TTiNestedRecordDataset }

procedure TTiNestedRecordDataset.DataEvent(Event: TDataEvent;Info: integer);
begin
  inherited;
  if Assigned(DataSetField) then begin
    //Force parent dataset to Modified (otherwise it could be cancelled)
    if(Event = deFieldChange)and not(DataSetField.Dataset.Modified)
    then TTiCustomDataset(DataSetField.Dataset).DataEvent(Event, integer(DataSetField));
  end;
end;

function TTiNestedRecordDataset.GetObjectRecord: TtiObject;
//We need a List from the parent object.. Problems because these items are
//"not present" because the list could not be read (see DoAfterOpen)
//It only occurs during opening fase of the Dataset.
begin
  if Assigned(DataSetField) then begin
    if not DataSetField.GetData(@Result)
    then Result := nil
    else if not(Result is TtiObject)
    then DatabaseErrorFmt('%s is not a TtiObject',[DataSetField.FieldName], Self);
  end
  else Result := nil;
end;

procedure TTiNestedRecordDataset.OpenCursor(InfoQuery: boolean);
begin
  if not Assigned(DataSetField)
  then DatabaseError('Missing DataSetField property', Self);
  FParentDataSet := DataSetField.Dataset as TTiCustomDataset;
  OpenParentDataSet(FParentDataSet);
  inherited;
end;
{$endif}
end.





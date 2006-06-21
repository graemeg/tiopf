{$IFDEF VER125}{C4}{$B-}{$X+}{$T-}{$H+}{$ENDIF}
{$IFDEF VER110}{C3}{$B-}{$X+}{$T-}{$H+}{$ENDIF}
{$IFDEF VER93}{C1}{$B-}{$X+}{$T-}{$H+}{$ENDIF}
{$B-}{$X+}{$T-}{$H+}

{$IFDEF VER150}
  {$DEFINE SQLite_D6PLUS}
{$ENDIF}
{$IFDEF VER140}
  {$DEFINE SQLite_D6PLUS}
{$ENDIF}
//    Layer for use for SQLite in tiOPF.
//    This layer is heavily inspired on the excellent Aducom-SQLite components.
//                                                    -------------  
//    but must be regarded as a new product
//    (See www.aducom.nl/sqlite for more information about the components)
//    Target is Delphi 5,6,7
//    Date: 20 januari 2005
//    Author: Bert Verhees
//    Email: bert .. .verhees .@ .rosa .. .nl (remove " .")

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.
}

unit SQLite3;

interface

uses
  DB,
  Classes,
  Windows,
  SysUtils;
const
  vtcIdentifier = 1;
  vtcNumber = 2;
  vtcAssignment = 3;
  vtcQString = 4;
  vtcDString = 5;
  vtcRelOp = 6;
  vtcFloat = 7;
  vtcDelimiter = 8;
  vtcEof = 9;


  SQLiteVersion = 'SQLite V3.0.8';

  MaxBuf = 30000;          // max stringbuffer for record (length) (excluding blob's)
  SQLITE_ROW         = 100;  // sqlite_step() has another row ready */
  SQLITE_DONE        = 101;  // sqlite_step() has finished executing */


  Crlf: string = #13#10;

type
  pFloat    = ^extended;
  pBoolean  = ^boolean;

  TConvertBuffer = array[1..255] of char;

  TtiSQLite3NotifyEvent = procedure(Sender: TObject) of object;

  TtiSQLite3Field = class
  public
    FieldNumber: integer;
    FieldName: string;
    FieldType: string;
    FieldNN: integer; // 1 if notnull
    FieldDefault: string;
    FieldPK: integer; // 1 if primary key
  end;

  TtiSQLite3DB = class(TObject)
  Private
    FAfterConnect: TtiSQLite3NotifyEvent;
    FBeforeConnect: TtiSQLite3NotifyEvent;
    FAfterDisconnect: TtiSQLite3NotifyEvent;
    FBeforeDisconnect: TtiSQLite3NotifyEvent;
    FVersion : String;
    FDataBaseName : String;
    procedure SetDataBaseName(Value:String);
    procedure GetFieldInfo(const FieldInfo: string; var FieldType: TFieldType;
                           var FieldLen, FieldDec: integer);
    function GetVersion : String;
  Protected
    FDatabase: string;
    FSQLiteVersion : string;
    FDefaultDir: string;
    FConnected: boolean;
    FMustExist: boolean;
    DBHandle: Pointer;
    FLastError: string;
    SQLite3_ErrorString: function(db : pointer): pchar; cdecl;
    SQLite3_Open: function(dbname: pchar; var db: pointer): integer; cdecl;
    SQLite3_Close: function (db: pointer): integer; cdecl;
    SQLite3_Exec: function(db: Pointer; SQLStatement: pchar; CallbackPtr: Pointer;
      Sender: TObject; var ErrMsg: pchar): integer; cdecl;
    SQLite3_GetTable: function(db: Pointer; SQLStatement: pchar; var ResultPtr: Pointer;
      var RowCount: cardinal; var ColCount: cardinal; var ErrMsg: pchar): integer; cdecl;
    SQLite3_FreeTable: procedure(Table: pchar); cdecl;
    SQLite3_FreeMem: procedure(P: pchar); cdecl;
    SQLite3_LastInsertRow: function(db: Pointer): integer; cdecl;
    SQLite3_Changes: function(db: Pointer): integer; cdecl;
    SQLite3_Prepare: function(db : Pointer; SQLStatement:pchar; nBytes:integer;
                              var hstatement : pointer; var Tail : pointer) : integer; cdecl;
    SQLite3_Finalize: function(hstatement : pointer): integer; cdecl;
    SQLite3_Reset: function(hstatement : pointer): integer; cdecl;
    SQLite3_Step: function(hstatement : pointer) : integer; cdecl;
    SQLite3_Column_blob: function(hstatement : pointer; iCol : integer) : pointer; cdecl;
    SQLite3_Column_bytes: function(hstatement : pointer; iCol : integer) : integer; cdecl;
    SQLite3_Column_count: function(hstatement : pointer) : integer; cdecl;
    SQLite3_Column_decltype: function(hstatement : pointer; iCol : integer) : pchar; cdecl;
    SQLite3_Column_double: function(hstatement : pointer; iCol : integer) : double; cdecl;
    SQLite3_Column_int: function(hstatement : pointer; iCol : integer) : integer; cdecl;
    SQLite3_Column_int64: function(hstatement : pointer; iCol : integer) : int64; cdecl;
    SQLite3_Column_name: function(hstatement : pointer; iCol : integer) : pchar; cdecl;
    SQLite3_Column_text: function(hstatement : pointer; iCol : integer) : pchar; cdecl;
    SQLite3_Column_type: function(hstatement : pointer; iCol : integer) : integer; cdecl;
    SQLite3_Bind_Blob: function (hstatement : pointer; iCol :integer; buf : pchar; n:integer; DestroyPtr: Pointer) : integer; cdecl;
    SQLite3_Version: function: PChar; cdecl;

    procedure DBConnect(Connected: boolean);
  Public
    DLLHandle: THandle;
    SessionName : String;
    constructor Create;
    destructor Destroy; override;
    function LoadLibs: boolean;
    procedure FSetDatabase(const Database: string);
    function RowsAffected: integer;
    procedure StartTransaction;
    procedure Open;
    procedure Close;
    procedure Commit;
    procedure RollBack;
    procedure GetTableNames(List: TStrings; SystemTables: boolean = false);
    procedure GetTableInfo(const TableName: string; List: TList);
    procedure GetIndexNames(List: TStrings; SystemTables: boolean = false);
    procedure GetFieldNames(const TableName: string; List: TStrings);
    procedure GetFieldDefInfo(const TableName,FieldName: string; var FieldType: TFieldType;
    var FieldLen, FieldDec: integer);
    procedure GetPrimaryKeys(const TableName: string; List: TStrings);
    procedure GetTableIndexNames(const TableName: String; List: TStrings);
    function SQLite3_Execute(db: Pointer;TheStatement: string; FParams: TParams; Sender : TObject) : integer;
    function SQLite3_ExecSQL(const TheStatement : string;VM:Pointer) : integer;overload;
    function SQLite3_ExecSQL(const TheStatement : string) : integer;overload;
    function SQLite3_ErrorMsg:String;
    procedure ShowError;
    property DatabaseName : String read FDataBaseName write SetDataBaseName;
  Published
    property Database: string Read FDatabase Write FSetDatabase;
    property DefaultDir: string Read FDefaultDir Write FDefaultDir;
    property Version: string Read GetVersion write FVersion;
    property Connected: boolean Read FConnected Write DBConnect;
    property MustExist: boolean Read FMustExist Write FMustExist;
    property AfterConnect: TtiSQLite3NotifyEvent Read FAfterConnect Write FAfterConnect;
    property BeforeConnect: TtiSQLite3NotifyEvent Read FBeforeConnect Write FBeforeConnect;
    property AfterDisconnect: TtiSQLite3NotifyEvent
      Read FAfterDisconnect Write FAfterDisconnect;
    property BeforeDisconnect: TtiSQLite3NotifyEvent
      Read FBeforeDisconnect Write FBeforeDisconnect;
  end;

  tiSQLiteError = class(Exception);

  PRecInfo = ^TRecInfo;

  TRecInfo = packed record
    Bookmark: integer;
    BookmarkFlag: TBookmarkFlag;
  end;

  TtiSQLite3BaseQuery = class;

  TFResult = class
  Protected
    Data: TList;
    BookMark: TList;
    RowId: TList;
    FLastBookmark: integer;
    FBufSize: integer;
    FDataSet : TtiSQLite3BaseQuery;
  Public
    constructor Create(TheDataSet : TtiSQLite3BaseQuery);
    destructor Destroy; override;
    procedure FreeBlobs;
    procedure SetBufSize(TheSize: integer);
    procedure Add(TheBuffer: pchar; TheRowId: integer);
    procedure Insert(Index: integer; TheBuffer: Pointer; TheRowId: integer);
    procedure Delete(Index: integer);
    function GetData(Index: integer): Pointer;
    function Count: integer;
    function IndexOf(TheBookMark: pointer): integer;
    function GetBookmark(Index: integer): integer;
    function GetRowId(Index: integer): integer;
  end;

  TtiSQLite3BaseQuery = class(TDataSet)
  Private
    FParams: TParams;
    FNoResults: boolean; 
    FAutoCommit: boolean;
    FTableDateFormat : string;
    FResult: TFResult;
    FSQL: TStrings;
    FPrepared: string;
    FRecBufSize: integer;
    FRecInfoOfs: integer;
    FCurRec: integer;
    FSaveChanges: boolean;
    MaxStrLen: integer;
    FConnection: TtiSQLite3DB;
    FMaxResults: integer;
    FStartResult: integer;
    SQLStr: string;
    ResultStr: pchar;
    DetailList: TList;
    function DateTimeToNative(DataType: TFieldType; Data: TDateTime): TDateTimeRec;
    function YYYYMMDDParser(const Str: PChar): TDateTime;
    function StrToFloatX(const StrIn : string) : Double;
    function StrToDateTimeX(const S: string): TDateTime;
    procedure SetSQL(const Value: TStrings);
    function UnpackBuffer(Buffer: pchar; FieldType: TFieldType) : TConvertBuffer;
  Protected
    function GetCalcFieldOffset(Field : TField): integer;

    procedure RegisterDetailDataset(DetailDataSet: TtiSQLite3BaseQuery);
    procedure LoadQueryData;
    function GetActiveBuffer(var Buffer: pchar): boolean;
    procedure NotifySQLiteMasterChanged;

    function AllocRecordBuffer: pchar; override;
    procedure FreeRecordBuffer(var Buffer: pchar); override;
    procedure GetBookmarkData(Buffer: pchar; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: pchar): TBookmarkFlag; override;
    function GetRecord(Buffer: pchar; GetMode: TGetMode;
      DoCheck: boolean): TGetResult; override;
    function GetRecordSize: word; override;
    procedure InternalAddRecord(Buffer: Pointer; Append: boolean); override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord(Buffer: pchar); override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalSetToRecord(Buffer: pchar); override;
    function IsCursorOpen: boolean; override;
    procedure SetBookmarkFlag(Buffer: pchar; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: pchar; Data: Pointer); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
  Protected
    function GetFieldSize(FieldNo: integer): integer;overload;
    function GetFieldSize(Field : TField): integer;overload;
    function GetNativeFieldSize(FieldNo: integer): integer;
    function GetFieldOffset(FieldNo: integer): integer;
    function GetRecordCount: integer; override;
    function GetRecNo: integer; override;
    procedure SetRecNo(Value: integer); override;
    property BaseSQL: TStrings Read FSQL Write SetSQL;
    procedure SetFilterText(const Value: string); override;
  Public
    constructor Create;reintroduce;virtual;
    destructor Destroy; override;
    procedure StartTransaction;
    procedure Commit;
    procedure RollBack;
    procedure SetFiltered(Value: Boolean); override;
    procedure SQLiteMasterChanged; virtual;
    function GetFieldData(Field: TField; Buffer: Pointer): boolean; override;
    function GetFieldData(FieldNo: integer; Buffer: Pointer): boolean; override;
    function GetLastInsertRow : integer;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    function Locate(const KeyFields: string; const KeyValues: variant;
      Options: TLocateOptions): boolean; override;
    function  BookmarkValid(Bookmark: Pointer) : boolean; override;
  Published
    property AutoCommit: boolean Read FAutoCommit Write FAutoCommit Default true;
    property TableDateFormat: string Read FTableDateFormat Write FTableDateFormat;
    property Connection: TtiSQLite3DB Read FConnection Write FConnection;
    property MaxResults: integer Read FMaxResults Write FMaxResults;
    property StartResult: integer Read FStartResult Write FStartResult;
    property Filter;
    property Filtered;
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
{$IFDEF SQLITE_D6PLUS}
    property BeforeRefresh;
    property AfterRefresh;
{$ENDIF}
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnNewRecord;
    property OnPostError;
  end;

   TtiSQLite3Query = class(TtiSQLite3BaseQuery)
  Private
    FParams: TParams;
    procedure SetSQL(const Value: TStrings);
    function GetSQL: TStrings;
    procedure SetParamsList(Value: TParams);
    procedure QueryChanged(Sender: TObject);
  Protected
    procedure InternalOpen; override;
    function GetParamsCount: word;
    procedure InternalClose; override;
  Public
    DataBaseName : String;
    constructor Create; override;
    destructor Destroy; override;
    procedure ExecSQL;
    property Params: TParams Read FParams Write SetParamsList Stored false;
    function ParamByName(const Value: string): TParam;
    function ParamCount:Integer;
  Published
    property SQL: TStrings Read GetSQL Write SetSQL;
  end;


  TtiSQLite3BlobStream = class(TMemoryStream)
  private
    FField: TBlobField;
    FDataSet: TtiSQLite3BaseQuery;
    FMode: TBlobStreamMode;
    FModified: Boolean;
    FOpened: Boolean;
    procedure LoadBlobData;
    procedure SaveBlobData;
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;


implementation

uses Forms,
     Dialogs;

function StrToIntX(const StrIn: string): integer;
begin
  try     Result := StrToInt(StrIn);
  except  Result := 0;
  end;
end;

procedure TtiSQLite3DB.SetDataBaseName(Value:String);
begin
  FDataBaseName := Value;
  DataBase := FDataBaseName;
end;

procedure TtiSQLite3DB.GetFieldInfo(const FieldInfo: string; var FieldType: TFieldType;
  var FieldLen, FieldDec: integer);
var
  p1, p2, pn : integer;
  vt:     string;
begin
  FieldType := ftString;
  FieldLen  := 255;
  FieldDec  := 0;

  p1 := pos('(', FieldInfo);
  if p1 <> 0 then
  begin
    p2 := pos(')', FieldInfo);
    if p2 <> 0 then
    begin
      vt := LowerCase(Copy(FieldInfo, 1, p1 - 1));
      if (vt = 'varchar') or (vt = 'char') or (vt = 'varchar2') then begin
         FieldType := ftString;
         FieldLen := StrToInt(Copy(FieldInfo, p1 + 1, p2 - p1 - 1));
      end else if (vt='numeric') then begin
         vt := Copy(FieldInfo, p1 + 1, p2 - p1 - 1);
         pn := pos('.',vt); if pn=0 then pn := pos(',',vt);
         FieldType := ftFloat;
         if pn = 0 then begin
            FieldLen := StrToInt(vt);
            FieldDec := 0;
         end else begin
            FieldLen := StrToInt(Copy(vt, 1, pn - 1));
            FieldDec := StrToInt(Copy(vt, pn+1, 2));
         end;
      end
      else if (vt = 'blob') then
      begin
        FieldType := ftBlob;
        FieldLen  := SizeOf(Pointer);
      end;
    end
    else
      FieldLen := 255;
  end
  else
  begin
    vt := LowerCase(FieldInfo);
    if vt = 'date' then
    begin
      FieldType := ftDate;
      FieldLen  := 10;
    end
    else if vt = 'datetime' then
    begin
      FieldType := ftDateTime;
      FieldLen  := 24;
    end
    else if vt = 'time' then
    begin
      FieldType := ftTime;
      FieldLen  := 12;
    end
    else if vt = 'timestamp' then
    begin
      FieldType := ftDateTime;
      FieldLen  := 24;
    end
    else if vt = 'integer' then
    begin
      FieldType := ftInteger;
      FieldLen  := 12;
    end
    else if (vt = 'float') or (vt = 'real') then
    begin
      FieldType := ftFloat;
      FieldLen  := 12;
    end
    else if (vt = 'boolean') or (vt = 'logical') then
    begin
      FieldType := ftBoolean;
      FieldLen  := 2;
    end
    else if (vt = 'text') or (vt='string') then
    begin
      FieldType := ftString;
      FieldLen  := 255;
    end
    else if (vt = 'currency') or (vt = 'financial') or (vt = 'money') then
    begin
      FieldType := ftCurrency;
      FieldLen := 10;
    end                                                                    
    else if (vt = 'blob') then
    begin
      FieldType := ftBlob;
      FieldLen  := SizeOf(Pointer);
    end
    else if (vt = 'graphic') then
    begin
      FieldType := ftGraphic;
      FieldLen  := SizeOf(Pointer);
    end
    else if (vt = 'clob') or (vt = 'memo') or (vt = 'longtext') then
    begin
      FieldType := ftMemo;
      FieldLen  := SizeOf(Pointer);
    end;
  end;
end;

function TtiSQLite3BaseQuery.DateTimeToNative(DataType: TFieldType; Data: TDateTime): TDateTimeRec;
var
  TimeStamp: TTimeStamp;
begin
  TimeStamp := DateTimeToTimeStamp(Data);
  case DataType of
    ftDate: Result.Date := TimeStamp.Date;
    ftTime: Result.Time := TimeStamp.Time;
    else Result.DateTime := TimeStampToMSecs(TimeStamp);
  end;
end;

constructor TFResult.Create(TheDataSet : TtiSQLite3BaseQuery);
begin
  Data          := TList.Create;
  Bookmark      := TList.Create;
  RowId         := TList.Create;
  FDataSet      := TheDataset;
  FLastBookmark := -1;
end;

destructor TFResult.Destroy;
var
  ptr: Pointer;
begin
  FreeBlobs;
  if Assigned(Data) then begin
    while Data.Count > 0 do begin
      ptr := Data.Items[0];
      if Assigned(ptr) then FreeMem(ptr, FBufSize);
      Data.Delete(0);
    end;
    Data.Free;                  
    Data := nil;
  end;
  if Assigned(Bookmark) then begin
     Bookmark.Free;
     Bookmark := nil;
  end;
  if Assigned(RowId) then begin
     RowId.Free;
     RowId := nil;
  end;
end;

procedure TFResult.FreeBlobs;
var i,j : integer;
    offset : integer;
    ptr : pchar;
    stream : TMemoryStream;
begin
 if not Assigned(FDataSet) then exit;
 if not Assigned(FDataSet.FieldList) then exit;
 for j := 0 to Data.Count - 1 do begin
   ptr := GetData(j);
   for i := 0 to FDataSet.FieldList.Count - 1 do begin
     if FDataSet.FieldList[i].DataType in [ftMemo, ftFmtMemo, ftGraphic, ftBlob] then begin
       Offset := FDataset.GetFieldOffset(FDataSet.FieldList[i].FieldNo);
       Move((ptr + Offset)^, Pointer(Stream), sizeof(Pointer));
       Stream.Free;
     end;
   end;
 end;
end;

procedure TFResult.SetBufSize(TheSize: integer);
begin
  FBufSize := TheSize;
end;

procedure TFResult.Add(TheBuffer: pchar; TheRowId: integer);
var
  ptr: pchar;
begin
  Inc(FLastBookmark);
  GetMem(Ptr, FBufSize);
  move(TheBuffer^, ptr^, FBufSize);
  Data.Add(Ptr);
  Bookmark.Add(Pointer(FLastBookMark));
  if TheRowId >= 0 then RowId.Add(Pointer(TheRowId))
  else                  RowId.Add(Pointer(RowId.Count));
end;

procedure TFResult.Insert(Index: integer; TheBuffer: pointer; TheRowId: integer);
var
  ptr: Pointer;
begin
  Inc(FLastBookmark);
  GetMem(Ptr, FBufSize);
  move(TheBuffer^, ptr^, FBufSize);
  Data.Insert(Index, Ptr);
  Bookmark.Insert(Index, Pointer(FLastBookMark));
  RowId.Insert(Index, Pointer(TheRowId));
end;

procedure TFResult.Delete(Index: integer);
var
  ptr: pointer;
begin
  if not ((Index < 0) or (Index >= Data.Count)) then
  begin
    ptr := Data.Items[Index];
    if ptr <> nil then  FreeMem(ptr, FBufSize);
    Data.Delete(Index);
    Bookmark.Delete(Index);
    Rowid.Delete(Index);
  end;
end;

function TFResult.GetData(Index: integer): Pointer;
begin
  if (Index < 0) or (Index >= Data.Count) then GetData := nil
  else  GetData := Data.Items[Index];
end;

function TFResult.GetBookmark(Index: integer): integer;
begin
  if (Index < 0) or (Index >= Data.Count) then GetBookmark := -1
  else  GetBookmark := integer(Bookmark.Items[Index]);
end;

function TFResult.GetRowId(Index: integer): integer;
begin
  if (Index < 0) or (Index >= RowId.Count) then GetRowId := -1
  else  GetRowId := integer(RowId.Items[Index]);
end;

function TFResult.Count: integer;
begin
  Result := Data.Count;
end;

function TFResult.IndexOf(TheBookMark: pointer): integer;
begin
  Result := BookMark.IndexOf(TheBookmark);
end;

function TtiSQLite3DB.LoadLibs: boolean;
begin
 try
  Result    := false;
  DLLHandle := LoadLibrary('sqlite3.dll');
  if DLLHandle <> 0 then
  begin
    @SQLite3_Open := GetProcAddress(DLLHandle, 'sqlite3_open');
    if not Assigned( @SQLite3_Open) then exit;
    @SQLite3_Close := GetProcAddress(DLLHandle, 'sqlite3_close');
    if not Assigned( @SQLite3_Close) then exit;
    @SQLite3_Exec := GetProcAddress(DLLHandle, 'sqlite3_exec');
    if not Assigned( @SQLite3_Exec) then exit;
    @SQLite3_ErrorString := GetProcAddress(DLLHandle, 'sqlite3_errmsg');
    if not Assigned( @SQLite3_ErrorString) then exit;
    @SQLite3_GetTable := GetProcAddress(DLLHandle, 'sqlite3_get_table');
    if not Assigned( @SQLite3_GetTable) then exit;
    @SQLite3_FreeTable := GetProcAddress(DLLHandle, 'sqlite3_free_table');
    if not Assigned( @SQLite3_FreeTable) then exit;
    @SQLite3_FreeMem := GetProcAddress(DLLHandle, 'sqlite3_free');
    if not Assigned( @SQLite3_FreeMem) then exit;
    @SQLite3_LastInsertRow := GetProcAddress(DLLHandle, 'sqlite3_last_insert_rowid');
    if not Assigned( @SQLite3_LastInsertRow) then exit;
    @SQLite3_Changes := GetProcAddress(DLLHandle, 'sqlite3_changes');
    if not Assigned( @SQLite3_Changes) then exit;
    @SQLite3_Prepare:= GetProcAddress(DLLHandle, 'sqlite3_prepare');
    if not Assigned( @SQLite3_Prepare) then exit;
    @SQLite3_Finalize:= GetProcAddress(DLLHandle, 'sqlite3_finalize');
    if not Assigned( @SQLite3_Finalize) then exit;
    @SQLite3_Reset:= GetProcAddress(DLLHandle, 'sqlite3_reset');
    if not Assigned( @SQLite3_Reset) then exit;
    @SQLite3_Step:=GetProcAddress(DLLHandle, 'sqlite3_step');
    if not Assigned( @SQLite3_Step) then exit;
    @SQLite3_Column_blob:=GetProcAddress(DLLHandle, 'sqlite3_column_blob');
    if not Assigned( @SQLite3_Column_blob) then exit;
    @SQLite3_Column_bytes:=GetProcAddress(DLLHandle, 'sqlite3_column_bytes');
    if not Assigned( @SQLite3_Column_bytes) then exit;
    @SQLite3_Column_count:=GetProcAddress(DLLHandle, 'sqlite3_column_count');
    if not Assigned( @SQLite3_Column_Count) then exit;
    @SQLite3_Column_decltype:=GetProcAddress(DLLHandle, 'sqlite3_column_decltype');
    if not Assigned( @SQLite3_Column_decltype) then exit;
    @SQLite3_Column_double:=GetProcAddress(DLLHandle, 'sqlite3_column_double');
    if not Assigned( @SQLite3_Column_double) then exit;
    @SQLite3_Column_int:=GetProcAddress(DLLHandle, 'sqlite3_column_int');
    if not Assigned( @SQLite3_Column_int) then exit;
    @SQLite3_Column_int64:=GetProcAddress(DLLHandle, 'sqlite3_column_int64');
    if not Assigned( @SQLite3_Column_int64) then exit;
    @SQLite3_Column_name:=GetProcAddress(DLLHandle, 'sqlite3_column_name');
    if not Assigned( @SQLite3_Column_name) then exit;
    @SQLite3_Column_text:=GetProcAddress(DLLHandle, 'sqlite3_column_text');
    if not Assigned( @SQLite3_Column_text) then exit;
    @SQLite3_Column_type:=GetProcAddress(DLLHandle, 'sqlite3_column_type');
    if not Assigned( @SQLite3_Column_type) then exit;
    @SQLite3_Version := GetProcAddress(DLLHandle,'sqlite3_libversion');
    if not Assigned( @SQLite3_Version) then exit;
    @SQLite3_Bind_Blob:=GetProcAddress(DLLHandle, 'sqlite3_bind_blob');
    if not Assigned( @SQLite3_Bind_blob) then exit;
    Result := true;
  end;
 finally
 end;
end;

procedure TtiSQLite3DB.ShowError;
var msg : pchar;
begin
  msg := SQLite3_ErrorString(DBHandle);
  raise EDatabaseError.Create(msg);
end;

function TtiSQLite3DB.SQLite3_ErrorMsg:String;
begin
  Result := StrPas(SQLite3_ErrorString(DBHandle));
end;

function TtiSQLite3DB.SQLite3_ExecSQL(const TheStatement : string) : integer;
var
 p : pointer;
 T : pointer;
 rv : integer;
begin
  rv := sqlite3_prepare(DBHandle, PChar(TheStatement), -1, p, t);
  if rv <> 0 then ShowError else begin
     repeat
        rv := sqlite3_step(p);
     until rv = sqlite_done;
  end;
  rv := sqlite3_finalize(p);
  if rv <> 0 then ShowError;
  Result := rv;
end;

function TtiSQLite3DB.SQLite3_ExecSQL(const TheStatement : string;VM:Pointer) : integer;
var
 T : pointer;
 rv : integer;
begin
  rv := sqlite3_prepare(DBHandle, PChar(TheStatement), -1, VM, t);
  Result := rv;
end;

function TtiSQLite3DB.SQLite3_Execute(db: Pointer;TheStatement: string; FParams: TParams; Sender : TObject) : integer;
var
 i : integer;
 p : pointer;
 t : pointer;
 rv : integer;
 mv : integer;
 RowIdCol : integer;
 RowId : integer;   
 colname, coltype : pchar;
 FieldType : TFieldType;
 FieldLen : integer;
 FieldDec : integer;
 bFirst : boolean;
 convertbuf : TConvertBuffer;
 PData : Pchar;
 BlobStream : TMemoryStream;
 wildcard : integer;
begin
 RowIdCol := -1;
 RowId := 0;
 Result := 0;
 if not (Sender is TtiSQLite3BaseQuery) then exit;
 try
   with (Sender as TtiSQLite3BaseQuery) do begin
     TheStatement := StringReplace(TheStatement, #2, '?', [rfReplaceAll, rfIgnoreCase]);

    bFirst := true;
    rv := sqlite3_prepare(DBHandle, PChar(TheStatement), -1, p, t);
    try
      wildcard := 1;
      if Assigned(FParams) then begin
        for i := 0 to FParams.Count -1 do begin
          if FParams[i].DataType in [ftBlob, ftGraphic] then begin
            SQLite3_Bind_Blob(p, WildCard, PChar(FParams[i].AsBlob),FParams[i].GetDataSize, nil);
            inc(WildCard);
          end;
        end;
      end;

      if rv <> 0 then ShowError else begin
        repeat
          FillChar(ResultStr^, MaxBuf, 0);
          rv := sqlite3_step(p);
          if rv = sqlite_row then begin
            if bFirst then begin  // retrieve metadata on first row
              FieldDefs.Clear;
              bFirst := false;
              if sqlite3_column_count(p) > 0 then FieldDefs.Clear;
              for i := 0 to sqlite3_column_count(p)-1 do begin
                colname := sqlite3_column_name(p, i);
                if CompareText(colname,'rowid')=0 then begin
                  RowIdCol := i;
                end else begin
                  coltype := sqlite3_column_decltype(p, i);
                  GetFieldInfo(ColType, FieldType, FieldLen, FieldDec);
                  with FieldDefs.AddFieldDef do begin
                    if FieldType <> ftString then begin
                      Name := ColName;
                      DataType := FieldType;
                      if FieldType = ftFloat then Precision := FieldDec;
                    end else begin
                      Name := ColName;
                      DataType := FieldType;
                      Size := FieldLen;
                    end;
                  end;
                  MaxStrLen := maxStrLen + GetNativeFieldSize(i+1);
                  FResult.SetBufSize(MaxStrLen + 1 + SizeOf(TBookMark));
                end;
              end;
            end;

            for i := 0 to sqlite3_column_count(p)-1 do begin
              if i = RowIdCol then begin 
                RowId := SQLite3_Column_int(p,i);
              end else begin
                PData := sqlite3_column_text(p,i);
                if PData <> nil then begin
                  if FieldDefs[i].DataType = ftString then begin
                    mv := GetNativeFieldSize(i + 1);
                    if StrLen(PData) < Cardinal(mv) then mv := StrLen(PData);
                    move(PData^, (ResultStr + GetFieldOffset(i + 1))^, mv);
                  end else begin
                    if FieldDefs[i].DataType in [ftMemo, ftGraphic, ftFMTMemo, ftBlob] then begin
                      BlobStream := TMemoryStream.Create;
                      BlobStream.Write(PData^, lstrlen(PData));
                      move(BlobStream, (ResultStr + GetFieldOffset(i + 1))^, SizeOf(BlobStream));
                    end else begin
                      ConvertBuf := UnpackBuffer(PData, FieldDefs[i].DataType);
                      move(ConvertBuf, (ResultStr + GetFieldOffset(i + 1))^, GetFieldSize(i + 1));
                    end;
                  end;
                end;
              end;
            end;
            FResult.Add(ResultStr, RowId);
          end;
        until rv = sqlite_done;
      end;
    finally
      sqlite3_reset(p);
      rv := sqlite3_finalize(p);
      if rv <> 0 then ShowMessage(IntToStr(rv));
    end;
  end;
 finally
 end;
end;

procedure TtiSQLite3DB.FSetDatabase(const Database: string);
begin
  FDatabase := Trim(Database);
end;

procedure TtiSQLite3DB.GetTableNames(List: TStrings; SystemTables: boolean = false);
var
  ResultPtr: Pointer;
  ResultStr: ^Pointer;
  RowCount: cardinal;
  ColCount: cardinal;
  ErrMsg: pchar;
  i: integer;
begin
  if not FConnected then
    Connected := true;
  if FConnected then
  begin
    SQLite3_GetTable(DBHandle, pchar(
      'SELECT name FROM sqlite_master WHERE type="table" ORDER BY name'),
      ResultPtr, RowCount, ColCount, ErrMsg);

    ResultStr := ResultPtr;
    List.Clear;
    Inc(ResultStr); // ignore header                  
    for i := 1 to RowCount do
    begin
      if (CompareText('name', pchar(ResultStr^)) <> 0) then
        List.Add(pchar(ResultStr^));
      Inc(ResultStr);
    end;
    if Assigned(ResultPtr) then SQLite3_FreeTable(ResultPtr);
  end;
end;

procedure TtiSQLite3DB.GetIndexNames(List: TStrings; SystemTables: boolean = false);
var
  ResultPtr: Pointer;
  ResultStr: ^Pointer;
  RowCount: cardinal;
  ColCount: cardinal;
  ErrMsg: pchar;
  i: integer;
begin
  if not FConnected then
    Connected := true;
  if FConnected then
  begin
    SQLite3_GetTable(DBHandle, pchar(
      'SELECT name FROM sqlite_master WHERE type="index" ORDER BY name'),
      ResultPtr, RowCount, ColCount, ErrMsg);

    ResultStr := ResultPtr;
    List.Clear;
    Inc(ResultStr); // ignore header
    for i := 1 to RowCount do
    begin
      List.Add(pchar(ResultStr^));
      Inc(ResultStr);
    end;
    if Assigned(ResultPtr) then SQLite3_FreeTable(ResultPtr);
  end;
end;

procedure TtiSQLite3DB.GetFieldNames(const TableName: string; List: TStrings);
var
  ResultPtr: Pointer;
  ResultStr: ^Pointer;
  RowCount: cardinal;
  ColCount: cardinal;
  ErrMsg: pchar;
  i: integer;
begin
  if not FConnected then
    Connected := true;
  if FConnected then
  begin
    SQLite3_GetTable(DBHandle, pchar('PRAGMA table_info("' + TableName + '");'),
      ResultPtr, RowCount, ColCount, ErrMsg);
    ResultStr := ResultPtr;
    List.Clear;
    Inc(ResultStr, 6); // headers can be ignored
    for i := 1 to RowCount do
    begin
      Inc(ResultStr);
      List.Add(pchar(ResultStr^));    // the second field contains the fieldname
      Inc(ResultStr, 5);
    end;
    if Assigned(ResultPtr) then SQLite3_FreeTable(ResultPtr);
  end;
end;

procedure TtiSQLite3DB.GetFieldDefInfo(const TableName,FieldName: string; var FieldType: TFieldType;
  var FieldLen, FieldDec: integer);
var
  ResultPtr: Pointer;
  ResultStr: ^Pointer;
  RowCount: cardinal;
  ColCount: cardinal;
  ErrMsg: pchar;
  i: integer;
  Temp : String;
begin
  if not FConnected then
    Connected := true;
  if FConnected then
  begin
    SQLite3_GetTable(DBHandle, pchar('PRAGMA table_info("' + TableName + '");'),
      ResultPtr, RowCount, ColCount, ErrMsg);
    ResultStr := ResultPtr;
    Inc(ResultStr, 6); // headers can be ignored
    for i := 1 to RowCount do
    begin
      Inc(ResultStr);
      Temp := pchar(ResultStr^);
      if Temp=FieldName then begin
        Inc(ResultStr);
        Temp := pchar(ResultStr^);
        GetFieldInfo(Temp,FieldType,FieldLen,FieldDec);
        exit;
      end;
      Inc(ResultStr, 4);
    end;
    if Assigned(ResultPtr) then SQLite3_FreeTable(ResultPtr);
  end;
end;

procedure TtiSQLite3DB.GetPrimaryKeys(const TableName: string; List: TStrings);
var
  ResultPtr: Pointer;
  ResultStr: ^Pointer;
  RowCount: cardinal;
  ColCount: cardinal;
  ErrMsg: pchar;
  Temp:   string;
  i:      integer;
begin
  if not FConnected then
    Connected := true;
  if FConnected then
  begin
    SQLite3_GetTable(DBHandle, pchar('PRAGMA table_info("' + TableName + '");'),
      ResultPtr, RowCount, ColCount, ErrMsg);
    ResultStr := ResultPtr;
    List.Clear;
    Inc(ResultStr, 6); // headers can be ignored
    for i := 1 to RowCount do
    begin
      Inc(ResultStr);
      Temp := pchar(ResultStr^);
      Inc(ResultStr, 4);
            // the last field reveils a indicator for primary key
      if pchar(ResultStr^) = '1' then
        List.Add(Temp);
      Inc(ResultStr);
    end;
    if Assigned(ResultPtr) then SQLite3_FreeTable(ResultPtr);
  end;
end;

procedure TtiSQLite3DB.GetTableInfo(const TableName: string; List: TList);
var
  ResultPtr: Pointer;
  ResultStr: ^Pointer;
  RowCount: cardinal;
  ColCount: cardinal;
  ErrMsg: pchar;
  Field: TtiSQLite3Field;
  i: integer;
begin
  if not FConnected then
    Connected := true;
  if FConnected then
  begin
    SQLite3_GetTable(DBHandle, pchar('PRAGMA table_info("' + TableName + '");'),
      ResultPtr, RowCount, ColCount, ErrMsg);
    ResultStr := ResultPtr;
    while List.Count > 0 do
    begin
      TtiSQLite3Field(List[0]).Free;
      List.Delete(0);
    end;
    List.Clear;

    for i := 1 to RowCount do
    begin
      Field := TtiSQLite3Field.Create;
      with Field do
      begin
        FieldNumber := StrToIntX(pchar(ResultStr^));
        Inc(ResultStr);
        FieldName := pchar(ResultStr^);
        Inc(ResultStr);
        FieldType := pchar(ResultStr^);
        Inc(ResultStr);
        FieldNN := StrToIntX(pchar(ResultStr^));
        Inc(ResultStr);
        FieldDefault := pchar(ResultStr^);
        Inc(ResultStr);
        FieldPK := StrToIntX(pchar(ResultStr^));
        Inc(ResultStr);
      end;
      List.Add(Field);
    end;
    if Assigned(ResultPtr) then SQLite3_FreeTable(ResultPtr);
  end;
end;

procedure TtiSQLite3DB.GetTableIndexNames(const TableName: String; List: TStrings);
var
  ResultPtr: Pointer;
  ResultStr: ^Pointer;
  RowCount: cardinal;
  ColCount: cardinal;
  ErrMsg: pchar;
  i: integer;
begin
  if not FConnected then
    Connected := true;
  if FConnected then
  begin
    SQLite3_GetTable(DBHandle, pchar(
      'PRAGMA  index_list("' + TableName + '");'),
      ResultPtr, RowCount, ColCount, ErrMsg);

    ResultStr := ResultPtr;
    List.Clear;
    Inc(ResultStr, 4);     // Skip header + 1st col.
    for i := 1 to RowCount do
    begin
      List.Insert(0, pchar(ResultStr^));
      Inc(ResultStr, 3);
    end;
    if Assigned(ResultPtr) then SQLite3_FreeTable(ResultPtr);
  end;
end;

procedure TtiSQLite3DB.DBConnect(Connected: boolean);
var
  ErrMsg: pchar;
  DBMS:   string;
begin
  if (Connected) and (FDatabase = '') then
  begin
    raise tiSQLiteError.Create('Missing database property');
    SQLite3_FreeMem(ErrMsg);
    FConnected := false;
    exit;
  end;

  if not Connected then
  begin
    if FConnected then
    begin
      if DLLHandle <> 0 then
      begin
        if Assigned(FBeforeDisconnect) then
          FBeforeDisconnect(self);
        if Assigned( @SQLite3_Close) then
          SQLite3_Close(DBHandle);
        FreeLibrary(DLLHandle);
        DLLHandle := 0;
        if Assigned(FAfterDisconnect) then
          FAfterDisconnect(self);
      end;
      FConnected := false;
      exit;
    end
  end
  else
  begin
    if DefaultDir <> '' then
    begin
      if DefaultDir[Length(DefaultDir)] <> '\' then
        DefaultDir := DefaultDir + '\';
      DBMS := DefaultDir + Database;
    end
    else
    begin
      if Pos('\', Database) = 0 then
        DBMS := GetCurrentDir + '\' + DataBase
      else
        DBMS := Database;
    end;                                                

    if FMustExist then
    begin
      if CompareText(':memory:', Database) <> 0 then
      begin
        if not FileExists(DBMS) then
        begin
          raise EDatabaseError.Create('Database ' + DBMS + ' does not exist');
          exit;
        end;
      end;
    end;

    if DLLHandle = 0 then
    begin
      if not LoadLibs then
      begin
        FConnected := false;
        raise tiSQLiteError.Create('Could not load SQLite library');
        exit;
      end;
    end;

    FConnected := true;
    DBHandle   := nil;
    ErrMsg     := nil;
    if Assigned(FBeforeConnect) then FBeforeConnect(self);
    if Assigned( @SQLite3_Open) then SQLite3_Open(pchar(DBMS), DBHandle);

    if Assigned(FAfterConnect) then  FAfterConnect(self);
    if DBHandle = nil then           FConnected := false;

    FLastError := ErrMsg;
    if ErrMsg <> nil then            SQLite3_FreeMem(ErrMsg);
  end;
end;

function TtiSQLite3DB.RowsAffected: integer;
begin
  if not FConnected then Result := -1
  else Result := SQLite3_Changes(DBHandle);
end;

function  TtiSQLite3DB.GetVersion:String;
begin
  Result := SQLite3_Version;
end;

procedure TtiSQLite3DB.StartTransaction;
begin
  if not FConnected then Connected := true; // trigger the 'dbconnect' event
  if FConnected     then SQLite3_ExecSQL('begin transaction');
end;

procedure TtiSQLite3DB.Open;
begin
  Connected := true;
  if DLLHandle = 0 then Connected := false;
end;

procedure TtiSQLite3DB.Close;
begin
  Connected := false;
end;

procedure TtiSQLite3DB.Commit;
begin
  if not FConnected then Connected := true;
  if FConnected then SQLite3_ExecSQL('commit transaction');
end;

procedure TtiSQLite3DB.RollBack;
begin
  if not FConnected then Connected := true;
  if FConnected then  SQLite3_ExecSQL('rollback transaction');
end;

constructor TtiSQLite3DB.Create;
begin
  Connected     := false;
  inherited;
end;

destructor TtiSQLite3DB.Destroy;
begin
  FConnected    := false;
  inherited Destroy;
end;

{
 Register detail dataset for a master-detail relationship
}
procedure TtiSQLite3BaseQuery.RegisterDetailDataset(DetailDataSet: TtiSQLite3BaseQuery);
var
  i: integer;
begin
 try
  for i := 0 to DetailList.Count - 1 do
    if DetailList[i] = DetailDataset then exit;
  DetailList.Add(DetailDataSet);
 finally
 end;
end;

procedure TtiSQLite3BaseQuery.SQLiteMasterChanged;
begin
end;

procedure TtiSQLite3BaseQuery.NotifySQLiteMasterChanged;
var
  i: integer;
begin
  for i := 0 to DetailList.Count - 1 do
    TtiSQLite3BaseQuery(DetailList[i]).SQLiteMasterChanged;
end;

function TtiSQLite3BaseQuery.YYYYMMDDParser(const Str: PChar): TDateTime;
var
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  Result := 0;

  try
    if Length(Str) >= 10 then // 10 = Length of YYYY-MM-DD
    begin
      Year := StrToInt(Copy(Str, 1, 4));
      Month := StrToInt(Copy(Str, 6, 2));
      Day := StrToInt(Copy(Str, 9, 2));

      Result := EncodeDate(Year, Month, Day);
    end;

    if Length(Str) > 10 then // it has a time
    begin
      Hour := StrToInt(Copy(Str, 12, 2));
      Min := StrToInt(Copy(Str, 15, 2));
      Sec := 0;
      MSec := 0;
      if Length(Str) > 16 then Sec := StrToInt(Copy(Str, 18, 2));
      if Length(Str) > 19 then Msec := StrToInt(Copy(Str, 21, 3));
      Result := Result + EncodeTime(Hour, Min, Sec, MSec);
    end;
  except
    Result := 0;
  end;
end;

function TtiSQLite3BaseQuery.UnpackBuffer(Buffer: pchar; FieldType: TFieldType) : TConvertBuffer;
var
  TempInt:    integer;
  TempDouble: double;
{$IFDEF SQLite_D6PLUS}
  TempBool   : WordBool;
{$ENDIF}
  TempT:      TDateTimeRec;
begin
  case FieldType of ftString: exit;
    ftInteger, ftSmallInt:
    begin
      TempInt := StrToIntX(Buffer);
      Move(TempInt, result, sizeof(TempInt));
    end;
    ftTime:
    begin
      TempT := DateTimeToNative(FieldType, StrToDateTimeX(Buffer));
      Move(TempT, result, sizeof(TDateTime));
    end;
    ftDate:
    begin
      TempT := DateTimeToNative(FieldType, StrToDateTimeX(Buffer));
      Move(TempT, result, sizeof(TDateTime));
    end;
    ftDateTime:
    begin
      TempT := DateTimeToNative(FieldType, YYYYMMDDParser(Buffer));
      Move(TempT, result, sizeof(TDateTime));
    end;
{$IFDEF SQLite_D6PLUS}
    ftTimeStamp:
    begin
      TempT := DateTimeToNative(FieldType, YYYYMMDDParser(Buffer));
      Move(TempT, result, sizeof(TDateTime));
    end;
{$ENDIF}
    ftFloat, ftBCD, ftCurrency:
    begin
      TempDouble := StrToFloatX(Buffer);
      Move(TempDouble, result, sizeof(TempDouble));
    end;
{$IFDEF SQLITE_D6PLUS}
    ftBoolean:
    begin
      TempBool := StrToBool(Buffer);
      Move(TempBool, result, sizeof(TempBool));
    end;
{$ENDIF}
    ftMemo, ftGraphic, ftBlob, ftFMTMemo: // pointer to stream
    begin
      TempInt := StrToIntX(Buffer);
      Move(TempInt, result, sizeof(TempInt));
    end;
  end;
end;

constructor TtiSQLite3BaseQuery.Create;
begin
  MaxStrLen := 0;
  FSQL      := TStringList.Create;
  DetailList := TList.Create;
  GetMem(ResultStr, MaxBuf);
  inherited Create(nil);
end;

function TtiSQLite3BaseQuery.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result := TtiSQLite3BlobStream.Create(Field as TBlobField, Mode);
end;

destructor TtiSQLite3BaseQuery.Destroy;
begin
  inherited Destroy;
  if Assigned(FSQL) then FSQL.Free;
  FSQL := nil;
  if Assigned(DetailList) then DetailList.Free;
  DetailList := nil;
  if Assigned(FConnection) then FConnection := nil;
  if Assigned(ResultStr) then   FreeMem(ResultStr, MaxBuf);
  if Assigned(FResult) then begin
    FResult.Free;
    FResult := nil;
  end;
  ResultStr := nil;
end;

procedure TtiSQLite3BaseQuery.StartTransaction;
begin
  if Assigned(FConnection) then FConnection.StartTransaction;
end;

procedure TtiSQLite3BaseQuery.Commit;
begin
  if Assigned(FConnection) then FConnection.Commit;
end;

procedure TtiSQLite3BaseQuery.RollBack;
begin
  if Assigned(FConnection) then FConnection.RollBack;
end;

function TtiSQLite3BaseQuery.Locate(const KeyFields: string;
 const KeyValues: variant; Options: TLocateOptions): boolean;
var
 bOk:     boolean;
 i, j, p: integer;
 Fields:  string;
 FieldList: TStringList;
 DebugStr: string;
begin
 FieldList := TStringList.Create;
 bOk := true;
 try
   Fields := KeyFields;
   p := pos(';', Fields);
   while p > 0 do
   begin
     FieldList.Add(Copy(Fields, 1, p - 1));
     System.Delete(Fields, 1, p);
     p := pos(';', Fields);
   end;
   if Fields <> '' then
     FieldList.Add(Fields);

   First;
   for i := 1 to FResult.Data.Count do
   begin
     SetRecNo(i);
     bOk := true;
     for j := 0 to FieldList.Count - 1 do
     begin
       if loCaseInsensitive in Options then
       begin
         if FieldList.Count = 1 then
         begin
           if CompareText(KeyValues, FieldByName(FieldList[j]).AsString) <> 0 then
           begin
             bOk := false;
             break;
           end;
         end
         else
         begin
           if CompareText(KeyValues[j], FieldByName(FieldList[j]).AsString) <> 0 then
           begin
             bOk := false;
             break;
           end;
         end
       end
       else
       begin
         if FieldList.Count = 1 then
         begin
           if CompareStr(KeyValues, FieldByName(FieldList[j]).AsString) <> 0 then
           begin
             bOk := false;
             break;
           end;
         end
         else
         begin
           if CompareStr(KeyValues[j], FieldByName(FieldList[j]).AsString) <> 0 then
           begin
             bOk := false;
             break;
           end;
         end;
       end;
     end;
     if bOk then
     begin
       break;
     end;
   end;
   if bOk then
   begin
     Result := true;
     DebugStr := 'TtiSQLite3BaseQuery.Locate true';
   end
   else
   begin
     Result := false;
     DebugStr := 'TtiSQLite3BaseQuery.Locate false';
   end;
 finally
   FieldList.Free;
 end;
end;

function TtiSQLite3BaseQuery.GetActiveBuffer(var Buffer: pchar): boolean;
begin
  case State of
    dsBrowse: if IsEmpty then
        Buffer := nil
      else
        Buffer := ActiveBuffer;
    dsEdit:   Buffer := ActiveBuffer;
    dsInsert: Buffer := ActiveBuffer;
    dsFilter: Buffer := ActiveBuffer;
    else Buffer := nil;
  end;
  Result := Buffer <> nil;
end;

function TtiSQLite3BaseQuery.GetNativeFieldSize(FieldNo: integer): integer;
begin
  Result := 0;
  case FieldDefs.Items[FieldNo - 1].Datatype of
    ftString: Inc(Result, FieldDefs.Items[FieldNo - 1].Size);
    ftInteger, ftSmallInt, ftDate, ftTime: Result := 12;
    ftDateTime: Result := 20;
{$IFDEF SQLite_D6PLUS}
    ftTimeStamp: Result := 20;
{$ENDIF}
    ftFloat, ftBCD, ftCurrency: Result := 12;
    ftBoolean: Result  := 12;
    ftGraphic, ftMemo, ftBlob, ftFmtMemo: Result := 12; // space for memory handles
    else
      raise tiSQLiteError.Create('GetNativeFieldSize: Fieldtype of Field "' + FieldDefs.Items[FieldNo - 1].Name +
        '" not supported!');
  end;
end;

function TtiSQLite3BaseQuery.GetFieldSize(FieldNo: integer): integer;
begin
  Result := 0;
  case FieldDefs.Items[FieldNo - 1].Datatype of
    ftString: Inc(Result, FieldDefs.Items[FieldNo - 1].Size);
    ftInteger, ftSmallInt, ftDate, ftTime: Inc(Result, sizeof(integer));
{$IFDEF SQLite_D6PLUS}
    ftTimeStamp: Inc(Result, sizeof(TDateTime));
{$ENDIF}
    ftDateTime: Inc(Result, sizeof(TDateTime));
    ftFloat, ftBCD, ftCurrency: Inc(Result, sizeof(double));
    ftBoolean: Inc(Result, sizeof(wordbool));
    ftGraphic, ftMemo, ftBlob, ftFmtMemo : Inc(Result, sizeof(pointer));
    else
      raise tiSQLiteError.Create('GetFieldSize: Fieldtype of Field "' + FieldDefs.Items[FieldNo - 1].Name +
        '" not supported!');
  end;
end;

function TtiSQLite3BaseQuery.GetFieldSize(Field : TField): integer;
begin
  Result := 0;
  case Field.DataType of
    ftString: Inc(Result, Field.Size);
    ftInteger, ftSmallInt, ftDate, ftTime: Inc(Result, sizeof(integer));
    ftDateTime: Inc(Result, sizeof(TDateTime));
{$IFDEF SQLite_D6PLUS}
    ftTimeStamp: Inc(Result, sizeof(TDateTime));
{$ENDIF}
    ftFloat, ftBCD, ftCurrency: Inc(Result, sizeof(double));
    ftBoolean: Inc(Result, sizeof(wordbool));
    ftGraphic, ftMemo, ftBlob, ftFmtMemo : Inc(Result, sizeof(pointer));
    else
      raise tiSQLiteError.Create('Fieldtype of Field "' + Field.FieldName +
        '" not supported!');
  end;
end;

function TtiSQLite3BaseQuery.GetFieldOffset(FieldNo: integer): integer;
var
  i:      integer;
  Offset: integer;
begin
  Offset := 0;
  if FieldNo > 1 then
    for i := 1 to FieldNo - 1 do
      OffSet := OffSet + GetFieldSize(i);
  Result := Offset;
end;

procedure TtiSQLite3BaseQuery.SetSQL(const Value: TStrings);
begin
  Close;
  if Assigned(FSQL) then FSQL.Assign(Value)
  else FSQL := Value;
end;

function TtiSQLite3BaseQuery.StrToFloatX(const StrIn : string) : Double;
begin
 try
    Result := StrToFloat(StrIn);
 except
    Result := 0;
 end;
end;

function TtiSQLite3BaseQuery.StrToDateTimeX(const S: string): TDateTime;
var
  df, tf,lf : String;
begin
  df := ShortDateFormat;
  tf := ShortTimeFormat;
  lf := LongTimeFormat;
  ShortDateFormat := 'yyyy-mm-dd';
  ShortTimeFormat := 'hh:mm';
  LongTimeFormat  := 'hh:nn:ss:zzz';
  if S = '' then
     Result := 0
  else
     Result := StrToDateTime(S);
  ShortDateFormat := df;
  ShortTimeFormat := tf;
  LongTimeFormat  := lf;
end;

procedure TtiSQLite3BaseQuery.LoadQueryData;
begin
  if Connection.FConnected then
     Connection.SQLite3_execute(Connection.DBHandle, pchar(FPrepared), FParams, self);
end;

procedure TtiSQLite3BaseQuery.InternalOpen;
begin
  if (Connection = nil) then
    raise tiSQLiteError.Create('no database connection')
  else
  begin
    if Connection.Connected = false then Connection.Connected := true;

    FResult := TFResult.Create(Self);
    LoadQueryData;
    FCurRec := -1;
    FRecInfoOfs := MaxStrLen;
    FRecBufSize := FRecInfoOfs + SizeOf(TRecInfo);
    BookmarkSize := SizeOf(integer);
    InternalInitFieldDefs;
    if DefaultFields then begin
      try
        CreateFields;
      except
        on e:Exception do ShowMessage(e.Message);
      end;
    end;
    BindFields(true);
  end;
end;

procedure TtiSQLite3BaseQuery.InternalClose;
begin
  if Assigned(FResult) then
  begin
    FResult.Free;
    FResult := nil;
  end;
  if DefaultFields then DestroyFields;
  FCurRec := -1;
end;

function TtiSQLite3BaseQuery.IsCursorOpen: boolean;
begin
  Result := Assigned(FResult);
end;

procedure TtiSQLite3BaseQuery.InternalInitFieldDefs;
begin
end;

procedure TtiSQLite3BaseQuery.InternalHandleException;
begin
  Application.HandleException(Self);
end;

procedure TtiSQLite3BaseQuery.InternalGotoBookmark(Bookmark: Pointer);
var
  Index: integer;
begin
  Index := FResult.IndexOf(TObject(PInteger(Bookmark)^));
  if Index <> -1 then FCurRec := Index
  else DatabaseError('Bookmark not found');
end;

function TtiSQLite3BaseQuery.BookmarkValid(Bookmark: Pointer) : boolean;
var
  Index: integer;
begin
  Index := FResult.IndexOf(TObject(PInteger(Bookmark)^));
  if Index <> -1 then Result := true
  else Result := false;
end;

procedure TtiSQLite3BaseQuery.InternalSetToRecord(Buffer: pchar);
begin
  InternalGotoBookmark( @PRecInfo(Buffer + FRecInfoOfs).Bookmark);
  NotifySQLiteMasterChanged;
end;

function TtiSQLite3BaseQuery.GetBookmarkFlag(Buffer: pchar): TBookmarkFlag;
begin
  Result := PRecInfo(Buffer + FRecInfoOfs).BookmarkFlag;
end;

function TtiSQLite3BaseQuery.GetCalcFieldOffset(Field : TField): integer;
var
  i:      integer;
  Offset: integer;
begin
  Offset := FRecBufSize + sizeof(TRecInfo);
  for i := 0 to FieldList.Count - 1 do begin
       if CompareText(FieldList[i].FieldName,Field.FieldName)=0 then begin
         Result := Offset;
         exit;
       end;
       if FieldList[i].Calculated then
          OffSet := OffSet + GetFieldSize(Field);
  end;
  Result := Offset;
end;

procedure TtiSQLite3BaseQuery.SetBookmarkFlag(Buffer: pchar; Value: TBookmarkFlag);
begin
  PRecInfo(Buffer + FRecInfoOfs).BookmarkFlag := Value;
end;

procedure TtiSQLite3BaseQuery.GetBookmarkData(Buffer: pchar; Data: Pointer);
begin
  PInteger(Data)^ := PRecInfo(Buffer + FRecInfoOfs).Bookmark;
end;

procedure TtiSQLite3BaseQuery.SetBookmarkData(Buffer: pchar; Data: Pointer);
begin
  PRecInfo(Buffer + FRecInfoOfs).Bookmark := PInteger(Data)^;
end;

function TtiSQLite3BaseQuery.GetRecordSize: word;
begin
  Result := MaxStrLen;
end;

function TtiSQLite3BaseQuery.AllocRecordBuffer: pchar;
begin
  GetMem(Result, FRecBufSize+CalcFieldsSize+sizeof(TRecinfo)+5);
  FillChar(Result^, FRecBufSize+CalcFieldsSize+sizeof(TRecinfo)+5, 0);
end;

procedure TtiSQLite3BaseQuery.FreeRecordBuffer(var Buffer: pchar);
begin
  try FreeMem(Buffer);
  except end;
end;

function TtiSQLite3BaseQuery.GetRecord(Buffer: pchar; GetMode: TGetMode;
  DoCheck: boolean): TGetResult;
var
  ptr: pointer;
begin
  if FResult.Count < 1 then
    Result := grEOF
  else
  begin
    Result := grOK;
    case GetMode of
      gmNext:
        if FCurRec >= RecordCount - 1 then Result := grEOF
        else Inc(FCurRec);
      gmPrior:
        if FCurRec <= 0 then Result := grBOF
        else Dec(FCurRec);
      gmCurrent:
        if (FCurRec < 0) or (FCurRec >= RecordCount) then Result := grError;
    end;
    if Result = grOK then
    begin
      ptr := FResult.GetData(FCurRec);
      Move(ptr^, Buffer^, MaxStrLen);
      if FResult.Count = 0 then InternalInitRecord(Buffer);
      with PRecInfo(Buffer + FRecInfoOfs)^ do
      begin
        BookmarkFlag := bfCurrent;
        Bookmark     := FResult.GetBookMark(FCurRec);
      end;
    end
    else if (Result = grError) and DoCheck then
      DatabaseError('No Records');
  end;
end;

procedure TtiSQLite3BaseQuery.InternalInitRecord(Buffer: pchar);
var
  i:     integer;
  TempT: TDateTimeRec;
  Stream: TMemoryStream;  
begin
  for i := 0 to FieldCount - 1 do
  begin
    case FieldDefs.Items[i].Datatype of
      ftMemo, ftGraphic, ftBlob, ftFmtMemo : begin
        Stream := TMemoryStream.Create;
        Move(Pointer(Stream), (Buffer + GetFieldOffset(i+1))^, sizeof(Pointer));
      end;
      ftString: pchar(Buffer + GetFieldOffset(i + 1))^    := #0;
      ftBoolean: pBoolean(Buffer + GetFieldOffset(i + 1))^ := false;
      ftFloat: pFloat(Buffer + GetFieldOffset(i + 1))^    := 0;
      ftSmallInt: pSmallInt(Buffer + GetFieldOffset(i + 1))^ := 0;
      ftInteger: pInteger(Buffer + GetFieldOffset(i + 1))^ := integer(nil);
      ftCurrency: pFloat(Buffer + GetFieldOffset(i + 1))^ := 0;
      ftDate:
      begin
        TempT := DateTimeToNative(ftDate, now);
        Move(TempT, (Buffer + GetFieldOffset(i + 1))^, sizeof(TDateTime));
      end;
      ftTime:
      begin
        TempT := DateTimeToNative(ftTime, now);
        Move(TempT, (Buffer + GetFieldOffset(i + 1))^, sizeof(TDateTime));
      end;
{$IFDEF SQLITE_D6PLUS}
      ftTimeStamp:
      begin
        TempT := DateTimeToNative(ftDateTime, now);
        Move(TempT, (Buffer + GetFieldOffset(i + 1))^, sizeof(TDateTime));
      end;
{$ENDIF}
      ftDateTime:
      begin
        TempT := DateTimeToNative(ftDateTime, now);
        Move(TempT, (Buffer + GetFieldOffset(i + 1))^, sizeof(TDateTime));
      end;
    end;
  end;
end;

function TtiSQLite3BaseQuery.GetFieldData(Field: TField; Buffer: Pointer): boolean;
var
  SrcBuffer: pchar;
begin
  if Field.FieldNo > 0 then begin
     Result := true; // indicates NotNull
     if GetActiveBuffer(SrcBuffer) then begin
        if (Assigned(Buffer)) and (Assigned(SrcBuffer)) then begin
            Move((SrcBuffer + GetFieldOffset(Field.FieldNo))^, Buffer^, GetFieldSize(Field.FieldNo));
            if Field.DataType = ftString then
               pchar(pchar(Buffer) + GetFieldSize(Field.FieldNo))^:=#0; // dev
            Result := true;
            exit;
        end;
        if Assigned(SrcBuffer) then
          if (Field.DataType <> ftDateTime) and ((SrcBuffer + GetFieldOffset(Field.FieldNo))^=#0) then
            Result := false
     end else begin
        pchar(Buffer)^ := #0;
        Result := false;
     end;
  end else begin {calcfields}
      Result := GetActiveBuffer(SrcBuffer);
      if Result and (State in [dsBrowse, dsEdit, dsInsert, dsCalcFields, dsBlockRead]) then begin
         if (Assigned(Buffer)) then
            Move((SrcBuffer  + GetCalcFieldOffset(Field))^, Buffer^, GetFieldSize(Field));
      end;
  end;
end;

function TtiSQLite3BaseQuery.GetFieldData(FieldNo: integer; Buffer: Pointer): boolean;
begin
  Result := GetFieldData(FieldByNumber(FieldNo), Buffer);
end;

procedure TtiSQLite3BaseQuery.SetFieldData(Field: TField; Buffer: Pointer);
var
  DestBuffer: pchar;
begin
  GetActiveBuffer(DestBuffer);
  if (Field.FieldNo > 0) and (Assigned(Buffer)) and (Assigned(DestBuffer)) then
  begin
    Move(Buffer^, (DestBuffer + GetFieldOffset(Field.FieldNo))^, // was activebuffer
      GetFieldSize(Field.FieldNo));
  end;
  DataEvent(deFieldChange, longint(Field));
end;

procedure TtiSQLite3BaseQuery.InternalFirst;
begin
  FCurRec := -1;
end;

procedure TtiSQLite3BaseQuery.InternalLast;
begin
  FCurRec := FResult.Count;
end;

function TtiSQLite3BaseQuery.GetLastInsertRow : integer;
begin
  if Assigned(Connection) then
    result := Connection.SQLite3_LastInsertRow(Connection.DBHandle)
  else result := -1;
end;

procedure TtiSQLite3BaseQuery.InternalPost;
var
  ptr: Pointer;
begin
  FSaveChanges := true;
  if State = dsEdit then
  begin
    ptr := FResult.GetData(FCurrec);
    move(ActiveBuffer^, ptr^, FRecBufSize);
  end
  else
  begin
    FResult.Insert(FCurRec, ActiveBuffer,
      Connection.SQLite3_LastInsertRow(Connection.DBHandle));
  end;
end;

procedure TtiSQLite3BaseQuery.InternalAddRecord(Buffer: Pointer; Append: boolean);
begin
  FSaveChanges := true;
  if Append then InternalLast;
  FResult.Insert(FCurRec, pchar(Buffer), Connection.SQLite3_LastInsertRow(
    Connection.DBHandle));
end;

procedure TtiSQLite3BaseQuery.InternalDelete;
begin
  FSaveChanges := true;
  FResult.Delete(FCurRec);
  if FCurRec >= FResult.Count then Dec(FCurRec);
end;

function TtiSQLite3BaseQuery.GetRecordCount: longint;
begin
  Result := FResult.Count;
end;

function TtiSQLite3BaseQuery.GetRecNo: longint;
begin
  UpdateCursorPos;
  if (FCurRec = -1) and (RecordCount > 0) then Result := 1
  else Result := FCurRec + 1;
end;

procedure TtiSQLite3BaseQuery.SetRecNo(Value: integer);
begin
  if (Value >= 0) and (Value < FResult.Count + 2) then
  begin
    FCurRec := Value - 1;
    Resync([]);
  end;
end;

procedure TtiSQLite3BaseQuery.SetFiltered(Value: Boolean);
begin
 try
   Inherited;
 finally
  try
   Open;
  except
    on E: Exception do begin
       Filtered := false;
       raise tiSQLiteError.Create('Filter error: '+E.Message);
    end;
  end;
 end;
end;

procedure TtiSQLite3BaseQuery.SetFilterText(const Value: string);
begin
  Close;
  inherited;
end;

constructor TtiSQLite3Query.Create;
begin
  inherited Create;
  FParams := TParams.Create(Self);
  TStringList(FSQL).OnChange := QueryChanged;
end;

destructor TtiSQLite3Query.Destroy;
begin
  TStringList(FSQL).OnChange := nil;
  if Assigned(FParams) then
  begin
    FParams.Free;
    FParams := nil;
  end;
  inherited Destroy;
end;

procedure TtiSQLite3Query.QueryChanged(Sender: TObject);
begin
  FNoResults := false;
  Close;
  SQLStr := FParams.ParseSQL(SQL.Text, true)
end;

procedure TtiSQLite3Query.SetSQL(const Value: TStrings);
begin
  FNoResults := false;
  if Assigned(FSQL) then FSQL.Assign(Value)
  else FSQL := Value;
end;

function TtiSQLite3Query.GetSQL: TStrings;
begin
  GetSQL := FSQL;
end;

procedure TtiSQLite3Query.InternalClose;
begin
  FPrepared := '';
  inherited;
end;

procedure TtiSQLite3Query.InternalOpen;
var
  p: integer;

  function SetQueryParams(InStr: string): string;
  var
    i: integer;
    TempParam: string;
    df, tf, lf : String;
  begin
    df := ShortDateFormat;
    tf := ShortTimeFormat;
    lf := LongTimeFormat;
    ShortDateFormat := 'yyyy-mm-dd';
    ShortTimeFormat := 'hh:mm';
    LongTimeFormat  := 'hh:nn:ss:zzz';
    for i := 0 to FParams.Count - 1 do begin
      if (TParam(Fparams.Items[i]).DataType=ftDateTime) then
        TempParam := DateTimeToStr(Fparams.Items[i].AsDateTime)
{$IFDEF SQLITE_D6PLUS}
      else if (TParam(Fparams.Items[i]).DataType=ftTimeStamp) then
        TempParam := DateTimeToStr(Fparams.Items[i].AsDateTime)
{$ENDIF}
      else TempParam := Fparams.Items[i].AsString;
      if (TempParam='') and (FParams.Items[i].bound) then begin
        InStr := StringReplace(Instr, '?', 'NULL', []);
      end else begin
        //Here we'll replace legitimate '?' characters with an unprintable character
        TempParam := StringReplace(TempParam, '?', #1, [rfReplaceAll]);
        InStr := StringReplace(Instr, '?', QuotedStr(TempParam), [rfIgnoreCase]);
      end;
    end;
    ShortDateFormat := df;
    ShortTimeFormat := tf;
    LongTimeFormat  := lf;
    //Here we'll restore legitimate '?' characters
    InStr := StringReplace(Instr, #1, '?', [rfReplaceAll]);
    InStr := StringReplace(Instr, crlf, ' ', [rfReplaceAll]);
    SetQueryParams := InStr;
  end;

begin
  if Trim(FSQL.Text) = '' then
  begin
    ShowMessage('no query specified');
    abort;
  end;

  if (FMaxResults = 0) and (FStartResult <> 0) then
    FMaxResults := -1;

  FPrepared := SQLStr;

  if (Filtered) and (Filter <> '') then
  begin
    p := pos('where', LowerCase(FPrepared)); // add filter code to usersql
    if p = 0 then
      FPrepared := FPrepared + ' where ' + Filter
    else
      FPrepared := FPrepared + ' and ' + Filter;
  end;

  if FParams.Count > 0 then
    FPrepared := SetQueryParams(FPrepared);

  if FMaxResults <> 0 then
    FPrepared := FPrepared + ' limit ' + IntToStr(FMaxResults);
  if FStartResult <> 0 then
    FPrepared := FPrepared + ' offset ' + IntToStr(FMaxResults);
  inherited;
end;

function TtiSQLite3Query.ParamByName(const Value: string): TParam;
begin
  Result := Params.ParamByName(Value);
end;

function TtiSQLite3Query.ParamCount:Integer;
begin
  Result := Params.Count;
end;

procedure TtiSQLite3Query.ExecSQL;
begin
  FNoResults := true;
  Close;
  if FAutoCommit then
  begin
    Connection.StartTransaction;
    Open;
    try
      Connection.Commit
    except
      Connection.RollBack;
      raise;
    end;
  end
  else
    Open;
end;

procedure TtiSQLite3Query.SetParamsList(Value: TParams);
begin
  FParams.AssignValues(Value);
end;

function TtiSQLite3Query.GetParamsCount: word;
begin
  Result := FParams.Count;
end;

// Blobfields in SQLite are in fact CLOB fields. However, since it is a large
// chunk of data for all types the ftBlob is used. Keep in mind that blobs are
// stored separately of TResult. Within the result structure only the memory
// handle of the blob is stored.

constructor TtiSQLite3BlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
begin
  FField := Field;
  FMode := Mode;
  FDataSet := FField.DataSet as TtiSQLite3BaseQuery;
  if Mode <> bmWrite then LoadBlobData;
end;

destructor TtiSQLite3BlobStream.Destroy;
begin
  if FModified then
    SaveBlobData;
  inherited;
end;

function TtiSQLite3BlobStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := inherited Read(Buffer, Count);
  FOpened := True;
end;

function TtiSQLite3BlobStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := inherited Write(Buffer, Count);
  FModified := True;
  FDataSet.SetModified(true);
end;

procedure TtiSQLite3BlobStream.LoadBlobData;
var
  Stream: TMemoryStream;
  Offset: Integer;
  RecBuffer: PChar;
begin
  Self.Size := 0;
  FDataset.GetActiveBuffer(RecBuffer);

  if RecBuffer <> nil then begin
    Offset := FDataset.GetFieldOffset(FField.FieldNo);
    Move((RecBuffer + Offset)^, Pointer(Stream), sizeof(Pointer));
    Self.CopyFrom(Stream, 0);
  end;
  Position := 0;
end;

procedure TtiSQLite3BlobStream.SaveBlobData;
var
  Stream: TMemoryStream;
  Offset: Integer;
  RecBuffer: Pchar;
begin
  FDataset.GetActiveBuffer(RecBuffer);
  if RecBuffer <> nil then
    begin
      Offset := FDataset.GetFieldOffset(FField.FieldNo);
      Move((RecBuffer + Offset)^, Pointer(Stream), sizeof(Pointer));
      Stream.Size := 0;
      Stream.CopyFrom(Self, 0);
      Stream.Position := 0;
    end;
end;

end.

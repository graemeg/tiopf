unit tiQuery;

{$I tiDefines.inc}

interface
uses
   Classes
//  ,Contnrs
  ,tiObject
  ,tiBaseObject
  ,SysUtils
  ,tiCriteria
  ,tiRTTI
 ;

const
  cErrorInvalidQueryFieldKind    = 'Invalid TtiQueryFieldKind';
  cErrorInvalidQueryFieldKindStr = 'Invalid TtiQueryFieldKind <%s>';
  cErrorInvalidTtiQueryFieldKind = 'Invalid TtiQueryFieldKind';
  cErrorSettingPropValue         = 'Error setting property value for <%s> on <%s> Message <%s>';
type
  // these constant strings map to the PARAM_TYPE column in the SQL Manager param
  // table. Please do not modify them.
  TtiQueryFieldKind = (
                        qfkString,
                        qfkInteger,
                        qfkFloat,
                        qfkDateTime,
                        qfkLogical,
                        qfkBinary,
                        qfkMacro,
                        qfkLongString
                      );
const
  cgaQueryFieldKind : array[TtiQueryFieldKind] of string = (
    'String'     ,
    'Integer'    ,
    'Float'      ,
    'Date'       ,
    'Logical'    ,
    'Binary'     ,
    'Macro'      ,
    'Long string'
 );
(*
  cgaQueryFieldKind : array[TtiQueryFieldKind] of string = (
    {$IFNDEF OPTIMISE_XMLDB_SIZE}'String'     {$ELSE} 's' {$ENDIF},
    {$IFNDEF OPTIMISE_XMLDB_SIZE}'Integer'    {$ELSE} 'i' {$ENDIF},
    {$IFNDEF OPTIMISE_XMLDB_SIZE}'Float'      {$ELSE} 'r' {$ENDIF},
    {$IFNDEF OPTIMISE_XMLDB_SIZE}'Date'       {$ELSE} 'd' {$ENDIF},
    {$IFNDEF OPTIMISE_XMLDB_SIZE}'Logical'    {$ELSE} 'b' {$ENDIF},
    {$IFNDEF OPTIMISE_XMLDB_SIZE}'Binary'     {$ELSE} 'n' {$ENDIF},
    {$IFNDEF OPTIMISE_XMLDB_SIZE}'Macro'      {$ELSE} 'm' {$ENDIF},
    {$IFNDEF OPTIMISE_XMLDB_SIZE}'Long string'{$ELSE} 'l' {$ENDIF}
 );
*)
  cgaQueryFieldKindSQLMgr : array[TtiQueryFieldKind] of string = (
    'String',
    'Integer',
    'Float',
    'Date',
    'Logical',
    'Binary',
    'Macro',
    'Long string'
 );

function tiTypeKindToQueryFieldKind(AValue : TtiTypeKind): TtiQueryFieldKind;

type

  TtiDBConnectionParams = record
    DatabaseName: string;
    UserName: string;
    Password: string;
    Params: string
  end;

  TtiQueryType = (
                   qtSelect,
                   qtInsert,
                   qtUpdate,
                   qtDelete,
                   qtDDL
                 );

  TtiQuery            = class;
  TtiDBMetaData       = class;
  TtiDBMetaDataTable  = class;
  TtiDBMetaDataField  = class;
  TtiQueryParams      = class;
  TtiQueryParamAbs    = class;
  TTableName          = String; //[ 255 ];   //
  TFieldName          = String; // [ 255 ];
  TtiQueryClass       = class of TtiQuery;
  TtiDatabaseClass    = class of TtiDatabase;
  TtiQueryParamClass  = class of TtiQueryParamAbs;


 TtiDBMetaData = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TtiDBMetaDataTable; reintroduce;
    procedure   SetItems(i: integer; const AValue: TtiDBMetaDataTable); reintroduce;
    function    GetCaption : string; override;
//    function    GetOwner: TtiObject; reintroduce;
//    procedure   SetOwner(const AValue: TtiObject); reintroduce;
  public
    property    Items[i:integer]: TtiDBMetaDataTable read GetItems write SetItems;
    procedure   Add(AObject: TtiDBMetaDataTable); reintroduce;
//    property    Owner       : TtiObject   read GetOwner      write SetOwner;
    procedure   Read(const ADBConnectionName: string = ''; APersistenceLayerName : string = ''); override;
    procedure   Clear; override;
    function    FindByTableName(const ATableName : TTableName): TtiDBMetaDataTable;
  end;
  

  TtiDBMetaDataTable = class(TtiObjectList)
  private
    FName : TTableName;
    FMaxFieldWidth : word;
  protected
    function    GetItems(i: integer):TtiDBMetaDataField ; reintroduce;
    procedure   SetItems(i: integer; const AValue: TtiDBMetaDataField); reintroduce;
    function    GetOwner: TtiDBMetaData; reintroduce;
    procedure   SetOwner(const AValue: TtiDBMetaData); reintroduce;
    function    GetCaption : string; override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    property    Items[i:integer]: TtiDBMetaDataField read GetItems write SetItems;
    property    Owner      : TtiDBMetaData   read GetOwner      write SetOwner;
    procedure   Add(AObject: TtiDBMetaDataField); reintroduce;
    function    AddInstance(const AFieldName : string;
                             const AFieldKind : TtiQueryFieldKind;
                             AFieldWidth : integer = 0): TtiDBMetaDataField; overload;
    function    AddInstance : TtiDBMetaDataField; overload;
    // Don't use AddField. Use AddInstance instead
    //    procedure   AddField(const AFieldName : string;
    //                          const AFieldKind : TtiQueryFieldKind;
    //                          AFieldWidth : integer = 0); deprecated;
    procedure   Read(const ADBConnectionName: string  = ''; APersistenceLayerName : string = ''); override;
    function    FindByFieldName(const AFieldName : TFieldName): TtiDBMetaDataField;
    function    IndexOfFieldName(const AFieldName: TFieldName): Integer;
    function    MaxFieldNameWidth : word;
    function    Clone : TtiDBMetaDataTable; reintroduce;
  published
    property    Name : TTableName read FName write FName;
  end;
  

  TtiDBMetaDataField = class(TtiObject)
  private
    FName: TFieldName;
    FWidth: integer;
    FKind: TtiQueryFieldKind;
    function    GetKindAsStr: string;
    procedure   SetKindAsStr(const AValue: string);
  protected
    function    GetRPadName: TFieldName;
    function    GetOwner: TtiDBMetaDataTable; reintroduce;
    procedure   SetOwner(const AValue: TtiDBMetaDataTable); reintroduce;
    function    GetCaption : string; override;
  public
    property    Owner      : TtiDBMetaDataTable             read GetOwner      write SetOwner;
    property    RPadName : TFieldName read GetRPadName;
    function    Clone : TtiDBMetaDataField; reintroduce;
  published
    property    Name : TFieldName read FName write FName;
    property    Kind : TtiQueryFieldKind read FKind write FKind;
    property    KindAsStr : string read GetKindAsStr write SetKindAsStr;
    property    Width : integer read FWidth write FWidth;
  end;


  TtiDatabase = class(TtiBaseObject)
  private
    FUserName    : string;
    FPassword    : string;
    FDatabaseName : string;
    FParams       : TStringList;
    FErrorInLastCall: boolean;
  protected
    // Implement these in the concrete
    procedure   SetConnected(AValue : boolean); virtual; abstract;
    function    GetConnected : boolean; virtual; abstract;
    property    Params : TStringList read FParams;
  public
    constructor Create; virtual;
    destructor  Destroy; override;

    procedure   Connect(const ADatabaseName, AUserName, APassword, AParams : string);

    property    DatabaseName : string  read FDatabaseName  write FDatabaseName;
    property    UserName    : string  read FUserName      write FUserName    ;
    property    Password    : string  read FPassword      write FPassword    ;
    property    Connected   : boolean read GetConnected    write SetConnected  ;
    property    ErrorInLastCall : boolean read FErrorInLastCall write FErrorInLastCall;

    // Implement these in the concrete
    class function  DatabaseExists(const ADatabaseName, AUserName, APassword : string; const AParams: string = ''): boolean; virtual; abstract;
    class procedure CreateDatabase(const ADatabaseName, AUserName, APassword : string; const AParams: string = ''); virtual; abstract;
    class procedure DropDatabase(const ADatabaseName, AUserName, APassword : string; const AParams: string = ''); virtual; abstract;
    class function  TestConnectTo( const ADatabaseName, AUserName, APassword, AParams: string): boolean; virtual;

    procedure   StartTransaction; virtual; abstract;
    function    InTransaction : boolean; virtual; abstract;
    procedure   Commit; virtual; abstract;
    procedure   RollBack; virtual; abstract;
    function    Test : boolean; virtual; abstract;
    function    TIQueryClass: TtiQueryClass; virtual; abstract;
    function    CreateTIQuery : TtiQuery;
    function    CreateAndAttachTIQuery: TtiQuery;
    procedure   ReadMetaDataTables(AData : TtiDBMetaData); virtual; abstract;
    procedure   ReadMetaDataFields(AData : TtiDBMetaDataTable); virtual; abstract;
    procedure   ExecSQL(const pSQL : string; AParams : TtiQueryParams = nil); virtual; // ToDo: Refactor down...

    procedure   DropTable(const ATableName : TTableName); overload;
    procedure   DropTable(const ATableMetaData : TtiDBMetaDataTable); overload; virtual; abstract;
    procedure   CreateTable(const ATableMetaData : TtiDBMetaDataTable); virtual; abstract;
    procedure   DeleteRow(  const ATableName : string;
                             const AWhere : TtiQueryParams); virtual;
    procedure   InsertRow(  const ATableName : string;
                             const AParams   : TtiQueryParams); virtual;
    procedure   UpdateRow(  const ATableName : string;
                             const AParams : TtiQueryParams;
                             const AWhere    : TtiQueryParams); virtual;
  end;
  

  TtiQuery = class(TtiBaseObject)
  private
    FContinueScan : boolean;
    FDatabase : TtiDatabase;
    FOptions: TStringList;
  protected
    FSupportsRowsAffected: boolean;
    //function  GetSession: TObject; virtual; abstract;
    //procedure SetSession(const AValue: TObject); virtual; abstract;
    function  GetSQL: TStrings; virtual; abstract;
    procedure SetSQL(const AValue: TStrings); virtual; abstract;
    function  GetQueryType: TtiQueryType;virtual;
    function  GetSQLText: string;virtual;
    procedure SetSQLText(const AValue: string);virtual;
    function  GetActive: boolean; virtual; abstract;
    procedure SetActive(const AValue: boolean); virtual; abstract;
    function  GetEOF: boolean; virtual; abstract;

    function  GetParamAsString(const AName: string): string; virtual; abstract;
    procedure SetParamAsString(const AName, AValue: string); virtual; abstract;
    function  GetParamAsBoolean(const AName: string): boolean; virtual; abstract;
    procedure SetParamAsBoolean(const AName: string;const AValue: boolean);virtual; abstract;
    function  GetParamAsFloat(const AName: string): extended;virtual; abstract;
    procedure SetParamAsFloat(const AName: string; const AValue: extended);virtual; abstract;
    function  GetParamAsInteger(const AName: string): Int64;virtual; abstract;
    procedure SetParamAsInteger(const AName: string;const AValue: Int64);virtual; abstract;
    function  GetParamAsTextBLOB(const AName: string): string; virtual; abstract;
    procedure SetParamAsTextBLOB(const AName, AValue: string); virtual; abstract;
    function  GetParamAsDateTime(const AName: string): TDateTime;virtual;abstract;
    procedure SetParamAsDateTime(const AName: string; const AValue: TDateTime); virtual; abstract;
    procedure SetParamAsMacro(const AName: string; const AValue: string); virtual;
    function  GetParamIsNull(const AName: String): Boolean; virtual; abstract;
    procedure SetParamIsNull(const AName: String; const AValue: Boolean); virtual; abstract;

    function  GetFieldAsString(const AName: string): string; virtual; abstract;
    function  GetFieldAsFloat(const AName: string): extended; virtual; abstract;
    function  GetFieldAsBoolean(const AName: string): boolean; virtual; abstract;
    function  GetFieldAsInteger(const AName: string): Int64; virtual; abstract;
    function  GetFieldAsDateTime(const AName: string):TDateTime; virtual; abstract;
    function  GetFieldIsNull(const AName: string): Boolean; virtual; abstract;

    function  GetFieldAsStringByIndex(AIndex: Integer): string    ; virtual; abstract;
    function  GetFieldAsFloatByIndex(AIndex: Integer)  : extended; virtual; abstract;
    function  GetFieldAsBooleanByIndex(AIndex: Integer): boolean ; virtual; abstract;
    function  GetFieldAsIntegerByIndex(AIndex: Integer): Int64   ; virtual; abstract;
    function  GetFieldAsDateTimeByIndex(AIndex: Integer):TDateTime; virtual; abstract;
    function  GetFieldIsNullByIndex(AIndex: Integer):Boolean      ; virtual; abstract;

    function  GetOptions: TStringList; virtual;
    procedure DoChangeOptions(Sender: TObject); virtual;
  public
    constructor Create; virtual;
    destructor  Destroy; override;
    property    Options: TStringList read GetOptions;
    procedure   AssignParams(const AParams : TtiQueryParams; const AWhere : TtiQueryParams = nil); virtual;
    property    SupportsRowsAffected: boolean read FSupportsRowsAffected;

    property  ParamAsString[ const AName : string ]: string
                read  GetParamAsString
                write SetParamAsString;
    property  ParamAsInteger[ const AName : string ]: Int64
                read  GetParamAsInteger
                write SetParamAsInteger;

    property  ParamAsBoolean[ const AName : string ]: boolean
                read  GetParamAsBoolean
                write SetParamAsBoolean;

    property  ParamAsFloat[ const AName : string ]: extended
                read  GetParamAsFloat
                write SetParamAsFloat;

    property  ParamAsDateTime[ const AName : string ]: TDateTime
                read GetParamAsDateTime
                write SetParamAsDateTime;

    property  ParamAsTextBLOB[ const AName : string ]: string
                read GetParamAsTextBLOB
                write SetParamAsTextBLOB;

//    property  ParamAsStream[ const AName : string ]: TStream
//                write  SetParamAsStream;

    property  ParamAsMacro[ const AName : string ]: string
                write SetParamAsMacro;

    property  ParamIsNull[const AName: String ]: boolean
                read GetParamIsNull
                write SetParamIsNull;


    property  FieldAsString[ const AName : string ]: string
                read GetFieldAsString;

    property  FieldAsFloat[ const AName : string ]: extended
                read  GetFieldAsFloat;

    property  FieldAsBoolean[ const AName : string ] : boolean
                read GetFieldAsBoolean;

    property  FieldAsInteger[ const AName : string ]: Int64
                read GetFieldAsInteger;

    property  FieldAsDateTime[ const AName : string ]: TDateTime
                read GetFieldAsDateTime;

    property  FieldIsNull[ const AName : string ]: Boolean
                read GetFieldIsNull;

    property FieldAsStringByIndex[ AIndex: Integer ]: string
                read GetFieldAsStringByIndex;
    property FieldAsFloatByIndex[ AIndex: Integer ]: extended
                read GetFieldAsFloatByIndex;
    property FieldAsBooleanByIndex[ AIndex: Integer ]: Boolean
                read GetFieldAsBooleanByIndex;
    property FieldAsIntegerByIndex[ AIndex: Integer ]: Int64
                read GetFieldAsIntegerByIndex;
    property FieldAsDateTimeByIndex[ AIndex: Integer ]: TDateTime
                read GetFieldAsDateTimeByIndex;
    property FieldIsNullByIndex[ AIndex: Integer ]: Boolean
                read GetFieldIsNullByIndex;

    property SQL         : TStrings read GetSQL    write SetSQL;
    // Don't use SQL.Text as there may be some code in SetSQL that must execute
    property SQLText     : string   read GetSQLText write SetSQLText;
    property Active      : boolean  read GetActive write SetActive;
    property EOF         : boolean  read GetEOF;
    property ContinueScan : boolean  read fContinueScan write fContinueScan;

    procedure Open; virtual; abstract;
    procedure Close; virtual; abstract;
    function  ExecSQL: integer; virtual; abstract;

    procedure SelectRow(const ATableName : string; const AWhere : TtiQueryParams; const ACriteria: TtiCriteria); overload; virtual; abstract;
    procedure SelectRow(const ATableName : string; const AWhere : TtiQueryParams); overload; virtual; abstract;
    procedure InsertRow(const ATableName : string; const AParams : TtiQueryParams); virtual; abstract;
    procedure DeleteRow(const ATableName : string; const AWhere : TtiQueryParams); virtual; abstract;
    procedure UpdateRow(const ATableName : string; const AParams : TtiQueryParams; const AWhere : TtiQueryParams); virtual; abstract;

    procedure Next; virtual; abstract;

    function  ParamCount : integer; virtual; abstract;
    function  ParamName(AIndex : integer): string; virtual; abstract;

    function  FieldCount : integer; virtual; abstract;
    function  FieldName(AIndex : integer): string; virtual; abstract;
    function  FieldIndex(const AName : string): integer; virtual; abstract;
    function  FieldKind(AIndex : integer): TtiQueryFieldKind; virtual; abstract;
    function  FieldSize(AIndex : integer): integer; virtual; abstract;
    function  HasNativeLogicalType : boolean; virtual; abstract;

    procedure   AssignParamFromStream(    const AName : string ; const AValue : TStream); virtual; abstract;
    procedure   AssignParamToStream(      const AName : string ; const AValue : TStream); virtual; abstract;
    procedure   AssignFieldAsStream(      const AName : string ; const AValue : TStream); virtual; abstract;
    procedure   AssignFieldAsStreamByIndex(     AIndex : integer; const AValue : TStream); virtual; abstract;

    // ToDo: Refactor with SetDatabase method replacing AttachDatabase & DetachDatabase
    property  Database : TtiDatabase read FDatabase write FDatabase;
    procedure AttachDatabase(ADatabase : TtiDatabase); virtual;
    procedure DetachDatabase;  virtual;
    procedure Reset; virtual; abstract;

    function  ParamsAsString : string; virtual;
    property  QueryType   : TtiQueryType read GetQueryType;

    procedure AssignToFieldString(const AField: TtiFieldString; const AName: string);
    procedure AssignFromFieldString(const AField: TtiFieldString; const AName: string);

    procedure AssignToFieldInteger(const AField: TtiFieldInteger; const AName: string);
    procedure AssignFromFieldInteger(const AField: TtiFieldInteger; const AName: string);

    procedure AssignToFieldFloat(const AField: TtiFieldFloat; const AName: string);
    procedure AssignFromFieldFloat(const AField: TtiFieldFloat; const AName: string);

    procedure AssignToFieldBoolean(const AField: TtiFieldBoolean; const AName: string);
    procedure AssignFromFieldBoolean(const AField: TtiFieldBoolean; const AName: string);

    procedure AssignToFieldDateTime(const AField: TtiFieldDateTime; const AName: string);
    procedure AssignFromFieldDateTime(const AField: TtiFieldDateTime; const AName: string);

  end;


  TtiDatabaseSQL = class(TtiDatabase)
  private
  protected
    function FieldMetaDataToSQLCreate(const AFieldMetaData : TtiDBMetaDataField): string; virtual; abstract;
  public
    procedure   DropTable(const ATableMetaData : TtiDBMetaDataTable); override;
    procedure   CreateTable(const ATableMetaData : TtiDBMetaDataTable); override;
  end;


  TtiQuerySQL = class(TtiQuery)
  private
  protected
    function    WhereClause(const AWhere: TtiQueryParams): string;
    function    SQLAndParamsAsString : string;
  public
    procedure   SelectRow(const ATableName : string; const AWhere : TtiQueryParams; const ACriteria: TtiCriteria ); override;
    procedure   SelectRow(const ATableName : string; const AWhere : TtiQueryParams); override;
    procedure   InsertRow(const ATableName : string; const AParams : TtiQueryParams); override;
    procedure   DeleteRow(const ATableName : string; const AWhere : TtiQueryParams); override;
    procedure   UpdateRow(const ATableName : string; const AParams : TtiQueryParams; const AWhere : TtiQueryParams); override;
  end;


  TtiQueryNonSQL = class(TtiQuery)
  private
    FParams : TtiQueryParams;
  protected
    property    Params : TtiQueryParams read FParams;
    function    GetParamAsBoolean(const AName: string): boolean; override;
    function    GetParamAsFloat(const AName: string): extended;override;
    function    GetParamAsInteger(const AName: string): Int64;override;
    function    GetParamAsTextBLOB(const AName: string): string; override;
    function    GetParamAsDateTime(const AName: string): TDateTime; override;
    function    GetParamIsNull(const AName: String): Boolean; override;
    procedure   SetParamAsBoolean(const AName: string;const AValue: boolean);override;
    procedure   SetParamAsFloat(const AName: string; const AValue: extended);override;
    procedure   SetParamAsInteger(const AName: string;const AValue: Int64);override;
    procedure   SetParamAsTextBLOB(const AName, AValue: string); override;
    procedure   SetParamAsDateTime(const AName :string; const AValue: TDateTime); override;
    procedure   SetParamAsMacro(const AName: string; const AValue: string); override;
    procedure   SetParamIsNull(const AName: String; const AValue: Boolean); override;

  public
    constructor Create; override;
    destructor  Destroy; override;

    function    ParamCount : integer; override;
    function    ParamName(AIndex : integer): string; override;
    function    ParamsAsString : string; override;

    procedure   AssignParamFromStream(    const AName : string ; const AValue : TStream); override;
    procedure   AssignParamToStream(      const AName : string ; const AValue : TStream); override;
    procedure   AssignFieldAsStream(      const AName : string ; const AValue : TStream); override;
    procedure   AssignFieldAsStreamByIndex(     AIndex : integer; const AValue : TStream); override;

  end;

  TtiQueryParams = class(TtiObjectList)
  private
    function    GetParamIsNull(const AName : string ): boolean;
    procedure   SetParamIsNull(const AName : string; AValue: boolean);
    function    GetAsString: string;
  protected
    function    GetItems(i: integer): TtiQueryParamAbs; reintroduce;
    procedure   SetItems(i: integer; const AValue: TtiQueryParamAbs); reintroduce;
    function    FindCreateParamByName(const AName : string; const AClass : TtiQueryParamClass): TtiQueryParamAbs; virtual;
  public
    property    Items[i:integer]: TtiQueryParamAbs read GetItems write SetItems;
    procedure   Add(AObject: TtiQueryParamAbs); reintroduce;
    function    FindParamByName(const AName : string): TtiQueryParamAbs; virtual;

    property    ParamIsNull[const AName : string ]: boolean read GetParamIsNull write SetParamIsNull;
    function    ParamName(AIndex : integer): string;
    property    AsString : string read GetAsString;

    procedure   SetValueAsString(const AName: string; const AValue: string);
    function    GetValueAsString(const AName: string): string;
    procedure   AssignFromFieldString(const AField: TtiFieldString; const AName: string);

    procedure   SetValueAsInteger(const AName: string; const AValue: Int64);
    function    GetValueAsInteger(const AName: string): Int64;
    procedure   AssignFromFieldInteger(const AField: TtiFieldInteger; const AName: string);

    procedure   SetValueAsFloat(const AName: string; const AValue: Extended);
    function    GetValueAsFloat(const AName: string): Extended;
    procedure   AssignFromFieldFloat(const AField: TtiFieldFloat; const AName: string);

    procedure   SetValueAsBoolean(const AName : string; const AValue : Boolean  );
    function    GetValueAsBoolean(const AName : string): Boolean;
    procedure   AssignFromFieldBoolean(const AField: TtiFieldBoolean; const AName: string);

    procedure   SetValueAsDateTime(const AName : string; const AValue : TDateTime);
    function    GetValueAsDateTime(const AName : string): TDateTime;
    procedure   AssignFromFieldDateTime(const AField: TtiFieldDateTime; const AName: string);
    procedure   AssignFromFieldDate(const AField: TtiFieldDate; const AName: string);

    procedure   SetValueAsStream(const AName : string; const AValue : TStream);
    function    GetValueAsStream(const AName : string): TStream;
    procedure   AssignValueToStream(const AName : string; const AStream : TStream);

    procedure   SetValueFromProp(const AFieldMetaData : TtiObject; const APropName : string; const pParamName : string);
    procedure   SetValueAsVariant(const AName : string; const AValue : variant);
    procedure   SetValueAsVarRec(const AName: string; const AValue: TVarRec);
  end;


  TtiQueryParamAbs = class(TtiObject)
  private
    FName: string;
    FIsNull: boolean;
    function    GetKindAsStr: string;
  protected
    function    GetOwner: TtiQueryParams; reintroduce;
    procedure   SetOwner(const AValue: TtiQueryParams); reintroduce;
    function    GetKind : TtiQueryFieldKind; virtual; abstract;
  public
    property    Owner      : TtiQueryParams             read GetOwner      write SetOwner;
    property    Kind       : TtiQueryFieldKind read GetKind;
    function    GetValueAsString : string; virtual; abstract;
    procedure   SetValueAsString(const AValue : string); virtual; abstract;
    procedure   AssignToTIQuery(const AQuery : TtiQuery); virtual; abstract;
    property    IsNull     : boolean read FIsNull write FIsNull;
  published
    property    Name       : string read FName write FName;
    property    KindAsStr  : string read GetKindAsStr;
    property    ValueAsString : string read GetValueAsString write SetValueAsString;
  end;


  TtiQueryParamString = class(TtiQueryParamAbs)
  private
    FValue : string;
  protected
    function    GetKind : TtiQueryFieldKind; override;
  public
    function    GetValueAsString : string; override;
    procedure   SetValueAsString(const AValue : string); override;
    procedure   AssignToTIQuery(const AQuery : TtiQuery); override;
  end;
  

  TtiQueryParamInteger = class(TtiQueryParamAbs)
  private
    FValue : Int64;
  protected
    function    GetKind : TtiQueryFieldKind; override;
  public
    procedure   SetValueAsString(const AValue : string); override;
    procedure   SetValueAsInteger(const AValue : Int64);
    function    GetValueAsInteger : Int64;
    function    GetValueAsString : string; override;
    procedure   AssignToTIQuery(const AQuery : TtiQuery); override;
  end;
  

  TtiQueryParamFloat = class(TtiQueryParamAbs)
  private
    FValue : extended;
  protected
    function    GetKind : TtiQueryFieldKind; override;
  public
    procedure   SetValueAsString(const AValue : string); override;
    procedure   SetValueAsFloat(const AValue : extended);
    function    GetValueAsFloat  : extended;
    function    GetValueAsString : string; override;
    procedure   AssignToTIQuery(const AQuery : TtiQuery); override;
  end;
  

  TtiQueryParamDateTime = class(TtiQueryParamAbs)
  private
    FValue : TDateTime;
  protected
    function    GetKind : TtiQueryFieldKind; override;
  public
    procedure   SetValueAsString(const AValue : string); override;
    procedure   SetValueAsDateTime(const AValue : TDateTime);
    function    GetValueAsDateTime   : TDateTime;
    function    GetValueAsString : string; override;
    procedure   AssignToTIQuery(const AQuery : TtiQuery); override;
  end;
  

  TtiQueryParamBoolean = class(TtiQueryParamAbs)
  private
    FValue : Boolean;
  protected
    function    GetKind : TtiQueryFieldKind; override;
  public
    procedure   SetValueAsString(const AValue : string); override;
    procedure   SetValueAsBoolean(const AValue : Boolean);
    function    GetValueAsBoolean : Boolean;
    function    GetValueAsString : string; override;
    procedure   AssignToTIQuery(const AQuery : TtiQuery); override;
  end;
  

  TtiQueryParamStream = class(TtiQueryParamAbs)
  private
    FStream : TMemoryStream;
  protected
    function    GetKind : TtiQueryFieldKind; override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    // ValuesAsString assumes the MIME encode
    procedure   SetValueAsString(const AValue : string); override;
    function    GetValueAsString : string; override;
    procedure   SetValueAsStream(const AValue : TStream);
    function    GetValueAsStream : TStream;
    procedure   AssignToTIQuery(const AQuery : TtiQuery); override;
  end;
  

const
  cgtiQueryMacroChr  = '&';


function  StrToQueryFieldKind(const AFieldKind: String): TtiQueryFieldKind;
function  QueryFieldKindToString(AFieldKind : TtiQueryFieldKind): string;
procedure QueryFieldKindsToStrings(AStrings : TStrings);


implementation
uses
  tiCommandLineParams
  ,tiUtils
  ,tiExcept
  ,tiLog
  ,tiDBConnectionPool
  ,tiConstants
  ,tiStreams
  ,tiVisitorCriteria
  ,Math
  ,TypInfo
 ;

function StrToQueryFieldKind(const AFieldKind: String): TtiQueryFieldKind;
var
  Index: TtiQueryFieldKind;
begin
  for Index := Low(TtiQueryFieldKind) to High(TtiQueryFieldKind) do
    if SameText(cgaQueryFieldKind[Index], AFieldKind) then
    begin
      Result := Index;
      Exit; //==>
    end;
  raise EtiOPFInternalException.CreateFmt(cErrorInvalidQueryFieldKindStr, [AFieldKind]);
  Result := Low(TtiQueryFieldKind);
end;

function QueryFieldKindToString(AFieldKind : TtiQueryFieldKind): string;
begin
  result := cgaQueryFieldKind[AFieldKind];
end;

procedure QueryFieldKindsToStrings(AStrings : TStrings);
var
  Index: TtiQueryFieldKind;
begin
  AStrings.Clear;
  for Index := Low(TtiQueryFieldKind) to High(TtiQueryFieldKind) do
    AStrings.Add(cgaQueryFieldKind[Index]);
end;

function tiTypeKindToQueryFieldKind(AValue : TtiTypeKind): TtiQueryFieldKind;
begin
  case AValue of
  tiTKInteger : result := qfkInteger;
  tiTKFloat   : result := qfkFloat;
  tiTKString  : result := qfkString;
  tiTKDateTime : result := qfkDateTime;
  tiTKBoolean : result := qfkLogical;
  else
    raise EtiOPFInternalException.Create(cErrorInvalidTtiQueryFieldKind);
    result := qfkInteger; // Just to shut the compiler up. Wont get here.
  end;
  //qfkBinary,
  //qfkMacro,
  //qfkLongString
end;

{ TtiDBMetaData }

procedure TtiDBMetaData.Add(AObject: TtiDBMetaDataTable);
begin
  inherited Add(AObject);
end;

procedure TtiDBMetaData.Clear;
begin
  inherited Clear;
  ObjectState := posEmpty;
end;

function TtiDBMetaData.FindByTableName(const ATableName: TTableName): TtiDBMetaDataTable;
var
  i : integer;
begin
  // result := TtiDBMetaDataTable(Inherited FindByProps(['Name'], [ATableName]));
  // Hard coding the check is faster, and more stable
  result := nil;
  for i := 0 to Count - 1 do
    if SameText(Items[i].Name, ATableName) then
    begin
      result := Items[i];
      Exit; //==>
    end;
end;

function TtiDBMetaData.GetCaption: string;
begin
  Assert(Owner <> nil, 'Owner is nill');
  result := TtiDBConnectionPool(Owner).DatabaseAlias;
end;

function TtiDBMetaData.GetItems(i: integer): TtiDBMetaDataTable;
begin
  result := TtiDBMetaDataTable(inherited GetItems(i));
end;

//function TtiDBMetaData.GetOwner: TtiObject;
//begin
//  result := TDBConnectionPool(inherited GetOwner);
//end;

procedure TtiDBMetaData.Read(const ADBConnectionName: string  = ''; APersistenceLayerName : string = '');
var
  LDatabase : TtiDatabase;
begin
  if ObjectState <> posEmpty then
    Exit; //==>
  LDatabase := TtiDBConnectionPool(Owner).Lock;
  try
    Clear;
    LDatabase.ReadMetaDataTables(Self);
  finally
    TtiDBConnectionPool(Owner).UnLock(LDatabase);
  end;
end;

procedure TtiDBMetaData.SetItems(i: integer;
  const AValue: TtiDBMetaDataTable);
begin
  inherited SetItems(i, AValue);
end;

//procedure TtiDBMetaData.SetOwner(const AValue: TtiObject);
//begin
//  inherited SetOwner(AValue as TtiObject);
//end;

{ TtiDBMetaDataTable }

procedure TtiDBMetaDataTable.Add(AObject: TtiDBMetaDataField);
begin
  inherited Add(AObject);
  FMaxFieldWidth := 0;
end;

function TtiDBMetaDataTable.AddInstance(const AFieldName: string;
  const AFieldKind: TtiQueryFieldKind; AFieldWidth: integer = 0): TtiDBMetaDataField;
begin
  { 9 Jan 2006, Graeme:  Please do not remove the 'Self.' part. Free Pascal
    needs it for some reason!!  This gave me a lot of extra grey hairs. }
  Result := Self.AddInstance;
  Result.Name := AFieldName;
  Result.Kind := AFieldKind;
  Result.Width := AFieldWidth;
  Result.ObjectState := posClean;
end;

function TtiDBMetaDataTable.AddInstance: TtiDBMetaDataField;
begin
  Result := TtiDBMetaDataField.Create;
  Add(Result);
end;

function TtiDBMetaDataTable.Clone: TtiDBMetaDataTable;
begin
  result := TtiDBMetaDataTable(inherited Clone);
end;

constructor TtiDBMetaDataTable.Create;
begin
  inherited;
  FMaxFieldWidth := 0;
end;

destructor TtiDBMetaDataTable.Destroy;
begin
  inherited;
end;

function TtiDBMetaDataTable.FindByFieldName(const AFieldName: TFieldName): TtiDBMetaDataField;
var
  i : integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if SameText(Items[i].Name, AFieldName) then
    begin
      result := Items[i];
      Exit; //==>
    end;
end;

function TtiDBMetaDataTable.IndexOfFieldName(const AFieldName: TFieldName): Integer;
var
  i : integer;
begin
  result := -1;
  for i := 0 to Count - 1 do
    if SameText(Items[i].Name, AFieldName) then
    begin
      result := i;
      Exit; //==>
    end;
end;

function TtiDBMetaDataTable.GetCaption: string;
begin
  result := Name;
end;

function TtiDBMetaDataTable.GetItems(i: integer): TtiDBMetaDataField;
begin
  result := TtiDBMetaDataField(inherited GetItems(i));
end;

function TtiDBMetaDataTable.GetOwner: TtiDBMetaData;
begin
  result := TtiDBMetaData(inherited GetOwner);
end;

function TtiDBMetaDataTable.MaxFieldNameWidth: word;
var
  i : integer;
begin
  if FMaxFieldWidth <> 0 then
  begin
    result := FMaxFieldWidth;
    Exit; //==>
  end;
  for i := 0 to Count - 1 do
    FMaxFieldWidth := Max(FMaxFieldWidth, Length(Items[i].Name));
  result := FMaxFieldWidth;
end;

procedure TtiDBMetaDataTable.Read(const ADBConnectionName: string  = ''; APersistenceLayerName : string = '');
var
  LDatabase : TtiDatabase;
begin
  if ObjectState <> posPK then
    Exit; //==>
  LDatabase := TtiDBConnectionPool(Owner.Owner).Lock;
  try
  LDatabase.ReadMetaDataFields(Self);
  finally
    TtiDBConnectionPool(Owner.Owner).UnLock(LDatabase);
  end;
end;

procedure TtiDBMetaDataTable.SetItems(i: integer;
  const AValue: TtiDBMetaDataField);
begin
  inherited SetItems(i, AValue);
end;

procedure TtiDBMetaDataTable.SetOwner(const AValue: TtiDBMetaData);
begin
  inherited SetOwner(AValue);
end;

{ TtiDBMetaDataField }

function TtiDBMetaDataField.Clone: TtiDBMetaDataField;
begin
  result := TtiDBMetaDataField(inherited Clone);
end;

function TtiDBMetaDataField.GetCaption: string;
begin
  result := name;
end;

function TtiDBMetaDataField.GetKindAsStr: string;
begin
  result := QueryFieldKindToString(Kind);
end;

function TtiDBMetaDataField.GetOwner: TtiDBMetaDataTable;
begin
  result := TtiDBMetaDataTable(inherited GetOwner);
end;

function TtiDBMetaDataField.GetRPadName: TFieldName;
begin
  result := tiPadR(Name, Owner.MaxFieldNameWidth);
end;

procedure TtiDBMetaDataField.SetKindAsStr(const AValue: string);
begin
  Kind := StrToQueryFieldKind(AValue);
end;

procedure TtiDBMetaDataField.SetOwner(const AValue: TtiDBMetaDataTable);
begin
  inherited SetOwner(AValue);
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// *  tiDatabase
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiDatabase.Connect(const ADatabaseName, AUserName, APassword, AParams: string);
var
  i : Integer;
begin
  Log('Attempting to connect to: %s Params: %s', [ADatabaseName, AParams], lsConnectionPool);
  DatabaseName    := ADatabaseName;
  UserName        := AUserName    ;
  Password        := APassword    ;
  for i := 1 to tiNumToken(AParams, ',') do
    FParams.Add(tiToken(AParams, ',', i));
  if gCommandLineParams.IsParam('VerboseDBConnection') then
    Log('Connected. Database: ' + ADatabaseName +
         ' UserName: ' + AUserName +
         ' Password: ' + APassword);
  if Connected then
    Connected := false;
  Connected := true;
  Log('Connect to %s successful.', [ADatabaseName], lsConnectionPool);
end;

constructor TtiDatabase.Create;
begin
  inherited;
  FErrorInLastCall := false;
  FParams         := TStringList.Create;
end;

function TtiDatabase.CreateAndAttachTIQuery: TtiQuery;
begin
  result:= CreateTIQuery;
  Result.AttachDatabase(Self);
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiQuery
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiQuery.Create;
begin
  inherited;
  FSupportsRowsAffected := False;
end;

destructor TtiQuery.destroy;
begin
  inherited;
  FOptions.Free;
end;

procedure TtiQuery.AssignToFieldBoolean(const AField: TtiFieldBoolean; const AName: string);
begin
  if not FieldIsNull[AName] then
    AField.AsBoolean:= FieldAsBoolean[AName]
  else
    AField.IsNull:= true;
end;

procedure TtiQuery.AssignToFieldDateTime(const AField: TtiFieldDateTime; const AName: string);
begin
  if not FieldIsNull[AName] then
    AField.AsDateTime:= FieldAsDateTime[AName]
  else
    AField.IsNull:= true;
end;

procedure TtiQuery.AssignToFieldFloat(const AField: TtiFieldFloat; const AName: string);
begin
  if not FieldIsNull[AName] then
    AField.AsFloat:= FieldAsFloat[AName]
  else
    AField.IsNull:= true;
end;

procedure TtiQuery.AssignToFieldInteger(const AField: TtiFieldInteger; const AName: string);
begin
  if not FieldIsNull[AName] then
    AField.AsInteger:= FieldAsInteger[AName]
  else
    AField.IsNull:= true;
end;

procedure TtiQuery.AssignToFieldString(const AField: TtiFieldString; const AName: string);
begin
  if not FieldIsNull[AName] then
    AField.AsString:= FieldAsString[AName]
  else
    AField.IsNull:= true;
end;

procedure TtiQuery.AttachDatabase(ADatabase: TtiDatabase);
begin
  FDatabase := ADatabase;
end;

function TtiQuery.ParamsAsString: string;
var
  i: integer;
begin
  try
    result := '';
    for i := 0 to ParamCount - 1 do
    begin
      result := tiAddTrailingValue(result, CrLf, true);
      result := result +
                ParamName(i) + ' := ';
      if ParamIsNull[ ParamName(i)] then      // Display the fact
        result := result + 'Null'
      else
        result := result + tiAddEllipsis(ParamAsString[ParamName(i)], 120);
    end;
  except
    on e:exception do
      result := 'Unknown';
  end;
end;

procedure TtiQuery.DetachDatabase;
begin
  if Active then
    Active := false;
  FDatabase := nil;
end;

procedure TtiQuery.DoChangeOptions(Sender: TObject);
begin
  // Do nothing. Implement in concrete if requred
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiQuerySQL
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiQuerySQL.DeleteRow(const ATableName: string; const AWhere: TtiQueryParams);
var
  lSQL : string;
begin
  lSQL := 'delete from ' + ATableName;
  lSQL := lSQL + WhereClause(AWhere);
  SQLText := lSQL;
  AssignParams(AWhere);
  ExecSQL;
end;

procedure TtiQuerySQL.InsertRow(const ATableName: string; const AParams: TtiQueryParams);
var
  lSQL : string;
  lFields : string;
  lParams : string;
  i : integer;
begin
  lSQL := 'insert into ' + ATableName;
  lFields := '';
  lParams := '';
  for i := 0 to AParams.Count - 1 do
  begin
    lFields := tiAddTrailingValue(lFields, ',' + CrLf);
    lParams := tiAddTrailingValue(lParams, ',' + CrLf);
    lFields := lFields + AParams.Items[i].Name;
    lParams := lParams + ':' + AParams.Items[i].Name;
  end;
  lSQL := lSQL + CrLf +
          '(' + lFields +
          ')' + CrLf +
          'values' + CrLf +
          '(' + CrLf +
          lParams + CrLf +
          ')';
  SQLText := lSQL;
  AssignParams(AParams);
  ExecSQL;
end;

procedure TtiQuerySQL.SelectRow(const ATableName: string; const AWhere: TtiQueryParams);
begin
  SelectRow(ATableName, AWhere, nil);
end;

procedure TtiQuerySQL.SelectRow(const ATableName: string;
  const AWhere: TtiQueryParams; const ACriteria: TtiCriteria);
var
  lSQL : string;
  i : integer;
  lWhere : string;
  lCriteriaWhere: string;
  lCriteriaParams: TtiQueryParams;
begin
  lCriteriaWhere:= '';
  lWhere := '';
  lCriteriaParams:= nil;

  try
    // This code is cloned from tiDatabaseSQL
    if (AWhere <> nil) then
      for i := 0 to AWhere.Count - 1 do
      begin
        lWhere := tiAddTrailingValue(lWhere, ' and ' + CrLf);
        lWhere := lWhere +
                  AWhere.Items[i].Name + ' = :' +
                  AWhere.Items[i].Name;
      end;

    if Assigned(ACriteria) and ACriteria.HasCriteria then
    begin
      lCriteriaParams:= TtiQueryParams.Create;
      lCriteriaWhere:= tiCriteriaAsSQL(ACriteria, lCriteriaParams);
      if lCriteriaWhere <> '' then
      begin
        lWhere := tiAddTrailingValue(lWhere, ' and ' + CrLf);
        lWhere := lWhere + lCriteriaWhere;
      end;
    end;

    if lWhere <> '' then
      lSQL := 'select * from ' + ATableName + CrLf +
              ' where ' + CrLf +
              lWhere
    else
      lSQL := 'select * from ' + ATableName;

    if Assigned(ACriteria) and ACriteria.HasOrderBy then
    begin
      lSQL:= lSQL + CrLf + tiCriteriaOrderByAsSQL(ACriteria);
    end;

    SQLText := lSQL;
    AssignParams(AWhere, lCriteriaParams);
    Open;
  finally
    lCriteriaParams.Free;
  end;
end;

function TtiQuery.GetOptions: TStringList;
begin
  if FOptions = nil then
  begin
    FOptions:= TStringList.Create;
    FOptions.OnChange:= DoChangeOptions;
  end;
  result:= FOptions;
end;
function TtiQuery.GetQueryType: TtiQueryType;
var
  lSQL : string;
begin
  // Must strip comments before this will work in all cases.
  lSQL := SQLText;
  lSQL := LowerCase(Trim(lSQL));
  lSQL := tiStrTran(lSQL, #10, '');
  lSQL := tiStrTran(lSQL, #13, ' ');
  lSQL := Copy(lSQL, 1, Pos(' ', lSQL)-1);
  if      lSQL = 'select' then
    result := qtSelect
  else if lSQL = 'insert' then
    result := qtInsert
  else if lSQL = 'update' then
    result := qtUpdate
  else if lSQL = 'delete' then
    result := qtDelete
  else if lSQL = 'create' then
    Result := qtDDL
  else if lSQL = 'alter' then
    Result := qtDDL
  else if lSQL = 'drop' then
    Result := qtDDL
  else
    raise Exception.CreateFmt(cTIOPFExcCanNotDetermineSQLType, [lSQL]);
end;

procedure TtiQuery.SetParamAsMacro(const AName, AValue: string);
begin
  // ToDo: ParamAsMacro will only work once on any given SQL statement because
  //       it replaces the macro character with 'AValue' If this is to work more
  //       than once, then the SQL must be saved, or the macro values must be
  //       cached.
  SQLText :=
    tiCIStrTran(SQLText,
                 cgtiQueryMacroChr + AName,
                 AValue);
end;

procedure TtiDatabase.DropTable(const ATableName: TTableName);
var
  lDBMetaDataTable : TtiDBMetaDataTable;
begin
  lDBMetaDataTable := TtiDBMetaDataTable.Create;
  try
    lDBMetaDataTable.Name := ATableName;
    DropTable(lDBMetaDataTable);
  finally
    lDBMetaDataTable.Free;
  end;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDatabaseSQL
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiDatabaseSQL.CreateTable(const ATableMetaData: TtiDBMetaDataTable);
var
  lSQL : string;
  i : integer;
begin
  lSQL := '';
  for i := 0 to ATableMetaData.Count - 1 do
  begin
    lSQL := tiAddTrailingValue(lSQL, ',' + CrLf);
    lSQL := lSQL +
            ATableMetaData.Items[i].Name + ' ' +
            FieldMetaDataToSQLCreate(ATableMetaData.Items[i]);
  end;
  lSQL := 'create table ' + ATableMetaData.Name + CrLf + '(' + CrLf +
          lSQL + CrLf +
          ')';
  ExecSQL(lSQL);
end;

procedure TtiDatabase.DeleteRow(const ATableName: string; const AWhere: TtiQueryParams);
var
  lQuery : TtiQuery;
  lHadToStartTransaction : boolean;
begin
  lQuery := CreateAndAttachTIQuery;
  try
    lHadToStartTransaction := not InTransaction;
    if lHadToStartTransaction then
      StartTransaction;
    try
      lQuery.DeleteRow(ATableName, AWhere);
      if lHadToStartTransaction then
        Commit;
    except
      on e:exception do
      begin
        if lHadToStartTransaction then
          RollBack;
        raise;
      end;
    end;
  finally
    lQuery.Free;
  end;
end;

procedure TtiDatabaseSQL.DropTable(const ATableMetaData: TtiDBMetaDataTable);
var
  lSQL : string;
begin
  lSQL := 'drop table ' + ATableMetaData.Name;
  ExecSQL(lSQL);
end;

{ TtiQueryParams }

procedure TtiQueryParams.Add(AObject: TtiQueryParamAbs);
begin
  inherited Add(AObject);
end;

function TtiQueryParams.FindCreateParamByName(const AName: string; const AClass : TtiQueryParamClass): TtiQueryParamAbs;
begin
  result := FindParamByName(AName);
  if result = nil then
  begin
    result := AClass.Create;
    result.Name := AName;
    Add(result);
  end;
end;

function TtiQueryParams.FindParamByName(const AName: string): TtiQueryParamAbs;
var
  i : integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if SameText(Items[i].Name, AName) then
    begin
      result := Items[i];
      Break; //==>
    end;
end;

function TtiQueryParams.GetAsString: string;
var
  i : integer;
begin
  result := '';
  for i := 0 to Count - 1 do
  begin
    if result <> '' then result := result + ', ';
    result := result + Items[i].Name + '=' + Items[i].GetValueAsString;
  end;
end;

function TtiQueryParams.GetItems(i: integer): TtiQueryParamAbs;
begin
  result := TtiQueryParamAbs(inherited GetItems(i));
end;

function TtiQueryParams.GetParamIsNull(const AName : string): boolean;
var
  lParam : TtiQueryParamAbs;
begin
  lParam := FindParamByName(AName);
  result := (lParam = nil) or (lParam.IsNull);
end;

procedure TtiQueryParams.SetValueAsString(const AName: string; const AValue: string);
var
  lParam : TtiQueryParamAbs;
begin
  lParam := FindCreateParamByName(AName, TtiQueryParamString);
  lParam.SetValueAsString(AValue);
end;

procedure TtiQueryParams.SetValueAsVariant(const AName: string;
  const AValue: variant);
begin
  case tiVarSimplePropType(AValue) of
    tiTKInteger:  SetValueAsInteger(AName, AValue);
    tiTKFloat:    SetValueAsFloat(AName, AValue);
    tiTKString:   SetValueAsString(AName, AValue);
    tiTKDateTime: SetValueAsDateTime(AName, AValue);
    tiTKBoolean:  SetValueAsBoolean(AName, AValue);
  else
    // handle other (unknown) types
    SetValueAsString(AName, AValue);
  end;
end;

procedure TtiQueryParams.SetValueAsVarRec(const AName: string;
  const AValue: TVarRec);
begin
  case AValue.VType of
    vtInteger:       SetValueAsInteger(AName, AValue.VInteger);
    vtBoolean:       SetValueAsBoolean(AName, AValue.VBoolean);
    vtExtended:      SetValueAsFloat(  AName, AValue.VExtended^);
    vtWideChar:      SetValueAsString( AName, String(AValue.VWideChar));
    vtUnicodeString: SetValueAsString( AName, String(AValue.VUnicodeString));
    // Implement (and unit test) as required
    //    vtString:     result := result + QuotedStr(string(VString^));
    //    vtChar:       result := result + QuotedStr(string(VChar));
    //    vtPWideChar: SetValueAsString(AName, String(AValue.VPWideChar));
    //    vtWideChar:   result := result + QuotedStr(string(VWideChar));
    //    vtPChar:      result := result + QuotedStr(string(VPChar));
    //    vtAnsiString: result := result + QuotedStr(string(VAnsiString));
    //    vtWideString: result := result + QuotedStr(string(VWideString));
    //    vtCurrency:   result := result + CurrToStr(VCurrency^);
    //    vtVariant:    result := result + QuotedStr(string(VVariant^));
    //    vtInt64:      result := result + IntToStr(VInt64^);
  else
    raise EtiOPFProgrammerException.CreateFmt('Invalid variant type. Index="%d"', [AValue.VType]);
    // Here are the possible values
    //  vtInteger       = 0;
    //  vtBoolean       = 1;
    //  vtChar          = 2;
    //  vtExtended      = 3;
    //  vtString        = 4;
    //  vtPointer       = 5;
    //  vtPChar         = 6;
    //  vtObject        = 7;
    //  vtClass         = 8;
    //  vtWideChar      = 9;
    //  vtPWideChar     = 10;
    //  vtAnsiString    = 11;
    //  vtCurrency      = 12;
    //  vtVariant       = 13;
    //  vtInterface     = 14;
    //  vtWideString    = 15;
    //  vtInt64         = 16;
    //  vtUnicodeString = 17;
  end;
end;

function TtiQueryParams.GetValueAsString(const AName: string): string;
var
  lParam : TtiQueryParamAbs;
begin
  lParam := FindParamByName(AName);
  Assert(lParam.TestValid(TtiQueryParamAbs), CTIErrorInvalidObject);
  result := lParam.GetValueAsString;
end;

function TtiQueryParams.ParamName(AIndex: integer): string;
var
  lParam : TtiQueryParamAbs;
begin
  lParam := Items[AIndex];
  result := lParam.Name;
end;

procedure TtiQueryParams.SetItems(i: integer; const AValue: TtiQueryParamAbs);
begin
  inherited SetItems(i, AValue);
end;

procedure TtiQueryParams.SetParamIsNull(const AName : string; AValue: boolean);
var
  lParam : TtiQueryParamAbs;
begin
  lParam := FindParamByName(AName);
  if lParam <> nil then
    lParam.IsNull := AValue;
end;

procedure TtiQueryParams.SetValueAsInteger(const AName: string; const AValue: Int64);
var
  lParam : TtiQueryParamInteger;
begin
  lParam := FindCreateParamByName(AName, TtiQueryParamInteger) as TtiQueryParamInteger;
  lParam.SetValueAsInteger(AValue);
end;

{
function TtiQueryParams.ParamAsInteger(const AName: string): Integer;
var
  lParam : TtiQueryParamInteger;
begin
  lParam := FindCreateParamByName(AName, TtiQueryParamInteger) as TtiQueryParamInteger;
  result := lParam.GetValue;
end;
}

procedure TtiQueryParams.SetValueAsFloat(const AName: string;const AValue: Extended);
var
  lParam : TtiQueryParamFloat;
begin
  lParam := FindCreateParamByName(AName, TtiQueryParamFloat) as TtiQueryParamFloat;
  lParam.SetValueAsFloat(AValue);
end;

{
function TtiQueryParams.ParamAsFloat(const AName: string): Extended;
var
  lParam : TtiQueryParamFloat;
begin
  lParam := FindCreateParamByName(AName, TtiQueryParamFloat) as TtiQueryParamFloat;
  result := lParam.GetValue;
end;
}

procedure TtiQueryParams.SetValueAsDateTime(const AName: string;const AValue: TDateTime);
var
  lParam : TtiQueryParamDateTime;
begin
  lParam := FindCreateParamByName(AName, TtiQueryParamDateTime) as TtiQueryParamDateTime;
  lParam.SetValueAsDateTime(AValue);
end;

{
function TtiQueryParams.ParamAsDateTime(const AName: string): TDateTime;
var
  lParam : TtiQueryParamDateTime;
begin
  lParam := FindCreateParamByName(AName, TtiQueryParamDateTime) as TtiQueryParamDateTime;
  result := lParam.GetValue;
end;
}

procedure TtiQueryParams.SetValueAsBoolean(const AName: string;const AValue: Boolean);
var
  lParam : TtiQueryParamBoolean;
begin
  lParam := FindCreateParamByName(AName, TtiQueryParamBoolean) as TtiQueryParamBoolean;
  lParam.SetValueAsBoolean(AValue);
end;

{
function TtiQueryParams.ParamAsBoolean(const AName: string): Boolean;
var
  lParam : TtiQueryParamBoolean;
begin
  lParam := FindCreateParamByName(AName, TtiQueryParamBoolean) as TtiQueryParamBoolean;
  result := lParam.GetValue;
end;
}

{ TtiQueryParam }

function TtiQueryParamAbs.GetKindAsStr: string;
begin
  result := QueryFieldKindToString(Kind);
end;

function TtiQueryParamAbs.GetOwner: TtiQueryParams;
begin
  result := TtiQueryParams(inherited GetOwner);
end;

procedure TtiQueryParamAbs.SetOwner(const AValue: TtiQueryParams);
begin
  inherited SetOwner(AValue);
end;

procedure TtiDatabase.ExecSQL(const pSQL: string; AParams : TtiQueryParams = nil);
var
  lQuery : TtiQuery;
  lHadToStartTransaction : boolean;
  lMessage : string;
begin
  lQuery := CreateAndAttachTIQuery;
  try
    lHadToStartTransaction := not InTransaction;
    if lHadToStartTransaction then
      StartTransaction;
    lQuery.SQLText := pSQL;
    lQuery.AssignParams(AParams);
    try
      lQuery.ExecSQL;
      if lHadToStartTransaction then
        Commit;
    except
      on e:exception do
      begin
        lMessage := e.message;
        if (lHadToStartTransaction {and InTransaction}) then
        begin
          try
            RollBack;
          except
            on e:exception do
              lMessage := lMessage + tiLineEnd +
                'Error rolling transaction after SQL failed:' + tiLineEnd + e.message;
          end;
        end;
        raise EtiOPFProgrammerException.Create(lMessage);
      end;
    end;
  finally
    lQuery.Free;
  end;
end;

procedure TtiDatabase.InsertRow(const ATableName : string;
                                    const AParams   : TtiQueryParams);
var
  LQuery : TtiQuery;
  LHadToStartTransaction : boolean;
begin
  LQuery:= CreateAndAttachTIQuery;
  try
    LHadToStartTransaction := not InTransaction;
    if LHadToStartTransaction then
      StartTransaction;
    try
      LQuery.InsertRow(ATableName, AParams);
      if LHadToStartTransaction then
        Commit;
    except
      on e:exception do
      begin
        if LHadToStartTransaction then
          RollBack;
        raise;
      end;
    end;
  finally
    LQuery.Free;
  end;
end;

procedure TtiDatabase.UpdateRow(  const ATableName : string;
                                   const AParams : TtiQueryParams;
                                   const AWhere : TtiQueryParams);
var
  lQuery : TtiQuery;
  lHadToStartTransaction : boolean;
begin
  lQuery := CreateAndAttachTIQuery;
  try
    lHadToStartTransaction := not InTransaction;
    if lHadToStartTransaction then
      StartTransaction;
    try
      lQuery.UpdateRow(ATableName, AParams, AWhere);
      if lHadToStartTransaction then
        Commit;
    except
      on e:exception do
      begin
        if lHadToStartTransaction then
          RollBack;
        raise;
      end;
    end;
  finally
    lQuery.Free;
  end;

  {
var
  lSQL : string;
  lFields : string;
  i : integer;
  lMergedParams : TtiQueryParams;
begin
  lSQL := 'update ' + ATableName + ' set ';
  lFields := '';
  for i := 0 to AParams.Count - 1 do
  begin
    lFields := tiAddTrailingValue(lFields, ',' + CrLf);
    lFields := lFields + AParams.Items[i].Name + ' =:' + AParams.Items[i].Name;
  end;

  lSQL := lSQL + CrLf +
          lFields +
          WhereClause(AParams);

  lMergedParams := TtiQueryParams.Create;
  lMergedParams.OwnsObjects := false;
  try
    for i := 0 to AWhere.Count - 1 do
      lMergedParams.Add(AWhere.Items[i]);
    for i := 0 to AParams.Count - 1 do
      lMergedParams.Add(AParams.Items[i]);

    ExecSQL(lSQL, lMergedParams);
  finally
    lMergedParams.Free;
  end;
}

end;

procedure TtiQuery.AssignFromFieldBoolean(const AField: TtiFieldBoolean; const AName: string);
begin
  if not AField.IsNull then
    ParamAsBoolean[AName]:= AField.AsBoolean
  else
    ParamIsNull[AName]:= True;
end;

procedure TtiQuery.AssignFromFieldDateTime(const AField: TtiFieldDateTime; const AName: string);
begin
  if not AField.IsNull then
    ParamAsDateTime[AName]:= AField.AsDateTime
  else
    ParamIsNull[AName]:= True;
end;

procedure TtiQuery.AssignFromFieldFloat(const AField: TtiFieldFloat; const AName: string);
begin
  if not AField.IsNull then
    ParamAsFloat[AName]:= AField.AsFloat
  else
    ParamIsNull[AName]:= True;
end;

procedure TtiQuery.AssignFromFieldInteger(const AField: TtiFieldInteger; const AName: string);
begin
  if not AField.IsNull then
    ParamAsInteger[AName]:= AField.AsInteger
  else
    ParamIsNull[AName]:= True;
end;

procedure TtiQuery.AssignFromFieldString(const AField: TtiFieldString; const AName: string);
begin
  if not AField.IsNull then
    ParamAsString[AName]:= AField.AsString
  else
    ParamIsNull[AName]:= True;
end;

procedure TtiQuery.AssignParams(const AParams: TtiQueryParams; const AWhere : TtiQueryParams = nil);
var
  i : integer;
begin
  if AParams <> nil then
    For i := 0 to AParams.Count - 1 do
      AParams.Items[i].AssignToTIQuery(Self);
  if AWhere <> nil then
    For i := 0 to AWhere.Count - 1 do
      AWhere.Items[i].AssignToTIQuery(Self);
end;

function TtiQuerySQL.SQLAndParamsAsString: string;
var
  i : integer;
  lParams: string;
begin
  result := 'SQL:';
  for i := 0 to SQL.Count - 1 do
    result := result + Cr + '    ' + SQL.Strings[i];
  lParams := '';
  for i := 0 to ParamCount - 1 do
  begin
   lParams := lParams + Cr + '    ' +
              ParamName(i) + ':= ';
    if ParamIsNull[ ParamName(i)] then      // Display the fact
      lParams := lParams + 'Null'
    else
      lParams := lParams + tiAddEllipsis(ParamAsString[ ParamName(i)], 120);
  end;
  if lParams <> '' then
    result := result + Cr(2) + 'Params:' + lParams;

end;

procedure TtiQuerySQL.UpdateRow(const ATableName: string; const AParams, AWhere: TtiQueryParams);
var
  lSQL : string;
  lFields : string;
  i : integer;
begin
  lSQL := 'update ' + ATableName + ' set ';
  lFields := '';
  for i := 0 to AParams.Count - 1 do
  begin
    lFields := tiAddTrailingValue(lFields, ',' + CrLf);
    lFields := lFields + AParams.Items[i].Name + ' =:' + AParams.Items[i].Name;
  end;

  lSQL := lSQL + CrLf +
          lFields +
          WhereClause(AWhere);

  SQLText := lSQL;
  AssignParams(AParams, AWhere);
  ExecSQL;
end;

function TtiQuerySQL.WhereClause(const AWhere: TtiQueryParams): string;
var
  i : integer;
begin
  result := '';
  if (AWhere = nil) or
     (AWhere.Count = 0) then
    Exit;
  for i := 0 to AWhere.Count - 1 do
  begin
    result := tiAddTrailingValue(Result, CrLf);
    result := result +
              AWhere.Items[i].Name + ' =:' +
              AWhere.Items[i].Name;
  end;
  if result <> '' then
    result := CrLf + 'where' + CrLf + result;
end;

procedure TtiQueryParams.SetValueFromProp(const AFieldMetaData: TtiObject; const APropName, pParamName: string);
var
  lString  : string;
  lInteger : Int64;
  lFloat   : Extended;
  lDate    : TDateTime;
  lBoolean : boolean;
  lStream  : TStream;
  lTypeKind : TtiTypeKind;
  lPropType : TTypeKind;
begin
  Assert(AFieldMetaData.TestValid(TtiObject), CTIErrorInvalidObject);
  Assert(APropName <> '', 'APropName not assigned');
  Assert(pParamName <> '', 'pParamName not assigned');
  Assert(IsPublishedProp(AFieldMetaData, APropName), APropName + ' is not a published property on ' + AFieldMetaData.ClassName);
  try
    // If it's an object type...
    // else
// ToDo: Better to return a qfkXXX here
    lTypeKind := tiGetSimplePropType(AFieldMetaData, APropName);
    case lTypeKind of
    tiTKString  : begin
                     lString := TypInfo.GetStrProp(AFieldMetaData, APropName);
                     SetValueAsString(pParamName, lString);
                   end;
    tiTKInteger : begin
                     lPropType := TypInfo.PropType(AFieldMetaData, APropName);
                     if (lPropType = tkInt64) then
                       lInteger := TypInfo.GetInt64Prop(AFieldMetaData, APropName)
                     else
                       lInteger := TypInfo.GetOrdProp(AFieldMetaData, APropName);
                     SetValueAsInteger(pParamName, lInteger);
                   end;
    tiTKFloat   : begin
                     lFloat := TypInfo.GetFloatProp(AFieldMetaData, APropName);
                     SetValueAsFloat(pParamName, lFloat);
                   end;
    tiTKDateTime : begin
                     lDate := TypInfo.GetFloatProp(AFieldMetaData, APropName);
                     SetValueAsDateTime(pParamName, lDate);
                   end;
    tiTKBoolean :  begin
                     lBoolean := Boolean(TypInfo.GetOrdProp(AFieldMetaData, APropName));
                     SetValueAsBoolean(pParamName, lBoolean);
                   end;
    tiTKBinary :  begin
                     lStream := (TypInfo.GetObjectProp(AFieldMetaData, APropName) as TStream);
                     SetValueAsStream(pParamName, lStream);
                   end;
    else
      raise EtiOPFProgrammerException.Create(cErrorInvalidTtiTypeKind);
    end;
  except
    on e:exception do
      raise EtiOPFProgrammerException.CreateFmt(cErrorSettingPropValue, [APropName, AFieldMetaData.ClassName, e.Message]);
  end;
end;

function TtiQueryParams.GetValueAsBoolean(const AName: string): Boolean;
var
  lParam : TtiQueryParamAbs;
begin
  lParam := FindParamByName(AName);
  Assert(lParam.TestValid(TtiQueryParamBoolean), CTIErrorInvalidObject);
  result := TtiQueryParamBoolean(lParam).GetValueAsBoolean;
end;

function TtiQueryParams.GetValueAsDateTime(const AName: string): TDateTime;
var
  lParam : TtiQueryParamAbs;
begin
  lParam := FindParamByName(AName);
  Assert(lParam.TestValid(TtiQueryParamDateTime), CTIErrorInvalidObject);
  result := TtiQueryParamDateTime(lParam).GetValueAsDateTime;
end;

function TtiQueryParams.GetValueAsFloat(const AName: string): Extended;
var
  lParam : TtiQueryParamAbs;
begin
  lParam := FindParamByName(AName);
  Assert(lParam.TestValid(TtiQueryParamFloat), CTIErrorInvalidObject);
  result := TtiQueryParamFloat(lParam).GetValueAsFloat;
end;

function TtiQueryParams.GetValueAsinteger(const AName: string): Int64;
var
  lParam : TtiQueryParamAbs;
begin
  lParam := FindParamByName(AName);
  Assert(lParam.TestValid(TtiQueryParamInteger), CTIErrorInvalidObject);
  result := TtiQueryParamInteger(lParam).GetValueAsInteger;
end;

function TtiQueryParams.GetValueAsStream(const AName: string): TStream;
var
  lParam : TtiQueryParamAbs;
begin
  lParam := FindParamByName(AName);
  Assert(lParam.TestValid(TtiQueryParamStream), CTIErrorInvalidObject);
  result := TtiQueryParamStream(lParam).GetValueAsStream;
end;

procedure TtiQueryParams.SetValueAsStream(const AName: string; const AValue: TStream);
var
  lParam : TtiQueryParamStream;
begin
  lParam := FindCreateParamByName(AName, TtiQueryParamStream) as TtiQueryParamStream;
  lParam.SetValueAsStream(AValue);
end;

procedure TtiQueryParams.AssignFromFieldString(const AField: TtiFieldString; const AName: string);
begin
  Assert(AField.TestValid, CTIErrorInvalidObject);
  SetValueAsString(AName, AField.AsString);
end;

procedure TtiQueryParams.AssignFromFieldBoolean(const AField: TtiFieldBoolean; const AName: string);
begin
  Assert(AField.TestValid, CTIErrorInvalidObject);
  SetValueAsBoolean(AName, AField.AsBoolean);
end;

procedure TtiQueryParams.AssignFromFieldDate(const AField: TtiFieldDate;
  const AName: string);
begin
  Assert(AField.TestValid, CTIErrorInvalidObject);
  SetValueAsDateTime(AName, AField.AsDateTime);
end;

procedure TtiQueryParams.AssignFromFieldDateTime(const AField: TtiFieldDateTime; const AName: string);
begin
  Assert(AField.TestValid, CTIErrorInvalidObject);
  SetValueAsDateTime(AName, AField.AsDateTime);
end;

procedure TtiQueryParams.AssignFromFieldFloat(const AField: TtiFieldFloat;const AName: string);
begin
  Assert(AField.TestValid, CTIErrorInvalidObject);
  SetValueAsFloat(AName, AField.AsFloat);
end;

procedure TtiQueryParams.AssignFromFieldInteger(const AField: TtiFieldInteger; const AName: string);
begin
  Assert(AField.TestValid, CTIErrorInvalidObject);
  SetValueAsInteger(AName, AField.AsInteger);
end;

procedure TtiQueryParams.AssignValueToStream(const AName: string; const AStream: TStream);
var
  lStream : TStream;
  lPos : integer;
begin
  lStream := GetValueAsStream(AName);
  lPos := lStream.Position;
  AStream.Size := 0;
  AStream.CopyFrom(lStream, lStream.Size);
  AStream.Seek(0, soFromBeginning);
  lStream.Seek(lPos, soFromBeginning);
end;

function TtiQuery.GetSQLText: string;
begin
  result := SQL.Text;
end;

procedure TtiQuery.SetSQLText(const AValue: string);
var
  lsl : TStringList;
begin
  // This will force any extra code in SetSQL to be executed.
  // Have had problems using SQL.Text as this extra code gets missed.
  lsl := TStringList.Create;
  try
    lsl.Text := AValue;
    SQL := lsl;
  finally
    lsl.Free;
  end;
end;

{ TtiQueryParamString }

procedure TtiQueryParamString.AssignToTIQuery(const AQuery: TtiQuery);
begin
  Assert(AQuery.TestValid(TtiQuery), CTIErrorInvalidObject);
  AQuery.ParamAsString[Name]:= GetValueAsString;
end;

function TtiQueryParamString.GetKind: TtiQueryFieldKind;
begin
  result := qfkString;
end;

function TtiQueryParamString.GetValueAsString: string;
begin
  result := FValue;
end;

procedure TtiQueryParamString.SetValueAsString(const AValue: string);
begin
  FValue := AValue;
  IsNull := false;
end;

{ TtiQueryParamInteger }

procedure TtiQueryParamInteger.AssignToTIQuery(const AQuery: TtiQuery);
begin
  Assert(AQuery.TestValid(TtiQuery), CTIErrorInvalidObject);
  AQuery.ParamAsInteger[Name]:= GetValueAsInteger;
end;

function TtiQueryParamInteger.GetKind: TtiQueryFieldKind;
begin
  result := qfkInteger;
end;

function TtiQueryParamInteger.GetValueAsInteger: Int64;
begin
  result := FValue;
end;

function TtiQueryParamInteger.GetValueAsString: string;
begin
  result := IntToStr(FValue);
end;

procedure TtiQueryParamInteger.SetValueAsInteger(const AValue: Int64);
begin
  FValue := AValue;
  IsNull := false;
end;

procedure TtiQueryParamInteger.SetValueAsString(const AValue: string);
begin
  if AValue <> '' then
    FValue := StrToInt64(AValue)
  else
    FValue := 0;
  IsNull := false;
end;

procedure TtiQueryParamBoolean.SetValueAsBoolean(const AValue: Boolean);
begin
  FValue := AValue;
  IsNull := false;
end;

procedure TtiQueryParamBoolean.SetValueAsString(const AValue: string);
begin
  FValue :=
     SameText(AValue, 'TRUE') or
     SameText(AValue, 'T') or
     SameText(AValue, '1');
  IsNull := false;
end;

{ TtiQueryParamFloat }

procedure TtiQueryParamFloat.AssignToTIQuery(const AQuery: TtiQuery);
begin
  Assert(AQuery.TestValid(TtiQuery), CTIErrorInvalidObject);
  AQuery.ParamAsFloat[Name]:= GetValueAsFloat;
end;

function TtiQueryParamFloat.GetKind: TtiQueryFieldKind;
begin
  result := qfkFloat;
end;

function TtiQueryParamFloat.GetValueAsFloat: extended;
begin
  result := FValue;
end;

function TtiQueryParamFloat.GetValueAsString: string;
begin
  result := FloatToStr(GetValueAsFloat);
end;

procedure TtiQueryParamFloat.SetValueAsFloat(const AValue: extended);
begin
  FValue := AValue;
  IsNull := false;
end;

procedure TtiQueryParamFloat.SetValueAsString(const AValue: string);
begin
  if AValue <> '' then
    FValue := StrToFloat(AValue)
  else
    FValue := 0;
  IsNull := false;
end;

{ TtiQueryParamDateTime }

procedure TtiQueryParamDateTime.AssignToTIQuery(const AQuery: TtiQuery);
begin
  Assert(AQuery.TestValid(TtiQuery), CTIErrorInvalidObject);
  AQuery.ParamAsDateTime[Name]:= GetValueAsDateTime;
end;

function TtiQueryParamDateTime.GetKind: TtiQueryFieldKind;
begin
  result := qfkDateTime;
end;

function TtiQueryParamDateTime.GetValueAsDateTime: TDateTime;
begin
  result := FValue;
end;

function TtiQueryParamDateTime.GetValueAsString: string;
begin
  result := tiDateTimeAsXMLString(GetValueAsDateTime);
end;

procedure TtiQueryParamDateTime.SetValueAsDateTime(const AValue: TDateTime);
begin
  FValue := AValue;
  IsNull := false;
end;

procedure TtiQueryParamDateTime.SetValueAsString(const AValue: string);
begin
  if AValue <> '' then
    FValue := tiXMLStringToDateTime(AValue)
  else
    FValue := 0;
  IsNull := false;
end;

{ TtiQueryParamBoolean }

procedure TtiQueryParamBoolean.AssignToTIQuery(const AQuery: TtiQuery);
begin
  Assert(AQuery.TestValid(TtiQuery), CTIErrorInvalidObject);
  AQuery.ParamAsBoolean[Name]:= GetValueAsBoolean;
end;

function TtiQueryParamBoolean.GetKind: TtiQueryFieldKind;
begin
  result := qfkLogical;
end;

function TtiQueryParamBoolean.GetValueAsBoolean: Boolean;
begin
  result := FValue;
end;

function TtiQueryParamBoolean.GetValueAsString: string;
begin
{$IFDEF BOOLEAN_NUM_1}
  if FValue then
    result := '1'
  else
    result := '0';
{$ELSE}
{$IFDEF BOOLEAN_CHAR_1}
  if FValue then
    result := 'T'
  else
    result := 'F';
{$ELSE}
  if FValue then
    result := 'TRUE'
  else
    result := 'FALSE';
{$ENDIF}
{$ENDIF}
end;

{ TtiQueryParamStream }

procedure TtiQueryParamStream.AssignToTIQuery(const AQuery: TtiQuery);
begin
  Assert(AQuery.TestValid(TtiQuery), CTIErrorInvalidObject);
  AQuery.AssignParamFromStream(Name, FStream);
end;

constructor TtiQueryParamStream.Create;
begin
  inherited;
  FStream := TMemoryStream.Create;
end;

destructor TtiQueryParamStream.Destroy;
begin
  FStream.Free;
  inherited;
end;

function TtiQueryParamStream.GetKind: TtiQueryFieldKind;
begin
  result := qfkBinary;
end;

function TtiQueryParamStream.GetValueAsStream: TStream;
begin
  result := FStream;
end;

function TtiQueryParamStream.GetValueAsString: string;
begin
  result:= tiStreamToMIMEEncodeString(FStream);
end;

procedure TtiQueryParamStream.SetValueAsStream(const AValue: TStream);
var
  lPos : integer;
begin
  lPos := AValue.Position;
  FStream.Size := 0;
  AValue.Seek(0, soFromBeginning);
  FStream.CopyFrom(AValue, AValue.Size);
  FStream.Seek(0, soFromBeginning);
  AValue.Seek(lPos, soFromBeginning);
end;

procedure TtiQueryParamStream.SetValueAsString(const AValue: string);
begin
  tiMIMEEncodeStringToStream(AValue, FStream);
end;

{ TtiQueryNonSQL }

constructor TtiQueryNonSQL.Create;
begin
  inherited;
  FParams := TtiQueryParams.Create;
end;

destructor TtiQueryNonSQL.Destroy;
begin
  FParams.Free;
  inherited;
end;

procedure TtiQueryNonSQL.AssignFieldAsStream(const AName: string; const AValue: TStream);
var
  ls : string;
  lStream : TStringStream;
begin
  ls := GetFieldAsString(AName);
  lStream := TStringStream.Create(ls);
  try
    AValue.Size := 0;
    MimeDecodeStream(lStream, AValue);
    AValue.Position := 0;
  finally
    lStream.Free;
  end;
end;

procedure TtiQueryNonSQL.AssignParamFromStream(const AName: string; const AValue: TStream);
begin
  FParams.SetValueAsStream(AName, AValue);
end;

procedure TtiQueryNonSQL.AssignParamToStream(const AName: string; const AValue: TStream);
begin
  FParams.AssignValueToStream(AName, AValue);
end;

function TtiQueryNonSQL.GetParamAsBoolean(const AName: string): boolean;
begin
  result := Params.GetValueAsBoolean(AName);
end;

function TtiQueryNonSQL.GetParamAsDateTime(const AName: string): TDateTime;
begin
  result := Params.GetValueAsDateTime(AName);
end;

function TtiQueryNonSQL.GetParamAsFloat(const AName: string): extended;
begin
  result := Params.GetValueAsFloat(AName);
end;

function TtiQueryNonSQL.GetParamAsInteger(const AName: string): Int64;
begin
  result := Params.GetValueAsInteger(AName);
end;

function TtiQueryNonSQL.GetParamAsTextBLOB(const AName: string): string;
begin
  result := Params.GetValueAsString(AName);
end;

function TtiQueryNonSQL.ParamCount: integer;
begin
  result := Params.Count;
end;

function TtiQueryNonSQL.ParamName(AIndex: integer): string;
begin
  result := Params.ParamName(AIndex);
end;

procedure TtiQueryNonSQL.SetParamAsBoolean(const AName: string; const AValue: boolean);
begin
  Params.SetValueAsBoolean(AName, AValue);
end;

procedure TtiQueryNonSQL.SetParamAsDateTime(const AName : string; const AValue: TDateTime);
begin
  Params.SetValueAsDateTime(AName, AValue);
end;

procedure TtiQueryNonSQL.SetParamAsFloat(const AName: string; const AValue: extended);
begin
  Params.SetValueAsFloat(AName, AValue);
end;

procedure TtiQueryNonSQL.SetParamAsInteger(const AName: string; const AValue: Int64);
begin
  Params.SetValueAsInteger(AName, AValue);
end;

procedure TtiQueryNonSQL.SetParamAsTextBLOB(const AName, AValue: string);
begin
  Params.SetValueAsString(AName, AValue);
end;

procedure TtiQueryNonSQL.SetParamAsMacro(const AName, AValue: string);
begin
  Assert(false, 'Not implemented in ' + ClassName);
end;

function TtiQueryNonSQL.GetParamIsNull(const AName: String): Boolean;
begin
  Result := Params.ParamIsNull[AName];
end;

procedure TtiQueryNonSQL.SetParamIsNull(const AName: String; const AValue: Boolean);
begin
  Params.ParamIsNull[ AName ]:= AValue;
end;

function TtiQueryNonSQL.ParamsAsString: string;
var
  i : integer;
begin
  result := '';
  for i := 0 to ParamCount - 1 do
  begin
    result := tiAddTrailingValue(result, CrLf);
    result := result +
              ParamName(i) + ':= ' +
              ParamAsString[ ParamName(i)];
  end;
end;

function TtiDatabase.CreateTIQuery: TtiQuery;
begin
  Assert(TIQueryClass <> nil, 'TIQueryClass not assigned');
  result := TIQueryClass.Create;
end;

class function TtiDatabase.TestConnectTo(const ADatabaseName, AUserName,
                                         APassword, AParams: string): boolean;
var
  lDatabase : TtiDatabase;
begin
  result := false;
  lDatabase := Create;
  try
    try
      // ToDo: Pass this params value
      lDatabase.Connect(ADatabaseName, AUserName, APassword, AParams);
      result := true;
    except
      on e:EtiOPFDBExceptionWrongServerVersion do
        raise;
      on e:EtiOPFProgrammerException do
        raise;
      on e:exception do
        result := false;
    end;
    lDatabase.Connected := false;
  finally
    lDatabase.Free;
  end;
end;

destructor TtiDatabase.destroy;
begin
  FParams.Free;
  inherited;
end;

procedure TtiQueryNonSQL.AssignFieldAsStreamByIndex(AIndex: Integer; const AValue: TStream);
var
  ls : string;
  lStream : TStringStream;
begin
  ls := GetFieldAsStringByIndex(AIndex);
  lStream := TStringStream.Create(ls);
  try
    AValue.Size := 0;
    MimeDecodeStream(lStream, AValue);
    AValue.Position := 0;
  finally
    lStream.Free;
  end;
end;

end.
























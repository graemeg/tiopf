{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Peter Hinrichsen, TechInsite Pty. Ltd.
  as the tiOPF (TechInsite Object Persistence Framework)
                           
    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm

  Please submit changes to tiOPF@techinsite.com.au

  Purpose:
    Use the Adapter Pattern [GoF 139] to wrapper a data access object and
    give it an interface which can be common accross all data access APIs

    For example, there are anoying differences between the DOA's TQuery,
    TQuery and TIBQuery. The TtiQuery adapter hides these differences.
    Also, the TtiQuery can be used to simplify the move from traditional
    client server to patitioning the data access layer in its own DLL,
    COM server, DCOM server or Package.

    The TtiQuery is an abstract base class which is the starting point
    for the concretes: TtiQuery???

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiQuery;

interface
uses
  Classes
  ,Contnrs
  ,tiClassToDBMap_BOM
  ,tiPtnVisPerObj
  ,tiObjAbs
  ,SysUtils
  ,tiUtils
  ,tiExcept
  ;

const
  cTIOPFExcMsgCanNotConnectToDatabase = 'Can not connect to database' ;
  cTIOPFExcMsgCanNotFindDatabase      = 'Can not find database' ;
  cTIOPFExcMsgInvalidUserNamePassword = 'Invalid username and password combination' ;
  cTIOPFExcMsgCanNotCreateDatabase    = 'Can not create database' ;
  cTIOPFExcMsgDatabaseAlreadyExists   = 'Database already exists' ;

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
                       ) ;
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
  ) ;
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
  ) ;
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
  ) ;

function tiTypeKindToQueryFieldKind( pValue : TtiTypeKind ) : TtiQueryFieldKind ;

type

  EtiOPFDBException = class( EtiOPFException )
    constructor Create( const pPerLayerName, pDatabaseName, pUserName, pPassword : string ; const pMessage: string = '' ) ; virtual ;
  end ;

  // Can not connect to database, reason unknown
  EtiOPFDBExceptionCanNotConnect = class( EtiOPFDBException )
    constructor Create( const pPerLayerName, pDatabaseName, pUserName, pPassword : string ; const pMessage: string = '' ) ; override ;
  end ;

  // Can not find the database
  EtiOPDDBCanNotFindException = class( EtiOPFDBExceptionCanNotConnect )
    constructor Create( const pPerLayerName, pDatabaseName, pUserName, pPassword : string ; const pMessage: string = '' ) ; override ;
  end ;

  // Invalid user name or password
  EtiOPFDBExceptionUserNamePassword = class( EtiOPFDBExceptionCanNotConnect )
    constructor Create( const pPerLayerName, pDatabaseName, pUserName, pPassword : string ; const pMessage: string = '' ) ; override ;
  end ;

  // Could not create a database
  EtiOPFDBExceptionCanNotCreateDatabase = class( EtiOPFDBException )
    constructor Create( const pPerLayerName, pDatabaseName, pUserName, pPassword : string ; const pMessage: string = '' ) ; override ;
  end ;

  // Could not create the database because it already exists
  EtiOPFDBExceptionAlreadyExists = class( EtiOPFDBException )
    constructor Create( const pPerLayerName, pDatabaseName, pUserName, pPassword : string ; const pMessage: string = '' ) ; override ;
  end ;

  EtiOPFDBExceptionWrongServerVersion = class( EtiOPFDBException );

  // This should be extended to:
  // a) Manage the removal of comments
  // b) Manage CREATE, UPDATE and DELETE SQL as well as DML SQL
  TtiQueryType = (
                   qtSelect,
                   qtOther
                  ) ;

  TtiQuery            = class ;
  TtiDBMetaData       = class ;
  TtiDBMetaDataTable  = class ;
  TtiDBMetaDataField  = class ;
  TtiQueryParams      = class ;
  TtiQueryParamAbs    = class ;
  TTableName          = String[ 255 ] ;
  TFieldName          = String[ 255 ] ;

  //----------------------------------------------------------------------------
 TtiDBMetaData = class( TPerObjList )
  private
  protected
    function    GetItems(i: integer): TtiDBMetaDataTable ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TtiDBMetaDataTable); reintroduce ;
    function    GetCaption : string ; override ;
    function    GetOwner: TPerObjAbs; reintroduce ;
    procedure   SetOwner(const Value: TPerObjAbs); reintroduce ;
  public
    property    Items[i:integer] : TtiDBMetaDataTable read GetItems write SetItems ;
    procedure   Add( pObject : TtiDBMetaDataTable ; pbDefaultDispOrder : boolean = true ) ; reintroduce ;
    property    Owner        : TPerObjAbs   read GetOwner      write SetOwner ;
    procedure   Read( const pDBConnectionName: string = '' ; pPerLayerName : string = '' ) ; override ;
    procedure   Clear ; override ;
    function    FindByTableName( const pTableName : TTableName ) : TtiDBMetaDataTable ;
  published
  end ;

  //----------------------------------------------------------------------------
  TtiDBMetaDataTable = class( TPerObjList )
  private
    FName : TTableName ;
    FMaxFieldWidth : word ;
  protected
    function    GetItems(i: integer):TtiDBMetaDataField  ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TtiDBMetaDataField); reintroduce ;
    function    GetOwner: TtiDBMetaData; reintroduce ;
    procedure   SetOwner(const Value: TtiDBMetaData); reintroduce ;
    function    GetCaption : string ; override ;
  public
    constructor create ; override ;
    destructor  destroy ; override ;
    property    Items[i:integer] : TtiDBMetaDataField read GetItems write SetItems ;
    property    Owner       : TtiDBMetaData   read GetOwner      write SetOwner ;
    procedure   Add( pObject : TtiDBMetaDataField ; pbDefaultDispOrder : boolean = true ) ; reintroduce ;
    function    AddInstance( const pFieldName : string ;
                             const pFieldKind : TtiQueryFieldKind ;
                             pFieldWidth : integer = 0 ) : TtiDBMetaDataField ; overload ;
    function    AddInstance : TtiDBMetaDataField ; overload ;
    // Don't use AddField. Use AddInstance instead
    procedure   AddField( const pFieldName : string ;
                          const pFieldKind : TtiQueryFieldKind ;
                          pFieldWidth : integer = 0 ) ;
    procedure   Read( const pDBConnectionName: string  = '' ; pPerLayerName : string = '' ) ; override ;
    function    FindByFieldName( const pFieldName : TFieldName ) : TtiDBMetaDataField ;
    function    MaxFieldNameWidth : word ;
    function    Clone : TtiDBMetaDataTable ; reintroduce ;
  published
    property    Name : TTableName read FName write FName ;
  end ;

  //----------------------------------------------------------------------------
  TtiDBMetaDataField = class( TPerObjAbs )
  private
    FName: TFieldName;
    FWidth: integer;
    FKind: TtiQueryFieldKind;
    function    GetKindAsStr: string;
    procedure   SetKindAsStr(const Value: string);
  protected
    function    GetRPadName: TFieldName;
    function    GetOwner: TtiDBMetaDataTable; reintroduce ;
    procedure   SetOwner(const Value: TtiDBMetaDataTable); reintroduce ;
    function    GetCaption : string ; override ;
  public
    property    Owner       : TtiDBMetaDataTable             read GetOwner      write SetOwner ;
    property    RPadName : TFieldName read GetRPadName ;
    function    Clone : TtiDBMetaDataField ; reintroduce ;
  published
    property    Name  : TFieldName read FName write FName ;
    property    Kind  : TtiQueryFieldKind read FKind write FKind ;
    property    KindAsStr : string read GetKindAsStr write SetKindAsStr ;
    property    Width : integer read FWidth write FWidth ;
  end ;

  // ---------------------------------------------------------------------------
  TtiDatabase = class( TtiObjAbs )
  private
    FsUserName     : string;
    FsPassword     : string;
    FsDatabaseName : string;
    FParams        : TStringList;
    FiTraceLevel   : integer;
    FErrorInLastCall: boolean;
  protected
    // Implement these in the concrete
    procedure   SetConnected( pbValue : boolean ) ; virtual ; abstract ;
    function    GetConnected : boolean ; virtual ; abstract ;
    property    Params : TStringList read FParams;
  public
    constructor create ; virtual ;
    destructor  destroy ; override ;

    procedure   Connect( const psDatabaseName, psUserName, psPassword, pParams : string) ;

    property    DatabaseName : string  read FsDatabaseName  write FsDatabaseName ;
    property    UserName     : string  read FsUserName      write FsUserName     ;
    property    Password     : string  read FsPassword      write FsPassword     ;
    property    Connected    : boolean read GetConnected    write SetConnected   ;
    property    TraceLevel   : integer read FiTraceLevel    write FiTraceLevel   ;
    property    ErrorInLastCall : boolean read FErrorInLastCall write FErrorInLastCall ;

    // Implement these in the concrete
    class function  DatabaseExists( const psDatabaseName, psUserName, psPassword : string ) : boolean ; virtual ; abstract ;
    class procedure CreateDatabase( const psDatabaseName, psUserName, psPassword : string ) ; virtual ; abstract ;
    class function  TestConnectTo(  const psDatabaseName, psUserName, psPassword, pParams : string ) : boolean ; virtual ;

    procedure   StartTransaction ; virtual ; abstract;
    function    InTransaction : boolean ; virtual ; abstract ;
    procedure   Commit ; virtual ; abstract ;
    procedure   RollBack ; virtual ; abstract ;
    function    Test : boolean ; virtual ; abstract ;
    function    CreateTIQuery : TtiQuery ;
    procedure   ReadMetaDataTables( pData : TPersistent ) ; virtual ; abstract ;
    procedure   ReadMetaDataFields( pData : TPersistent ) ; virtual ; abstract ;
    procedure   ExecSQL( const pSQL : string ; pParams : TtiQueryParams = nil ) ; virtual ; // ToDo: Refactor down...

    procedure   DropTable( const pTableName : TTableName ) ; overload ;
    procedure   DropTable( const pTableMetaData : TtiDBMetaDataTable ) ; overload ; virtual ; abstract ;
    procedure   CreateTable( const pTableMetaData : TtiDBMetaDataTable ) ; virtual ; abstract ;
    procedure   DeleteRow(   const pTableName : string ;
                             const pWhere : TtiQueryParams ) ; virtual ;
    procedure   InsertRow(   const pTableName : string ;
                             const pParams    : TtiQueryParams ) ; virtual ;
    procedure   UpdateRow(   const pTableName : string ;
                             const pParams : TtiQueryParams ;
                             const pWhere     : TtiQueryParams ) ; virtual ;
  end ;

  // ---------------------------------------------------------------------------
  TtiQuery = class( TtiObjAbs )
  private
    FContinueScan  : boolean;
    FDatabase : TtiDatabase ;
  protected

    //function  GetSession: TObject; virtual ; abstract ;
    //procedure SetSession(const Value: TObject); virtual ; abstract ;
    function  GetSQL: TStrings; virtual ; abstract ;
    procedure SetSQL(const Value: TStrings); virtual ; abstract ;
    function  GetQueryType: TtiQueryType;virtual;
    function  GetSQLText: string;virtual;
    procedure SetSQLText(const Value: string);virtual;
    function  GetActive: boolean; virtual ; abstract ;
    procedure SetActive(const Value: boolean); virtual ; abstract ;
    function  GetEOF: boolean; virtual ; abstract ;

    function  GetParamAsString( const psName: string): string; virtual ; abstract ;
    procedure SetParamAsString( const psName, Value: string); virtual ; abstract ;
    function  GetParamAsBoolean(const psName: string): boolean; virtual ; abstract ;
    procedure SetParamAsBoolean(const psName: string;const Value: boolean);virtual ; abstract ;
    function  GetParamAsFloat(const psName: string): real;virtual ; abstract ;
    procedure SetParamAsFloat(const psName: string; const Value: real);virtual ; abstract ;
    function  GetParamAsInteger(const psName: string): Int64;virtual ; abstract ;
    procedure SetParamAsInteger(const psName: string;const Value: Int64 );virtual ; abstract ;
    function  GetParamAsTextBLOB(const psName: string): string; virtual ; abstract ;
    procedure SetParamAsTextBLOB(const psName, Value: string); virtual ; abstract ;
    function  GetParamAsDateTime(const psName: string): TDateTime;virtual ;abstract ;
    procedure SetParamAsDateTime(const psName: string; const Value: TDateTime); virtual ; abstract ;
    procedure SetParamAsMacro(const psName: string; const Value: string); virtual ;
    function  GetParamIsNull(const psName: String): Boolean; virtual; abstract;
    procedure SetParamIsNull(const psName: String; const Value: Boolean); virtual; abstract;

    function  GetFieldAsString(const psName: string): string ; virtual ; abstract ;
    function  GetFieldAsFloat(const psName: string): real ; virtual ; abstract ;
    function  GetFieldAsBoolean(const psName: string): boolean ; virtual ; abstract ;
    function  GetFieldAsInteger(const psName: string): Int64 ; virtual ; abstract ;
    function  GetFieldAsDateTime(const psName: string):TDateTime ; virtual ; abstract ;
    function  GetFieldIsNull(const psName: string): Boolean; virtual ; abstract ;

    function  GetFieldAsStringByIndex(pIndex: Integer): string     ; virtual ; abstract ;
    function  GetFieldAsFloatByIndex(pIndex: Integer)   : real     ; virtual ; abstract ;
    function  GetFieldAsBooleanByIndex(pIndex: Integer) : boolean  ; virtual ; abstract ;
    function  GetFieldAsIntegerByIndex(pIndex: Integer) : Int64    ; virtual ; abstract ;
    function  GetFieldAsDateTimeByIndex(pIndex: Integer):TDateTime ; virtual ; abstract ;
    function  GetFieldIsNullByIndex(pIndex: Integer):Boolean       ; virtual ; abstract ;

  public
    constructor Create ; virtual ;
    destructor  Destroy ; override ;
    procedure   AssignParams( const pParams : TtiQueryParams ; const pWhere : TtiQueryParams = nil ) ; virtual ;

    property  ParamAsString[ const psName : string ] : string
                read  GetParamAsString
                write SetParamAsString ;
    property  ParamAsInteger[ const psName : string ] : Int64
                read  GetParamAsInteger
                write SetParamAsInteger ;

    property  ParamAsBoolean[ const psName : string ] : boolean
                read  GetParamAsBoolean
                write SetParamAsBoolean ;

    property  ParamAsFloat[ const psName : string ] : real
                read  GetParamAsFloat
                write SetParamAsFloat ;

    property  ParamAsDateTime[ const psName : string ] : TDateTime
                read GetParamAsDateTime
                write SetParamAsDateTime ;

    property  ParamAsTextBLOB[ const psName : string ] : string
                read GetParamAsTextBLOB
                write SetParamAsTextBLOB ;

//    property  ParamAsStream[ const psName : string ] : TStream
//                write  SetParamAsStream ;

    property  ParamAsMacro[ const psName : string ] : string
                write SetParamAsMacro ;

    property  ParamIsNull[const psName: String ] : boolean
                read GetParamIsNull
                write SetParamIsNull ;


    property  FieldAsString[ const psName : string ] : string
                read GetFieldAsString ;

    property  FieldAsFloat[ const psName : string ] : real
                read  GetFieldAsFloat ;

    property  FieldAsBoolean[ const psName : string ]  : boolean
                read GetFieldAsBoolean ;

    property  FieldAsInteger[ const psName : string ] : Int64
                read GetFieldAsInteger ;

    property  FieldAsDateTime[ const psName : string ] : TDateTime
                read GetFieldAsDateTime ;

    property  FieldIsNull[ const psName : string ] : Boolean
                read GetFieldIsNull ;

    property FieldAsStringByIndex[ pIndex: Integer ] : string
                read GetFieldAsStringByIndex ;
    property FieldAsFloatByIndex[ pIndex: Integer ] : Real
                read GetFieldAsFloatByIndex ;
    property FieldAsBooleanByIndex[ pIndex: Integer ] : Boolean
                read GetFieldAsBooleanByIndex ;
    property FieldAsIntegerByIndex[ pIndex: Integer ] : Int64
                read GetFieldAsIntegerByIndex ;
    property FieldAsDateTimeByIndex[ pIndex: Integer ] : TDateTime
                read GetFieldAsDateTimeByIndex ;
    property FieldIsNullByIndex[ pIndex: Integer ] : Boolean
                read GetFieldIsNullByIndex ;

    property SQL          : TStrings read GetSQL    write SetSQL ;
    // Don't use SQL.Text as there may be some code in SetSQL that must execute
    property SQLText      : string   read GetSQLText write SetSQLText ;
    property Active       : boolean  read GetActive write SetActive ;
    property EOF          : boolean  read GetEOF ;
    property ContinueScan : boolean  read fContinueScan write fContinueScan;

    procedure Open ; virtual ; abstract ;
    procedure Close ; virtual ; abstract ;
    procedure ExecSQL ; virtual ; abstract ;

    procedure SelectRow( const pTableName : string ; const pWhere  : TtiQueryParams ) ; virtual ; abstract ;
    procedure InsertRow( const pTableName : string ; const pParams : TtiQueryParams ) ; virtual ; abstract ;
    procedure DeleteRow( const pTableName : string ; const pWhere  : TtiQueryParams ) ; virtual ; abstract ;
    procedure UpdateRow( const pTableName : string ; const pParams : TtiQueryParams ; const pWhere  : TtiQueryParams ) ; virtual ; abstract ;

    procedure Next ; virtual ; abstract ;

    function  ParamCount : integer ; virtual ; abstract ;
    function  ParamName( pIndex : integer ) : string ; virtual ; abstract ;

    function  FieldCount : integer ; virtual ; abstract ;
    function  FieldName( pIndex : integer ) : string ; virtual ; abstract ;
    function  FieldIndex( const psName : string ) : integer ; virtual ; abstract ;
    function  FieldKind( pIndex : integer ) : TtiQueryFieldKind ; virtual ; abstract ;
    function  FieldSize( pIndex : integer ) : integer ; virtual ; abstract ;
    function  HasNativeLogicalType : boolean ; virtual ; abstract ;

    procedure   AssignParamFromStream(     const pName  : string  ; const pVaule : TStream ) ; virtual ; abstract ;
    procedure   AssignParamToStream(       const pName  : string  ; const pVaule : TStream ) ; virtual ; abstract ;
    procedure   AssignFieldAsStream(       const pName  : string  ; const pValue : TStream ) ; virtual ; abstract ;
    procedure   AssignFieldAsStreamByIndex(      pIndex : integer ; const pValue : TStream ) ; virtual ; abstract ;

    property  Database : TtiDatabase read FDatabase write FDatabase ;
    procedure AttachDatabase( pDatabase : TtiDatabase ) ; virtual ;
    procedure DetachDatabase ;  virtual ;
    procedure Reset ; virtual ; abstract ;

    function  ParamsAsString : string ; virtual ;
    property  QueryType    : TtiQueryType read GetQueryType ;

  end ;

  TtiDatabaseSQL = class( TtiDatabase )
  private
  protected
    function FieldMetaDataToSQLCreate( const pFieldMetaData : TtiDBMetaDataField ) : string ; virtual ; abstract ;
  public
    procedure   DropTable( const pTableMetaData : TtiDBMetaDataTable ) ; override ;
    procedure   CreateTable( const pTableMetaData : TtiDBMetaDataTable ) ; override ;
  end ;

  TtiQuerySQL = class( TtiQuery )
  private
  protected
    function    WhereClause( const pWhere: TtiQueryParams ) : string ;
    function    SQLAndParamsAsString : string ;
  public
    procedure   SelectRow( const pTableName : string ; const pWhere : TtiQueryParams ) ; override ;
    procedure   InsertRow( const pTableName : string ; const pParams    : TtiQueryParams ) ; override ;
    procedure   DeleteRow(   const pTableName : string ; const pWhere : TtiQueryParams ) ; override ;
    procedure   UpdateRow( const pTableName : string ; const pParams : TtiQueryParams ; const pWhere  : TtiQueryParams ) ; override ;
  end ;

  TtiQueryNonSQL = class( TtiQuery )
  private
    FParams : TtiQueryParams ;

  protected
    property    Params : TtiQueryParams read FParams ;

    function    GetParamAsBoolean(const psName: string): boolean; override ;
    function    GetParamAsFloat(const psName: string): real;override ;
    function    GetParamAsInteger(const psName: string): Int64 ;override ;
    function    GetParamAsTextBLOB(const psName: string): string; override ;
    function    GetParamAsDateTime(const psName: string): TDateTime ; override ;
    function    GetParamIsNull( const psName: String): Boolean; override;

    procedure   SetParamAsBoolean(const psName: string;const Value: boolean);override ;
    procedure   SetParamAsFloat(const psName: string; const Value: real);override ;
    procedure   SetParamAsInteger(const psName: string;const Value: Int64);override ;
    procedure   SetParamAsTextBLOB(const psName, Value: string); override ;
    procedure   SetParamAsDateTime(const psName :string ; const Value: TDateTime); override ;
    procedure   SetParamAsMacro( const psName: string;
                                 const Value: string); override ;
    procedure   SetParamIsNull( const psName: String; const Value: Boolean); override;

  public
    constructor Create ; override ;
    destructor  Destroy ; override ;

    function    ParamCount : integer ; override ;
    function    ParamName( pIndex : integer ) : string ; override ;
    function    ParamsAsString : string ; override ;

    procedure   AssignParamFromStream(     const pName  : string  ; const pValue : TStream ) ; override ;
    procedure   AssignParamToStream(       const pName  : string  ; const pValue : TStream ) ; override ;
    procedure   AssignFieldAsStream(       const pName  : string  ; const pValue : TStream ) ; override ;
    procedure   AssignFieldAsStreamByIndex(      pIndex : integer ; const pValue : TStream ) ; override ;

  end ;

  TtiQueryClass      = class of TtiQuery ;
  TtiDatabaseClass   = class of TtiDatabase ;
  TtiQueryParamClass = class of TtiQueryParamAbs ;


  //----------------------------------------------------------------------------
  TtiQueryParams = class( TPerObjList )
  private
    function    GetParamIsNull( const pName : string  ): boolean;
    procedure   SetParamIsNull( const pName : string ; Value: boolean);
    function    GetAsString: string;
  protected
    function    GetItems(i: integer): TtiQueryParamAbs ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TtiQueryParamAbs); reintroduce ;
    function    FindCreateParamByName( const pName : string ; const pClass : TtiQueryParamClass ) : TtiQueryParamAbs ; virtual ;
  public
    property    Items[i:integer] : TtiQueryParamAbs read GetItems write SetItems ;
    procedure   Add( pObject : TtiQueryParamAbs   ; pDefDispOrdr : boolean = true ) ; reintroduce ;
    function    FindParamByName( const pName : string ) : TtiQueryParamAbs ; virtual ;

    property    ParamIsNull[const pName : string ] : boolean read GetParamIsNull write SetParamIsNull ;
    function    ParamName( pIndex : integer ) : string ;
    property    AsString : string read GetAsString ;

    procedure   SetValueAsString(const pName : string ; const pValue : string    ) ;
    function    GetValueAsString(const pName : string) : string ;

    procedure   SetValueAsInteger(const pName : string ; const pValue : Int64   ) ;
    function    GetValueAsInteger(const pName : string ) : Int64 ;

    procedure   SetValueAsFloat(const pName : string ; const pValue : real      ) ;
    function    GetValueAsFloat(const pName : string ) : Real ;

    procedure   SetValueAsBoolean(const pName : string ; const pValue : Boolean   ) ;
    function    GetValueAsBoolean(const pName : string ) : Boolean ;

    procedure   SetValueAsDateTime(const pName : string ; const pValue : TDateTime ) ;
    function    GetValueAsDateTime(const pName : string ) : TDateTime ;

    procedure   SetValueAsStream(const pName : string ; const pValue : TStream ) ;
    function    GetValueAsStream(const pName : string ) : TStream ;
    procedure   AssignValueToStream( const pName : string ; const pStream : TStream ) ;

    procedure   SetValueFromProp(const pInstance : TPerObjAbs ; const pPropName : string ; const pParamName : string ) ;


  published
  end ;

  //----------------------------------------------------------------------------
  TtiQueryParamAbs = class( TPerObjAbs )
  private
    FName: string;
    FIsNull: boolean;
    function    GetKindAsStr: string;
  protected
    function    GetOwner: TtiQueryParams; reintroduce ;
    procedure   SetOwner(const Value: TtiQueryParams); reintroduce ;
    function    GetKind : TtiQueryFieldKind ; virtual ; abstract ;
  public
    property    Owner       : TtiQueryParams             read GetOwner      write SetOwner ;
    property    Kind        : TtiQueryFieldKind read GetKind ;
    function    GetValueAsString : string ; virtual ; abstract ;
    procedure   SetValueAsString(const pValue : string ) ; virtual ; abstract ;
    procedure   AssignToTIQuery(const pQuery : TtiQuery); virtual ; abstract ;
    property    IsNull      : boolean read FIsNull write FIsNull ;
  published
    property    Name        : string read FName write FName ;
    property    KindAsStr   : string read GetKindAsStr ;
    property    ValueAsString : string read GetValueAsString write SetValueAsString ;
  end ;

  TtiQueryParamString = class( TtiQueryParamAbs )
  private
    FValue : string ;
  protected
    function    GetKind : TtiQueryFieldKind ; override ;
  public
    function    GetValueAsString : string ; override ;
    procedure   SetValueAsString(const pValue : string ) ; override ;
    procedure   AssignToTIQuery(const pQuery : TtiQuery); override ;
  end ;

  TtiQueryParamInteger = class( TtiQueryParamAbs )
  private
    FValue : Int64 ;
  protected
    function    GetKind : TtiQueryFieldKind ; override ;
  public
    procedure   SetValueAsString(const pValue : string ) ; override ;
    procedure   SetValueAsInteger(const pValue : Int64 ) ;
    function    GetValueAsInteger : Int64 ;
    function    GetValueAsString  : string ; override ;
    procedure   AssignToTIQuery(const pQuery : TtiQuery); override ;
  end ;

  TtiQueryParamFloat = class( TtiQueryParamAbs )
  private
    FValue : real ;
  protected
    function    GetKind : TtiQueryFieldKind ; override ;
  public
    procedure   SetValueAsString(const pValue : string ) ; override ;
    procedure   SetValueAsFloat(const pValue : real ) ;
    function    GetValueAsFloat   : real ;
    function    GetValueAsString : string ; override ;
    procedure   AssignToTIQuery(const pQuery : TtiQuery); override ;
  end ;

  TtiQueryParamDateTime = class( TtiQueryParamAbs )
  private
    FValue : TDateTime ;
  protected
    function    GetKind : TtiQueryFieldKind ; override ;
  public
    procedure   SetValueAsString(const pValue : string ) ; override ;
    procedure   SetValueAsDateTime(const pValue : TDateTime ) ;
    function    GetValueAsDateTime    : TDateTime ;
    function    GetValueAsString : string ; override ;
    procedure   AssignToTIQuery(const pQuery : TtiQuery); override ;
  end ;

  TtiQueryParamBoolean = class( TtiQueryParamAbs )
  private
    FValue : Boolean ;
  protected
    function    GetKind : TtiQueryFieldKind ; override ;
  public
    procedure   SetValueAsString(const pValue : string ) ; override ;
    procedure   SetValueAsBoolean(const pValue : Boolean ) ;
    function    GetValueAsBoolean : Boolean ;
    function    GetValueAsString : string ; override ;
    procedure   AssignToTIQuery(const pQuery : TtiQuery); override ;
  end ;

  TtiQueryParamStream = class( TtiQueryParamAbs )
  private
    FStream : TMemoryStream ;
  protected
    function    GetKind : TtiQueryFieldKind ; override ;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    // ValuesAsString assumes the MIME encode
    procedure   SetValueAsString(const pValue : string ) ; override ;
    function    GetValueAsString : string ; override ;
    procedure   SetValueAsStream(const pValue : TStream ) ;
    function    GetValueAsStream : TStream ;
    procedure   AssignToTIQuery(const pQuery : TtiQuery); override ;
  end ;

const
  cgtiQueryMacroChr  = '&' ;

function  StrToQueryFieldKind(const psFieldKind: String): TtiQueryFieldKind;
function  QueryFieldKindToString( pFieldKind : TtiQueryFieldKind ) : string ;
procedure QueryFieldKindsToStrings( pStrings : TStrings ) ;

implementation
uses
  tiCommandLineParams
  ,tiLog
  ,tiPersist
  ,tiDBConnectionPool
  {$IFDEF MSWINDOWS}
  ,Controls
  ,Forms
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  ,QControls
  ,QForms
  {$ENDIF LINUX}
  ,Math
  ,ctiPersist
  ,TypInfo
  ,tiRJMime
  ,tiXML
  ;

function StrToQueryFieldKind(const psFieldKind: String): TtiQueryFieldKind;
var
  Index: TtiQueryFieldKind;
begin
  Result := Low(TtiQueryFieldKind);

  for Index := Low(TtiQueryFieldKind) to High(TtiQueryFieldKind) do
    if SameText( cgaQueryFieldKind[Index], psFieldKind ) then
    begin
      Result := Index;
      Exit ; //==>
    end;

  tiFmtException( 'Invalid field kind <%s>',
                  [psFieldKind],
                  'Unknown',
                  'StrToQueryFieldKind');

end;

function QueryFieldKindToString( pFieldKind : TtiQueryFieldKind ) : string ;
begin
  result := cgaQueryFieldKind[pFieldKind] ;
end ;

procedure QueryFieldKindsToStrings( pStrings : TStrings ) ;
var
  Index: TtiQueryFieldKind;
begin
  pStrings.Clear ;
  for Index := Low(TtiQueryFieldKind) to High(TtiQueryFieldKind) do
    pStrings.Add( cgaQueryFieldKind[Index] ) ;
end ;

function tiTypeKindToQueryFieldKind( pValue : TtiTypeKind ) : TtiQueryFieldKind ;
begin
  case pValue of
  tiTKInteger  : result := qfkInteger ;
  tiTKFloat    : result := qfkFloat ;
  tiTKString   : result := qfkString ;
  tiTKDateTime : result := qfkDateTime ;
  tiTKBoolean  : result := qfkLogical ;
  else
    tiFmtException( 'Invalid TtiTypeKind', '', 'tiTypeKindToQueryFieldKind' ) ;
    result := qfkInteger ; // Just to shut the compiler up. Wont get here.
  end ;
  //qfkBinary,
  //qfkMacro,
  //qfkLongString
end;

{ TtiDBMetaData }

procedure TtiDBMetaData.Add(pObject: TtiDBMetaDataTable; pbDefaultDispOrder: boolean);
begin
  inherited Add( pObject, pbDefaultDispOrder ) ;
end;

procedure TtiDBMetaData.Clear;
begin
  inherited Clear ;
  ObjectState := posEmpty ;
end;

function TtiDBMetaData.FindByTableName( const pTableName: TTableName): TtiDBMetaDataTable;
var
  i : integer ;
begin
  // result := TtiDBMetaDataTable( Inherited FindByProps( ['Name'], [pTableName] )) ;
  // Hard coding the check is faster, and more stable
  result := nil ;
  for i := 0 to Count - 1 do
    if SameText( Items[i].Name, pTableName ) then
    begin
      result := Items[i];
      Exit ; //==>
    end ;
end;

function TtiDBMetaData.GetCaption: string;
begin
  Assert( Owner <> nil, 'Owner is nill' ) ;
  result := TDBConnectionPool( Owner ).DBConnectParams.DatabaseName ;
end;

function TtiDBMetaData.GetItems(i: integer): TtiDBMetaDataTable;
begin
  result := TtiDBMetaDataTable( inherited GetItems( i ) );
end;

function TtiDBMetaData.GetOwner: TPerObjAbs;
begin
  result := TDBConnectionPool( inherited GetOwner ) ;
end;

procedure TtiDBMetaData.Read( const pDBConnectionName: string  = '' ; pPerLayerName : string = '' );
var
  lPooledDB : TPooledDB ;
  lCursor : TCursor ;
begin
  if ObjectState <> posEmpty then
    Exit ; //==>
  lCursor := Screen.Cursor ;
  Screen.Cursor := crHourGlass ;
  try
    lPooledDB := TDBConnectionPool( Owner ).Lock ;
    try
      Clear ;
      lPooledDB.Database.ReadMetaDataTables( Self ) ;
    finally
      TDBConnectionPool( Owner ).UnLock( lPooledDB ) ;
    end ;
  finally
    Screen.Cursor := lCursor ;
  end ;
end;

procedure TtiDBMetaData.SetItems(i: integer;
  const Value: TtiDBMetaDataTable);
begin
  inherited SetItems( i, Value ) ;
end;

procedure TtiDBMetaData.SetOwner(const Value: TPerObjAbs);
begin
  inherited SetOwner( Value as TPerObjAbs ) ;
end;

{ TtiDBMetaDataTable }

procedure TtiDBMetaDataTable.Add(pObject: TtiDBMetaDataField;
  pbDefaultDispOrder: boolean);
begin
  inherited Add( pObject, pbDefaultDispOrder ) ;
  FMaxFieldWidth := 0 ;
end;

// Don't use AddField. Use AddInstance instead
procedure TtiDBMetaDataTable.AddField(const pFieldName: string;
  const pFieldKind: TtiQueryFieldKind; pFieldWidth: integer);
begin
  AddInstance( pFieldName, pFieldKind, pFieldWidth ) ;
end;

function TtiDBMetaDataTable.AddInstance(const pFieldName: string;
  const pFieldKind: TtiQueryFieldKind; pFieldWidth: integer = 0 ):TtiDBMetaDataField;
begin
  Result := AddInstance;
  Result.Name  := pFieldName ;
  Result.Kind  := pFieldKind ;
  Result.Width := pFieldWidth ;
  Result.ObjectState := posClean ;
end;

function TtiDBMetaDataTable.AddInstance: TtiDBMetaDataField;
begin
  Result := TtiDBMetaDataField.Create ;
  Add( Result ) ;
end;

function TtiDBMetaDataTable.Clone: TtiDBMetaDataTable;
begin
  result := TtiDBMetaDataTable( inherited Clone ) ;
end;

constructor TtiDBMetaDataTable.create;
begin
  inherited;
  FMaxFieldWidth := 0 ;
end;
                       
destructor TtiDBMetaDataTable.destroy;
begin
  inherited;
end;

function TtiDBMetaDataTable.FindByFieldName( const pFieldName: TFieldName): TtiDBMetaDataField;
var
  i : integer ;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if SameText( Items[i].Name, pFieldName ) then
    begin
      result := Items[i];
      Exit ; //==>
    end ;
end;

function TtiDBMetaDataTable.GetCaption: string;
begin
  result := Name ;
end;

function TtiDBMetaDataTable.GetItems(i: integer): TtiDBMetaDataField;
begin
  result := TtiDBMetaDataField( inherited GetItems( i )) ;
end;

function TtiDBMetaDataTable.GetOwner: TtiDBMetaData;
begin
  result := TtiDBMetaData( inherited GetOwner ) ;
end;

function TtiDBMetaDataTable.MaxFieldNameWidth: word;
var
  i : integer ;
begin
  if FMaxFieldWidth <> 0 then
  begin
    result := FMaxFieldWidth ;
    Exit ; //==>
  end ;
  for i := 0 to Count - 1 do
    FMaxFieldWidth := Max( FMaxFieldWidth, Length( Items[i].Name )) ;
  result := FMaxFieldWidth ;
end;

procedure TtiDBMetaDataTable.Read( const pDBConnectionName: string  = '' ; pPerLayerName : string = '' );
var
  lPooledDB : TPooledDB ;
  lCursor : TCursor ;
begin
  if ObjectState <> posPK then
    Exit ; //==>
  lCursor := Screen.Cursor ;
  Screen.Cursor := crHourGlass ;
  try
    lPooledDB := TDBConnectionPool( Owner.Owner ).Lock ;
    try
    lPooledDB.Database.ReadMetaDataFields( Self ) ;
    finally
      TDBConnectionPool( Owner.Owner ).UnLock( lPooledDB ) ;
    end ;
  finally
    Screen.Cursor := lCursor ;
  end ;
end;

procedure TtiDBMetaDataTable.SetItems(i: integer;
  const Value: TtiDBMetaDataField);
begin
  inherited SetItems( i, Value ) ;
end;

procedure TtiDBMetaDataTable.SetOwner(const Value: TtiDBMetaData);
begin
  inherited SetOwner( Value ) ;
end;

{ TtiDBMetaDataField }

function TtiDBMetaDataField.Clone: TtiDBMetaDataField;
begin
  result := TtiDBMetaDataField( inherited Clone ) ;
end;

function TtiDBMetaDataField.GetCaption: string;
begin
  result := name ;
end;

function TtiDBMetaDataField.GetKindAsStr: string;
begin
  result := QueryFieldKindToString( Kind ) ;
end;

function TtiDBMetaDataField.GetOwner: TtiDBMetaDataTable;
begin
  result := TtiDBMetaDataTable( inherited GetOwner ) ;
end;

function TtiDBMetaDataField.GetRPadName: TFieldName;
begin
  result := tiPadR( Name, Owner.MaxFieldNameWidth ) ;
end;

procedure TtiDBMetaDataField.SetKindAsStr(const Value: string);
begin
  Kind := StrToQueryFieldKind( Value ) ;
end;

procedure TtiDBMetaDataField.SetOwner(const Value: TtiDBMetaDataTable);
begin
  inherited SetOwner( Value ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// *  tiDatabase
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiDatabase.Connect(const psDatabaseName, psUserName, psPassword, pParams: string);
var
  i : Integer ;
begin
  LogFmt( 'Attempting to connect to: %s Params: %s', [psDatabaseName, pParams] ) ;
  DatabaseName     := psDatabaseName ;
  UserName         := psUserName     ;
  Password         := psPassword     ;
  for i := 1 to tiNumToken(pParams, ',') do
    FParams.Add(tiToken(pParams, ',', i));
  if gCommandLineParams.IsParam( 'VerboseDBConnection' ) then
    Log( 'Connected. Database: ' + psDatabaseName +
         ' UserName: ' + psUserName +
         ' Password: ' + psPassword ) ;
  if Connected then
    Connected := false ;
  Connected := true ;
  LogFmt( 'Connect to %s successful.', [psDatabaseName] ) ;
end;

constructor TtiDatabase.create;
begin
  inherited ;
  FErrorInLastCall := false ;
  FParams          := TStringList.Create;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiQuery
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiQuery.Create;
begin
  inherited ;
end;

// -----------------------------------------------------------------------------
destructor TtiQuery.destroy;
begin
  inherited;
end;

// -----------------------------------------------------------------------------
procedure TtiQuery.AttachDatabase(pDatabase: TtiDatabase);
begin
  FDatabase := pDatabase ;
end;

// -----------------------------------------------------------------------------
function TtiQuery.ParamsAsString: string;
var
  i : integer ;
begin
  try
    result := '' ;
    for i := 0 to ParamCount - 1 do
    begin
      result := tiAddTrailingValue( result, CrLf, true ) ;
      result := result +
                ParamName( i ) + ' := ';
      if ParamIsNull[ ParamName( i )] then      // Display the fact
        result := result + 'Null'
      else
        result := result + ParamAsString[ ParamName( i )] ;
    end ;
  except
    on e:exception do
      result := 'Unknown' ;
  end ;
end;

// -----------------------------------------------------------------------------
procedure TtiQuery.DetachDatabase;
begin
  if Active then
    Active := false ;
  FDatabase := nil ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiQuerySQL
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiQuerySQL.DeleteRow(const pTableName: string; const pWhere: TtiQueryParams);
var
  lSQL : string ;
begin
  try
  lSQL := 'delete from ' + pTableName ;
  lSQL := lSQL + WhereClause( pWhere ) ;
  SQLText := lSQL ;
  AssignParams( pWhere ) ;
  ExecSQL ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'DeleteRow') ;
  end ;

end;

procedure TtiQuerySQL.InsertRow(const pTableName: string; const pParams: TtiQueryParams);
var
  lSQL : string ;
  lFields : string ;
  lParams : string ;
  i : integer ;
begin
  try
    lSQL := 'insert into ' + pTableName ;
    lFields := '' ;
    lParams := '' ;
    for i := 0 to pParams.Count - 1 do
    begin
      lFields := tiAddTrailingValue( lFields, ',' + CrLf ) ;
      lParams := tiAddTrailingValue( lParams, ',' + CrLf ) ;
      lFields := lFields + pParams.Items[i].Name ;
      lParams := lParams + ':' + pParams.Items[i].Name ;
    end ;
    lSQL := lSQL + CrLf +
            '(' + lFields +
            ')' + CrLf +
            'values' + CrLf +
            '(' + CrLf +
            lParams + CrLf +
            ')' ;
    SQLText := lSQL ;
    AssignParams( pParams ) ;
    ExecSQL ;
  except
    on e:exception do
    begin
      tiFmtException( e, ClassName, 'InsertRow' ) ;
    end ;
  end ;
end;

procedure TtiQuerySQL.SelectRow(const pTableName: string; const pWhere: TtiQueryParams);
var
  lSQL : string ;
  i : integer ;
  lWhere : string ;
begin

  try
   lWhere := '' ;
  // This code is cloned from tiDatabaseSQL
  if ( pWhere <> nil ) then
    for i := 0 to pWhere.Count - 1 do
    begin
      lWhere := tiAddTrailingValue( lWhere, ' and ' + CrLf ) ;
      lWhere := lWhere +
                pWhere.Items[i].Name + ' = :' +
                pWhere.Items[i].Name ;
    end ;

  if lWhere <> '' then
    lSQL := 'select * from ' + pTableName + CrLf +
            'where' + CrLf +
            lWhere
  else
    lSQL := 'select * from ' + pTableName ;

  SQLText := lSQL ;
  AssignParams( pWhere ) ;
  Open ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'SelectRow' ) ;
  end ;

end;

function TtiQuery.GetQueryType: TtiQueryType;
var
  lSQL : string ;
begin
  // Must strip comments before this will work in all cases.
  lSQL := SQLText ;
  lSQL := Trim( lSQL ) ;
  if SameText( Copy( lSQL, 1, 6 ), 'select' ) then
    result := qtSelect
  else
    result := qtOther ;
end;

//------------------------------------------------------------------------------
procedure TtiQuery.SetParamAsMacro(const psName, Value: string);
begin
  // ToDo: ParamAsMacro will only work once on any given SQL statement because
  //       it replaces the macro character with 'Value' If this is to work more
  //       than once, then the SQL must be saved, or the macro values must be
  //       cached.
  SQLText :=
    tiCIStrTran( SQLText,
                 cgtiQueryMacroChr + psName,
                 Value ) ;
end;

procedure TtiDatabase.DropTable(const pTableName: TTableName);
var
  lDBMetaDataTable : TtiDBMetaDataTable ;
begin
  lDBMetaDataTable := TtiDBMetaDataTable.Create ;
  try
    lDBMetaDataTable.Name := pTableName ;
    DropTable( lDBMetaDataTable ) ;
  finally
    lDBMetaDataTable.Free;
  end ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDatabaseSQL
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiDatabaseSQL.CreateTable( const pTableMetaData: TtiDBMetaDataTable);
var
  lSQL : string ;
  i : integer ;
begin

  lSQL := '' ;
  for i := 0 to pTableMetaData.Count - 1 do
  begin
    lSQL := tiAddTrailingValue( lSQL, ',' + CrLf ) ;
    lSQL := lSQL +
            pTableMetaData.Items[i].Name + ' ' +
            FieldMetaDataToSQLCreate( pTableMetaData.Items[i] ) ;
  end ;
  lSQL := 'create table ' + pTableMetaData.Name + CrLf + '(' + CrLf +
          lSQL + CrLf +
          ')' ;
  try
    ExecSQL( lSQL ) ;
  except
    on e:exception do
      tiFmtException( e, 'Unable to execute create SQL', ClassName, 'CreateTable' ) ;
  end;

end;

procedure TtiDatabase.DeleteRow(const pTableName: string; const pWhere: TtiQueryParams);
var
  lQuery : TtiQuery ;
  lHadToStartTransaction : boolean ;
begin
  lQuery    := gTIPerMgr.RegPerLayers.CreateTIQuery(TtiDatabaseClass(ClassType)) ;
  try
    lQuery.AttachDatabase( Self ) ;
    lHadToStartTransaction := not InTransaction ;
    if lHadToStartTransaction then
      StartTransaction ;
    try
      lQuery.DeleteRow( pTableName, pWhere ) ;
      if lHadToStartTransaction then
        Commit ;
    except
      on e:exception do
      begin
        if lHadToStartTransaction then
          RollBack ;
        raise ;
      end ;
    end ;
  finally
    lQuery.Free ;
  end ;
end;

procedure TtiDatabaseSQL.DropTable( const pTableMetaData: TtiDBMetaDataTable);
var
  lSQL : string ;
begin
  lSQL := 'drop table ' + pTableMetaData.Name ;
  ExecSQL( lSQL ) ;
end;

{ TtiQueryParams }

procedure TtiQueryParams.Add(pObject: TtiQueryParamAbs; pDefDispOrdr: boolean);
begin
  inherited Add( pObject, pDefDispOrdr ) ;
end;

function TtiQueryParams.FindCreateParamByName(const pName: string ; const pClass : TtiQueryParamClass ): TtiQueryParamAbs;
begin
  result := FindParamByName(pName);
  if result = nil then
  begin
    result := pClass.Create ;
    result.Name := pName ;
    Add(result);
  end;
end;

function TtiQueryParams.FindParamByName( const pName: string): TtiQueryParamAbs;
var
  i : integer ;
begin
  result := nil ;
  for i := 0 to Count - 1 do
    if SameText( Items[i].Name, pName ) then
    begin
      result := Items[i] ;
      Break ; //==>
    end ;
end;

function TtiQueryParams.GetAsString: string;
var
  i : integer ;
begin
  result := '' ;
  for i := 0 to Count - 1 do
  begin
    if result <> '' then result := result + ', ' ;
    result := result + Items[i].Name + '=' + Items[i].GetValueAsString ;
  end;
end;

function TtiQueryParams.GetItems(i: integer): TtiQueryParamAbs;
begin
  result := TtiQueryParamAbs( inherited GetItems( i )) ;
end;

function TtiQueryParams.GetParamIsNull( const pName : string ): boolean;
var
  lParam : TtiQueryParamAbs ;
begin
  lParam := FindParamByName( pName ) ;
  result := ( lParam = nil ) or ( lParam.IsNull );
end;

procedure TtiQueryParams.SetValueAsString(const pName, pValue: string);
var
  lParam : TtiQueryParamAbs ;
begin
  lParam := FindCreateParamByName(pName, TtiQueryParamString) ;
  lParam.SetValueAsString(pValue) ;
end;

function TtiQueryParams.GetValueAsString(const pName: string): string;
var
  lParam : TtiQueryParamAbs ;
begin
  lParam := FindParamByName(pName) ;
  Assert( lParam.TestValid(TtiQueryParamAbs), cTIInvalidObjectError );
  result := lParam.GetValueAsString ;
end;

function TtiQueryParams.ParamName(pIndex: integer): string;
var
  lParam : TtiQueryParamAbs ;
begin
  lParam := Items[pIndex] ;
  result := lParam.Name ;
end;

procedure TtiQueryParams.SetItems(i: integer; const Value: TtiQueryParamAbs);
begin
  inherited SetItems( i, Value ) ;
end;

procedure TtiQueryParams.SetParamIsNull( const pName : string ; Value: boolean);
var
  lParam : TtiQueryParamAbs ;
begin
  lParam := FindParamByName( pName ) ;
  if lParam <> nil then
    lParam.IsNull := Value ;
end;

procedure TtiQueryParams.SetValueAsInteger(const pName: string;const pValue: Int64);
var
  lParam : TtiQueryParamInteger ;
begin
  lParam := FindCreateParamByName(pName, TtiQueryParamInteger) as TtiQueryParamInteger;
  lParam.SetValueAsInteger(pValue) ;
end;

{
function TtiQueryParams.ParamAsInteger(const pName: string): Integer;
var
  lParam : TtiQueryParamInteger ;
begin
  lParam := FindCreateParamByName(pName, TtiQueryParamInteger) as TtiQueryParamInteger;
  result := lParam.GetValue ;
end;
}

procedure TtiQueryParams.SetValueAsFloat(const pName: string;const pValue: real);
var
  lParam : TtiQueryParamFloat ;
begin
  lParam := FindCreateParamByName(pName, TtiQueryParamFloat) as TtiQueryParamFloat;
  lParam.SetValueAsFloat(pValue) ;
end;

{
function TtiQueryParams.ParamAsFloat(const pName: string): real;
var
  lParam : TtiQueryParamFloat ;
begin
  lParam := FindCreateParamByName(pName, TtiQueryParamFloat) as TtiQueryParamFloat;
  result := lParam.GetValue ;
end;
}

procedure TtiQueryParams.SetValueAsDateTime(const pName: string;const pValue: TDateTime);
var
  lParam : TtiQueryParamDateTime ;
begin
  lParam := FindCreateParamByName(pName, TtiQueryParamDateTime) as TtiQueryParamDateTime;
  lParam.SetValueAsDateTime(pValue) ;
end;

{
function TtiQueryParams.ParamAsDateTime(const pName: string): TDateTime;
var
  lParam : TtiQueryParamDateTime ;
begin
  lParam := FindCreateParamByName(pName, TtiQueryParamDateTime) as TtiQueryParamDateTime;
  result := lParam.GetValue ;
end;
}

procedure TtiQueryParams.SetValueAsBoolean(const pName: string;const pValue: Boolean);
var
  lParam : TtiQueryParamBoolean ;
begin
  lParam := FindCreateParamByName(pName, TtiQueryParamBoolean) as TtiQueryParamBoolean;
  lParam.SetValueAsBoolean(pValue) ;
end;

{
function TtiQueryParams.ParamAsBoolean(const pName: string): Boolean;
var
  lParam : TtiQueryParamBoolean ;
begin
  lParam := FindCreateParamByName(pName, TtiQueryParamBoolean) as TtiQueryParamBoolean;
  result := lParam.GetValue ;
end;
}

{ TtiQueryParam }

function TtiQueryParamAbs.GetKindAsStr: string;
begin
  result := QueryFieldKindToString( Kind ) ;
end;

function TtiQueryParamAbs.GetOwner: TtiQueryParams;
begin
  result := TtiQueryParams( inherited GetOwner ) ;
end;

procedure TtiQueryParamAbs.SetOwner(const Value: TtiQueryParams);
begin
  inherited SetOwner( Value ) ;
end;

procedure TtiDatabase.ExecSQL(const pSQL: string ; pParams : TtiQueryParams = nil );
var
  lQuery : TtiQuery ;
  lHadToStartTransaction : boolean ;
  lMessage : string ;
begin
  lQuery    := CreateTIQuery ;
  try
    lQuery.AttachDatabase( Self ) ;
    lHadToStartTransaction := not InTransaction ;
    if lHadToStartTransaction then
      StartTransaction ;
    lQuery.SQLText := pSQL ;
    lQuery.AssignParams( pParams ) ;
    try
      lQuery.ExecSQL ;
      if lHadToStartTransaction then
        Commit ;
    except
      on e:exception do
      begin
        lMessage := e.message ;
        if ( lHadToStartTransaction {and InTransaction} ) then
        begin
          try
            RollBack ;
          except
            on e:exception do
              lMessage := lMessage + Cr +
                'Error rolling transaction after SQL failed:' + Cr + e.message ;
          end;
        end ;
        tiFmtException( lMessage, ClassName, 'ExecSQL' );
      end ;
    end ;
  finally
    lQuery.Free ;
  end ;
end;

procedure TtiDatabase.InsertRow( const pTableName : string ;
                                    const pParams    : TtiQueryParams ) ;
var
  lQuery : TtiQuery ;
  lHadToStartTransaction : boolean ;
begin
  lQuery    := gTIPerMgr.RegPerLayers.CreateTIQuery(TtiDatabaseClass(ClassType)) ;
  try
    lQuery.AttachDatabase( Self ) ;
    lHadToStartTransaction := not InTransaction ;
    if lHadToStartTransaction then
      StartTransaction ;
    try
      lQuery.InsertRow( pTableName, pParams ) ;
      if lHadToStartTransaction then
        Commit ;
    except
      on e:exception do
      begin
        if lHadToStartTransaction then
          RollBack ;
        raise ;
      end ;
    end ;
  finally
    lQuery.Free ;
  end ;
end;

procedure TtiDatabase.UpdateRow(   const pTableName : string ;
                                   const pParams : TtiQueryParams ;
                                   const pWhere  : TtiQueryParams ) ;
var
  lQuery : TtiQuery ;
  lHadToStartTransaction : boolean ;
begin
  lQuery    := gTIPerMgr.RegPerLayers.CreateTIQuery(TtiDatabaseClass(ClassType)) ;
  try
    lQuery.AttachDatabase( Self ) ;
    lHadToStartTransaction := not InTransaction ;
    if lHadToStartTransaction then
      StartTransaction ;
    try
      lQuery.UpdateRow( pTableName, pWhere, pParams ) ;
      if lHadToStartTransaction then
        Commit ;
    except
      on e:exception do
      begin
        if lHadToStartTransaction then
          RollBack ;
        raise ;
      end ;
    end ;
  finally
    lQuery.Free ;
  end ;

  {
var
  lSQL : string ;
  lFields : string ;
  i : integer ;
  lMergedParams : TtiQueryParams ;
begin
  lSQL := 'update ' + pTableName + ' set ' ;
  lFields := '' ;
  for i := 0 to pParams.Count - 1 do
  begin
    lFields := tiAddTrailingValue( lFields, ',' + CrLf ) ;
    lFields := lFields + pParams.Items[i].Name + ' = :' + pParams.Items[i].Name ;
  end ;

  lSQL := lSQL + CrLf +
          lFields +
          WhereClause( pParams ) ;

  lMergedParams := TtiQueryParams.Create ;
  lMergedParams.OwnsObjects := false ;
  try
    for i := 0 to pWhere.Count - 1 do
      lMergedParams.Add( pWhere.Items[i] ) ;
    for i := 0 to pParams.Count - 1 do
      lMergedParams.Add( pParams.Items[i] ) ;

    ExecSQL( lSQL, lMergedParams ) ;
  finally
    lMergedParams.Free ;
  end ;
}

end;

procedure TtiQuery.AssignParams(const pParams: TtiQueryParams ; const pWhere : TtiQueryParams = nil );
var
  i : integer ;
begin
  try
    if pParams <> nil then
      For i := 0 to pParams.Count - 1 do
        pParams.Items[i].AssignToTIQuery(Self);
    if pWhere <> nil then
      For i := 0 to pWhere.Count - 1 do
        pWhere.Items[i].AssignToTIQuery(Self);
  except
    on e:exception do
      tiFmtException( e, ClassName, 'AssignParams' ) ;
  end ;
end;

function TtiQuerySQL.SQLAndParamsAsString: string;
var
  i : integer ;
begin
  result := 'SQL:';
  for i := 0 to SQL.Count - 1 do
    result := result + Cr + '    ' + SQL.Strings[i] ;
  result := result + Cr(2) + 'Params:';
  for i := 0 to ParamCount - 1 do
  begin
   result := result + Cr + '    ' +
              ParamName( i ) + ' := ';
    if ParamIsNull[ ParamName( i )] then      // Display the fact
      result := result + 'Null'
    else
      result := result + ParamAsString[ ParamName( i )] ;
  end ;
end;

procedure TtiQuerySQL.UpdateRow(const pTableName: string; const pParams, pWhere: TtiQueryParams);
var
  lSQL : string ;
  lFields : string ;
  i : integer ;
begin
  try
  lSQL := 'update ' + pTableName + ' set ' ;
  lFields := '' ;
  for i := 0 to pParams.Count - 1 do
  begin
    lFields := tiAddTrailingValue( lFields, ',' + CrLf ) ;
    lFields := lFields + pParams.Items[i].Name + ' = :' + pParams.Items[i].Name ;
  end ;

  lSQL := lSQL + CrLf +
          lFields +
          WhereClause( pWhere ) ;
          
  SQLText := lSQL ;
  AssignParams( pParams, pWhere ) ;
  ExecSQL ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'UpdateRow') ;
  end ;

end;

function TtiQuerySQL.WhereClause(const pWhere: TtiQueryParams): string;
var
  i : integer ;
begin
  result := '' ;
  if ( pWhere = nil ) or
     ( pWhere.Count = 0 ) then
    Exit ;
  for i := 0 to pWhere.Count - 1 do
  begin
    result := tiAddTrailingValue( Result, CrLf ) ;
    result := result +
              pWhere.Items[i].Name + ' = :' +
              pWhere.Items[i].Name ;
  end ;
  if result <> '' then
    result := CrLf + 'where' + CrLf + result ;
end;

procedure TtiQueryParams.SetValueFromProp(const pInstance: TPerObjAbs; const pPropName, pParamName: string);
var
  lString   : string  ;
  lInteger  : Int64   ;
  lFloat    : real    ;
  lDate     : TDateTime ;
  lBoolean  : boolean ;
  lStream   : TStream ;
  lTypeKind : TtiTypeKind ;
  lPropType : TTypeKind ;
begin
  Assert( pInstance.TestValid(TPerObjAbs), cTIInvalidObjectError );
  Assert( pPropName <> '', 'pPropName not assigned');
  Assert( pParamName <> '', 'pParamName not assigned');
  Assert( IsPublishedProp( pInstance, pPropName ), pPropName + ' is not a published property on ' + pInstance.ClassName ) ;
  try
    // If it's an object type...
    // else
// ToDo: Better to return a qfkXXX here
    lTypeKind := tiGetSimplePropType( pInstance, pPropName ) ;
    case lTypeKind of
    tiTKString   : begin
                     lString := TypInfo.GetStrProp( pInstance, pPropName ) ;
                     SetValueAsString(pParamName, lString);
                   end ;
    tiTKInteger  : begin
                     lPropType := PropType( pInstance, pPropName ) ;
                     if ( lPropType = tkInt64 ) then
                       lInteger := TypInfo.GetInt64Prop( pInstance, pPropName )
                     else
                       lInteger := TypInfo.GetOrdProp( pInstance, pPropName ) ;
                     SetValueAsInteger(pParamName, lInteger);
                   end ;
    tiTKFloat    : begin
                     lFloat := TypInfo.GetFloatProp( pInstance, pPropName ) ;
                     SetValueAsFloat(pParamName, lFloat);
                   end ;
    tiTKDateTime : begin
                     lDate := TypInfo.GetFloatProp( pInstance, pPropName ) ;
                     SetValueAsDateTime(pParamName, lDate);
                   end;
    tiTKBoolean :  begin
                     lBoolean := Boolean(TypInfo.GetOrdProp( pInstance, pPropName )) ;
                     SetValueAsBoolean(pParamName, lBoolean);
                   end;
    tiTKBinary  :  begin
                     lStream := (TypInfo.GetObjectProp( pInstance, pPropName ) as TStream) ;
                     SetValueAsStream(pParamName, lStream);
                   end ;
    else
      tiFmtException( 'Invalid property type', ClassName, 'SetValueFromProp' ) ;
    end ;
  except
    on e:exception do
      tiFmtException( e,
                      'Error getting property value for <' + pPropName + '> on <' + pInstance.ClassName + '>',
                      pInstance.ClassName, 'SetValueFromProp' ) ;
  end ;
end;

function TtiQueryParams.GetValueAsBoolean(const pName: string): Boolean;
var
  lParam : TtiQueryParamAbs ;
begin
  lParam := FindParamByName(pName) ;
  Assert( lParam.TestValid(TtiQueryParamBoolean), cTIInvalidObjectError );
  result := TtiQueryParamBoolean(lParam).GetValueAsBoolean ;
end;

function TtiQueryParams.GetValueAsDateTime(const pName: string): TDateTime;
var
  lParam : TtiQueryParamAbs ;
begin
  try
    lParam := FindParamByName(pName) ;
    Assert( lParam.TestValid(TtiQueryParamDateTime), cTIInvalidObjectError );
    result := TtiQueryParamDateTime(lParam).GetValueAsDateTime ;
  except
    on e:exception do
    begin
      result := 0 ;
      tiFmtException(e, ClassName, 'GetValueAsDateTime' );
    end ;
  end ;
end;

function TtiQueryParams.GetValueAsFloat(const pName: string): Real;
var
  lParam : TtiQueryParamAbs ;
begin
  lParam := FindParamByName(pName) ;
  Assert( lParam.TestValid(TtiQueryParamFloat), cTIInvalidObjectError );
  result := TtiQueryParamFloat(lParam).GetValueAsFloat ;
end;

function TtiQueryParams.GetValueAsinteger(const pName: string): Int64;
var
  lParam : TtiQueryParamAbs ;
begin
  lParam := FindParamByName(pName) ;
  Assert( lParam.TestValid(TtiQueryParamInteger), cTIInvalidObjectError );
  result := TtiQueryParamInteger(lParam).GetValueAsInteger ;
end;

function TtiQueryParams.GetValueAsStream(const pName: string): TStream;
var
  lParam : TtiQueryParamAbs ;
begin
  lParam := FindParamByName(pName) ;
  Assert( lParam.TestValid(TtiQueryParamStream), cTIInvalidObjectError );
  result := TtiQueryParamStream(lParam).GetValueAsStream ;
end;

procedure TtiQueryParams.SetValueAsStream(const pName: string; const pValue: TStream);
var
  lParam : TtiQueryParamStream ;
begin
  lParam := FindCreateParamByName(pName, TtiQueryParamStream) as TtiQueryParamStream;
  lParam.SetValueAsStream(pValue) ;
end;

procedure TtiQueryParams.AssignValueToStream(const pName: string; const pStream: TStream);
var
  lStream : TStream ;
  lPos : integer ;
begin
  lStream := GetValueAsStream(pName);
  lPos := lStream.Position ;
  pStream.Size := 0 ;
  pStream.CopyFrom( lStream, lStream.Size ) ;
  pStream.Seek(0, soFromBeginning);
  lStream.Seek( lPos, soFromBeginning ) ;
end;

{ EtiDBAlreadyExists }

constructor EtiOPFDBExceptionAlreadyExists.Create( const pPerLayerName, pDatabaseName, pUserName, pPassword : string ; const pMessage: string = '' ) ;
begin
  inherited Create( pPerLayerName, pDatabaseName, pUserName, pPassword, pMessage ) ;
  Message :=
    cTIOPFExcMsgDatabaseAlreadyExists + Cr +
    Message ;
end;

{ EtiDBFailedCreatingDatabase }

constructor EtiOPFDBExceptionCanNotCreateDatabase.Create( const pPerLayerName, pDatabaseName, pUserName, pPassword : string ; const pMessage: string = '' ) ;
begin
  inherited Create( pPerLayerName, pDatabaseName, pUserName, pPassword, pMessage ) ;
  Message :=
    cTIOPFExcMsgCanNotCreateDatabase + Cr +
    Message ;
end;

function TtiQuery.GetSQLText: string;
begin
  result := SQL.Text ;
end;

procedure TtiQuery.SetSQLText(const Value: string);
var
  lsl : TStringList ;
begin
  // This will force any extra code in SetSQL to be executed.
  // Have had problems using SQL.Text as this extra code gets missed.
  lsl := TStringList.Create ;
  try
    lsl.Text := Value ;
    SQL := lsl ;
  finally
    lsl.Free;
  end;
end;

{ TtiQueryParamString }

procedure TtiQueryParamString.AssignToTIQuery(const pQuery: TtiQuery);
begin
  Assert( pQuery.TestValid(TtiQuery), cTIInvalidObjectError );
  pQuery.ParamAsString[Name] := GetValueAsString ;
end;

function TtiQueryParamString.GetKind: TtiQueryFieldKind;
begin
  result := qfkString;
end;

function TtiQueryParamString.GetValueAsString: string;
begin
  result := FValue ;
end;

procedure TtiQueryParamString.SetValueAsString(const pValue: string);
begin
  FValue := pValue ;
  IsNull := false ;
end;

{ TtiQueryParamInteger }

procedure TtiQueryParamInteger.AssignToTIQuery(const pQuery: TtiQuery);
begin
  Assert( pQuery.TestValid(TtiQuery), cTIInvalidObjectError );
  pQuery.ParamAsInteger[Name] := GetValueAsInteger ;
end;

function TtiQueryParamInteger.GetKind: TtiQueryFieldKind;
begin
  result := qfkInteger;
end;

function TtiQueryParamInteger.GetValueAsInteger: Int64;
begin
  result := FValue ;
end;

function TtiQueryParamInteger.GetValueAsString: string;
begin
  result := IntToStr( FValue ) ;
end;

procedure TtiQueryParamInteger.SetValueAsInteger(const pValue: Int64);
begin
  FValue := pValue ;
  IsNull := false ;
end;

procedure TtiQueryParamInteger.SetValueAsString(const pValue: string);
begin
  try
    if pValue <> '' then
      FValue := StrToInt64(pValue)
    else
      FValue := 0;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'SetValueAsString' ) ;
  end;
  IsNull := false ;
end;

procedure TtiQueryParamBoolean.SetValueAsBoolean(const pValue: Boolean);
begin
  FValue := pValue ;
  IsNull := false ;
end;

procedure TtiQueryParamBoolean.SetValueAsString(const pValue: string);
begin
  FValue :=
     SameText( pValue, 'TRUE' ) or
     SameText( pValue, 'T' ) or
     SameText( pValue, '1' ) ;
  IsNull := false ;
end;

{ TtiQueryParamFloat }

procedure TtiQueryParamFloat.AssignToTIQuery(const pQuery: TtiQuery);
begin
  Assert( pQuery.TestValid(TtiQuery), cTIInvalidObjectError );
  pQuery.ParamAsFloat[Name] := GetValueAsFloat ;
end;

function TtiQueryParamFloat.GetKind: TtiQueryFieldKind;
begin
  result := qfkFloat;
end;

function TtiQueryParamFloat.GetValueAsFloat: real;
begin
  result := FValue ;
end;

function TtiQueryParamFloat.GetValueAsString: string;
begin
  result := FloatToStr( GetValueAsFloat ) ;
end;

procedure TtiQueryParamFloat.SetValueAsFloat(const pValue: real);
begin
  FValue := pValue ;
  IsNull := false ;
end;

procedure TtiQueryParamFloat.SetValueAsString(const pValue: string);
begin
  try
    if pValue <> '' then
      FValue := StrToFloat(pValue)
    else
      FValue := 0;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'SetValueAsString' ) ;
  end;
  IsNull := false ;
end;

{ TtiQueryParamDateTime }

procedure TtiQueryParamDateTime.AssignToTIQuery(const pQuery: TtiQuery);
begin
  Assert( pQuery.TestValid(TtiQuery), cTIInvalidObjectError );
  pQuery.ParamAsDateTime[Name] := GetValueAsDateTime ;
end;

function TtiQueryParamDateTime.GetKind: TtiQueryFieldKind;
begin
  result := qfkDateTime;
end;

function TtiQueryParamDateTime.GetValueAsDateTime: TDateTime;
begin
  result := FValue ;
end;

function TtiQueryParamDateTime.GetValueAsString: string;
begin
  try
    result := tiDateTimeAsXMLString(GetValueAsDateTime);
  except
    on e:exception do
      tiFmtException(e, ClassName, 'GetValueAsString' ) ;
  end ;
end;

procedure TtiQueryParamDateTime.SetValueAsDateTime(const pValue: TDateTime);
begin
  FValue := pValue ;
  IsNull := false ;
end;

procedure TtiQueryParamDateTime.SetValueAsString(const pValue: string);
begin
  try
    if pValue <> '' then
      FValue := tiXMLStringToDateTime(pValue)
    else
      FValue := 0 ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'SetValueAsString' ) ;
  end;
  IsNull := false ;
end;

{ TtiQueryParamBoolean }

procedure TtiQueryParamBoolean.AssignToTIQuery(const pQuery: TtiQuery);
begin
  Assert( pQuery.TestValid(TtiQuery), cTIInvalidObjectError );
  pQuery.ParamAsBoolean[Name] := GetValueAsBoolean ;
end;

function TtiQueryParamBoolean.GetKind: TtiQueryFieldKind;
begin
  result := qfkLogical ;
end;

function TtiQueryParamBoolean.GetValueAsBoolean: Boolean;
begin
  result := FValue ;
end;

function TtiQueryParamBoolean.GetValueAsString: string;
begin
{$IFDEF BOOLEAN_CHAR_1}
  if FValue then
    result := 'TRUE'
  else
    result := 'FALSE' ;
{$ELSE}
  if FValue then
    result := 'T'
  else
    result := 'F' ;
{$ENDIF}
end;

{ TtiQueryParamStream }

procedure TtiQueryParamStream.AssignToTIQuery(const pQuery: TtiQuery);
begin
  Assert( pQuery.TestValid(TtiQuery), cTIInvalidObjectError );
  pQuery.AssignParamFromStream(Name, FStream);
end;

constructor TtiQueryParamStream.Create;
begin
  inherited;
  FStream := TMemoryStream.Create ;
end;

destructor TtiQueryParamStream.Destroy;
begin
  FStream.Free ;
  inherited;
end;

function TtiQueryParamStream.GetKind: TtiQueryFieldKind;
begin
  result := qfkBinary ;
end;

function TtiQueryParamStream.GetValueAsStream: TStream;
begin
  result := FStream ;
end;

function TtiQueryParamStream.GetValueAsString: string;
var
  lStream : TStringStream ;
begin
  lStream := TStringStream.Create('') ;
  try
    FStream.Position := 0 ;
    MimeEncodeStream(FStream, lStream);
    result := lStream.DataString ;
  finally
    lStream.Free;
  end;
end;

procedure TtiQueryParamStream.SetValueAsStream(const pValue: TStream);
var
  lPos : integer ;
begin
  lPos := pValue.Position ;
  FStream.Size := 0 ;
  pValue.Seek(0, soFromBeginning);
  FStream.CopyFrom( pValue, pValue.Size ) ;
  FStream.Seek(0, soFromBeginning);
  pValue.Seek( lPos, soFromBeginning ) ;
end;

procedure TtiQueryParamStream.SetValueAsString(const pValue: string);
var
  lStream : TStringStream ;
begin
  lStream := TStringStream.Create(pValue) ;
  try
    FStream.Size := 0 ;
    MimeDecodeStream(lStream, FStream);
    FStream.Position := 0 ;
  finally
    lStream.Free;
  end;
end;

{ TtiQueryNonSQL }

constructor TtiQueryNonSQL.Create;
begin
  inherited;
  FParams := TtiQueryParams.Create ;
end;

destructor TtiQueryNonSQL.Destroy;
begin
  FParams.Free;
  inherited;
end;

procedure TtiQueryNonSQL.AssignFieldAsStream(const pName: string; const pValue: TStream);
var
  ls : string ;
  lStream : TStringStream ;
begin
  ls := GetFieldAsString(pName);
  lStream := TStringStream.Create(ls) ;
  try
    pValue.Size := 0 ;
    MimeDecodeStream(lStream, pValue);
    pValue.Position := 0 ;
  finally
    lStream.Free;
  end;
end;

procedure TtiQueryNonSQL.AssignParamFromStream(const pName: string; const pValue: TStream);
begin
  FParams.SetValueAsStream(pName, pValue);
end;

procedure TtiQueryNonSQL.AssignParamToStream(const pName: string; const pValue: TStream);
begin
  FParams.AssignValueToStream(pName, pValue);
end;

// -----------------------------------------------------------------------------
function TtiQueryNonSQL.GetParamAsBoolean(const psName: string): boolean;
begin
  result := Params.GetValueAsBoolean(psName);
end;

// -----------------------------------------------------------------------------
function TtiQueryNonSQL.GetParamAsDateTime(const psName: string): TDateTime;
begin
  try
    result := Params.GetValueAsDateTime(psName);
  except
    on e:exception do
    begin
      result := 0 ;
      tiFmtException( e, ClassName, 'GetParamAsDateTime' ) ;
    end;
  end ;
end;

// -----------------------------------------------------------------------------
function TtiQueryNonSQL.GetParamAsFloat(const psName: string): real;
begin
  result := Params.GetValueAsFloat(psName);
end;

// -----------------------------------------------------------------------------
function TtiQueryNonSQL.GetParamAsInteger(const psName: string): Int64;
begin
  result := Params.GetValueAsInteger(psName);
end;

// -----------------------------------------------------------------------------
function TtiQueryNonSQL.GetParamAsTextBLOB(const psName: string): string;
begin
  result := Params.GetValueAsString(psName);
end;

// -----------------------------------------------------------------------------
function TtiQueryNonSQL.ParamCount: integer;
begin
  result := Params.Count ;
end;

// -----------------------------------------------------------------------------
function TtiQueryNonSQL.ParamName(pIndex: integer): string;
begin
  result := Params.ParamName(pIndex) ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryNonSQL.SetParamAsBoolean(const psName: string; const Value: boolean);
begin
  Params.SetValueAsBoolean(psName, Value);
end;

// -----------------------------------------------------------------------------
procedure TtiQueryNonSQL.SetParamAsDateTime(const psName : string ; const Value: TDateTime);
begin
  try
    Params.SetValueAsDateTime(psName, Value);
  except
    on e:exception do
      tiFmtException( e, ClassName, 'SetParamAsDateTime' ) ;
  end ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryNonSQL.SetParamAsFloat(const psName: string; const Value: real);
begin
  Params.SetValueAsFloat(psName, Value);
end;

// -----------------------------------------------------------------------------
procedure TtiQueryNonSQL.SetParamAsInteger(const psName: string; const Value: Int64);
begin
  Params.SetValueAsInteger(psName, Value);
end;

// -----------------------------------------------------------------------------
procedure TtiQueryNonSQL.SetParamAsTextBLOB(const psName, Value: string);
begin
  Params.SetValueAsString(psName, Value);
end;

// -----------------------------------------------------------------------------
procedure TtiQueryNonSQL.SetParamAsMacro(const psName, Value: string);
begin
  Assert( false, 'Not implemented in ' + ClassName ) ;
end;

// -----------------------------------------------------------------------------
function TtiQueryNonSQL.GetParamIsNull(const psName: String): Boolean;
begin
  Result := Params.ParamIsNull[psName];
end;

// -----------------------------------------------------------------------------
procedure TtiQueryNonSQL.SetParamIsNull(const psName: String; const Value: Boolean);
begin
  Params.ParamIsNull[ psName ] := Value ;
end;

//------------------------------------------------------------------------------
function TtiQueryNonSQL.ParamsAsString: string;
var
  i : integer ;
begin
  result := '' ;
  for i := 0 to ParamCount - 1 do
  begin
    result := tiAddTrailingValue( result, CrLf ) ;
    result := result +
              ParamName( i ) + ' := ' +
              ParamAsString[ ParamName( i )] ;
  end ;
end;

{ EtiDBException }

constructor EtiOPFDBException.Create( const pPerLayerName, pDatabaseName, pUserName, pPassword : string ; const pMessage: string = '' ) ;
begin
  Message :=
    'Database name:       ' + pDatabaseName + Cr +
    'User name:           ' + pUserName     + Cr +
    'Password:            ' + tiReplicate( 'X', Length( pPassword )) + Cr +
    'Persistence layer:   ' + pPerLayerName ;
  if pMessage <> '' then
    Message := Message + Cr(2) +
      'Message:' + Cr+ pMessage ;
end;

{ EtiDBCanNotConnectToDBException }

constructor EtiOPFDBExceptionCanNotConnect.Create( const pPerLayerName, pDatabaseName, pUserName, pPassword : string ; const pMessage: string = '' ) ;
begin
  inherited Create( pPerLayerName, pDatabaseName, pUserName, pPassword, pMessage ) ;
  Message :=
    cTIOPFExcMsgCanNotConnectToDatabase + Cr(2) +
    Message ;
end;

{ EtiDBCanNotFindException }

constructor EtiOPDDBCanNotFindException.Create( const pPerLayerName, pDatabaseName, pUserName, pPassword : string ; const pMessage: string = '' ) ;
begin
  inherited Create( pPerLayerName, pDatabaseName, pUserName, pPassword, pMessage ) ;
  Message :=
    cTIOPFExcMsgCanNotFindDatabase + Cr +
    Message ;
end;

{ EtiDBUserNamePasswordException }

constructor EtiOPFDBExceptionUserNamePassword.Create( const pPerLayerName, pDatabaseName, pUserName, pPassword : string ; const pMessage: string = '' ) ;
begin
  inherited Create( pPerLayerName, pDatabaseName, pUserName, pPassword, pMessage ) ;
  Message :=
    cTIOPFExcMsgInvalidUserNamePassword + Cr +
    Message ;
end;

function TtiDatabase.CreateTIQuery: TtiQuery;
begin
  result := gTIPerMgr.RegPerLayers.CreateTIQuery(TtiDatabaseClass(ClassType)) ;
end;

class function TtiDatabase.TestConnectTo(const psDatabaseName, psUserName,
                                         psPassword, pParams: string): boolean;
var
  lDatabase : TtiDatabase ;
begin
  result := false ;
  lDatabase := Create ;
  try
    try
      // ToDo: Pass this params value
      lDatabase.Connect(psDatabaseName, psUserName, psPassword, pParams);
      result := true ;
    except
      on e:EtiOPFDBExceptionWrongServerVersion do
        raise ;
      on e:exception do
        result := false ;
    end ;
    lDatabase.Connected := false ;
  finally
    lDatabase.Free;
  end ;
end;

destructor TtiDatabase.destroy;
begin
  FParams.Free;
  inherited;
end;

procedure TtiQueryNonSQL.AssignFieldAsStreamByIndex(pIndex: Integer; const pValue: TStream);
var
  ls : string ;
  lStream : TStringStream ;
begin
  ls := GetFieldAsStringByIndex(pIndex);
  lStream := TStringStream.Create(ls) ;
  try
    pValue.Size := 0 ;
    MimeDecodeStream(lStream, pValue);
    pValue.Position := 0 ;
  finally
    lStream.Free;
  end;
end;

end.



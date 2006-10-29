unit tiUtils;

{$I tiDefines.inc}

interface

uses
   tiBaseObject
  ,tiVisitor 
  ,tiObject
  ,SysUtils
  ,Classes
  {$IFDEF MSWINDOWS}
  ,Windows
  ,shellAPI
  ,Messages
  {$ENDIF MSWINDOWS}
  ,Graphics
  ,Controls
  ,Forms
  ,Dialogs
  ,StdCtrls
  ,Buttons
  ,ExtCtrls
  ,Math
  ,TypInfo
  {$IFDEF FPC}
  ,LCLType
  {$ENDIF}
  {$IFDEF DELPHI6ORABOVE}
  ,Variants
  {$ENDIF}
  ,Contnrs
  ,tiConstants
 ;


{$IFDEF VER130}
type
  TVarType = Word;
{$ENDIF}

  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // * String manipulation
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  { ToDo 5 -cUtils: Convert params to const }
  // Scan the string AValue, and replace any characters ADel with AIns (Case sensitive)
  function  tiStrTran(        AValue, ADel, AIns : string): string;
  function  tiStrTran1(AValue, ADel, AIns : string): string;
  // Scan the string AValue, and replace any characters ADel with AIns (Case insensitive)
  function  tiCIStrTran(      AValue, ADel, AIns : string): string;
  // Count the number of blocks of text in AValue seperated by AToken
  function  tiNumToken(       const AValue, AToken : string): integer;
  // Extract the APos(th) block of text in AValue, seperated by AToken
  function  tiToken(          const AValue, AToken : string; const APos : integer): string;
  // Return a string of spaces ALen long
  function  tiSpace(          ALen : integer): string;
  // Pad AValue with spaces on the RHS to make it ALen long
  function  tiPadR(const AValue : string; ALen : integer): string;
  // Pad AValue with spaces on the LHS to make it ALen long
  function  tiPadL(           AValue : string; ALen : integer): string;
  // Pad AValue with spaces on both sides to make it ALen long
  function  tiPadC(           AValue : string; ALen : integer): string;
  // Pad AValue with zeros on the LHS to make it ALen long
  function  tiPad0(           AValue : string; ALen : integer): string;
  // Remove any leading zeros from a string
  function  tiRemoveLeading0( AValue : string): string;
  // Convert a string into mized case using a simple algorithm
  // Note: There is a more complete version of this (that takes care of works like
  //       McCrae or King Charles IV
  //       It will also work with characters other than #32 to sep words
  function  tiMixedCase(      AValue : string): string;
  // Replicate the string AValue to return a string pIntLength long
  function  tiReplicate(       const AValue : string; ARepCount : Word): string;
  // If ALine is not '', add the trailing value AValue
  function  tiAddTrailingValue(const ALine, AValue : string; ADuplicates : boolean = true): string;
  // If the last character of ALine is AValue, then remove it
  function  tiRemoveTrailingValue(ALine, AValue : string): string;
  // If AValue is not '', add a trailing ', '
  function  tiAddTrailingComma(AValue : string): string;
  // If AValue is not '', add a trailing ' and '
  function  tiAddTrailingAnd( AValue : string): string;
  // If AValue is not '', add a trailing ' or '
  function  tiAddTrailingOr(  AValue : string): string;
  // If AStr is not '', add a trailing ' '
  function  tiAddTrailingSpace(AValue : string): string;
  // Return the first position of AValue in ATarget from the right.
  // Same as Delphi's Pos, except from the right, not left
  function  tiPosR(           ATarget, AValue : string): integer;
  // Does the wildcard pPattern occur in the string ASource ?
  function tiWildcardMatch(const ASource, APattern: String; const ACaseSensitive: boolean = false): Boolean;
  // Extract a sub string within defined delimiters
  function  tiSubStr(const ASource, AStartDelim, AEndDelim : string; AIndex : integer = 1): string;
  // Trunc a string to AWidth length and add '...'
  function  tiAddEllipsis(const AString : string; AWidth : integer = 20): string;
  // Trim all characters after and including ATrim from AString
  function  tiTrimR(const AString, ATrim : string; ACaseSensitive : boolean = false): string;
  // Trim all characters before and including ATrim from AString
  function  tiTrimL(const AString, ATrim : string; ACaseSensitive : boolean = false): string;
  // Remove Cr & LF characters
  function  tiRemoveCrLf(const AString : string): string;
  // Remove all the trailing white space characters (#32, #10, #13)
  // from the end of a string
  function tiTrimTrailingWhiteSpace(const AString : string): string;
  // Returns true if the email address is valid
  function tiIsEMailAddressValid(const AValue: string): boolean;
  // Returns true if the file name is valid. Will not test if the
  // directory part of the file name exists, or is valid, will just
  // test the file name part
  function tiIsFileNameValid(const AFileName : string): boolean;
  {: Replacement for Delphi's StrPos, but much faster. Not for FPC though!}
  function tiStrPos(const AString, ASubString: PChar): PChar;
  {: Normalize the string by replacing all repeated Spaces, Tabs and NewLines
    chars with a single space. It's so easy with Regular Expression!:(}
  function tiNormalizeStr(const AString: string): string;

  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // * Directory, file and file name manipulation
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // Get a tempory file name with the extension pStrExt
  function  tiGetTempFile(       const AValue : string): string;
  // Add a trailing slash ('\' or '/' based on platform) if there is not already one
  function  tiAddTrailingSlash(  const AValue : string): string;
  // Remove a trailing slash ('\' or '/' based on platform) if there is one
  function  tiRemoveTrailingSlash(const AValue : string): string;
  // Remove a leading slash ('\' or '/' based on platform) if there is one
  function  tiRemoveLeadingSlash(const AValue : string): string;
  // Get the systems tempory directory
  function  tiGetTempDir  : string;
{$IFDEF MSWINDOWS}
  // Get the windows system directory
  function  tiGetWindowsSysDir : string;
{$ENDIF MSWINDOWS}
  {: Read a file's date with correction for the file's locale}
  function  tiReadFileDate(const AFileName : string): TDateTime;
  {: Read a file's size}
  function  tiReadFileSize(const AFileName : string): DWord;
  {: Read a file's date and size}
  procedure tiReadFileDateSize(const AFileName : string;
                                var   ADateTime : TDateTime;
                                var   AFileSize : integer);
  // Set a files date and time
  procedure tiSetFileDate(const AFileName : string; const ADateTime : TDateTime);
  // What is the path of the current EXE?
  function  tiGetEXEPath : string;
  // Add the EXE path to a file name
  function  tiAddEXEPath(const AFileName: string): string;
  // Extract the file name part of a file name (ie, remove the path and extension)
  function  tiExtractFileNameOnly(     AValue : string): string;
  // Remove the extension from a filename. Similar to Delphi's ChangeFileExt() except the '.' is managed.
  function  tiRemoveExtension(         AValue : string): string;
  // Change a file's extenstion. Similar to Delphi's ChangeFileExt() except the '.' is managed.
  function  tiSwapExt(                 const AFileName, AExt : string): string;
  // Extract a file's extension. Similar to Delphi's ExtractFileExt except the '.' is not extracted.
  function  tiExtractExtension(        AValue : string): string;
  // Copy a file from pStrFileFrom to pStrFileTo
  procedure tiCopyFile(          const AFrom, ATo : string);
  // Move a file from pStrFromFileName to pStrToFileName
  procedure tiMoveFile(                AFrom, ATo: string);
  // Read a file's size in bytes
  function  tiGetFileSize(             AValue : string): longInt;
  // Remove the drive letter from a file name
  function  tiRemoveDrive(             AValue : string): string;
  // Removed part of a directory
  function  tiRemoveDirectory(         const AFileName: string; const AToRemove: string): string;
  // Set a file's readonly attribute
  procedure tiSetFileReadOnly(         AFileName : string; AReadOnly : boolean);
  // Is a file's readonly attribute set?
  function  tiIsFileReadOnly(AValue : string): boolean;
  // Copy all the directories, starting at pStrStartDir to the stringList slTree
  procedure tiDirectoryTreeToStringList(const AStartDir : string; const ADirList : TStringList; ARecurse : boolean);
  // Copy all the files, from the directory pStrStartDir, matching the wildcard pStrWildCard
  // to the stringList slResult
  procedure tiFilesToStringList(       const AStartDir,
                                         AWildCard : string;
                                         AResults : TStringList;
                                         const ARecurse : boolean);
  // Copy one directory tree to another
  procedure tiXCopy(const ASource, ATarget: string);

  {: Delete all the files that match AWildCard found in ADirectory}
  procedure tiDeleteFiles(const ADirectory, AWildCard: string);

  {: Delete a file, but without BDS2006 function inlining warning}
  function tiDeleteFile(const AFileName: string): boolean;

  // Does a directory have any subdirectories?
  function  tiHasSubDirectory(AStartDir : string): boolean;
  // Write the string in psText to a file named AFileName
  procedure tiStringToFile(const AText, AFileName : string);
  // Read a text file into a string
  function  tiFileToString(const AFileName : TFileName): string;
  // Get the current directory
  // Extract a directory name to a certain level.
  // eg tiExtractDirToLevel('c:\temp\dir', 0) gives 'c:'
  //    tiExtractDirToLevel('c:\temp\dir', 1) gives 'c:\temp'
  //    tiExtractDirToLevel('c:\temp\dir', 2) gives 'c:\temp\dir'
  function tiExtractDirToLevel(const AFileName : TFileName; ALevel : byte): TFileName;
  // Same as Delphi's ForceDirectory, but will raise an exception if create fails
  procedure tiForceDirectories(const AValue : TFileName);
  // Remove a directory, and all it's owned directories and files
  function tiForceRemoveDir(const AValue : TFileName): boolean;
  // Join two path elements. Elements can have trailing delimiters or not. Result will not have trailing delimiter.
  function tiJoinPath(const ALeftSide, ARightSide: string): string; overload;
  // Join multiple path elements. Elements can have trailing delimiters or not. Result will not have trailing delimiter.
  function tiJoinPath(const AElements: array of string): string; overload;
  // Fixes the path separator for the *Unix platform. See implementation for more details.
  function tiFixPathDelim(const AText: string): string;


  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // * Number manipulation
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  Type TtiDecimalRoundingCtrl =    {The defined rounding methods}
     (drNone,    {No rounding.}
      drHalfEven,{Round to nearest or to even whole number. (a.k.a Bankers) }
      drHalfPos, {Round to nearest or toward positive.}
      drHalfNeg, {Round to nearest or toward negative.}
      drHalfDown,{Round to nearest or toward zero.}
      drHalfUp,  {Round to nearest or away from zero.}
      drRndNeg,  {Round toward negative.                    (a.k.a. Floor) }
      drRndPos,  {Round toward positive.                    (a.k.a. Ceil) }
      drRndDown, {Round toward zero.                        (a.k.a. Trunc) }
      drRndUp);  {Round away from zero.}

  // Set the precision of AValue to pPrecision decimal places
  function  tiSetPrecision(const AValue : Extended; const APrecision : integer = 3): Extended;
  { The following functions have a two times "epsilon" error built in for the
    single, double, and extended argument respectively: }
  Function tiDecimalRoundSgl(AValue: single;   ANDFD: Integer; ACtrl: TtiDecimalRoundingCtrl = drHalfUp): Extended;
  Function tiDecimalRoundDbl(AValue: double;   ANDFD: integer; ACtrl: TtiDecimalRoundingCtrl = drHalfUp): Extended;
  Function tiDecimalRoundExt(AValue: extended; ANDFD: integer; ACtrl: TtiDecimalRoundingCtrl = drHalfUp): Extended;

  // Calculate pNum div ADenom, and return 0 if an error
  function  tiSafeDiv(ANum, ADenom: longInt): longInt; overload;
  // Calcuate pNum / ADenom and return 0 if an error
  function  tiSafeDiv(ANum, ADenom: Extended): Extended; overload;
  // Round a float to an integer
  function  tiRound(const AValue : Extended): Int64;
  // Convert an interger (word) to a base26 string (A..Z) characters only
  function  tiEncodeWordBase26(const AValue : Word): String;
  // Convert a base26 string (A..Z characters only) to an integer (word)
  function  tiDecodeWordBase26(AValue : String): Word;


  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // * TDateTime manipulation
  // * Also see the VCL unit DateUtils
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // What is the date of the previous week day?
  function tiDateToPreviousWeekDayDate(AValue : TDateTime): TDateTime;
  // Get the current year as an integer
  function tiYear(AValue : TDateTime = 0.0): Word;
  // Is a date within a given range?
  function tiDateWithinRange(const ADate, AFrom, ATo: TDateTime): Boolean;
  {: Convert a year to the date of the beginning of the financial year.
     2005 will return 01/07/2004}
  function tiYearToStartAusFinancialYear(AFinancialYear: Word): TDateTime;
  {: Convert a year to the date of the end of the financial year
     2005 will return 30/06/2005}
  function tiYearToEndAusFinancialYear(AFinancialYear: Word): TDateTime;
  {: Converts a year to a financial year string
     tiYearToEndAusFinancialYear(2005) will return 2004 - 2005}
  function tiAusFinancialYearToString(AFinancialYear: Word): string;
  {: Converts a date to its financial year
     tiDateToAusFinancialYear(EncodeDate(2005/03/01)) will return 2005}
  function tiDateToAusFinancialYear(ADate: TDateTime): Word;

  function tiDateTimeAsXMLString(const ADateTime: TDateTime): string;
  function tiXMLStringToDateTime(const AValue : string): TDateTime;
  
  function tiDateTimeAsIntlDateStor(const ADateTime: TDateTime): string;
  function tiDateTimeAsIntlDateDisp(const ADateTime: TDateTime): string;
  function tiIntlDateStorAsDateTime(const AValue: string): TDateTime;
  function tiIntlDateDispAsDateTime(const AValue: string): TDateTime;


  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // * Type conversions
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // Convert a string to an integer using StrToInt, default to 0 if an exception is raised
  function  tiStrToInt(         const AValue : string)   : integer;
  // Convert a string to a float using StrToFloat, default to 0 if an exception is raised
  function  tiStrToFloat(       const AValue : string)   : Extended;
  // Convert a date to a string using FormatDateTime with the standard date format
  function  tiDateToStr(        const pDTValue  : TDateTime; const psFormat : string = csWinDateFormat): string;
  // Convert a dateTime to a string using FormatDateTime with the standard dateTime format
  function  tiDateTimeToStr(    const ADateTime  : TDateTime): string;
  // Convert a time to a string using FormatDateTime with the standard time format
  function  tiTimeToStr(        const ADateTime  : TDateTime;
                                  const ATimeFormat : string = ''): string;
  // Convert an integer to a string and return '' if 0
  function  tiIntToStrHide0(    const AValue : longInt)  : string;
  // Converts an integer to a string with commas between thousands
  function  tiIntToCommaStr(    const AValue   : integer): string;
  // Convert a float to a currency string and return '' if 0
  function  tiFloatToCurrencyHide0(const AValue : Extended)     : string;
  // Convert a float to a currency string
  function  tiFloatToCurrency(    const AValue : Extended)     : string;

  // ToDo: Tidy these up. String may be T/F, Y/N, True/False, TRUE/FALSE, 1/0, etc.
  // Convert a boolean to 'True' or 'False'
  function  tiBooleanToStr(     const AValue : boolean)  : string;
  // Convert a boolean to 'True' or 'False'
  function  tiBoolToStr(        const AValue : boolean)  : string;
  // Convert a string to a boolean
  function  tiStrToBool(const AValue : string): boolean;

  // Convert a float to a string in the format 9999.99
  function  tiFloatToStr(       const AValue : Extended;
                                  const APrecision : integer = 3): string;
  // Convert a float to a string in the format 9,999.99
  function  tiFloatToCommaStr(  const AValue : Extended;
                                  const APrecision : integer = 3): string;
  // Convert a float to a string in the format passed by AFormat
  function  _tiFloatToStr(      const AValue : Extended;
                                  const APrecision : integer;
                                  const AFormat : string): string;

  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // *  Win32 API wrappers
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
{$IFDEF MSWINDOWS}
  // Call Windows ShellExecute with exception handling
  function _tiShellExecute(AHwnd : integer;
                            AOpperation, AFile, AParameters, ADirectory : PChar;
                            AShowCmd : integer): integer;
  // Simple pascal call to ShellExecute
  function tiShellExecute(const AEXE : string;
                           const AParameters : string = '';
                           const AWinState : integer = SW_SHOWNORMAL): integer;
{$ENDIF MSWINDOWS}
  // Run an EXE and wait for it to finish
  procedure tiRunEXEAndWait(AEXE : string);

  // Edit a file using the standard editor for this file type
  function  tiEditFile(const AFileName : string): integer;
  // Get the currently logged on user ID
  function  tiGetUserName : string;
  // Get the computer name
  function  tiGetComputerName : string;

  // Bit manipulation
  // Is a particular bit set?
  function tiIsBitSet(const AValue: longint; const ABit: byte): boolean;
  // Convert a particular bit to a string: '1' or '0'
  function tiBitToString(const AValue: longint; const ABit: byte): string;
  // Convert a Int32 to a string of 0s and 1s
  function tiInt32ToBinString(const AValue : longInt): string;

  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // * Mouse cursor routines
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  function tiAutoWaitCursor: IUnknown;
  function tiAutoCursor(ACursor: TCursor = crHourglass): IUnknown;

  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // * Graphics routines
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  {: This class prevents control flicker using a brute-force method.
     The passed <i>AControlToMask</i> is hidden behind a topmost screen snapshot of the control.
     The snapshot is removed when the instance is freed. }
{$IFDEF MSWINDOWS}
type
  TtiBruteForceNoFlicker = class(TCustomControl)
  private
    FMaskControl: TControl;
    FControlSnapshot: TBitmap;

    procedure ScreenShot(ABitmap: TBitmap; ALeft, ATop, AWidth, AHeight: Integer; AWindow: HWND);
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure Paint; override;
    procedure CreateParams(var AParams: TCreateParams); override;
  public
    constructor Create(AControlToMask: TControl); reintroduce;
    destructor Destroy; override;
  end;
{$ENDIF MSWINDOWS}

  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // * Other routines
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

  {: Compare two float values for equality, using a given number of significant digits.
     Ref: http://www.adtmag.com/joop/crarticle.asp?ID=396
     @param(FEPS Determines the significant digits. @br
                 FEPS value of 0.00001 means 5 significant digits. @br
                 FEPS value of 0.01 means 2 significant digits.) }
  function tiIsNearEnough(N, D: Double;  FEPS: Double = 0.00001): Boolean;
  {: If ACondition is true, return AResultTrue, otherwise, return AResultFalse.
     @param ACondition Result of a boolean evaluation.
     @param AResultTrue AValue to return if ACondition is true.
     @param AResultFalse AValue to return if ACondition is false.}
  function tiIf(ACondition: Boolean; AResultTrue, AResultFalse: Extended): Extended; overload;
  {: If ACondition is true, return AResultTrue, otherwise, return AResultFalse.
     @param ACondition Result of a boolean evaluation.
     @param AResultTrue AValue to return if ACondition is true.
     @param AResultFalse AValue to return if ACondition is false.}
  function tiIf(ACondition: Boolean; AResultTrue, AResultFalse: Integer): Integer; overload;
  {: If ACondition is true, return AResultTrue, otherwise, return AResultFalse.
     @param ACondition Result of a boolean evaluation.
     @param AResultTrue AValue to return if ACondition is true.
     @param AResultFalse AValue to return if ACondition is false.}
  function tiIf(ACondition: Boolean; AResultTrue, AResultFalse: String): String; overload;

  // Convert a variant array of variants to a string
  function  tiVariantArrayToString(AValue : oleVariant): string;
  // Is a variant of a given type
  function  tiIsVariantOfType(AVariant : Variant; AVarType : TVarType): boolean;
  // Return a string with ACount #10 characters
  function  Lf(const ACount : Byte = 1): string;
  // Return a string with ACount #13 characters
  function  Cr(const ACount : Byte = 1): string;
  // Return a string with ACount #13+#10 characters
  function  CrLf(const ACount : Byte = 1): string;
  // Returns a string with ACount #9 characters
  function  Tab(const ACount : Byte = 1): string;
  // Returns the checksum of a string of numbers
  function tiCheckSum(const AValue: string): Integer;
  {: Write a string into a stream overwriting all contents}
  procedure tiStringToStream(const AStr : string; const AStream : TStream);
  {: Append a string to a stream}
  procedure tiAppendStringToStream(const AStr : string; const AStream : TStream);
  {: Insert a string to a stream}
  procedure tiInsertStringToStream(const AStr : string; const AStream : TStream; APos: Longword);
  {: Read the entire contents of a stream as a string}
  function  tiStreamToString(const AStream : TStream): string; overload;
  {: Read part of the contents of a stream as a string}
  function  tiStreamToString(const AStream : TStream; AStart, AEnd: Longword): string; overload;
  // Write a string into a stream
  procedure tiFileToStream(const AFileName : string; const AStream : TStream);
  // Read the contents of a stream as a string
  function  tiStreamToFile(const AFileName : string; const AStream : TStream): string;
  // Copy one stream to another
  procedure tiCopyStream(const AStreamFrom, AStreamTo : TStream);

  {:Copy a TtiObjectList of TtiObject(s) data to a TStream using CSV format}
  procedure tiListToStream(AStream: TStream;
                           AList: TtiObjectList;
                           AFieldDelim: string;
                           ARowDelim: string;
                           AColsSelected: TStringList); overload;

  procedure tiListToStream(AStream : TStream;
                           AList : TtiObjectList); overload;

  // Copy a TList of TtiBaseObject's to a CSV file (Prompt the user for the file name)
  procedure tiListToCSV(AList: TtiObjectList;
                         const AFileName: string;
                         AColsSelected: TStringList); overload;

  procedure tiListToCSV(AList: TtiObjectList;
                         const AFileName: string); overload;

  // Copy a TList of TtiBaseObject's to the clipboard
  procedure tiListToClipboard(AList: TtiObjectList;
                              AColsSelected: TStringList); overload;

  procedure tiListToClipboard(AList: TtiObjectList); overload;

  // Cloned from IdSoapTestingUtils.pas (IndySoap) by Grahame Grieve & Andrew Cumming
  function tiTestStreamsIdentical(AStream1, AStream2 : TStream; Var VMessage : string):boolean; overload;
  function tiTestStreamsIdentical(AStream1, AStream2 : TStream):boolean; overload;

  function tiHasRTTI(AObject : TObject): boolean; overload;
  function tiHasRTTI(AClass : TClass): boolean; overload;

  {: Writes "Press <Enter> to continue and waits for CrLf in a console application.}
  procedure tiConsoleAppPause;
  
  {: Platform neutral function to return a GUID string. }
  function  tiCreateGUIDString: string;
  {: Platform neutral function to return the systems TickCount. }
  function  tiGetTickCount: Cardinal;

type

  TtiIntegerListItem = class(TtiBaseObject)
  private
    FValue: Int64;
  public
    property AValue: Int64 Read FValue Write FValue;
  end;

  TtiIntegerList = class(TtiBaseObject)
  private
    FList: TObjectList;
    function    GetItems(i: Integer): Int64;
    procedure   SetItems(i: Integer; const AValue: Int64);
    function    GetCount: Integer;
  public
    constructor Create; virtual;
    destructor  Destroy; override;
    procedure   Add(AValue: Int64);
    function    IndexOf(AValue: Int64): Integer;
    function    Remove(AValue: Int64): Integer;
    property    Items[i: Integer]: Int64 Read GetItems Write SetItems;
    property    Count: Integer Read GetCount;
  end;

//  // These functions where added in Delphi 6, and found their way into the framework
//  // Added here for D5 support
//  {$ifndef Delphi6OrAbove}
//     function IsNan(const AValue: Extended): Boolean;
//     function SameValue(const A, B: Extended; Epsilon: Extended): Boolean;
//     function CompareValue(const A, B: Int64): Integer; overload;
//     function StrToFloatDef(const S: string; const Default: Extended): Extended;
//  {$endif}

implementation
uses
   tiRegINI
  ,tiExcept
  ,ClipBrd
  {$IFDEF MSWINDOWS}
    {$IFDEF DELPHI5}
    ,FileCtrl
    {$ENDIF}
  ,tiWin32
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  ,unix
  ,baseunix
  ,tiLinux
  {$ENDIF UNIX}
  {$IFDEF FPC}
  ,Process
  ,StrUtils   // used for DelSpace1
  {$ENDIF}
 ;

//{$ifndef Delphi6OrAbove}
//const
//  FuzzFactor = 1000;
//  ExtendedResolution = 1E-19 * FuzzFactor;
//
//function IsNan(const AValue: Extended): Boolean;
//type
//  TExtented = packed record
//    Mantissa: Int64;
//    Exponent: Word;
//  end;
//  PExtended = ^TExtented;
//begin
//  Result := ((PExtended(@AValue)^.Exponent and $7FFF)  = $7FFF) and
//            ((PExtended(@AValue)^.Mantissa and $7FFFFFFFFFFFFFFF) <> 0);
//end;
//
//function SameValue(const A, B: Extended; Epsilon: Extended): Boolean;
//begin
//  if Epsilon = 0 then
//    Epsilon := Max(Min(Abs(A), Abs(B)) * ExtendedResolution, ExtendedResolution);
//  if A > B then
//    Result := (A - B) <= Epsilon
//  else
//    Result := (B - A) <= Epsilon;
//end;
//
//function CompareValue(const A, B: Int64): Integer;
//begin
//  if A = B then
//    Result := 0
//  else if A < B then
//    Result := -1
//  else
//    Result := 1;
//end;
//
//function StrToFloatDef(const S: string; const Default: Extended): Extended;
//begin
//  if not TextToFloat(PChar(S), Result, fvExtended) then
//    Result := Default;
//end;
//
//{$endif}

type
  TAutoCursor = class(TInterfacedObject)
  private
  public
    constructor Create(ANewCursor: TCursor);
    destructor Destroy; override;
  end;


var
  uCursorStack: TList;


function tiGetTempFile(const AValue : string): string;
{$IFNDEF FPC}
const
  cMaxPathLen = 255;
var
  pcTemp : array[0..cMaxPathLen] of char;
  pcApp : array[0..cMaxPathLen] of char;
  pcPath : array[0..cMaxPathLen] of char;
{$ENDIF FPC}
begin
  {$IFDEF FPC}
  Result := SysUtils.GetTempFileName('', '');    // prefix of TMP is default
  {$ELSE}
  strPCopy(pcApp, copy(extractFileName(application.exeName), 1, 3));
  getTempPath(cMaxPathLen, pcPath);
  getTempFileName(pcPath, pcApp, 0, pcTemp);
  deleteFile(pcTemp); // This is using the Window deleteFile, not Delphi's
  result := strPas(pcTemp);
  {$ENDIF}
  if pos('.', result) <> 0 then
  begin
    if AValue = '' then
      result := tiRemoveExtension(result)
    else
      result := copy(result, 1, pos('.', result)) + AValue;
  end;
end;


function tiGetTempDir : string;
begin
  {$IFDEF FPC}
  result := GetTempDir;
  {$ELSE}
  result := ExtractFilePath(tiGetTempFile('tmp'));
  {$ENDIF}
end;


{$IFDEF MSWINDOWS}
function tiGetWindowsSysDir : string;
const
  cMaxPathLen = 255;
var
  pcDir : array[0..cMaxPathLen] of char;
begin
  GetSystemDirectory(pcDir, cMaxPathLen);
  result := String(pcDir);
end;
{$ENDIF MSWINDOWS}


// Seaches <sStr> and replaces <sDel> with <sIns>
// Case sensitive.
function tiStrTran(AValue, ADel, AIns : string): string;
var
  i : integer;
  sToChange : string;
begin
  result := '';
  sToChange := AValue;
  i := pos(ADel, sToChange);
  while i <> 0 do begin
    result := result + copy(sToChange, 1, i-1) + AIns;
    delete(sToChange, 1, i+length(ADel)-1);
    i := pos(ADel, sToChange);
  end;
  result := result + sToChange;
end;


function tiStrTran1(AValue, ADel, AIns : string): string;
var
  // order of declaration is significant here - earlier declared local vars
  // get assigned to registers rather than pushed to stack
  LMatch, LValue, LResult: PChar;
  LLen: cardinal;
  LIns, LDel: PChar;
begin
  LLen := Length(ADel);
  // max possible length = every char in LValue requires substitution
  SetLength(result, Length(AIns) * Length(AValue));
  LValue := Pointer(AValue);

  // empty src begets empty result...
  if LValue = nil then
    exit;

  LResult := Pointer(result);
  LIns := Pointer(AIns);
  LDel := Pointer(ADel);

  // Advance through LValue looking for LDel instances...
  LMatch := StrPos(LValue, LDel);

  // terminate on no match
  while LMatch <> nil do
  begin

    // Copy all chars from current LValue into LResult up to match point,
    // moving both pointers along as well
    while LValue < LMatch do
    begin
      LResult^:= LValue^;
      Inc(LResult);
      Inc(LValue);
    end;

    // Add in LIns and move LResult beyond it
    LResult := StrECopy(LResult, LIns);

    // move LValue beyond instance of  LDel
    Inc(LValue, LLen);
    LMatch := StrPos(LValue, LDel);
  end;

  // no more matches  - copy remainder of LValue to LResult
  StrCopy(LResult, LValue);
  // resync (Delphi string) length of result after pchar manipulations...
  result := PChar(result);
end;


// Seaches <sStr> and replaces <sDel> with <sIns>
// Case in-sensitive.
function tiCIStrTran(AValue, ADel, AIns : string): string;
var
  i: integer;
  sToChange: string;
begin
  Result := '';
  sToChange := AValue;
  i := pos(upperCase(ADel), upperCase(sToChange));
  while i <> 0 do begin
    result := result + copy(sToChange, 1, i-1) + AIns;
    delete(sToChange, 1, i+length(ADel)-1);
    i := pos(upperCase(ADel), upperCase(sToChange));
  end;
  Result := Result + sToChange;
end;


function tiNumToken(const AValue, AToken : string): integer;
var
  i, iCount : integer;
  lsValue : string;
begin
  Result := 0;
  if AValue = '' then
    Exit; //==>

  iCount := 0;
  lsValue := AValue;
  i := pos(AToken, lsValue);
  while i <> 0 do begin
    delete(lsValue, i, length(AToken));
    inc(iCount);
    i := pos(AToken, lsValue);
  end;
  Result := iCount + 1;
end;


function tiToken(const AValue, AToken : string; const APos : integer): string;
var
  i, iCount, iNumToken : integer;
  lsValue : string;
begin
  result := '';

  iNumToken := tiNumToken(AValue, AToken);
  if APos = 1 then begin
    if pos(AToken, AValue) = 0 then result := AValue
    else result := copy(AValue, 1, pos(AToken, AValue)-1);
    end
  else if (iNumToken < APos-1) or (APos<1) then begin
    result := '';
    end
  else begin

    { Remove leading blocks }
    iCount := 1;
    lsValue := AValue;
    i := pos(AToken, lsValue);
    while (i<>0) and (iCount<APos) do begin
      delete(lsValue, 1, i + length(AToken) - 1);
      inc(iCount);
      i := pos(AToken, lsValue);
    end;

    if (i=0) and (iCount=APos) then result := lsValue
    else if (i=0) and (iCount<>APos) then
      result := ''
    else
      result := copy(lsValue, 1, i-1);
  end;
end;


function tiSpace(ALen : integer): string;
var i : integer;
    sString : string;
begin
  sString := '';
  for i := 1 to ALen do
    sString := sString + ' ';
  result := sString;
end;


function tiPadR(const AValue : string; ALen : integer): string;
var
  ls : string;
begin
  ls := AValue;
  if length(ls) < ALen then begin
    while length(ls) < ALen do begin
      ls := ls + ' ';
    end;
  end
  else if length(ls) > ALen then
    ls := copy(ls, 1, ALen);
  result := ls;
end;


function tiPadL(AValue : string; ALen : integer): string;
begin
  if length(AValue) < ALen then begin
    while length(AValue) < ALen do begin
      AValue := ' ' + AValue;
    end;
  end
  else if length(AValue) > ALen then
    AValue := copy(AValue, length(AValue)-ALen+1, ALen);
  result := AValue;
end;


function  tiPadC(AValue : string; ALen : integer): string;
var
  liPad : integer;
begin
  if Length(AValue) = ALen then
  begin
    result := AValue;
    Exit; //==>
  end;
  
  if Length(AValue) + 1 = ALen then
  begin
    result := AValue + ' ';
    Exit; //==>
  end;

  if Length(AValue) > ALen then
    raise exception.Create('Can not call tiPadC when the string to be ' +
                            'padded is longer than the target length');

  liPad := (ALen - length(AValue)) div 2;
  if liPad > 0 then
    result := tiSpace(liPad) + AValue + tiSpace(liPad);

  // To handle situations where ALen < length(AValue) and
  // when length(AValue) is an odd number
  result := tiPadR(result, ALen);
end;


function tiPad0(AValue : string; ALen : integer): string;
begin
  if length(AValue) < ALen then begin
    while length(AValue) < ALen do begin
      AValue := '0' + AValue;
    end;
  end
  else if length(AValue) > ALen then begin
    AValue := copy(AValue, length(AValue)-ALen, ALen);
  end;
  result := AValue;
end;


function tiRemoveLeading0(AValue : string): string;
var i : integer;
begin
  for i := 1 to length(AValue) do begin
    if copy(AValue, 1, 1) = '0' then begin
      AValue := copy(AValue, 2, length(AValue) - 1);
    end else begin
      break;
    end;
  end;
  result := AValue;
end;


function  tiYear(AValue : TDateTime = 0.0): Word;
var
  lD, lM : Word;
  lDate : TDateTime;
begin
  if AValue = 0.0 then
    lDate := Date
  else
    lDate := AValue;
  DecodeDate(lDate, Result, lM, lD);
end;


function tiDateWithinRange(const ADate, AFrom, ATo: TDateTime): Boolean;
var
  lDate: TDateTime;
begin
  lDate := Trunc(ADate);
  Result := (lDate >= Trunc(AFrom)) and (lDate <= Trunc(ATo));
end;


function tiYearToStartAusFinancialYear(AFinancialYear: Word): TDateTime;
begin
  Result := EncodeDate(AFinancialYear - 1, 07, 1);
end;


function tiYearToEndAusFinancialYear(AFinancialYear: Word): TDateTime;
begin
  Result := EncodeDate(AFinancialYear, 06, 30);
end;


function tiAusFinancialYearToString(AFinancialYear: Word): string;
begin
  Result := IntToStr(AFinancialYear-1) + '-' + IntToStr(AFinancialYear);
end;


function tiDateToAusFinancialYear(ADate: TDateTime): Word;
var
  lY, lM, lD: Word;
begin
  DecodeDate(ADate, lY, lM, lD);
  if lM <= 6 then
    Result := lY
  else
    Result := lY + 1;
end;


function tiMixedCase(AValue : string): string;
var iToken : integer;
    i : integer;
    sBuffer : string;
begin
  iToken := tiNumToken(AValue, ' ');
  result := '';
  AValue := lowerCase(AValue);
  for i := 1 to iToken do begin
    sBuffer := tiToken(AValue, ' ', i);
    result := tiAddTrailingValue(result, ' ', true);
    result := result +
               upperCase(copy(sBuffer, 1, 1)) +
               copy(sBuffer, 2, length(sBuffer) - 1);
  end;
end;


{$IFDEF MSWINDOWS}
function tiShellExecute(const AEXE : string;
                         const AParameters : string = '';
                         const AWinState : integer = SW_SHOWNORMAL): integer;
var
  lFileName  : array[0..255] of char;
  lParameters : array[0..255] of char;
  lHandle : THandle;
begin;

  strPCopy(lFileName,   AEXE);
  strPCopy(lParameters, AParameters);

  // Screen.ActiveForm.Handle is not thread safe
  //lHandle := screen.activeForm.handle;
  lHandle := 0;
  result := _tiShellExecute(lHandle,
                             nil,
                             lFileName,
                             lParameters,
                             nil,
                             AWinState);
end;
{$ENDIF MSWINDOWS}


{$IFDEF MSWINDOWS}
function _tiShellExecute(AHwnd : integer;
                         AOpperation, AFile, AParameters, ADirectory : PChar;
                         AShowCmd : integer): integer;
var sMessage    : string;
begin

  result := ShellExecute(AHwnd,
                          AOpperation,
                          AFile,
                          AParameters,
                          ADirectory,
                          AShowCmd);

  { These error messages were pasted from the WINAPI help on shellExecute() }
  case result of
    0 : sMessage := ('System was out of memory, executable file was corrupt, or ' +
                       'relocations were invalid.');
    2 : sMessage := ('File was not found.');
    3 : sMessage := ('Path was not found.');
    5 : sMessage := ('Attempt was made to dynamically link to a task, or there ' +
                       'was a sharing or network-protection error.');
    6 : sMessage := ('Library required separate data segments for each task.');
    8 : sMessage := ('There was insufficient memory to start the application.');
    10 : sMessage := ('Windows version was incorrect.');
    11 : sMessage := ('Executable file was invalid. Either it was not a Windows ' +
                       'application or there was an error in the .EXE image.');
    12 : sMessage := ('Application was designed for a different operating system.');
    13 : sMessage := ('Application was designed for MS-DOS 4.0.');
    14 : sMessage := ('Type of executable file was unknown.');
    15 : sMessage := ('Attempt was made to load a real-mode application (developed ' +
                       'for an earlier version of Windows).');
    16 : sMessage := ('Attempt was made to load a second instance of an executable ' +
                       'file containing multiple data segments that were not marked ' +
                       'read-only.');
    19 : sMessage := ('Attempt was made to load a compressed executable file. The ' +
                       'file must be decompressed before it can be loaded.');
    20 : sMessage := ('Dynamic-link library (DLL) file was invalid. One of the ' +
                       'DLLs required to run this application was corrupt.');
    21 : sMessage := ('Application requires Windows 32-bit extensions.');
    else
      sMessage := '';
      { ShellExe ran OK, do nothing. }
    end;

    if sMessage <> '' then
      raise Exception.Create('Error executing external application.' + Cr +
                              'Error: ' + sMessage);
end;
{$ENDIF MSWINDOWS}


function tiEditFile(const AFileName : string): integer;
begin
  // screen.activeForm.handle,
  {$IFDEF MSWINDOWS}
  result := ShellExecute(Application.MainForm.Handle,
                          nil,
                          PChar(AFileName),
                          nil,
                          nil,
                          SW_SHOWNORMAL);
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  { TODO: There is no standard editor included with all
    flavours of linux. Might implement tiEditFile as a form with
    a memo control and basic edit functions. ie: Something like NotePad for Windows }
  Result := 0;    // To get rid of the compiler warning, until I implement this.
  {$ENDIF LINUX}
end;


function tiExtractFileNameOnly(AValue : string): string;
begin
  result := tiRemoveExtension(extractFileName(AValue));
end;


function tiRemoveExtension(AValue : string): string;
var i : integer;
begin
  i := tiPosR('.', AValue);
  if i <> 0 then begin
    result := copy(AValue, 1, i - 1);
  end else begin
    result := AValue;
  end;
end;


function  tiSwapExt(const AFileName, AExt : string): string;
begin
  result := tiAddTrailingValue(tiRemoveExtension(AFileName), '.', false) + AExt;
end;


function tiExtractExtension(AValue : string): string;
var i : integer;
begin
  i := tiPosR('.', AValue);
  if i <> 0 then begin
    result := copy(AValue, i+1, length(AValue) - i);
  end else begin
    result := '';
  end;
end;


procedure tiCopyFile(const AFrom, ATo : string);
var
  iErrorCode : Longword;
  {$IFDEF FPC}
    function fpcCopyFile(Org, Dest:string): boolean;
    var
      Source, Target:TFileStream;
    begin
      Result := false;
      try
        try
          Source := TFileStream.Create(Org, fmShareDenyNone or fmOpenRead);
          try
            Target := TFileStream.Create(Dest, fmOpenWrite or fmCreate);
            Target.CopyFrom(Source, Source.Size);
            Result := true;
          finally
            Target.Free;
          end;
        finally
          Source.Free;
        end;
      except
      end;
    end;
  {$ENDIF}
begin
{$IFNDEF FPC}
  copyFile(pChar(AFrom), pChar(ATo), false);
  iErrorCode := getLastError();
{$ELSE}
  if fpcCopyFile(AFrom, ATo) then
    iErrorCode := 0
  else
    iErrorCode := GetLastOSError;
{$ENDIF}

  if iErrorCode <> 0 then begin
    raise exception.Create('Unable to copy <' +
                            AFrom +
                            '> to <' +
                            ATo + '>' + #13 +
                            'System error code: ' +
                            intToStr(iErrorCode) + #13 +
                            'System error message: ' +
                            sysErrorMessage(iErrorCode));
  end;
end;


procedure tiMoveFile(AFrom, ATo: string);
begin
  RenameFile(AFrom, ATo);   { Rename the file }
end;


function tiGetFileSize(AValue : string): longInt;
var f: file of Byte;
begin
  AssignFile(f, AValue);
  Reset(f);
  result := FileSize(f);
  closeFile(f);
end;


function tiReplicate(const AValue : string; ARepCount : Word): string;
var
  LResult, LValue: PChar;
  LLen: cardinal;
begin
  if (ARepCount = 0) or (Pointer(AValue) = nil) then
    exit;

  LLen := Length(AValue);
  SetLength(Result, LLen * ARepCount);
  LResult  := Pointer(Result);
  LValue   := Pointer(AValue);

  while ARepCount <> 0 do
  begin
    Move(LValue^, LResult^, LLen);
    Inc(LResult, LLen);
    Dec(ARepCount);
  end;
end;


{ Check that the string contained in a TControl can be converted
  to a valid float. Pass the control as a parameter and the
  function will perform the test and retrun focus to the control
  if the test fails.

  Currently programmed to test mastEdits, some more work is necessary
  to test other controls. }
{
function tiIsControlValidFloat(pControl : TControl): boolean;
var sString : string;
begin

  if (pControl is TMaskEdit) then begin
    sString := TMaskEdit(pControl).text;
  end else begin
    tiAppError('Invalid control type passed to uIsControlValidFloar()');
  end;

  try
    result := true;
  except
    tiAppError('<' + sString + '> is not a valid number.');
    TMaskEdit(pControl).setFocus;
    result := false;
  end;

end;
}


function _RemoveNonNumChars(AValue : string): string;
begin
  result := AValue;
  result := tiStrTran(result, ' ', '');
  result := tiStrTran(result, ',', '');
  result := tiStrTran(result, '$', '');
end;


function tiStrToInt(const AValue : string): integer;
begin
  try
    result := strToInt(_RemoveNonNumChars(AValue));
  except
    result := 0;
  end;
end;


function tiStrToFloat(const AValue : string): Extended;
begin
  try
    result := strToFloat(_RemoveNonNumChars(AValue));
  except
    result := 0;
  end;
end;


function tiDateToStr(const pdtValue : TDateTime; const psFormat : string = csWinDateFormat): string;
begin
  result := FormatDateTime(psFormat, pdtValue);
end;


function tiDateTimeToStr(const ADateTime : TDateTime): string;
begin
  result := FormatDateTime(csWinDateTimeFormat, ADateTime);
end;


function  tiTimeToStr(const ADateTime  : TDateTime;
                       const ATimeFormat : string = ''): string;
begin
  if ATimeFormat = '' then
    result := FormatDateTime(csWinTimeFormat, ADateTime)
  else
    result := FormatDateTime(ATimeFormat, ADateTime)
end;


function tiSafeDiv(ANum, ADenom: longInt): longInt;
begin
  if ADenom <> 0 then 
    result := ANum div ADenom
  else
    result := 0;
end;


function tiIntToStrHide0(const AValue : longInt): string;
begin
  if AValue = 0 then begin
    result := '';
  end else begin
    result := intToStr(AValue);
  end;
end;


function  tiIntToCommaStr(const AValue : integer): string;
begin
  result := tiFloatToCommaStr(AValue, 0);
end;


function tiFloatToCurrencyHide0(const AValue: Extended): string;
begin
  if (AValue < 0.005) and (AValue > -0.005) then
    result := ''
  else
    result := tiFloatToCurrency(AValue);
end;


function tiFloatToCurrency(const AValue: Extended): string;
begin
  try
    result := FormatFloat('$ #,##0.00', AValue);
  except
    result := '0.00';
  end;
end;


function tiBooleanToStr(const AValue : boolean): string;
begin
  if AValue then
    result := cTrueDB
  else
    result := cFalseDB;
end;


function tiBoolToStr(const AValue : boolean): string;
begin
  result := tiBooleanToStr(AValue);
end;


function  tiStrToBool(const AValue : string): boolean;
var
  ls : string;
begin
  ls := upperCase(AValue);
  if (ls = 'TRUE') or
     (ls = 'T'  ) or
     (ls = 'YES') or
     (ls = 'Y'  ) or
     (ls = '1'  ) then
    result := true
  else
    result := false;
end;


function tiIsNearEnough(N, D: Double;  FEPS: Double): Boolean;
begin
  if D = 0.0 then
    Result := (N > -feps) and (N < +feps)
  else if N = 0.0 then
    Result := (D > -feps) and (D < +feps)
  else
    Result := (((1-feps) < (N/D)) and ((N/D) < (1+feps)));
end;


function tiIf(ACondition: Boolean; AResultTrue, AResultFalse: Extended): Extended;
begin
  if ACondition then
    Result := AResultTrue
  else
    Result := AResultFalse;
end;


function tiIf(ACondition: Boolean; AResultTrue, AResultFalse: Integer): Integer;
begin
  if ACondition then
    Result := AResultTrue
  else
    Result := AResultFalse;
end;


function tiIf(ACondition: Boolean; AResultTrue, AResultFalse: String): String;
begin
  if ACondition then
    Result := AResultTrue
  else
    Result := AResultFalse;
end;


function  tiVariantArrayToString(AValue : oleVariant): string;
  procedure appendVariantToStringList(pStringList : TStringList;
                                       pVariant : oleVariant;
                                       var pIndent : integer);
  var i : integer;
      iLow : integer;
      iHigh : integer;
  begin
    if tiIsVariantOfType(pVariant, varArray) then begin
      iLow := varArrayLowBound(pVariant, 1);
      iHigh := varArrayHighBound(pVariant, 1);
      for i := iLow to iHigh do begin
        inc(pIndent);
        if i = iLow then pStringList.add(tiSpace(pIndent*3) + '[');
        appendVariantToStringList(pStringList, pVariant[i], pIndent);
        if i = iHigh then pStringList.add(tiSpace(pIndent*3) + ']');
        dec(pIndent);
      end;
    end else begin
      pStringList.add(tiSpace(pIndent*3 + 1) + varToStr(pVariant));
    end;
  end;

var
  lStringList : TStringList;
  pIndent : integer;
begin
  lStringList := TStringList.Create;
  try
    pIndent := -1;
    appendVariantToStringList(lStringList, AValue, pIndent);
    result := lStringList.Text;
  finally
    lStringList.free;
  end;
end;

{
varEmpty	The variant is Unassigned.
varNull	The variant is Null.
VarSmallint	16-bit signed integer (type Smallint).
varInteger	32-bit signed integer (type Integer).
varSingle	Single-precision floating-point value (type Single).
varDouble	Double-precision floating-point value (type Double).
varCurrency	Currency floating-point value (type Currency).
varDate	Date and time value (type TDateTime).
varOLEStr	Reference to a dynamically allocated UNICODE string.
varDispatch	Reference to an Automation object (an IDispatch interface pointer).
varError	Operating system error code.
varBoolean	16-bit boolean (type WordBool).
varUnknown	Reference to an unknown COM object (an IUnknown interface pointer).
varByte	8-bit unsigned integer (type Byte).
varString	Reference to a dynamically allocated Pascal string (type AnsiString).
varTypeMask	Bit mask for extracting type code.
varArray	Bit indicating variant array.
varByRef	Bit indicating variant contains a reference (rather than a value).
}
function tiIsVariantOfType(AVariant : Variant; AVarType : TVarType): boolean;
var
  xVT : TVarType;
  xVTHigh : TVarType;
//  xVTLow : TVarType;
begin
//  result := (varType(pVariant) and pVarType) = pVarType;
// Contr: VarType is varDate = 0007, pVarType is varInteger=0003.
// 0007 and 0003 = 0003. WRONG!

  xVT:=VarType(AVariant);
//  xVTLow:=xVT and varTypeMask;
  xVTHigh:=xVT and (not varTypeMask);

  // in true pVarType can be and OR of two types: varArray and varString (or others)
  // we have to recognize it.
  // there shouldn't be xVTLow because when we have array of string (normal) then
  // xVT=$2008 = $2000 (var Array) or $0008 (var String)
  // then when we asked:
  //   is $2000 (varArray)? we should receive TRUE (xVTHigh=pVarType)
  //   is $2008 (varArray of varString)? we should receive TRUE (xVT=pVarType)
  //   is $0008 (varString)? we should receive FALSE
  Result:=(xVT=AVarType) or ((xVTHigh=AVarType) and (xVTHigh<>varEmpty));
end;


function  tiAddTrailingValue(const ALine, AValue : string; ADuplicates : boolean = true): string;
begin
  if ALine = '' then
  begin
    result := ALine;
    Exit; //==>
  end;

  if ADuplicates then
  begin
    result := ALine + AValue;
    Exit; //==>
  end;

  if (not SameText(Copy(ALine,
                           Length(ALine) - Length(AValue) + 1,
                           Length(AValue)),
                     AValue)) then
    result := ALine + AValue
  else
    result := ALine;

end;


function  tiRemoveTrailingValue(ALine, AValue : string): string;
var
  lLHS : integer;
  lRHS : integer;
begin
  lLHS := length(ALine) - Length(AValue) + 1;
  lRHS := Length(AValue);

  if copy(ALine, lLHS, lRHS) = AValue then
    result :=
      copy(ALine, 1, lLHS - 1)
  else
    result := ALine;
end;


function tiAddTrailingComma(AValue : string): string;
begin
  result := tiAddTrailingValue(AValue, ',', true);
end;


function  tiAddTrailingAnd(AValue : string): string;
begin
  result := tiAddTrailingValue(AValue, ' and ', false);
end;


function  tiAddTrailingOr(AValue : string): string;
begin
  result := tiAddTrailingValue(AValue, ' or ', false);
end;


function  tiAddTrailingSpace(AValue : string): string;
begin
  result := tiAddTrailingValue(AValue, ' ', true);
end;


function tiDateToPreviousWeekDayDate(AValue : TDateTime): TDateTime;
var iDay : integer;
begin
  result := AValue;
  iDay  := dayOfWeek(result);
  case iDay of
  1 : result := result - 2; // Sunday
  2 : result := result - 3; // Monday
  3 : result := result - 1; // Tuesday
  4 : result := result - 1; // Wednesday
  5 : result := result - 1; // Thursday
  6 : result := result - 1; // Friday
  7 : result := result - 1; // Saturday
  end;
end;


function tiAddTrailingSlash(const AValue : string): string;
begin
  result := tiAddTrailingValue(AValue, PathDelim, false);
end;


function tiRemoveTrailingSlash(const AValue : string): string;
begin
  result := tiRemoveTrailingValue(tiFixPathDelim(AValue), PathDelim);
end;


function tiRemoveLeadingSlash(const AValue: string): string;
var
  lStr: string;
begin
  lStr := tiFixPathDelim(AValue);
  if copy(lStr, 1, 1) = PathDelim then begin
    result := copy(lStr, 2, length(lStr) - 1);
  end else begin
    result := lStr;
  end;
end;


function tiPosR(ATarget, AValue : string): integer;
var i : integer;
    iTargetLength : integer;
begin
  i := length(AValue);
  iTargetLength := length(ATarget);
  while i > 0 do begin
    if copy(AValue, i, iTargetLength) = ATarget then begin
      break; //==>
    end;
    dec(i);
  end;
  result := i;
end;


function tiGetUserName : string;
begin
  {$IFDEF MSWINDOWS}
    Result := tiWin32GetUserName;
  {$ENDIF}
  {$IFDEF LINUX}
    Result := tiLinuxGetUserName;
  {$ENDIF}
end;


function tiGetComputerName : string;
begin
  {$IFDEF MSWINDOWS}
  Result := tiWin32GetComputerName;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Result := tiLinuxGetComputerName;
  {$ENDIF LINUX}
end;


function tiRemoveDrive(AValue : string): string;
var
  sDrive : string;
begin
  sDrive := extractFileDrive(AValue);
  if sDrive <> '' then begin
    result := copy(AValue, length(sDrive)+1, length(AValue) - length(sDrive));
  end else begin
    result := AValue;
  end;
end;


function  tiRemoveDirectory(const AFileName: string; const AToRemove: string): string;
var
  lToRemove: string;
  lFileName: string;
begin
  lToRemove := UpperCase(tiRemoveTrailingSlash(AToRemove));
  lFileName := UpperCase(AFileName);
  if Pos(lToRemove, lFileName) <> 1 then
    raise EtiOPFProgrammerException.CreateFmt(cErrorRemoveDirectory, [AToRemove, lFileName]);
  Result := Copy(AFileName, Length(lToRemove)+1, Length(lFileName));
  while Copy(Result, 1, 1) = PathDelim do
    Result := Copy(Result, 2, Length(Result));
end;


function tiIsBitSet(const AValue: longint; const ABit: byte): boolean;
begin
  result := (AValue and (1 shl ABit)) <> 0;
end;


function tiBitToString(const AValue: longint; const ABit: byte): string;
begin
  if tiIsBitSet(AValue, ABit) then
    result := '1'
  else
    result := '0';
end;


function tiInt32ToBinString(const AValue : longInt): string;
var i : integer;
begin
  result := '';
  for i := 31 downto 0 do begin
    result := result + tiBitToString(AValue, i);
  end;
end;


{$IFDEF MSWINDOWS}
procedure tiSetFileReadOnly(AFileName : string; AReadOnly : boolean);
const // This is copied from sysUtils, as in it's native form,
      // there is confusion with ordinals defined in DB.PAS
      cReadOnly  = $00000001;
var   iCurrentState : integer;
      lBoolReadOnly : boolean;
begin
  lBoolReadOnly := tiIsFileReadOnly(AFileName);
  if lBoolReadOnly = AReadOnly then exit; //==>

  iCurrentState := tiWin32FileGetAttr(AFileName);
  if AReadOnly then begin
    tiWin32FileSetAttr(AFileName, iCurrentState or cReadOnly);
  end else begin
   tiWin32FileSetAttr(AFileName, iCurrentState xor cReadOnly);
  end;
end;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{ Works on Owner rights only. Unix has 3 sets of rights: Owner, Group and Global }
procedure tiSetFileReadOnly(pStrFileTo : string; pBoolReadOnly : boolean);
var
  lAttr: LongInt;
  Info: Stat;
begin
  lAttr := FileGetAttr(pStrFileTo);
  if lAttr <> -1 then
  begin
    if FpStat(pStrFileTo, Info) < 0 then
      raise Exception.Create('Unable to set ReadOnly attribute');

    if pBoolReadOnly then
      FpChmod(pStrFileTo, Info.st_mode xor S_IWUSR)   // remove write access. Owner rights only.
    else
      FpChmod(pStrFileTo, Info.st_mode or S_IWUSR);   // add write access.
  end
  else
  begin
    raise Exception.Create('Unable to set ReadOnly attribute');
  end
end;
{$ENDIF LINUX}


{$IFDEF MSWINDOWS}
function tiIsFileReadOnly(AValue : string): boolean;
const // This is copied from sysUtils, as in it's native form,
      // there is confusion with ordinals defined in DB.PAS
      cReadOnly  = $00000001;
var   iCurrentState : integer;
begin
  iCurrentState := tiWin32FileGetAttr(AValue);
  result := tiIsBitSet(iCurrentState, 0);
end;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
function tiIsFileReadOnly(pStrFileTo : string): boolean;
begin
  Result := FileIsReadOnly(pStrFileTo);
end;
{$ENDIF LINUX}


function tiGetEXEPath : string;
var
  path: array[0..MAX_PATH - 1] of char;
begin
  {$IFDEF MSWINDOWS}
  if IsLibrary then
    SetString(Result, path, GetModuleFileName(HInstance, path, SizeOf(path)))
  else
  {$ENDIF}
    result := paramStr(0);
  result := tiRemoveTrailingSlash(ExtractFilePath(Result));
end;


function tiAddEXEPath(const AFileName: string): string;
begin
  Result :=
    ExpandFileName(tiAddTrailingSlash(tiGetEXEPath) +
      ExtractFileName(AFileName));
end;


procedure tiDirectoryTreeToStringList(const AStartDir : string; const ADirList : TStringList; ARecurse : boolean);
  procedure _ReadDirectories(const psStartDir : string; slTree : TStringList; bRecurse : boolean);
    procedure _AddIfDir(searchRec : TSearchRec; sStartDir : string; slTree : TStringList; bRecurse : boolean);
    begin
        if ((searchRec.attr and faDirectory) > 0) and
           (searchRec.name <> '.') and
           (searchRec.name <> '..') then begin
          slTree.add(sStartDir + searchRec.name);
          if bRecurse then begin
            _ReadDirectories(sStartDir + searchRec.name, slTree, bRecurse);
          end;
        end;
    end;
  var
    lsStartDir : string;
    SearchRec : TSearchRec;
  begin
    lsStartDir := tiAddTrailingSlash(psStartDir);
    try
      if sysUtils.FindFirst(lsStartDir + AllFilesWildCard,
                             faDirectory,
                             SearchRec) = 0 then
      begin
        _AddIfDir(searchRec, lsStartDir, slTree, bRecurse);
        while sysUtils.findNext(searchRec) = 0 do
        begin
          _AddIfDir(searchRec, lsStartDir, slTree, bRecurse);
        end;
      end;
    finally
      sysUtils.FindClose(SearchRec);
    end;
  end;
var
  lStartDir : string;
begin

  lStartDir := tiRemoveTrailingSlash(AStartDir);
  ADirList.Clear;

  if not DirectoryExists(lStartDir) then
    exit;

  ADirList.Add(lStartDir);
  _ReadDirectories(lStartDir, ADirList, ARecurse);

end;


{$IFDEF DELPHI6ORAVOVE} {$WARN SYMBOL_PLATFORM OFF} {$ENDIF}
procedure tiFilesToStringList(const AStartDir,
                               AWildCard : string;
                               AResults : TStringList;
                               const ARecurse : boolean);
  // Locally visible proc
  procedure AddFile(searchRec : TSearchRec; sStartDir, pStrWildCard : string; slTree : TStringList; bRecurse : boolean);
  begin
    Assert(not ((searchRec.attr and faDirectory) > 0), 'A directory passed, but a file expected');
    Assert((searchRec.name <> '.'),                      'A directory passed, but a file expected');
    Assert((searchRec.name <> '..'),                    'A directory passed, but a file expected');
    slTree.add(sStartDir + searchRec.name);
  end;

var
  SearchRec : TSearchRec;
  lsStartDir : string;
  lslDirTree : TStringList;
  i : integer;
begin
  AResults.Clear;
  lsStartDir := tiAddTrailingSlash(AStartDir);
  lslDirTree := TStringList.Create;
  try
    if ARecurse then
      tiDirectoryTreeToStringList(lsStartDir, lslDirTree, ARecurse)
    else
      lslDirTree.Add(lsStartDir);
    for i := 0 to lslDirTree.Count-1 do
    begin
      lsStartDir := tiAddTrailingSlash(lslDirTree.Strings[i]);
      {$IFDEF MSWINDOWS}
      try
        if tiWin32FindFirstFile(lsStartDir + AWildCard, SearchRec) = 0 then
        begin
          AddFile(searchRec, lsStartDir, AWildCard, AResults, ARecurse);
          while sysUtils.findNext(searchRec) = 0 do
          begin
            AddFile(searchRec, lsStartDir, AWildCard, AResults, ARecurse);
          end;
        end;
      finally
        sysUtils.FindClose(SearchRec);
      end;
      {$ENDIF MSWINDOWS}
      {$IFDEF LINUX}
      try
        if sysUtils.FindFirst(lsStartDir + psWildCard,
                               faAnyFile-faSysFile-faDirectory,
                               SearchRec) = 0 then begin
          AddFile(searchRec, lsStartDir, psWildCard, slResult, pbRecurse);
          while sysUtils.findNext(searchRec) = 0 do begin
            AddFile(searchRec, lsStartDir, psWildCard, slResult, pbRecurse);
          end;
        end;
      finally
        sysUtils.FindClose(SearchRec);
      end;
      {$ENDIF LINUX}
    end;
  finally
    lslDirTree.Free;
  end;
end;
{$IFDEF DELPHI6ORAVOVE}{$WARN SYMBOL_PLATFORM ON}{$ENDIF}


procedure tiXCopy(const ASource, ATarget: string);
var
  lFiles: TStringList;
  i: Integer;
  lErrors: string;
  lSource: string;
  lTarget: string;
begin
  lFiles := TStringList.Create;
  try
    tiForceDirectories(ATarget);
    tiFilesToStringList(ASource, AllFilesWildCard, lFiles, true);
    for i := 0 to lFiles.Count - 1 do
    begin
      lSource := lFiles.Strings[i];
      lTarget := tiAddTrailingSlash(ATarget) + tiRemoveDirectory(lSource, ASource);
      tiForceDirectories(ExtractFileDir(lTarget));
      try
        tiCopyFile(lSource, lTarget);
      except
        on e:Exception do
        begin
          if lErrors <> '' then lErrors := lErrors + Cr;
          lErrors := lErrors + e.message;
        end;
      end;
    end;
    if lErrors <> '' then
      raise EtiOPFFileSystemException.Create(lErrors);
  finally
    lFiles.Free;
  end;
end;


procedure tiDeleteFiles(const ADirectory, AWildCard: string);
var
  Lsl: TStringList;
  i : Integer;
begin
  Lsl:= TStringList.Create;
  try
    tiFilesToStringList(ADirectory, AWildCard, Lsl, false);
    for i := 0 to Lsl.Count - 1 do
      if not SysUtils.DeleteFile(Lsl.Strings[i]) then
        raise EtiOPFFileSystemException.CreateFmt(cErrorCanNotDeleteFile, [Lsl.Strings[i]]);
  finally
    Lsl.Free;
  end;
end;

{$HINTS OFF}
function tiDeleteFile(const AFileName: string): boolean;
begin
  result:= SysUtils.DeleteFile(AFileName);
end;
{$HINTS ON}

function tiHasSubDirectory(AStartDir : string): boolean;
var
  slTree : TStringList;
begin
  slTree := TStringList.Create;
  try
    tiDirectoryTreeToStringList(tiFixPathDelim(AStartDir), slTree, false);
    result := slTree.count > 1;
  finally
    slTree.free;
  end;
end;


function tiExtractDirToLevel(const AFileName : TFileName; ALevel : byte): TFileName;
var
  i : integer;
begin
  result := '';
  for i := 0 to ALevel+1 do
  begin
    if result <> '' then
      result := tiAddTrailingSlash(result);
    result := result + tiToken(AFileName, PathDelim, i);
  end;
  result := tiRemoveTrailingSlash(result);
  {$IFDEF UNIX}
  result := PathDelim + result;
  {$ENDIF}
end;


procedure tiForceDirectories(const AValue : TFileName);
var
  lDirName: string;
begin
  if Pos('.', AValue) <> 0 then
    lDirName := ExtractFilePath(ExpandFileName(AValue))
  else
    lDirName := AValue;
  if not ForceDirectories(lDirName) then
    raise EtiOPFFileSystemException.CreateFmt(cErrorUnableToCreateDirectory, [lDirName]);
end;


// ToDo: Must add code to handle hidden files.
function tiForceRemoveDir(const AValue : TFileName): boolean;
var
  lsl : TStringList;
  i : integer;
begin
  try
    lsl := TStringList.Create;
    try
      tiFilesToStringList(AValue, AllFilesWildCard, lsl, true);
      for i := 0 to lsl.Count - 1 do
      begin
        if tiIsFileReadOnly(lsl.Strings[i]) then
          tiSetFileReadOnly(lsl.Strings[i], false);
        SysUtils.DeleteFile(lsl.Strings[i]);
      end;
      tiDirectoryTreeToStringList(AValue, lsl, true);
      for i := lsl.Count - 1 downto 0 do
        SysUtils.RemoveDir(lsl.Strings[i]);
      result := not DirectoryExists(AValue);
    finally
      lsl.Free;
    end;
  except
    on e:exception do
      result := false;
  end;
end;


function tiJoinPath(const ALeftSide, ARightSide: string): string;
var
  lLeftSide, lRightSide: string;
begin
  lLeftSide  := tiFixPathDelim(ALeftSide);
  lRightSide := tiFixPathDelim(ARightSide);
  Result := tiRemoveTrailingSlash(tiAddTrailingSlash(lLeftSide) + lRightSide);
end;


function tiJoinPath(const AElements: array of string): string;
var
  I: Integer;
begin
  if Length(AElements) > 0 then
  begin
    Result := tiRemoveTrailingSlash(AElements[0]);
    for I := 1 to High(AElements) do
      Result := tiJoinPath(Result, AElements[I]);
  end
  else
    Result := '';
end;


{ This allows us to always use a \ as a path separator. For Win32 it will
  do nothing, but for *Unix it will replace all \'s with /'s.  Now we don't have
  to have so many IFDEFs in the Unit Tests! }
function tiFixPathDelim(const AText: string): string;
begin
  {$IFDEF MSWINDOWS}
  result := AText;
  {$ENDIF}
  {$IFDEF UNIX}
  result := SetDirSeparators(AText);
  {$ENDIF}
end;


function tiFloatToStr(const AValue : Extended;
    const APrecision : integer = 3): string;
begin
  result := _tiFloatToStr(AValue, APrecision, '###0');
end;


function  tiFloatToCommaStr(const AValue: Extended;
    const APrecision: integer = 3): string;
begin
  result := _tiFloatToStr(AValue, APrecision, '#,##0');
end;


function _tiFloatToStr(const AValue: Extended; const APrecision: integer;
    const AFormat: string): string;
var
  lsFormat: string;
begin
  lsFormat := AFormat;
  if APrecision <> 0 then
    lsFormat := lsFormat + '.' + tiReplicate('0', APrecision);
  try
    result := formatFloat(lsFormat, tiDecimalRoundDbl(AValue, APrecision));
  except
    on e:exception do
      raise exception.Create('Unable to format floating point number. ' + Cr(2) +
                              'Called in tiFloatToStr() ' +
                              'Format mask <' + lsFormat + '>' + Cr +
                              'Error message: ' + e.message);
  end;
end;


function  tiSafeDiv(ANum, ADenom : Extended): Extended;
begin
  if ADenom <> 0 then begin
    result := ANum / ADenom;
  end else begin
    result := 0;
  end;
end;


procedure tiRunEXEAndWait(AEXE : string);
begin
{$IFDEF MSWINDOWS}
  tiWin32RunEXEAndWait(AEXE);
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
  tiLinuxRunEXEAndWait(pStrEXE);
{$ENDIF}
end;


function  tiRound(const AValue : Extended): Int64;
begin
  Result := Trunc(AVAlue);
  if AValue - Result >= 0.5 then
    Inc(Result);
end;


// Get the decimal part of a real number
{
Q:  How do I find one string inside another with wildcards?

A:  There are many times when you need to compare two strings, but want to
use wild cards in the match - all last names that begin with 'St', etc.  The
following is a piece of code I got from Sean Stanley in Tallahassee Florida
in C.  I translated it into Delphi an am uploading it here for all to use. I
have not tested it extensivly, but the original function has been tested
quite thoughly. }
function tiWildcardMatch(const ASource, APattern: String; const ACaseSensitive: boolean = false): Boolean;

  function MatchPatternStr(const psElement, APattern: String): Boolean;
  var PatternChar: Char;
  begin
    if (APattern = '*') then Result := True
    else if (psElement = EmptyStr) then Result := (APattern = EmptyStr)
    else begin
      if (Copy(APattern, 1, 1) <> EmptyStr) then PatternChar := Copy(APattern, 1, 1)[1]
      else PatternChar := #0;

      case PatternChar of
      '*': begin
        if MatchPatternStr(psElement, Copy(APattern, 2, Length(APattern))) then Result := True
        else Result := MatchPatternStr(Copy(psElement, 2, Length(psElement)), APattern);
      end;//'*'
      '?': Result := MatchPatternStr(Copy(psElement, 2, Length(psElement)), Copy(APattern, 2, Length(APattern)));
      else
        if Copy(psElement, 1, 1) = Copy(APattern, 1, 1) then
          Result := MatchPatternStr(Copy(psElement, 2, Length(psElement)), Copy(APattern, 2, Length(APattern)))
        else Result := False;
      end;//case
    end;//else
  end;

begin
  if ACaseSensitive then
    Result := MatchPatternStr(ASource, APattern)
  else
    Result := MatchPatternStr(UpperCase(ASource), UpperCase(APattern));
end;


{ TODO : 
This will work:
  tiSubStr('my <d>string</d>', '<d>', '</d>',);
but this will not:
  tiSubStr('my <u>long</e><d>string</e>', '<d>', '</e>',); }
function  tiSubStr(const ASource, AStartDelim, AEndDelim : string; AIndex : integer = 1): string;
var
  liStart : integer;
  liEnd  : integer;
begin
  Assert(AIndex = 1, 'Under development, AIndex not yet in use');

  result := '';

  liStart := Pos(AStartDelim, ASource);
  if liStart <> 0 then
    liStart := liStart + length(AStartDelim);

  liEnd := Pos(AEndDelim, ASource);
  if liEnd <> 0 then
    liEnd := liEnd - 1;

  if (liStart = 0) or (liEnd = 0) then
    Exit; //==>

  result := Copy(ASource, liStart, liEnd - liStart + 1);
end;


function tiReadFileDate(const AFileName : string): TDateTime;
begin
  Result := FileDateToDateTime(FileAge(AFileName));
end;


function tiReadFileSize(const AFileName : string): DWord;
var
  LFileStream : TFileStream;
begin
  LFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LFileStream.Size;
  finally
    LFileStream.Free;
  end;
end;


procedure tiReadFileDateSize(const AFileName: string; var ADateTime: TDateTime;
    var AFileSize: integer);
begin
  ADateTime  := tiReadFileDate(AFileName);
  AFileSize := tiReadFileSize(AFileName);
end;


procedure tiSetFileDate(const AFileName : string; const ADateTime : TDateTime);
var
  lFileDate  : Integer;
  {$IFDEF MSWINDOWS}
  lFileHandle : Integer;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  lError     : Integer;
  {$ENDIF LINUX}
begin
  lFileDate  := DateTimeToFileDate(ADateTime);
  {$IFDEF MSWINDOWS}
  lFileHandle := FileOpen(AFileName, fmOpenWrite or fmShareDenyNone);
  try
    if lFileHandle > 0 then
      FileSetDate(lFileHandle, lFileDate)
    else
      raise exception.Create('Unable to set file date on <' +
                              AFileName);
  finally
    FileClose(lFileHandle);
  end;
  {$ENDIF MSWINDOWS}

  {$IFDEF LINUX}
    lError := FileSetDate(AFileName, lFileDate);
    if lError <> 0 then
      raise Exception.Create('Unable to set file date on <' + AFileName);
  {$ENDIF LINUX}
end;


function  Cr(const ACount : Byte = 1): string;
begin
  result := tiReplicate(#13, ACount);
end;


function  Lf(const ACount : Byte = 1): string;
begin
  result := tiReplicate(#10, ACount);
end;


function CrLf(const ACount : Byte = 1): string;
begin
  result := tiReplicate(#13 + #10, ACount);
end;


function  Tab(const ACount : Byte = 1): string;
begin
  result := tiReplicate(#9, ACount);
end;


{
procedure tiSortList(AData : TList; pDoCompare : TSortCompare);
  procedure _SortList(AData : TList;
                       pDoCompare : TSortCompare);
  var
    i, j : integer;
    lTemp : TObject;
    liComp : integer;
  begin
    for i := AData.Count - 1 downto 0 do
      for j := 0 to AData.Count - 2 do begin
        pDoCompare(AData.Items[j], AData.Items[j + 1], liComp);
        if liComp > 0 then begin
          lTemp := AData.Items[j];
          AData.Items[j]:= AData.Items[j + 1];
          AData.Items[j + 1]:= lTemp;
        end;
      end;
  end;

begin
  _SortList(AData, pDoCompare);
end;
}


procedure tiStringToStream(const AStr : string; const AStream : TStream);
var
  lBuffer : PChar;
  lLen : integer;
begin
  lBuffer := PChar(AStr);
  lLen := length(AStr);
  AStream.Size := 0;
  AStream.write(lBuffer^, lLen);
  AStream.Position := 0;
end;


procedure tiAppendStringToStream(const AStr : string; const AStream : TStream);
var
  lPC : PChar;
begin
  Assert(AStream <> nil, 'Stream unassigned.');
  AStream.Position := AStream.Size;
  lPC := PChar(AStr);
  AStream.WriteBuffer(lPC^, length(AStr));
end;


procedure tiInsertStringToStream(const AStr : string; const AStream : TStream; APos: Longword);
var
  LRHLength: Longword;
  LRHPChar: PChar;
  lSize: Longword;
begin
  Assert(AStream <> nil, 'Stream unassigned.');
  Assert(APos <= AStream.Size, 'Pos > AStream.Size');
  lSize := AStream.Size;
  Assert(APos <= lSize, 'Pos > AStream.Size');

  // Copy the RH portion to a string
  LRHLength:= AStream.Size - APos;
  AStream.Position:= APos;

  GetMem(LRHPChar, LRHLength);
  try
    AStream.Read(LRHPChar^, LRHLength);

    // Chop the RH portion off
    AStream.Size:= APos+1;
    AStream.Position:= APos;

    // Append the new string + the RH portion
    AStream.WriteBuffer(PChar(AStr)^, Length(AStr));
    AStream.WriteBuffer(LRHPChar^, LRHLength);
  finally
    FreeMem(LRHPChar);
  end;
end;


function  tiStreamToString(const AStream : TStream): string;
var
  lPos : integer;
begin
  lPos := AStream.Position;
  AStream.Position := 0;
  SetLength(Result,  AStream.Size);
  AStream.Read(Result[1], AStream.Size);
  AStream.Position := lPos;
end;


function  tiStreamToString(const AStream : TStream; AStart, AEnd: Longword): string;
var
  LPos: Longword;
  LEnd: Longword;
  LSize: Longword;
  LEndExt: Extended;
begin
  Assert(AStart<=AEnd, 'AStart>AEnd');

  { AStream.Size could be zero which causes a range error. }
  LEndExt := Min(AEnd, AStream.Size-1);
  if LEndExt < 0 then
    LEnd := 0
  else
    LEnd := trunc(LEndExt);

  LSize := LEnd - AStart + 1;

  if (AStart >= AStream.Size) or
     (LSize = 0) then     // LSize may never be less than 0, it is a LongWord
  begin
    Result:= '';
    Exit; //==>
  end;

  LPos := AStream.Position;
  try
    AStream.Position := AStart;
    SetLength(Result,  LSize);
    AStream.Read(Result[1], LSize);
  finally
    AStream.Position:= LPos;
  end;

end;

procedure tiFileToStream(const AFileName : string; const AStream : TStream);
var
  lStream : TMemoryStream;
begin
  lStream := TMemoryStream.Create;
  try
    lStream.LoadFromFile(AFileName);
    tiCopyStream(lStream, AStream);
  finally
    lStream.Free;
  end;
end;


function  tiStreamToFile(const AFileName : string; const AStream : TStream): string;
var
  lStream : TMemoryStream;
  lPosition: Integer;
begin
  lPosition := AStream.Position;
  try
    lStream := TMemoryStream.Create;
    try
      AStream.Position := 0;
      lStream.LoadFromStream(AStream);
      lStream.Position := 0;
      lStream.SaveToFile(AFileName);
    finally
      lStream.Free;
    end;
  finally
    AStream.Position := lPosition;
  end;
end;


procedure tiCopyStream(const AStreamFrom, AStreamTo : TStream);
begin
  AStreamFrom.Position := 0;
  AStreamTo.Size := 0;
  AStreamTo.CopyFrom(AStreamFrom, AStreamFrom.Size);
  AStreamTo.Position := 0;
  AStreamFrom.Position := 0;
end;


procedure tiListToStream(AStream : TStream;
                         AList : TtiObjectList;
                         AFieldDelim : string;
                         ARowDelim: string;
                         AColsSelected : TStringList);
var
  i, j      : integer;
  lsValue   : string;
  lFieldName : string;
  AData     : TtiBaseObject;
  lLine     : string;
  lPropType: TTypeKind;
begin
  // Write column headings
  for i := 0 to AColsSelected.Count - 1 do begin
    tiAppendStringToStream(AColsSelected.Strings[i], AStream);
    if i < AColsSelected.Count - 1 then
      tiAppendStringToStream(AFieldDelim, AStream)
    else
      tiAppendStringToStream(ARowDelim, AStream);
  end;

  // Write the data
  for i := 0 to AList.Count - 1 do
  begin
    AData := (TObject(AList.Items[i]) as TtiBaseObject);
    lLine := '';
    for j := 0 to AColsSelected.Count - 1 do
    begin
      if lLine <> '' then
        lLine := lLine + AFieldDelim;
      lFieldName := AColsSelected.Strings[j];
      if GetPropInfo(AData,lFieldName)^.PropType^.Name = 'TDateTime' then
        lsValue := tiDateTimeToStr(GetPropValue(AData,lFieldName))
      else
      begin
        lPropType := TypInfo.PropType(AData, lFieldName);
        case lPropType of
          tkChar       : lsValue := IntToStr(GetOrdProp(AData, lFieldName));
          tkWChar      : lsValue := IntToStr(GetOrdProp(AData, lFieldName));
          tkString     : lsValue := GetStrProp(AData, lFieldName);
          tkLString    : lsValue := GetStrProp(AData, lFieldName);
          tkWString    : lsValue := GetWideStrProp(AData, lFieldName);
          {$IFDEF FPC}
          tkAString    : lsValue := GetStrProp(AData, lFieldName);
          {$ENDIF}
          tkInteger    : lsValue := IntToStr(GetInt64Prop(AData, lFieldName));
          tkInt64      : lsValue := IntToStr(GetInt64Prop(AData, lFieldName));
          tkFloat      : lsValue := FloatToStr(GetFloatProp(AData, lFieldName));
          tkEnumeration : lsValue := IntToStr(GetOrdProp(AData, lFieldName));
          {$IFDEF FPC}
          tkBool       : lsValue := IntToStr(GetInt64Prop(AData, lFieldName));
          {$ENDIF}
        end;
      end;
      lLine := lLine + lsValue;
    end;
    if i <> 0 then
      lLine := ARowDelim + lLine;
    tiAppendStringToStream(lLine, AStream)
  end;
end;


procedure tiListToStream(AStream : TStream; AList : TtiObjectList);
var
  lFields : TStringList;
begin
  Assert(AStream<>nil, 'AStream not assigned');
  Assert(AList.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AList.Count > 0, 'AList.Count = 0');
  lFields := TStringList.Create;
  try
    tiGetPropertyNames(AList.Items[0], lFields);
    tiListToStream(AStream, AList, ',', CrLf, lFields);
  finally
    lFields.Free;
  end;
end;


procedure tiListToCSV(AList: TtiObjectList;
                       const AFileName: string;
                       AColsSelected: TStringList);
var
  lStream   : TFileStream;
begin
  Assert(AList.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AFileName<>'', 'AFileName not assigned');
  Assert(AColsSelected<>nil, 'AColsSelected not assigned');

  lStream := TFileStream.Create(AFileName, fmCreate);
  try
    tiListToStream(lStream, AList, ',', CrLf, AColsSelected);
  finally
    lStream.Free;
  end;
end;


procedure tiListToCSV(AList: TtiObjectList; const AFileName: string);
var
  lStream   : TFileStream;
  lFields: TStringList;
begin
  Assert(AList.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AFileName<>'', 'AFileName not assigned');
  Assert(AList.Count > 0, 'AList.Count = 0');
  lFields:= TStringList.Create;
  try
    lStream := TFileStream.Create(AFileName, fmCreate);
    try
      tiGetPropertyNames(AList.Items[0], lFields);
      tiListToStream(lStream, AList, ',', CrLf, lFields);
    finally
      lStream.Free;
    end;
  finally
    lFields.Free;
  end;
end;


procedure tiListToClipboard(AList: TtiObjectList; AColsSelected: TStringList);
var
  lStream: TStringStream;
begin
  Assert(AList.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AList.Count > 0, 'AList.Count = 0');
  Assert(AColsSelected<>nil, 'AColsSelected not assigned');
  lStream := TStringStream.Create('');
  try
    tiListToStream(lStream, AList, Tab, CrLf, AColsSelected);
    ClipBoard.AsText := lStream.DataString;
  finally
    lStream.Free;
  end;
end;


procedure tiListToClipboard(AList: TtiObjectList);
var
  lFields : TStringList;
begin
  Assert(AList.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AList.Count > 0, 'AList.Count = 0');
  lFields := TStringList.Create;
  try
    tiGetPropertyNames(AList.Items[0], lFields);
    tiListToClipboard(AList, lFields);
  finally
    lFields.Free;
  end;
end;


procedure tiStringToFile(const AText, AFileName : string);
var
  lStream : TFileStream;
  lpcText : PChar;
begin
  lStream := TFileStream.Create(AFileName, fmCreate or fmShareCompat);
  try
    lpcText := PChar(AText);
    lStream.WriteBuffer(lpcText^, length(AText));
  finally
    lStream.Free;
  end;
end;


function  tiFileToString(const AFileName : TFileName): string;
var
  lFileStream: TFileStream;
begin
  result := '';
  lFileStream := TFileStream.Create(AFileName,
                                     fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result,  lFileStream.Size);
    lFileStream.Read(Result[1], lFileStream.Size);
  finally
    lFileStream.Free;
  end;
end;


function tiHasRTTI(AObject : TObject): boolean;
var
  lClass : TClass;
begin
  lClass := AObject.ClassType;
  result := tiHasRTTI(lClass);
end;


function tiHasRTTI(AClass : TClass): boolean;
var
  lClassInfo : Pointer;
begin
  lClassInfo := AClass.ClassInfo;
  result := lClassInfo <> nil;
end;


function  tiAddEllipsis(const AString : string; AWidth : integer = 20): string;
var
  lLen : integer;
begin

  lLen := Length(AString);
  if lLen <= AWidth then
    result := AString
  else if (lLen > AWidth) then
    result := Copy(AString, 1, AWidth - 3) + '...'
  else
    result := Copy(AString, 1, lLen - 3) + '...'

end;


function  tiTrimR(const AString, ATrim : string; ACaseSensitive : boolean = false): string;
var
  li : integer;
begin
  if ACaseSensitive then
    li := pos(ATrim, AString)
  else
    li := pos(UpperCase(ATrim), UpperCase(AString));

  if li <> 0 then
    result := Copy(AString, 1, li - 1)
  else
    result := AString;
end;


function  tiTrimL(const AString, ATrim : string; ACaseSensitive : boolean = false): string;
var
  li : integer;
begin
  if ACaseSensitive then
    li := pos(ATrim, AString)
  else
    li := pos(UpperCase(ATrim), UpperCase(AString));

  if li <> 0 then
  begin
    li := li+Length(ATrim);
    result := Copy(AString, li, Length(AString)-li+1)
  end
  else
    result := AString;
end;


function  tiRemoveCrLf(const AString : string): string;
begin
  result := tiStrTran(AString, Lf, '');
  result := tiStrTran(result,  Cr, ' ');
end;


function tiTrimTrailingWhiteSpace(const AString : string): string;
const
  cWhiteSpace = [ #13, #10, #32 ];
var
  i : integer;
  lTruncChar : integer;
begin
  lTruncChar := Length(AString);
  for i := Length(AString) downto 1 do
  begin
    if AString[i] in cWhiteSpace then
      Dec(lTruncChar)
    else
      Break; //==>
  end;
  result := Copy(AString, 1, lTruncChar);
end;

// Returns true if the email address is valid
// Author/Autor: Ernesto D'Spirito <edspirito@latiumsoftware.com>
// Accompanying article at http://www.howtodothings.com/showarticle.asp?article=297
function tiIsEMailAddressValid(const AValue: string): boolean;
const
  // Valid characters in an "atom"
  atom_chars = [#33..#255] - ['(', ')', '<', '>', '@', ',', ';', ':',
                              '\', '/', '"', '.', '[', ']', #127];
  // Valid characters in a "quoted-string"
  quoted_string_chars = [#0..#255] - ['"', #13, '\'];
  // Valid characters in a subdomain
  letters = ['A'..'Z', 'a'..'z'];
  letters_digits = ['0'..'9', 'A'..'Z', 'a'..'z'];
  subdomain_chars = ['-', '0'..'9', 'A'..'Z', 'a'..'z'];
type
  States = (STATE_BEGIN, STATE_ATOM, STATE_QTEXT, STATE_QCHAR,
    STATE_QUOTE, STATE_LOCAL_PERIOD, STATE_EXPECTING_SUBDOMAIN,
    STATE_SUBDOMAIN, STATE_HYPHEN);
var
  State: States;
  i, n, subdomains: integer;
  c: char;
begin
  State := STATE_BEGIN;
  n := Length(AValue);
  i := 1;
  subdomains := 1;
  while (i <= n) do begin
    c := AValue[i];
    case State of
    STATE_BEGIN:
      if c in atom_chars then
        State := STATE_ATOM
      else if c = '"' then
        State := STATE_QTEXT
      else
        break;
    STATE_ATOM:
      if c = '@' then
        State := STATE_EXPECTING_SUBDOMAIN
      else if c = '.' then
        State := STATE_LOCAL_PERIOD
      else if not (c in atom_chars) then
        break;
    STATE_QTEXT:
      if c = '\' then
        State := STATE_QCHAR
      else if c = '"' then
        State := STATE_QUOTE
      else if not (c in quoted_string_chars) then
        break;
    STATE_QCHAR:
      State := STATE_QTEXT;
    STATE_QUOTE:
      if c = '@' then
        State := STATE_EXPECTING_SUBDOMAIN
      else if c = '.' then
        State := STATE_LOCAL_PERIOD
      else
        break;
    STATE_LOCAL_PERIOD:
      if c in atom_chars then
        State := STATE_ATOM
      else if c = '"' then
        State := STATE_QTEXT
      else
        break;
    STATE_EXPECTING_SUBDOMAIN:
      if c in letters then
        State := STATE_SUBDOMAIN
      else
        break;
    STATE_SUBDOMAIN:
      if c = '.' then begin
        inc(subdomains);
        State := STATE_EXPECTING_SUBDOMAIN
      end else if c = '-' then
        State := STATE_HYPHEN
      else if not (c in letters_digits) then
        break;
    STATE_HYPHEN:
      if c in letters_digits then
        State := STATE_SUBDOMAIN
      else if c <> '-' then
        break;
    end;
    inc(i);
  end;
  if i <= n then
    Result := False
  else
    Result := (State = STATE_SUBDOMAIN) and (subdomains >= 2);
end;


function tiIsFileNameValid(const AFileName : string): boolean;
var
  lFileName : string;
  i : integer;
const
  ExcludedChars = [ '\', '/', ':', '*', '?', '"', '<', '>', '|' ];
begin
  lFileName := ExtractFileName(AFileName);
  result :=
    (Length(lFileName) <= 255) and
    (Length(lFileName) > 0);
  if not result then
    Exit; //==>

  // From the NT help
  //A filename can contain up to 255 characters, including spaces.
  // But, it cannot contain any of the following characters:
  // \ /: * ? " < > |
  for i := 1 to Length(lFileName) do
    if  lFileName[i] in ExcludedChars then
    begin
      result := false;
      Exit; //==>
    end;
end;


function tiCheckSum(const AValue: string): Integer;
// http://www.scalabium.com/faq/dct0129.htm
// The basic algorithm:
//
// add the values of the digits in the odd positions (1, 3, 5...)
// multiply this result by 3
// add the values of the digits in the even positions (2, 4, 6...)
// sum the results of steps 2 and 3
// the check digit is the smallest number which,
// when added to the result in step 4, produces a multiple of 10.
// Small example. Assume the source data is 08137919805
//
// 1. 0+1+7+1+8+5=22
// 2. 22*3=66
// 3. 8+3+9+9+0=29
// 4. 66+29=95
// 5. 95+??=100 where ?? is a 5 (our checksum)
var
  i, intOdd, intEven: Integer;
begin
  {add all odd seq numbers}
  intOdd := 0;
  i := 1;
  while (i <= Length(AValue)) do
  begin
    Inc(intOdd, StrToIntDef(AValue[i], 0));
    Inc(i, 2);
  end;

  {add all even seq numbers}
  intEven := 0;
  i := 2;
  while (i <= Length(AValue)) do
  begin
    Inc(intEven, StrToIntDef(AValue[i], 0));
    Inc(i, 2);
  end;

  Result := 3*intOdd + intEven;
  {modulus by 10 to get}
  Result := Result mod 10;
  if Result <> 0 then
    Result := 10 - Result
end;


function tiEncodeWordBase26(const AValue : Word): String;
var
  lNum : Int64;
  lPos : byte;
begin
  if AValue = 0 then
  begin
    Result := cZeroChar;
    Exit; //==>
  end;

  Result := '     '; // If the param is changed from Word, this lenght must be changed.
  lPos := 5;
  lNum := AValue;
  While lNum > 0 Do
  Begin
    Result[lPos]:= Chr(Ord(cZeroChar) + lNum Mod cBase);
    Dec(lPos);
    lNum := lNum div cBase;
  End;
  Result := Copy(Result, lPos+1, 5 - lPos);
end;


function  tiDecodeWordBase26(AValue : String): Word;
var
  i: Integer;
begin
  Result := 0;
  If Length(AValue) = 0 Then
    Exit; //==>
  i:= 1;
  While i <= Length(AValue) Do
  Begin
    If (AValue[i] < cZeroChar) or
       ((Ord(AValue[i]) - Ord(cZeroChar)) >= cBase)
    Then
      raise EConvertError.CreateFmt(cErrorDecodeNumError, [AValue, cBase]);
    Result := Result * cBase + Ord(AValue[i]) - Ord(cZeroChar);
    Inc(i);
  End;
end;


// Cloned from IdSoapTestingUtils.pas (IndySoap) by Grahame Grieve & Andrew Cumming
function tiTestStreamsIdentical(AStream1, AStream2 : TStream; Var VMessage : string):boolean;
var
  LByte1, LByte2 : byte;
begin
  LByte1 := 0;    // stop the compiler from complaining
  LByte2 := 0;
  result := true;
  if AStream1.Size <> AStream2.Size then
  begin
    result := false;
    VMessage := 'Streams have different sizes ('+inttostr(AStream1.Size)+'/'+inttostr(AStream2.Size)+')';
    Exit; //==>
  end;

  if AStream1.Position <> AStream2.Position then
  begin
    result := false;
    VMessage := 'Streams have different positions ('+IntToStr(AStream1.Position)+'/'+IntToStr(AStream2.Position)+')';
    Exit; //==>
  end;

  while (AStream1.Size - AStream1.Position > 0) do
  begin
    AStream1.Read(LByte1, 1);
    AStream2.Read(LByte2, 1);
    if LByte1 <> LByte2 then
    begin
      result := false;
      VMessage := 'Streams Differ at position '+inttostr(AStream1.Position)+' of '+inttostr(AStream1.Size)+': '+inttostr(LByte1)+'/'+inttostr(LByte2);
      Exit; //==>
    end;
  end;
end;


function tiTestStreamsIdentical(AStream1, AStream2 : TStream):Boolean;
var
  ls: string;
begin
  Result := tiTestStreamsIdentical(AStream1, AStream2, ls);
end;

function CursorStack: TList;
begin
  if not Assigned(uCursorStack) then
    uCursorStack := TList.Create;
  Result := uCursorStack;
end;


{ TAutoCursor }

constructor TAutoCursor.Create(ANewCursor: TCursor);
begin
  inherited Create;
  // push
  CursorStack.Add(@(Screen.Cursor));
  Screen.Cursor := ANewCursor;
end;

destructor TAutoCursor.Destroy;
begin
  // pop
  Screen.Cursor := TCursor(CursorStack.Last);
  CursorStack.Delete(uCursorStack.Count-1);
  inherited;
end;

function tiAutoWaitCursor: IUnknown;
begin
  if GetCurrentThreadId = MainThreadId then
    Result := TAutoCursor.Create(crHourglass)
  else
    Result := nil;
end;

function tiAutoCursor(ACursor: TCursor = crHourglass): IUnknown;
begin
  if GetCurrentThreadId = MainThreadId then
    Result := TAutoCursor.Create(ACursor);
end;

{ TtiBruteForceNoFlicker }
{$IFDEF MSWINDOWS}
constructor TtiBruteForceNoFlicker.Create(AControlToMask: TControl);
var
  ScreenPt: TPoint;
begin
  inherited Create(nil);
  FMaskControl := AControlToMask;
  BoundsRect := FMaskControl.BoundsRect;

  ScreenPt := FMaskControl.ClientToScreen(Classes.Point(0,0));

  FControlSnapshot := TBitmap.Create;

  ScreenShot(FControlSnapshot, ScreenPt.X, ScreenPt.Y, Width, Height, HWND_DESKTOP);

  Parent := FMaskControl.Parent;

  Update;
end;

procedure TtiBruteForceNoFlicker.CreateParams(var AParams: TCreateParams);
begin
  inherited;
  AParams.Style := WS_CHILD or WS_CLIPSIBLINGS;
  AParams.ExStyle := WS_EX_TOPMOST;
end;

destructor TtiBruteForceNoFlicker.Destroy;
begin
  FreeAndNil(FControlSnapshot);
  inherited;
end;

procedure TtiBruteForceNoFlicker.Paint;
begin
end;

// From Jedi JCL JCLGraphics.pas
procedure TtiBruteForceNoFlicker.ScreenShot(ABitmap: TBitmap; ALeft, ATop, AWidth, AHeight: Integer; AWindow: HWND);
var
  WinDC: HDC;
  Pal: TMaxLogPalette;
begin
  ABitmap.Width := AWidth;
  ABitmap.Height := AHeight;

  // Get the HDC of the window...
  WinDC := GetDC(AWindow);
  try
    if WinDC = 0 then
      raise Exception.Create('No DeviceContext For Window');

    // Palette-device?
    if (GetDeviceCaps(WinDC, RASTERCAPS) and RC_PALETTE) = RC_PALETTE then
    begin
      FillChar(Pal, SizeOf(TMaxLogPalette), #0);  // fill the structure with zeros
      Pal.palVersion := $300;                     // fill in the palette version

      // grab the system palette entries...
      Pal.palNumEntries := GetSystemPaletteEntries(WinDC, 0, 256, Pal.palPalEntry);
      if Pal.PalNumEntries <> 0 then
        {$IFDEF FPC}
        ABitmap.Palette := CreatePalette(LPLOGPALETTE(@Pal)^);
        {$else}
        ABitmap.Palette := CreatePalette(PLogPalette(@Pal)^);
        {$endif}
    end;

    // copy from the screen to our bitmap...
    BitBlt(ABitmap.Canvas.Handle, 0, 0, AWidth, AHeight, WinDC, ALeft, ATop, SRCCOPY);
  finally
    ReleaseDC(AWindow, WinDC);        // finally, relase the DC of the window
  end;
end;

procedure TtiBruteForceNoFlicker.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  BitBlt(
    Message.DC,
    0,0, Width,Height,
    FControlSnapshot.Canvas.Handle,
    0,0,
    SRCCOPY);
end;
{$ENDIF MSWINDOWS}

function tiStrPos(const AString, ASubString: PChar): PChar;
  {
  From Paul Spain. paul@xpro.com.au
  You may remember during the XML optimisation project that there was a
  method where I couldn't get any performance benefit over a string-based
  method that was using quite a few Copy() calls? What, it's not just like
  yesterday for you?;-)

  Well, I finally figured out why last night after trying to optimise
  StringReplace(). It turns out that the RTL implementation of StrPos() is
  a dog. Written in assembler, but a dog nonetheless. If you find any
  calls to StrPos() in the XML code, you can crank some more from it using
  this implementation:
 }
var
 sub, str: PChar;
begin
  {$IFDEF FPC}
  { Free Pascal's implementation of StrPos is even faster than the tiStrPos
    version, so we rather call the Free Pascal one. }
  result := StrPos(AString, ASubString);
  {$ELSE}
  if (ASubString <> nil) and (ASubString^ <> #0) and (AString <> nil) then
  begin
     str := AString;
     while str^ <> #0 do
     begin
       sub := ASubString;
       // look for start of sub in str or end of str
       while (str^ <> sub^) and (str^ <> #0) do
         Inc(str);
       // check end of str
       if str^ <> #0 then
       begin
         // remember start of sub in str
         Result := str;
         // look for end of sub in str or end of sub or end of str
         while (str^ = sub^) and (str^ <> #0) do
         begin
           Inc(str);
           Inc(sub);
         end;
         // success if sub^ = #0
         if sub^ = #0 then
           exit;
       end;
     end;
   end;
   Result := nil;
  {$ENDIF}
end;


function tiNormalizeStr(const AString: string): string;
var
  s: string;

  {$IFNDEF FPC}
  function DelSpace1(const S: string): string;
  var
    i: Integer;
  begin
    Result:=S;
    for i:=Length(Result) downto 2 do
      if (Result[i]=' ') and (Result[I-1]=' ') then
        Delete(Result,I,1);
  end;
  {$ENDIF}

begin
  s := Trim(AString);
  s := tiStrTran(s, #9, ' ');   // replace Tabs
  s := tiStrTran(s, #10, ' ');  // replace LF
  s := tiStrTran(s, #13, ' ');  // replace CR
  Result := DelSpace1(s);
end;


function tiDateTimeAsXMLString(const ADateTime: TDateTime): string;
  function _IntToStr(AValue, pSize : integer): string;
  begin
    result := IntToStr(AValue);
    result := tiPad0(result, pSize);
  end;
var
  lY, lM, lD, lH, lMi, lS, lMs : Word;
begin
    DecodeDate(ADateTime, lY, lM, lD);
    DecodeTime(ADateTime, lH, lMi, lS, lMs);
    {$IFDEF DATEFORMAT_YYYYMMDD}
    Result :=
      _IntToStr(lY, 4) + '-' +    // NB '-' separator deliberate
      _IntToStr(lM, 2) + '-' +
      _IntToStr(lD, 2) + ' ' +
      _IntToStr(lH, 2) + ':' +
      _IntToStr(lMi, 2) + ':' +
      _IntToStr(lS, 2) + ':' +
      _IntToStr(lMs, 3);
    {$ELSE}
    Result :=
      _IntToStr(lD, 2) + '/' +
      _IntToStr(lM, 2) + '/' +
      _IntToStr(lY, 4) + ' ' +
      _IntToStr(lH, 2) + ':' +
      _IntToStr(lMi, 2) + ':' +
      _IntToStr(lS, 2) + ':' +
      _IntToStr(lMs, 3);
    {$ENDIF}

  { What about using the ISO 8601 standard!  See cISODateTimeFmt1 in
    tiConstants.
    Also, why not use FormatDateTime()? }
end;

function tiXMLStringToDateTime(const AValue : string): TDateTime;
var
  lY, lM, lD, lH, lMi, lS, lMs : Word;
begin
  if AValue = '' then
  begin
    Result := 0;
    Exit; //==>
  end;

  try
    //          1         2
    // 12345678901234567890123
    // YYYY/MM/DD HH:MM:SS:NN   (legacy format)
    // DD/MM/YYYY HH:MM:SS:NN   (current format - see function above)
    if  (Length(AValue) > 3)
    and (AValue[3] <> '/') then // assume lecacy format
    begin
      lY := StrToInt(Copy(AValue, 1, 4));
      lM := StrToInt(Copy(AValue, 6, 2));
      lD := StrToInt(Copy(AValue, 9, 2));
    end
    else
    begin
      lD := StrToInt(Copy(AValue, 1, 2));
      lM := StrToInt(Copy(AValue, 4, 2));
      lY := StrToInt(Copy(AValue, 7, 4));
    end;

    lH := StrToInt(Copy(AValue, 12, 2));
    lMi := StrToIntDef(Copy(AValue, 15, 2), 0);
    lS := StrToIntDef(Copy(AValue, 18, 2), 0);
    lMs := StrToIntDef(Copy(AValue, 21, 3), 0);
    result :=
      EncodeDate(lY, lM, lD) +
      EncodeTime(lH, lMi, lS, lMs);
  except
    on e:Exception do
      raise Exception.CreateFmt(cErrorXMLStringToDate, [AValue, e.message]);
  end;
end;


function tiDateTimeAsIntlDateStor(const ADateTime: TDateTime): string;
begin
  Result := FormatDateTime(cIntlDateTimeStor, ADateTime);
  if Pos('18991230', Result) = 1 then
    Result := tiStrTran(Result, '18991230', '00000000');
end;


function tiDateTimeAsIntlDateDisp(const ADateTime: TDateTime): string;
begin
  Result := FormatDateTime(cIntlDateTimeDisp, ADateTime);
  if Pos('1899-12-30', Result) = 1 then
    Result := tiStrTran(Result, '1899-12-30', '0000-00-00');
end;


function tiIntlDateStorAsDateTime(const AValue: string): TDateTime;
var
  lY, lM, lD, lH, lMi, lS: Word;
begin
  if Trim(AValue) = '' then
  begin
    Result := 0;
    Exit; //==>
  end;
  
    //          1         2
    // 12345678901234567890123
    // yyyymmddThhmmss
  lY := StrToInt(Copy(AValue, 1, 4));
  lM := StrToInt(Copy(AValue, 5, 2));
  lD := StrToInt(Copy(AValue, 7, 2));
  lH := StrToInt(Copy(AValue, 10, 2));
  lMi := StrToInt(Copy(AValue, 12, 2));
  lS := StrToInt(Copy(AValue, 14, 2));
  
  { Cannot EncodeDate if any part equals 0. EncodeTime is okay. }
  if (lY = 0) or (lM = 0) or (lD = 0) then
    Result := EncodeTime(lH, lMi, lS, 0)
  else
    Result := EncodeDate(lY, lM, lD) + EncodeTime(lH, lMi, lS, 0);
end;


function tiIntlDateDispAsDateTime(const AValue: string): TDateTime;
var
  lY, lM, lD, lH, lMi, lS: Word;
begin
  if Trim(AValue) = '' then
  begin
    Result := 0;
    Exit; //==>
  end;
  
    //          1         2
    // 12345678901234567890123
    // yyyy-mm-dd hh:mm:ss
  lY := StrToInt(Copy(AValue, 1, 4));
  lM := StrToInt(Copy(AValue, 6, 2));
  lD := StrToInt(Copy(AValue, 9, 2));
  lH := StrToInt(Copy(AValue, 12, 2));
  lMi := StrToInt(Copy(AValue, 15, 2));
  lS := StrToInt(Copy(AValue, 18, 2));
  
  { Cannot EncodeDate if any part equals 0. EncodeTime is okay. }
  if (lY = 0) or (lM = 0) or (lD = 0) then
    Result := EncodeTime(lH, lMi, lS, 0)
  else
    Result := EncodeDate(lY, lM, lD) + EncodeTime(lH, lMi, lS, 0);
end;


procedure tiConsoleAppPause;
begin
  WriteLn('');
  WriteLn('Press <Enter> to continue...');
  ReadLn;
end;


function tiCreateGUIDString: string;
{$IFDEF FPC}
var
  lGUID: TGUID;
{$ENDIF}
begin
{$IFDEF FPC}
  CreateGUID(lGUID);
  Result := GUIDToString(lGUID);
{$ELSE}
  Result := tiWin32CoCreateGUID;
{$ENDIF}
end;


function tiGetTickCount: Cardinal;
begin
{$IFDEF MSWINDOWS}
  Result := tiWin32GetTickCount;
{$ENDIF}
{$IFDEF LINUX}
  Result := tiLinuxGetTickCount;
{$ENDIF}
end;


{ TtiIntegerList }

procedure TtiIntegerList.Add(AValue: Int64);
var
  L: TtiIntegerListItem;
begin
  L:= TtiIntegerListItem.Create;
  L.AValue:= AValue;
  FList.Add(L);
end;


constructor TtiIntegerList.Create;
begin
  inherited Create;
  FList:= TObjectList.Create(True);
end;


destructor TtiIntegerList.Destroy;
begin
  FList.Free;
  inherited;
end;


function TtiIntegerList.GetCount: Integer;
begin
  Result := FList.Count;
end;


function TtiIntegerList.GetItems(i: Integer): Int64;
begin
  Result := (FList.Items[i] as TtiIntegerListItem).AValue;
end;


function TtiIntegerList.IndexOf(AValue: Int64): Integer;
var
  i: Integer;
begin
  for i:= 0 to FList.Count-1 do
    if (FList.Items[i] as TtiIntegerListItem).AValue = AValue then
    begin
      Result:= i;
      Exit; //==>
    end;
  Result:= -1;
end;


function TtiIntegerList.Remove(AValue: Int64): Integer;
var
  LIndex: Integer;
begin
  LIndex:= IndexOf(AValue);
  if LIndex = -1 then
    raise EtiOPFDataException.CreateFmt(
      'Attempt to remove an integer from %s that does not exist: "%d"',
      [ClassName, AValue]);
  FList.Delete(LIndex);
  Result:= LIndex;
end;


procedure TtiIntegerList.SetItems(i: Integer; const AValue: Int64);
begin
  (FList.Items[i] as TtiIntegerListItem).AValue := AValue;
end;


(* *****************************************************************************

 ROUTINES FOR ROUNDING IEEE-754 FLOATS TO SPECIFIED NUMBER OF DECIMAL FRACTIONS

  These routines round input values to fit as closely as possible to an
  output number with desired number of decimal fraction digits.

  Because, in general, numbers with decimal fractions cannot be exactly
  represented in IEEE-754 floating binary point variables, error limits
  are used to determine if the input numbers are intended to represent an
  exact decimal fraction rather than a nearby value.   Thus an error limit
  will be taken into account when deciding that a number input such as
  1.295, which internally is represented 1.29499 99999
  be considered exactly 1.295 and that 0.29999 99999 ... really should
  be interpreted as 0.3 when applying the rounding rules.

  Some terminology:
    "NDFD" is used for Number of Decimal Fraction Digits.  If NDFD is
        negative, then the inputs will be rounded so that there are zeros
        on the left of the decimal point.  I.E. if NDFD = -3, then the
        output will be rounded to an integral multiple of a thousand.
    "MaxRelError" designates the maximum relative error to be allowed in the
        input values when deciding they are supposed to represent exact
        decimal fractions (as mentioned above).
    "Ctrl" determines the type of rounding to be done.  Nine kinds of
        rounding (plus no rounding) are defined.  They include almost
        every kind of rounding known.  See the definition of
        tDecimalRoundingCtrl below for the specific types.

 Rev. 2002.01.14 by John Herbster to add overloaded IsNaN() functions to
     solve problem with invalid operation when loading values for test.
 Rev. 2001.11.24 by John Herbster to add KnownErrorLimit, SafetyFactor,
     and MaxRelErrXxx constants. Also corrected DecimalRoundingCtrlStrs
     and comments per feedback from Marjan Venema.
 Pgm. 2001.09.19 by John Herbster.  Please send feedback and suggestions
     to author at herb-sci@swbell.net.

 Dedicated to the participants in the borland.public.delphi.objectpascal
     forum.  With special thanks to Brad White for his proofing and
     suggestions.

  http://cc.borland.com/Item.aspx?id=21909

***************************************************************************** *)
{ The following "epsilon" values are representative of the resolution of the
  floating point numbers divided by the number being represented.
  These constants are supplied to the rounding routines to determine how much
  correction should be allowed for the natural errors in representing
  decimal fractions.  Using 2 times or higher multiples of these values may
  be advisable if the data have been massaged through arithmetic
  calculations. }
Const
  SglEps = 1.1920928955e-07;
  DblEps = 2.2204460493e-16;
  ExtEps = 1.0842021725e-19;
{ These epsilon values are smallest for which "1 + epsilon <> 1".
      For "1 - epsilon <> 1", divide the following values by 2. }

  KnownErrorLimit = 1.234375;
  SafetyFactor = 2;
  MaxRelErrSgl = SglEps * KnownErrorLimit * SafetyFactor;
  MaxRelErrDbl = DblEps * KnownErrorLimit * SafetyFactor;
  MaxRelErrExt = ExtEps * KnownErrorLimit * SafetyFactor;
{ If MaxRelErr < XxxEps * KnownErrorLimit then errors can occur. }

{ The following DecimalRound function is for doing the best possible job of
  rounding floating binary point numbers to the specified NDFD.  MaxRelError
  is the maximum relative error that will be allowed when determining the
  cut points for applying the rounding rules. }
Function tiDecimalRound(AValue: extended; NDFD: integer; MaxRelErr: double;
                         Ctrl: TtiDecimalRoundingCtrl = drHalfEven): extended;
var i64, j64: Int64; j: integer; m, ScaledVal, ScaledErr: extended;
begin

  If IsNaN(AValue) or (Ctrl = drNone)
    then begin Result := AValue; EXIT end;

{ Compute 10^NDFD and scale the AValue and MaxError: }
  m := 1; For j := 1 to abs(NDFD) do m := m*10;
  If NDFD >= 0
    then begin
      ScaledVal := AValue * m;
      ScaledErr := abs(MaxRelErr*AValue) * m;
      end
    else begin
      ScaledVal := AValue / m;
      ScaledErr := abs(MaxRelErr*AValue) / m;
      end;

{ Do the diferent basic types separately: }
  Case Ctrl of
    drHalfEven: begin
      i64 := round((ScaledVal - ScaledErr));
      j64 := round((ScaledVal + ScaledErr));
      if Odd(i64)
        then i64 := j64;
      end;
    drHalfDown:  {Round to nearest or toward zero.}
      i64 := round((abs(ScaledVal) - ScaledErr));
    drHalfUp:    {Round to nearest or away from zero.}
      i64 := round((abs(ScaledVal) + ScaledErr));
    drHalfPos:   {Round to nearest or toward positive.}
      i64 := round((ScaledVal + ScaledErr));
    drHalfNeg:   {Round to nearest or toward negative.}
      i64 := round((ScaledVal - ScaledErr));
    drRndNeg:    {Truncate toward negative. (a.k.a. Floor)}
      i64 := round((ScaledVal + (ScaledErr - 1/2)));
    drRndPos:    {Truncate toward positive. (a.k.a. Ceil)}
      i64 := round((ScaledVal - (ScaledErr - 1/2)));       {}
    drRndDown:   {Truncate toward zero (a.k.a. Trunc).}
      i64 := round((abs(ScaledVal) + (ScaledErr - 1/2)));
    drRndUp:     {Truncate away from zero.}
      i64 := round((abs(ScaledVal) - (ScaledErr - 1/2)));
    else i64 := round(ScaledVal);
    end{cases};

{ Finally convert back to the right order: }
  If NDFD >= 0
    then Result := i64 / m
    else Result := i64 * m;

  If (Ctrl in [drHalfDown,drHalfUp,drRndDown,drRndUp]) and
     (AValue < 0)
    then Result := -Result;

end{DecimalRound};

{ The following functions have a two times "epsilon" error built in for the
  single, double, and extended argument respectively: }
Function tiDecimalRoundSgl(AValue: single; ANDFD: integer;
                         ACtrl: TtiDecimalRoundingCtrl = drHalfUp): extended;
begin
  Result := tiDecimalRound(AValue, ANDFD, MaxRelErrSgl, ACtrl);
end;

Function tiDecimalRoundDbl(AValue: double; ANDFD: integer;
                         ACtrl: TtiDecimalRoundingCtrl = drHalfUp): extended;
begin
  Result := tiDecimalRound(AValue, ANDFD, MaxRelErrDbl, ACtrl);
end;

Function tiDecimalRoundExt(AValue: extended; ANDFD: integer;
                         ACtrl: TtiDecimalRoundingCtrl = drHalfUp): extended;
begin
  Result := tiDecimalRound(AValue, ANDFD, MaxRelErrExt, ACtrl);
end;

function tiSetPrecision(const AValue : Extended; const APrecision : integer = 3): Extended;
begin
  Result:= tiDecimalRoundDbl(AValue, APrecision);
end;


initialization
  // Set Windows internal date and time format strings
//  ShortDateFormat := csWinDateFormat;
//  ShortTimeFormat := csWinTimeFormat;
finalization
  FreeAndNil(uCursorStack);
end.


































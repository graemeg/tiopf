program demo_23;

{$ifdef FPC}
  {$mode objfpc}{$h+}
{$endif}

{
  HOW TO SWITCH PERSISTENCE LAYERS
  ================================
  It is done purely via Compiler Defines. Enable the appropriate LINK_*
  compiler define globally - that includes the tiOPF source code.

  The choices here are:
    LINK_SQLDB_IB
    LINK_SQLDB_SQLite3
}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils,
  tiOPFManager, tiConstants, tiLog,
  tiCommandLineParams, tiLogToConsole, tiLogToFile,
  tiQuery,
  tiPersistenceLayers,
  FPimage,
  book;

var
  BookList: TBookList;
  NeedData: Boolean = False;

const
  {$ifdef LINK_SQLDB_IB}
  cPersistentLayerName = cTIPersistSqldbIB;
  cDatabase = '../_Data/demo23.fdb';
  cUsername = 'sysdba';
  cPassword = 'masterkey';
  {$endif}
  {$ifdef LINK_SQLDB_SQLite3}
  cPersistentLayerName = cTIPersistSqldbSQLite3;
  cDatabase = '../_Data/demo23.sqlite3';
  cUsername = '';
  cPassword = '';
  {$endif}


procedure AddBook(Name: String;Author: String; ImageFile: String = '');
var
  Book: TBook;
  Stream: TFileStream;
  lReader: TFPCustomImageReader;
begin
  Book := TBook.CreateNew;
  Book.Name:=Name;
  Book.Author:=Author;
  Book.ProcessedDate := Now();
  Book.SomeIntProp := Random(100);
  if (ImageFile <> '') and FileExists(ImageFile) then
  begin
    Stream := TFileStream.Create(ImageFile, fmOpenReadWrite);
    lReader := ImageHandlers.ImageReader['Portable Network Graphics'].Create;
    Book.Image:=lReader.ImageRead(Stream, nil);
    Stream.Free;
    lReader.Free;
  end
  else
    Book.Image := nil;
  BookList.Add(Book);
end;


procedure CreateDatabase;
var
  PerLayer: TtiPersistenceLayer;
begin
  PerLayer := GTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(cPersistentLayerName);
  if not PerLayer.DatabaseExists(cDatabase, cUsername, cPassword) then
    PerLayer.CreateDatabase(cDatabase, cUsername, cPassword);
end;

procedure CreateTables;
var
  DBMetadata: TtiDBMetaData;
  Database: TtiDatabase;
  Table: TtiDBMetaDataTable;
  NeedTAble: Boolean;
begin
  DBMetaData:= TtiDBMetaData.Create;
  try
    Database := GTIOPFManager.DefaultDBConnectionPool.Lock;
    try
      Database.ReadMetaDataTables(DBMetadata);
      NeedTable := DBMetadata.FindByTableName('Book') = nil;
    finally
      GTIOPFManager.DefaultDBConnectionPool.UnLock(Database);
    end;

  finally
    DBMetadata.Free;
  end;
  if NeedTable then
  begin
    // Create Table
    Table := TtiDBMetaDataTable.Create;
    try
      Table.Name:='Book';
      Table.AddInstance('BK_OID', qfkString, 36);
      Table.AddInstance('BK_NAME', qfkString, 50);
      Table.AddInstance('BK_AUTHOR', qfkString, 40);
      Table.AddInstance('BK_INTPROP', qfkInteger);
      Table.AddInstance('BK_DATE_PROCESSED', qfkString, 15);
      Table.AddInstance('BK_IMAGE', qfkBinary);
      GTIOPFManager.CreateTable(Table);
    finally
      Table.Free;
    end;
    NeedData := True;
  end;
end;


procedure PrintFilter;
begin
  Log('Clear the list and load it again using some filter criteria', lsDebug);
  BookList.Clear;
  BookList.Criteria.ClearAll;
  BookList.Criteria.AddLike('Author', 'Patrick%');
  BookList.Read;
end;

var
  i: integer;

begin
  WriteLn('Available persistence layers:');
  for i := 0 to GTIOPFManager.PersistenceLayers.Count-1 do
    writeln('  - ', GTIOPFManager.PersistenceLayers[i].PersistenceLayerName);
  writeln('');

  { just to make sure the persistence layer we expected is compiled in }
  if GTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(cPersistentLayerName) = nil then
    raise Exception.Create('The system failed to find the <' + cPersistentLayerName + '> persistence layer');
  GTIOPFManager.DefaultPersistenceLayerName := cPersistentLayerName;
  GTIOPFManager.ConnectDatabase(cDatabase, cUsername, cPassword, '');

  { define what we want to appear in the log output. }
  gLog.SevToLog :=  [
    lsNormal
    ,lsUserInfo
//    ,lsObjCreation
    ,lsVisitor
//    ,lsConnectionPool
    ,lsAcceptVisitor
//    ,lsQueryTiming
    ,lsDebug
    ,lsWarning
    ,lsError
    ,lsSQL
    ];

  { Logging is enabled via various command line parameters }
  if gCommandLineParams.IsParam(csLogConsole) then
    gLog.RegisterLog(TtiLogToConsole);
  if gCommandLineParams.IsParam(csLog) then
    gLog.RegisterLog(TtiLogToFile.CreateWithFileName('.',ApplicationName+'.log', True));


  CreateDatabase; // if not exist
  CreateTables;  // if not exist
  BookList := TBookList.Create;
  Log('Before: BookList.Count = ' + IntToStr(BookList.Count), lsDebug);
  BookList.Read;
  Log('After: BookList.Count = ' + IntToStr(BookList.Count), lsDebug);

  if NeedData then // true if we just created the table
  begin
    Log('Inside NeedData block', lsDebug);
    // Add some rows
    AddBook('Gone with the wind', 'Some P. Author', 'test.png');
    AddBook('Name of the Wind', 'Patrick Rothfuss');
    AddBook('The Last Ship', 'Graeme Geldenhuys');
    AddBook('The Story Book', 'Andrew Haines');
    BookList.Save;
  end;

  BookList.SortByProps(['Author']);
  // show data load - no criteria was used
  WriteLn('============== No criteria ======================');
  WriteLn(BookList.AsDebugString);

  WriteLn('============== Images? ==========================');
  // check if any have images
  for  i := 0 to BookList.Count-1 do
  begin
    if BookList.Items[i].Image <> nil then
      WriteLn(Format('"%s" has image! %dx%d', [BookList.Items[i].Name, BookList.Items[i].Image.Width, BookList.Items[i].Image.Width]));
  end;

  // clear list and load data via filter criteria
  PrintFilter;
  Log('BookList.Count = ' + IntToStr(BookList.Count), lsDebug);
  WriteLn('============== With criteria ======================');
  WriteLn(BookList.AsDebugString);

  BookList.Free;
end.


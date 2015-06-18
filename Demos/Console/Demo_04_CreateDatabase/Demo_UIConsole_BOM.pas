unit Demo_UIConsole_BOM;

{$I tiDefines.inc}

interface

uses
  Classes
  ,SysUtils
  ,contnrs
  ,tiBaseObject
  ,tiObject
  ;

type
  TDemoUIConsole = class(TtiBaseObject)
  private
    FAppObject: TtiObject;
    FCommandList: TObjectList;
    procedure   ProcessCommand(const ACommand: string; out AExit: boolean);
    procedure   ParseCommand(const AInput: string; out ACommand, AParams: string);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Execute;
    procedure   RegisterCommand(const ACommand: TObject);
    property    AppObject: TtiObject read FAppObject write FAppObject;
  end;

  TUIConsoleCommand = class(TtiBaseObject)
  public
    function    CanExecute(const ACommand: string): boolean; virtual; abstract;
    procedure   Execute(const AAppObject: TtiObject; const AParams: string); virtual; abstract;
  end;


  TUIConsoleCommandHelp = class(TUIConsoleCommand)
  public
    function    CanExecute(const ACommand: string): boolean; override;
    procedure   Execute(const AAppObject: TtiObject; const AParams: string); override;
  end;

  TUIConsoleCommandCLS = class(TUIConsoleCommand)
  public
    function    CanExecute(const ACommand: string): boolean; override;
    procedure   Execute(const AAppObject: TtiObject; const AParams: string); override;
  end;


implementation

uses
  tiOPFManager
  ,tiUtils
  ,tiConstants
  ;

{ TDemoUIConsole }

procedure TDemoUIConsole.ProcessCommand(const ACommand: string; out AExit: boolean);
var
  i: integer;
  LCommand: string;
  LParams: string;
//  LAdrsBook: TAdrsBook;
begin
  AExit:= SameText(ACommand, 'e') or SameText(ACommand, 'q');
  if AExit then
    Exit; //==>

  ParseCommand(ACommand, LCommand, LParams);

//  LAdrsBook:= TAdrsBook.Create;
  try
//    LAdrsBook.Read;
    try
      for i := 0 to FCommandList.Count - 1 do
        if (FCommandList.Items[i] as TUIConsoleCommand).CanExecute(LCommand) then
        begin
          (FCommandList.Items[i] as TUIConsoleCommand).Execute(AppObject, LParams);
          Exit; //==>
        end;
    except
      on e:exception do
      begin
        WriteLn('');
        WriteLn(e.message);
        WriteLn('');
      end;
    end;
  finally
//    LAdrsBook.Free;
  end;
end;

procedure TDemoUIConsole.ParseCommand(const AInput: string; out ACommand, AParams: string);
begin
  ACommand:= tiToken(AInput, ' ', 1);
  AParams:= Trim(Copy(AInput, Length(ACommand) + 1, Length(AInput) - 1));
end;

constructor TDemoUIConsole.Create;
begin
  inherited Create;
  FAppObject := nil;
//  Adrs_SrvAutoMap.RegisterMappings;
//  GTIOPFManager.ConnectDatabase('adrs.fdb', 'SYSDBA', 'masterkey');

//  GTIOPFManager.DefaultPersistenceLayerName:= CTIPersistRemote;
//  GTIOPFManager.ConnectDatabase('adrs', 'http://localhost:8088', '', '', '', '');

  FCommandList:= TObjectList.Create;
//  FCommandList.Add(TUIConsoleCommandList.Create);
//  FCommandList.Add(TAdrsBookConsoleCommandAdd.Create);
//  FCommandList.Add(TAdrsBookConsoleCommandDelete.Create);
  FCommandList.Add(TUIConsoleCommandCLS.Create);
  FCommandList.Add(TUIConsoleCommandHelp.Create);
end;

destructor TDemoUIConsole.Destroy;
begin
  FCommandList.Free;
  inherited Destroy;
end;

procedure TDemoUIConsole.Execute;
var
  LExit: boolean;
  LCommand: string;
begin
  ProcessCommand('c', LExit);
  LExit:= True;
  repeat
    WriteLn('');
    Write('Enter command >');
    ReadLn(LCommand);
    ProcessCommand(LCommand, LExit);
  until LExit;
end;

procedure TDemoUIConsole.RegisterCommand(const ACommand: TObject);
begin
  FCommandList.Add(ACommand);
end;

{ TUIConsoleCommandHelp }

function TUIConsoleCommandHelp.CanExecute(const ACommand: string): boolean;
begin
  result:= SameText(ACommand, 'h');
end;

procedure TUIConsoleCommandHelp.Execute(const AAppObject: TtiObject; const AParams: string);
begin
  WriteLn('h                                     - Describe the commands');
  WriteLn('l                                     - List all entries');
  WriteLn('a <Title>, <First Name>, <Last Name>  - Add a new entry');
  WriteLn('d <Index number>                      - Delete an entry');
  WriteLn('c                                     - Clear the screen');
  WriteLn('e or q                                - Quit the application');
end;

{ TUIConsoleCommandCLS }

function TUIConsoleCommandCLS.CanExecute(const ACommand: string): boolean;
begin
  result:= SameText(ACommand, 'c');
end;

procedure TUIConsoleCommandCLS.Execute(const AAppObject: TtiObject; const AParams: string);
var
  i: integer;
begin
  for i := 1 to 24 do
    WriteLn;
//  WriteLn(ConnectionDetailsAsString);
end;



end.


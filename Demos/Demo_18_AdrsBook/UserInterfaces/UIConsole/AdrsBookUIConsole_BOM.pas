unit AdrsBookUIConsole_BOM;

interface
uses
  tiBaseObject,
  Contnrs,
  Adrs_BOM;

type

  TAdrsBookConsoleCommand = class(TtiBaseObject)
  public
    function CanExecute(const ACommand: string): boolean; virtual; abstract;
    procedure Execute(
      const AAdrsBook: TAdrsBook; const AParams: string); virtual; abstract;
  end;

  TAdrsBookConsoleCommandList = class(TAdrsBookConsoleCommand)
  public
    function CanExecute(const ACommand: string): boolean; override;
    procedure Execute(
      const AAdrsBook: TAdrsBook; const AParams: string); override;
  end;

  TAdrsBookConsoleCommandAdd = class(TAdrsBookConsoleCommand)
  public
    function CanExecute(const ACommand: string): boolean; override;
    procedure Execute(
      const AAdrsBook: TAdrsBook; const AParams: string); override;
  end;

  TAdrsBookConsoleCommandDelete = class(TAdrsBookConsoleCommand)
  public
    function CanExecute(const ACommand: string): boolean; override;
    procedure Execute(
      const AAdrsBook: TAdrsBook; const AParams: string); override;
  end;

  TAdrsBookConsoleCommandCLS = class(TAdrsBookConsoleCommand)
  public
    function CanExecute(const ACommand: string): boolean; override;
    procedure Execute(
      const AAdrsBook: TAdrsBook; const AParams: string); override;
  end;

  TAdrsBookConsoleCommandHelp = class(TAdrsBookConsoleCommand)
  public
    function CanExecute(const ACommand: string): boolean; override;
    procedure Execute(
      const AAdrsBook: TAdrsBook; const AParams: string); override;
  end;


  TAdrsBookUIConsole = class(TtiBaseObject)
  private
    FAdrsBook: TAdrsBook;
    FCommandList: TObjectList;
    procedure ProcessCommand(const ACommand: string; out AExit: boolean);
    procedure ParseCommand(const AInput: string; out ACommand, AParams: string);

  public
    constructor Create;
    destructor  Destroy; override;
    procedure Execute;
  end;

implementation
uses
  tiOPFManager,
  tiUtils,
  tiQueryIBX,
  Adrs_SrvAutoMap,
  SysUtils;

{ TAdrsBookUIConsole }

constructor TAdrsBookUIConsole.Create;
begin
  inherited;
  Adrs_SrvAutoMap.RegisterMappings;
  GTIOPFManager.ConnectDatabase('adrs.fdb', 'SYSDBA', 'masterkey');
  FAdrsBook:= TAdrsBook.Create;
  FAdrsBook.Read;
  FCommandList:= TObjectList.Create;
  FCommandList.Add(TAdrsBookConsoleCommandList.Create);
  FCommandList.Add(TAdrsBookConsoleCommandAdd.Create);
  FCommandList.Add(TAdrsBookConsoleCommandDelete.Create);
  FCommandList.Add(TAdrsBookConsoleCommandCLS.Create);
  FCommandList.Add(TAdrsBookConsoleCommandHelp.Create);
end;

destructor TAdrsBookUIConsole.Destroy;
begin
  FAdrsBook.Free;
  FCommandList.Free;
  inherited;
end;

procedure TAdrsBookUIConsole.Execute;
var
  LExit: boolean;
  LCommand: string;
begin
  ProcessCommand('CLS', LExit);
  LExit:= True;
  repeat
    WriteLn('');
    Write('Enter command >');
    ReadLn(LCommand);
    ProcessCommand(LCommand, LExit);
  until LExit;
end;

procedure TAdrsBookUIConsole.ParseCommand(const AInput: string; out ACommand,
  AParams: string);
begin
  ACommand:= tiToken(AInput, ' ', 1);
  AParams:= Trim(Copy(AInput, Length(ACommand) + 1));
end;

procedure TAdrsBookUIConsole.ProcessCommand(const ACommand: string;
  out AExit: boolean);
var
  i: integer;
  LCommand: string;
  LParams: string;
begin
  AExit:= SameText(ACommand, 'exit') or SameText(ACommand, 'quit') or
          SameText(ACommand, 'e')    or SameText(ACommand, 'q');
  if AExit then
    Exit; //==>

  ParseCommand(ACommand, LCommand, LParams);

  try
    for i := 0 to FCommandList.Count - 1 do
      if (FCommandList.Items[i] as TAdrsBookConsoleCommand).CanExecute(LCommand) then
      begin
        (FCommandList.Items[i] as TAdrsBookConsoleCommand).Execute(FAdrsBook, LParams);
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
end;

{ TAdrsBookConsoleCommandList }

function TAdrsBookConsoleCommandList.CanExecute(
  const ACommand: string): boolean;
begin
  result:= SameText(ACommand, 'list') or SameText(ACommand, 'l');
end;

procedure TAdrsBookConsoleCommandList.Execute(const AAdrsBook: TAdrsBook; const AParams: string);
var
  i: integer;
begin
  for i := 0 to AAdrsBook.PersonList.Count - 1 do
    WriteLn(IntToStr(i) + ' ' + AAdrsBook.PersonList.Items[i].Caption);
end;

{ TAdrsBookConsoleCommandAdd }

function TAdrsBookConsoleCommandAdd.CanExecute(const ACommand: string): boolean;
begin
  result:= SameText(ACommand, 'add') or SameText(ACommand, 'a');
end;

procedure TAdrsBookConsoleCommandAdd.Execute(const AAdrsBook: TAdrsBook;
  const AParams: string);
var
  LItem: TPerson;
  LMessage: string;
begin
  LItem:= TPerson.CreateNew;
  LItem.Title:= tiToken(AParams, ',', 1);
  LItem.FirstName:= tiToken(AParams, ',', 2);
  LItem.LastName:= tiToken(AParams, ',', 3);
  if LItem.IsValid(LMessage) then
  begin
    AAdrsBook.PersonList.Add(LItem);
    LItem.Save;
  end else
  begin
    WriteLn('Error');
    WriteLn(LMessage);
    LItem.Free;
  end;
end;

{ TAdrsBookConsoleCommandDelete }

function TAdrsBookConsoleCommandDelete.CanExecute(
  const ACommand: string): boolean;
begin
  result:= SameText(ACommand, 'delete') or SameText(ACommand, 'del') or SameText(ACommand, 'd');
end;

procedure TAdrsBookConsoleCommandDelete.Execute(const AAdrsBook: TAdrsBook;
  const AParams: string);
var
  LItem: TPerson;
begin
  LItem:= AAdrsBook.PersonList.Items[StrToInt(AParams)];
  LItem.Deleted:= True;
  LItem.Save;
  AAdrsBook.PersonList.FreeDeleted;
end;

{ TAdrsBookConsoleCommandCLS }

function TAdrsBookConsoleCommandCLS.CanExecute(const ACommand: string): boolean;
begin
  result:= SameText(ACommand, 'cls') or SameText(ACommand, 'c');
end;

procedure TAdrsBookConsoleCommandCLS.Execute(const AAdrsBook: TAdrsBook;
  const AParams: string);
var
  i: integer;
begin
  for i := 1 to 24 do
    WriteLn;
end;

{ TAdrsBookConsoleCommandHelp }

function TAdrsBookConsoleCommandHelp.CanExecute(
  const ACommand: string): boolean;
begin
  result:= SameText(ACommand, 'help') or SameText(ACommand, 'h');
end;

procedure TAdrsBookConsoleCommandHelp.Execute(const AAdrsBook: TAdrsBook;
  const AParams: string);
begin
  WriteLn('help                                    - Describe the commands');
  WriteLn('list                                    - List all entries');
  WriteLn('add <Title>, <First Name>, <Last Name>  - Add a new entry');
  WriteLn('del <Index number>                      - Delete an entry');
  WriteLn('cls                                     - Clear the screen');
end;

end.

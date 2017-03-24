
{ This unit works with the latest Free Pascal 2.4.x and later releases only. }

unit tiOPFSqlScript;

{$I tiDefines.inc}

interface

{$IF defined(FPC) and not defined(VER2_2)}
{$define HAVESQLSCRIPT}
{$endif}

{ So we can compile with 2.2.x as well }
{$IFDEF HAVESQLSCRIPT}
uses
  Classes, SysUtils, sqlscript, tiQuery;

type

  TtiSQLScript = class (TCustomSQLScript)
  private
    FOnDirective: TSQLScriptDirectiveEvent;
    FQuery : TtiQuery;
    FDatabase: TtiDatabase;
    procedure CheckDatabase;
  protected
    procedure ExecuteStatement (Statement: TStrings; var StopExecution: Boolean); override;
    procedure ExecuteDirective (Directive, Argument: String; var StopExecution: Boolean); override;
    procedure ExecuteCommit{$if FPC_FULLVERSION >= 20701}(CommitRetaining: boolean=true){$endif}; override;
  public
    procedure Execute; override;
    property  Database : TtiDatabase read FDatabase write FDatabase;
    property OnDirective: TSQLScriptDirectiveEvent read FOnDirective write FOnDirective;
    property Directives;
    property Defines;
    property Script;
    property Terminator;
    property CommentsinSQL;
    property UseSetTerm;
    property UseCommit;
    property AutoCommit;
    property UseDefines;
    property OnException;
  end;


  TtiOPFSQLscript = class(TtiSQLScript)
  end deprecated 'Use TtiSQLScript instead.';
{$ENDIF}


implementation

{$IFDEF HAVESQLSCRIPT}

resourcestring
  sNoDatabase = 'No database assigned to script';

{ TtiSQLScript }

procedure TtiSQLScript.CheckDatabase;
begin
  if not assigned (FDatabase) then
    raise exception.create(sNoDatabase);
end;

procedure TtiSQLScript.ExecuteStatement(Statement: TStrings;
  var StopExecution: Boolean);
begin
  FQuery.SQLText:=Statement.text;
  FQuery.ExecSQL;
end;

procedure TtiSQLScript.ExecuteDirective(Directive, Argument: String;
  var StopExecution: Boolean);
begin
  if assigned (FOnDirective) then
    FOnDirective (Self, Directive, Argument, StopExecution);
end;

procedure TtiSQLScript.ExecuteCommit{$if FPC_FULLVERSION >= 20701}(CommitRetaining: boolean){$endif};
begin
  FDatabase.Commit;
end;

procedure TtiSQLScript.Execute;
begin
  CheckDatabase;
  FQuery := FDatabase.CreateAndAttachTIQuery;
  try
    inherited Execute;
  finally
    FQuery.Free;
  end;
end;
{$ENDIF}

end.


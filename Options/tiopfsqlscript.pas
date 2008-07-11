
{ This unit works with the latest Free Pascal 2.3.x only. }

unit tiopfsqlscript;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlscript, tiQuery;

type

  { TtiOPFSQLscript }

  TtiOPFSQLscript = class (TCustomSQLScript)
  private
    FOnDirective: TSQLScriptDirectiveEvent;
    FQuery : TtiQuery;
    FDatabase: TtiDatabase;
    procedure CheckDatabase;
  protected
    procedure ExecuteStatement (Statement: TStrings; var StopExecution: Boolean); override;
    procedure ExecuteDirective (Directive, Argument: String; var StopExecution: Boolean); override;
    procedure ExecuteCommit; override;
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
    property UseDefines;
    property OnException;
  end;


implementation

resourcestring
  sNoDatabase = 'No database assigned to script';

{ TtiOPFSQLscript }

procedure TtiOPFSQLscript.CheckDatabase;
begin
  if not assigned (FDatabase) then
    raise exception.create(sNoDatabase);
end;

procedure TtiOPFSQLscript.ExecuteStatement(Statement: TStrings;
  var StopExecution: Boolean);
begin
  FQuery.SQLText:=Statement.text;
  FQuery.ExecSQL;
end;

procedure TtiOPFSQLscript.ExecuteDirective(Directive, Argument: String;
  var StopExecution: Boolean);
begin
  if assigned (FOnDirective) then
    FOnDirective (Self, Directive, Argument, StopExecution);
end;

procedure TtiOPFSQLscript.ExecuteCommit;
begin
  FDatabase.Commit;
end;

procedure TtiOPFSQLscript.Execute;
begin
  CheckDatabase;
  FQuery := FDatabase.CreateAndAttachTIQuery;
  try
    inherited Execute;
  finally
    FQuery.Free;
  end;
end;

end.


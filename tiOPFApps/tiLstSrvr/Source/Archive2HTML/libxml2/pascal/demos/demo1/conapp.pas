unit conapp;
{Unit for application methods and variables
which are in the application object if you have a gui-application}

interface

//uses classes;

procedure outLog(text:string='');
procedure outSQLLog(text:string='');
procedure outDebugLog(text:string='');

procedure EnableOutput;
procedure DisableOutput;

implementation

var OutputEnabled: boolean;

procedure EnableOutput;
begin
 OutputEnabled:=true;
end;

procedure DisableOutput;
begin
  OutputEnabled:=false;
end;

procedure outLog(text:string);
begin
  if OutputEnabled then
    writeln(text);
end;

procedure outSQLLog(text:string);
begin
  if OutputEnabled then
    writeln(text);
end;

procedure outDebugLog(text:string);
begin
  writeln(text);
end;

begin
  EnableOutput;
end.

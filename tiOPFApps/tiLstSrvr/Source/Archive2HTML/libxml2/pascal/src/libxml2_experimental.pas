unit libxml2_experimental;
//$Id: libxml2_experimental.pas,v 1.2 2002/08/03 20:56:46 pkozelka Exp $
(**
 * Title:        libxml2 experimental unit
 * Description:  Contains experimental code for support or development of libxml2
 * Copyright:    Copyright (c) 2002
 * Company:
 * @author       Petr Kozelka <pkozelka@email.cz>
 * @version 0.1
 *)
interface

uses
{$IFDEF WIN32}
  windows,
{$ENDIF}
  libxml2;

type
  TMessageHandler = procedure(aMsg: WideString) of object;

procedure RegisterErrorHandler(aHandler: TMessageHandler);

implementation

uses
  SysUtils;

var
  myErrH: TMessageHandler;

{$IFDEF VER130}

function UTF8Decode(aText: String): WideString;
begin
  Result := aText; //DIRTY
end;

{$ENDIF}


// error output redirected to OutputDebugString

procedure myGenericErrorFunc(aCtx: Pointer; aMsg: PChar); cdecl;
var
  msg: WideString;
  p: Pointer;
  arg: PChar;
  n: Integer;
begin
  msg := UTF8Decode(aMsg);
  // dirty hack for C varargs:
  n := Pos('%s', msg);
  if (n>0) then begin
    p := @aMsg;
    Inc(PChar(p), sizeof(Pointer));
    arg := PChar(p^);
    msg := StringReplace(msg, '%s', UTF8Decode(arg), []);
  end;
  // (end of dirty hack)
  if Assigned(myErrH) then begin
    myErrH(msg);
  end;
{$IFDEF WIN32}
  OutputDebugStringW(PWideChar(msg));
{$ENDIF}
{$IFDEF LINUX}
  Writeln(msg);
{$ENDIF}
end;

procedure RegisterErrorHandler(aHandler: TMessageHandler);
begin
  myErrH := aHandler;
end;

initialization
  // redirect error output
  xmlSetGenericErrorFunc(nil, @myGenericErrorFunc);

end.

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Chris Latta, Data Solutions Pty Ltd
  for inclusion in the tiOPF (TechInsite Object Persistence Framework) from
    TechInsite Pty. Ltd.
    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm
    EMail:           info@techinsite.com.au

  Please submit changes to tiOPF@techinsite.com.au

  Purpose: A boolean stack, for creating an accumulation of boolean values
  which derive an overall result via an AND operation, and which can
  be added to (pushed) and taken from (popped) one at a time like a stack
  operates. For use in tiOPF wizards.

  Revision History:
    March 2002, Chris Latta, Created

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
unit tiOPFBooleanStack;

interface

type
  TtiBooleanStack = class(TObject)
  private
    FResult: Boolean;
    FValues: array[0..20] of Boolean;
    Index: Integer;
    procedure CalcResult;
  public
    property Result: Boolean read FResult;
    constructor Create;
    procedure Push(Value: Boolean);
    procedure Pop;
  end;

implementation

uses
  SysUtils;

{ TtiBooleanStack }

{ This is just a boolean stack which you push values onto
  and it calculates the total Result of all the values as
  a logical AND.
  Push puts another boolean value into the equation
  Pop discards the last value pushed
}

constructor TtiBooleanStack.Create;
begin
  inherited;
  Index := Low(FValues);
  FResult := True;
  FValues[0] := True;
end;

procedure TtiBooleanStack.CalcResult;
var
  i: Integer;
begin
  FResult := True;
  for i := Low(FValues) to Index do
    FResult := FResult and FValues[i];
end;

procedure TtiBooleanStack.Pop;
begin
  if Index = Low(FValues) then
    raise Exception.Create('Boolean stack underflow')
  else
  begin
    Dec(Index);
    CalcResult;
  end;
end;

procedure TtiBooleanStack.Push(Value: Boolean);
begin
  if Index = High(FValues) then
    raise Exception.Create('Boolean stack index overflow: Maximum of ' + IntToStr(High(FValues)) + ' exceeded.')
  else
  begin
    Inc(Index);
    FValues[Index] := Value;
    FResult := FResult and Value;
  end;
end;

end.

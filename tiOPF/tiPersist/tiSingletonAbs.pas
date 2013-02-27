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

  Please submit changes to tiOPF@techinsite.com.au

  Created: 01/03/2002 by Marco Dissel, m.dissel@broekhuis.nl

  Notes: Abstract Singleton Class

  Usage: Descend a new class and implement:
            public
               class function Instance: T<MyClass>; reintroduce;

         You can access the instance with T<MyClass>.Instance but to have easier
         access to the class implement a global function.
           function ti<MyClass:T<MyClass>;


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}


{$I tiDefines.inc}

unit tiSingletonAbs;

interface

uses SysUtils;

type

  TSingletonAbs = class
  private
    constructor CreateInstance;
    class function AccessInstance(Request: Integer): TSingletonAbs;
  public
    constructor Create;
    destructor Destroy; override;
    class function Instance: TSingletonAbs; virtual;
    class procedure ReleaseInstance;
  end;

implementation

{ TSingletonAbs }

var
 FInstance: TSingletonAbs = nil;

constructor TSingletonAbs.Create;
begin
  inherited Create;
end;

destructor TSingletonAbs.Destroy;
begin
  if AccessInstance(0) = Self then AccessInstance(2);
  inherited Destroy;
end;

class function TSingletonAbs.AccessInstance(Request: Integer): TSingletonAbs;
begin
 case Request of
    0 : ;
    1 : if not Assigned(FInstance) then FInstance := CreateInstance;
    2 : FInstance := nil;
  else
    raise Exception.CreateFmt('Illegal request %d in AccessInstance', [Request]);
  end;
  Result := FInstance;
end;

constructor TSingletonAbs.CreateInstance;
begin
  inherited Create;
end;

class function TSingletonAbs.Instance: TSingletonAbs;
begin
  Result := AccessInstance(1);
end;

class procedure TSingletonAbs.ReleaseInstance;
begin
  AccessInstance(0).Free;
end;

procedure ShutDown;
begin
  TSingletonAbs.ReleaseInstance;
end;

initialization

finalization
  ShutDown;
end.


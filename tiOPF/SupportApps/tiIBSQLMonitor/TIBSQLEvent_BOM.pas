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
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm

  Please submit changes to tiOPF@techinsite.com.au

  Revision history:

  Purpose:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit TIBSQLEvent_BOM;

interface
uses
   tiPtnVisPerObj
  ,Classes
  ;

type
  TIBSQLEvent = class(TPerObjAbs)
  private
    fEventTime      : TDateTime;
    fLastEventTime  : TDateTime;
    fApplicationName: string;
    fDescription    : string;
    fEventTypeName  : string;
    fTableName      : string;
    fRepeats        : integer;
    function GetEventTimeCaption: string;
    procedure SetEventText(const Value: string);
    function GetRepeatTimeCaption: string;
  protected
    function GetCaption         : string; override;
    function DeriveTableFromSQL( pSQL : string ) : string ;
  public
    property EventTime          : TDateTime read fEventTime   write fEventTime ;
    property EventText          : string                      write SetEventText;
    procedure IncRepeats( const pEventTime : TDateTime );
  published
    property EventTimeCaption   : string    read GetEventTimeCaption;
    property ApplicationName    : string    read fApplicationName;
    property EventTypeName      : string    read fEventTypeName;
    property TableName          : string    read fTableName;
    property Repeats            : integer   read fRepeats     write fRepeats;
    property RepeatTimeCaption  : string    read GetRepeatTimeCaption;
    property Description        : string    read fDescription write fDescription;
  end;

  TIBSQLEventList = class(TPerObjList)
  private
    fLastIBSQLEvent : TIBSQLEvent ;
  protected
    function GetTheItems(Index: Integer): TIBSQLEvent;
  public
    property  Items[Index: Integer]: TIBSQLEvent   read GetTheItems ;
    procedure AddEvent(pEventText: string; pEventTime: TDateTime);
    procedure Clear ; override ;
  end;

implementation

uses SysUtils;

{ TIBSQLEvent }


function TIBSQLEvent.DeriveTableFromSQL(pSQL: string): string;
var
  lStr : integer ;
  lTablePart : string ;
begin
  { This logic is a hack for now. Even basic token parse logic would be an improvement }

  result := '';
  if (Pos('SELECT ', UpperCase(pSQL)) > 0)
  or (Pos('DELETE ', UpperCase(pSQL)) > 0) then begin
    lStr   := Pos(' FROM ', UpperCase(pSQL));
    lTablePart := Trim(Copy(pSQL, lStr + 6, 99)) + ' ';
    result := Copy( lTablePart, 1, Pos(' ', lTablePart) - 1);
    end
  else begin
    lStr   := Pos('UPDATE ', UpperCase(pSQL));
    if lStr > 0 then begin
      lTablePart := Trim(Copy(pSQL, lStr + 6, 99)) + ' ';
      result := Copy( lTablePart, 1, Pos(' ', lTablePart) - 1);
      end ;
    end ;
end;

function TIBSQLEvent.GetCaption: string;
begin
  result := EventTimeCaption;
end;

function TIBSQLEvent.GetEventTimeCaption: string;
begin
  result := FormatDateTime('hh:nn:ss.zzz', EventTime);
end;

function TIBSQLEvent.GetRepeatTimeCaption: string;
begin
  if fLastEventTime > fEventTime then
    result := FormatDateTime('hh:nn:ss.zzz', (fLastEventTime - fEventTime))
  else
    result := '';
end;

procedure TIBSQLEvent.IncRepeats( const pEventTime : TDateTime );
begin
  Inc( fRepeats );
  fLastEventTime := pEventTime;
end;

procedure TIBSQLEvent.SetEventText(const Value: string);
var
  s : String;
  Start, Ending, len : Integer;
begin
  s                := StringReplace( Value, #13#10, '  ', [rfReplaceAll]);
  len              := Length( s );

  Start            := Pos('[Application:', s) + 14;
  Ending           := Pos(']', s) ;
  fApplicationName := Copy( s, Start, Ending - Start );

  s                := Copy( s, Ending + 1, len ); // remove application from text
  Start            := Pos('[', s) + 1;
  Ending           := Pos(']', s) ;
  fEventTypeName   := Copy( s, Start, Ending - Start );

  fDescription     := Trim(Copy( s, Ending + 1, len )); // remove event type from text

  fTableName       := DeriveTableFromSQL( FDescription );
end;



{ TIBSQLEventList }

procedure TIBSQLEventList.AddEvent(pEventText: string; pEventTime : TDateTime);
var
  lIBSQLEvent : TIBSQLEvent;
begin
  lIBSQLEvent                 := TIBSQLEvent.Create;
  lIBSQLEvent.EventTime       := pEventTime;
  lIBSQLEvent.EventText       := pEventText;
  if  Assigned( fLastIBSQLEvent )
  and SameText( fLastIBSQLEvent.Description, lIBSQLEvent.Description ) then begin
    fLastIBSQLEvent.IncRepeats( lIBSQLEvent.EventTime );
    FreeAndNil( lIBSQLEvent ) ;
    end
  else begin
    Add( lIBSQLEvent );
    fLastIBSQLEvent    := lIBSQLEvent;
    end;
end;

procedure TIBSQLEventList.Clear;
begin
  inherited;
  fLastIBSQLEvent := nil;
end;

function TIBSQLEventList.GetTheItems(Index: Integer): TIBSQLEvent;
begin
  Result:= TIBSQLEvent(GetItems(Index));
end;

end.

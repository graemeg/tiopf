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

{$I tiDefines.inc}

unit tiTreeviewEditor;



interface

uses
  Classes
  ,tiTreeView
  ,TypInfo
  {$IFNDEF FPC}
  {$IFNDEF VER130}
    ,DesignIntf
    ,DesignEditors
  {$ELSE}
    ,DsgnIntf
  {$ENDIF}
  {$ELSE}
  ,ComponentEditors
  ,PropEdits
  ,LazarusPackageIntf
  {$ENDIF}
  ;

type
  TtiTVNodeEventPropertyEditor = class(TMethodProperty)
  public
    //function AllEqual: Boolean; override;
    //procedure Edit; override;
    //function GetAttributes: TPropertyAttributes; override;
    //function GetEditLimit: Integer; override;
    //function GetValue: string; override;
    //procedure GetValues(Proc: TGetStrProc); override;
    //procedure SetValue(const AValue: string); override;
    {$IFNDEF FPC}
    function GetFormMethodName: string; override ;
    {$ELSE}
    function GetFormMethodName: shortstring; override ;
    {$ENDIF}
    
    //function GetTrimmedEventName: string;
  end;

implementation

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiTVNodeEventPropertyEditor
// *
// * Property editor for:
// *   TtiTVNodeEvent
// *   TtiTVNodeAvailableEvent
// * Changes the default name of the event which is added to the pas file.
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
{$IFNDEF FPC}
function TtiTVNodeEventPropertyEditor.GetFormMethodName: string;
{$ELSE}
function TtiTVNodeEventPropertyEditor.GetFormMethodName: shortstring;
{$ENDIF}
var
  lData : TtiTVDataMapping ;
begin
  Assert( GetComponent( 0 ) is TtiTVDataMapping,
          'Invalid class type <' +
          GetComponent( 0 ).ClassName ) ;

  lData := TtiTVDataMapping( GetComponent( 0 )) ;
  result := lData.Name + GetName ;

end;

end.

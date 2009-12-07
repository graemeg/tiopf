unit tiTreeviewEditor;

{$I tiDefines.inc}

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
    function GetFormMethodName: string; override;
    {$ELSE}
    function GetFormMethodName: shortstring; override;
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
  lData : TtiTVDataMapping;
begin
  Assert(GetComponent(0) is TtiTVDataMapping,
          'Invalid class type <' +
          GetComponent(0).ClassName);

  lData := TtiTVDataMapping(GetComponent(0));
  result := lData.Name + GetName;

end;

end.

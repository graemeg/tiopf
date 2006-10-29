unit DtiDefaultActionValues;

{$I tiDefines.inc}

interface
uses
  SysUtils, Classes, ActnList
  {$IFDEF FPC}
  ,LResources
  {$ENDIF}
  ,tiPerAwareCtrls
 ;

type
  TtidmDefaultActionValues = class(TDataModule)
    ActionList1: TActionList;
  private
  public
  end;

var
  tidmDefaultActionValues: TtidmDefaultActionValues;

implementation

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
initialization
  {$I DtiDefaultActionValues.lrs}
{$ENDIF}

end.

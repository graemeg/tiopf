unit tiRTTI_TST;

{$I tiDefines.inc}

interface

uses
  Classes
  {$IFDEF FPC}
  ,testregistry
  {$ELSE}
  ,TestFrameWork
  {$ENDIF}
  ,tiTestFramework
  ;
  
type

  TTesttiRTTI= class(TtiTestCase)
  published
    procedure TestSetProperty;
    { Graeme: many more tests on the way! Stay tuned. }
  end; 
  
  

procedure RegisterTests;


implementation

uses
  tiDUnitDependencies
  ,tstPerFramework_BOM
  ,tiRTTI
  ,SysUtils
  ;


procedure RegisterTests;
begin
  RegisterNonPersistentTest(TTesttiRTTI);
end;

procedure TTesttiRTTI.TestSetProperty;
const
  cExpected = '01%s01%s2000 06%s30%s15';
var
  c: TtiOPFTestItem;
  lDate: TDateTime;
begin
  lDate := EncodeDate(2007, 2, 15) + EncodeTime(6, 30, 15, 10);

  c := TtiOPFTestItem.Create;
  try
    tiSetProperty(c, 'StrField', 'Graeme');
    CheckEquals('Graeme', c.StrField, 'Failed on 1');

    tiSetProperty(c, 'IntField', 32);
    CheckEquals(32, c.IntField, 'Failed on 2');

    tiSetProperty(c, 'FloatField', 12.345);
    CheckEquals(12.345, c.FloatField, 'Failed on 3');

    tiSetProperty(c, 'DateField', lDate);
    CheckEquals(lDate, c.DateField, 'Failed on 4');

    tiSetProperty(c, 'BoolField', True);
    CheckEquals(True, c.BoolField, 'Failed on 5');
  finally
    c.Free;
  end;

end; 


end.


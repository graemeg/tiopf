unit WhichPersistenceMechanism;

interface

type
  TRegisterPersistenceMethod = procedure;

function Execute(
  ARegisterAutoMapMethod,
  ARegisterHardCodedMethod,
  ARegisterDBIndependentMethod: TRegisterPersistenceMethod
 ): boolean;

implementation
uses
  tiDialogs
  ,tiUtils
  ,tiOPFManager
  ,tiConstants
  ,SysUtils
 ;

function Execute(
    ARegisterAutoMapMethod,
    ARegisterHardCodedMethod,
    ARegisterDBIndependentMethod: TRegisterPersistenceMethod
 ): boolean;
var
  LSelected: string;
const
  cAutoMap = '&Auto map';
  cHardCodedVisitors = '&Hard coded';
  cDBIndependantVisitors = 'DB &independant';
  cCancel='&Cancel';

begin
  Assert(Assigned(ARegisterAutoMapMethod));
  Assert(Assigned(ARegisterHardCodedMethod));
  Assert(Assigned(ARegisterDBIndependentMethod));
  Result:= true;
  // ToDo: Require a property on the PersistenceLayer - SQLSupport or the like
  if (gtiOPFManager.DefaultPerLayerName = cTIPersistIBX) or
     (gTIOPFManager.DefaultPerLayerName = cTIPersistADOAccess) then
    LSelected:= tiMessageDlg(
      'Which persistence mechanism?' + CrLF(2) +
      'Auto map: Good for simple, small lists of objects.'+ CrLf +
      'Required for persistence to non SQL databases.' + CrLf(2) +
      'DB independent visitors: Persistence to SQL and'+ CrLf +
      'non SQL database with more control that auto-map.' + CrLf(2) +
      'Hard coded visitors: The most flexible persistence'+ CrLf +
      'to SQL databases.',
      [cAutoMap, cDBIndependantVisitors, cHardCodedVisitors,  cCancel])
  else
    LSelected:= tiMessageDlg(
      'Which persistence mechanism?' + CrLF(2) +
      'Auto map: Good for simple, small lists of objects.'+ CrLf +
      'Required for persistence to non SQL databases.' + CrLf(2) +
      'DB independent visitors: Persistence to SQL and' + CrLf +
      'non SQL database with more control that auto-map.'+ CrLf(2) +
      '(Hard coded visitors are not available for non-SQL databases.)',
      [cAutoMap, cDBIndependantVisitors, cCancel]);

  if LSelected = cAutoMap then
    ARegisterAutoMapMethod
  else if LSelected=cHardCodedVisitors then
    ARegisterHardCodedMethod
  else if LSelected=cDBIndependantVisitors then
    ARegisterDBIndependentMethod
  else if LSelected=cCancel then
    result:= false
  else
    raise Exception.Create('Invalid option');
end;

end.

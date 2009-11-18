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
  if (GTIOPFManager.DefaultPersistenceLayerName = cTIPersistIBX) or
     (GTIOPFManager.DefaultPersistenceLayerName = cTIPersistADOAccess) then
    LSelected:= tiMessageDlg(
      'Which persistence mechanism?' + tiLineEnd(2) +
      'Auto map: Good for simple, small lists of objects.'+ tiLineEnd +
      'Required for persistence to non SQL databases.' + tiLineEnd(2) +
      'DB independent visitors: Persistence to SQL and'+ tiLineEnd +
      'non SQL database with more control that auto-map.' + tiLineEnd(2) +
      'Hard coded visitors: The most flexible persistence'+ tiLineEnd +
      'to SQL databases.',
      [cAutoMap, cDBIndependantVisitors, cHardCodedVisitors,  cCancel])
  else
    LSelected:= tiMessageDlg(
      'Which persistence mechanism?' + tiLineEnd(2) +
      'Auto map: Good for simple, small lists of objects.'+ tiLineEnd +
      'Required for persistence to non SQL databases.' + tiLineEnd(2) +
      'DB independent visitors: Persistence to SQL and' + tiLineEnd +
      'non SQL database with more control that auto-map.'+ tiLineEnd(2) +
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

unit Adrs_SQL;

{$I tiDefines.inc}

interface
const

  cQryPeople_Select =
    'select '+
    '  oid '+
    ' ,first_name '+
    ' ,last_name '+
    ' ,title '+
    ' ,initials '+
    ' ,notes '+
    'from '+
    ' person '+
    'order by ' +
    '  last_name ' +
    '  ,first_name ';

  cQryPeople_Create =
    'insert into person '+
    '  ( ' +
    '  oid '+
    ' ,first_name '+
    ' ,last_name '+
    ' ,title '+
    ' ,initials '+
    ' ,notes '+
    '  ) ' +
    'values '+
    '  ( ' +
    '  :oid '+
    ' ,:first_name '+
    ' ,:last_name '+
    ' ,:title '+
    ' ,:initials '+
    ' ,:notes '+
    '  ) ';

  cQryPeople_Update =
    'update person set '+
    '  first_name = :first_name '+
    ' ,last_name  = :last_name '+
    ' ,title      = :title '+
    ' ,initials   = :initials '+
    ' ,notes      = :notes '+
    'where ' +
    '  oid = :oid';

  cQryPeople_Delete =
    'delete from person '+
    'where ' +
    '  oid = :oid';

  cQryEAdrs_Read =
    'select '+
    '  oid '+
    ' ,eadrs_type '+
    ' ,eadrs_text '+
    'from '+
    ' eadrs '+
    'where ' +
    '  oid_person = :oid_person ' +
    'order by ' +
    '  eadrs_type ' +
    '  ,eadrs_text ';

  cQryEAdrs_Create =
   'insert into eadrs ' +
   '( ' +
   ' oid ' +
   '  ,oid_person ' +
   '  ,eadrs_type ' +
   '  ,eadrs_text ' +
   ') ' +
   'values ' +
   '( ' +
   '  :oid ' +
   '  ,:oid_person ' +
   '  ,:eadrs_type ' +
   '  ,:eadrs_text ' +
   ') ';

  cQryEAdrs_Update =
    'update eadrs set ' +
    '   eadrs_type = :eadrs_type '+
    '  ,eadrs_text = :eadrs_text '+
    'where ' +
    '  oid = :oid';

  cQryEAdrs_Delete =
    'delete from eadrs '+
    'where ' +
    '  oid = :oid';


implementation

end.

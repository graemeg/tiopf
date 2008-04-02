connect  "Adrs.fdb" user "SYSDBA" password "masterkey" ;

insert into adrs_type
(oid, text)
values
(1001, 'Work');

insert into adrs_type
(oid, text)
values
(1002, 'Home');

insert into adrs_type
(oid, text)
values
(1003, 'Postal');

insert into eadrs_type
(oid, text)
values
(2001, 'Work');

insert into eadrs_type
(oid, text)
values
(2002, 'Home');

insert into eadrs_type
(oid, text)
values
(2003, 'Mobile');

insert into eadrs_type
(oid, text)
values
(2004, 'Fax');

insert into eadrs_type
(oid, text)
values
(2005, 'EMail');

insert into person
(oid, first_name, last_name, title)
values
(3000, 'Edna', 'Everage', 'Dame');

insert into adrs
(oid, oid_person, oid_adrs_type, lines, suburb, state, pcode, country)
values
(4000, 3000, 1001, 'Arts Center', 'Melbourne', 'VIC', '3000', 'Australia');

insert into eadrs
(oid, oid_person, oid_eadrs_type, eadrs_text)
values
(5000, 3000, 2005, 'dame_edna@housewifesuperstar.com.au');

commit;

exit;

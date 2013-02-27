/*
  SQLManager tables
  DDL for Interbase
*/

/*****************************************************************
  Connect to the database
*/

connect  "c:\techinsite\Demos\DemotiPerFramework\adrs.gdb"
user "SYSDBA" password "masterkey" ;

drop table  sqlman_param ;
drop table  sqlman_sql ;
drop table  sqlman_group ;

drop domain domain_sql     ;
drop domain domain_str50   ;
drop domain domain_str20   ;
drop domain domain_str100  ;
drop domain domain_integer ;
drop domain domain_boolean ;

create domain domain_sql     as blob sub_type 1 ;
create domain domain_str20   as varchar( 20 ) not null ;
create domain domain_str50   as varchar( 50 ) not null ;
create domain domain_str100  as varchar( 100 ) ;
create domain domain_integer as integer default 0 not null ;
/* create domain domain_boolean as char( 1 ) default 'F' check ( value in ( 'T', 'F' )) ; */
/* This has not been tested (the line above has, and works in all cases except the 
   tiQueryIBX.GetFieldAsVariant() method. The line below fixes this, but will break any
   existing SQL manager tables. */
create domain domain_boolean as varchar( 5 ) default 'FALSE' check ( value in ( 'TRUE', 'FALSE' )) ;

/* SQLMan_Group table */

create table sqlman_group
( oid          domain_oid,
  group_name   domain_str50,
  disp_order   domain_integer,
  primary key  ( oid )) ;

/* SQLMan_SQL table */
create table sqlman_sql
( oid                 domain_oid,
  group_oid           domain_oid,
  disp_order          domain_integer,
  query_version       domain_integer,
  query_name          domain_str50,
  query_description   domain_sql,
  query_locked        domain_boolean,
  test_include        domain_boolean,
  query_sql           domain_sql,
  primary key ( oid )) ;

/* SQLMan_Param Table */
create table sqlman_param
( oid          domain_oid,
  sql_oid      domain_oid,
  disp_order   domain_integer,
  param_name   domain_str20,
  param_type   domain_str20,
  param_value  domain_str50,
  param_isnull domain_boolean,
  primary key ( oid )) ;

/* Add ref integ */
alter table SQLMan_SQL
add foreign key ( Group_OID ) references SQLMan_Group ( OID ) ;

alter table SQLMan_Param
add foreign key ( SQL_OID ) references SQLMan_SQL ( OID ) ;

/* Add unique constraint */
create unique index I_SQLMan_Group
on SQLMan_Group
( Group_Name ) ;

create unique index I_SQLMan_SQL
on SQLMan_SQL
( Query_Name ) ;

create unique index I_SQLMan_Param
on SQLMan_Param
( SQL_OID, Param_Name ) ;

insert into sqlman_group values ( 1, 'Queries', 0 ) ;

commit ;


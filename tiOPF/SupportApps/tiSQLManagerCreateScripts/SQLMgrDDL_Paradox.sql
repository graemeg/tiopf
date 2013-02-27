/* Drop all tables to start off */

drop table  sqlman_param ;
drop table  sqlman_sql ;
drop table  sqlman_group ;

/* SQLMan_Group table */
create table sqlman_group
( oid          integer,
  group_name   VarChar( 50 ),
  disp_order   Integer,
  primary key  ( oid )) ;

/* SQLMan_SQL table */
create table sqlman_sql
( oid                 Integer,
  group_oid           Integer,
  disp_order          Integer,
  query_version       Integer,
  query_name          VarChar( 50 ),
  query_description   blob( 1, 1 ),
  query_locked        Boolean,
  test_include        Boolean,
  query_sql           Blob( 1, 1 ),
  primary key ( oid )) ;

/* SQLMan_Param Table */
create table sqlman_param
( oid          Integer,
  sql_oid      Integer,
  disp_order   Integer,
  param_name   VarChar( 20 ),
  param_type   VarChar( 20 ),
  param_value  VarChar( 50 ),
  param_isnull Boolean,
  primary key ( oid )) ;

/* Add unique constraint */

-- Cant run these. Why? Might be a problem with the BDE layer?
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


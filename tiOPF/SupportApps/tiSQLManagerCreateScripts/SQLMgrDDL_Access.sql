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
  query_description   Memo,
  query_locked        Logical,
  test_include        Logical,
  query_sql           Memo,
  primary key ( oid )) ;

/* SQLMan_Param Table */
create table sqlman_param
( oid          Integer,
  sql_oid      Integer,
  disp_order   Integer,
  param_name   VarChar( 20 ),
  param_type   VarChar( 20 ),
  param_value  VarChar( 50 ),
  param_isnull Logical,
  primary key ( oid )) ;

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

insert into sqlman_group
( oid, group_name, disp_order ) values
( 1, 'System Queries', 0 ) ;

insert into sqlman_group
( oid, group_name, disp_order ) values
( 2, 'Developer Queries', 0 ) ;

/*
  SQLManager tables
  DDL for Oracle
*/

/* SQLMan_Group table */
drop table sqlman_group ;

create table sqlman_group
( oid          number( 12 ),
  group_name   varchar2( 50 ) constraint sqlmg_group_name_nn not null,
  disp_order   number( 6 )    default 0 constraint sqlmg_disp_order_nn not null,
  constraint Sqlman_Group_PK
  Primary Key ( oid ) using index
  TableSpace Index_Data ) Tablespace User_Data;

create unique index I_SQLMan_Group
on SQLMan_Group
( Group_Name ) ;


/* SQLMan_SQL table */
drop table sqlman_sql ;

create table sqlman_sql
( oid                 number( 12 ),
  group_oid           number( 12 )     constraint sqlms_group_oid_nn not null,
  disp_order          number( 6 )      default 0 constraint sqlms_disp_order_nn not null,
  query_version       number( 3 )      default 0 constraint sqlms_query_version_nn not null,
  query_name          varchar2( 50 )   constraint sqlms_query_name_nn not null,
  query_description   varchar2( 2000 )  null,
  sql                 long,
  query_locked	      varchar2(1)      constraint sqlms_query_locked_ch check (query_locked in ('T','F')),
  test_include        varchar2(1)      constraint sqlms_test_include_ch check (test_include in ('T','F')),
  constraint Sqlman_Sql_Pk
  Primary key ( oid ) using index
  Tablespace Index_Data ) Tablespace User_Data;


alter table SQLMan_SQL
add foreign key ( Group_OID ) references SQLMan_Group ( OID ) ;

create unique index I_SQLMan_SQL
on SQLMan_SQL
( Query_Name ) ;

alter table SQLMan_SQL modify Query_Locked constraint sqlms_query_locked_nn not null ;

alter table SQLMan_SQL modify Test_Include constraint sqlms_test_include_nn not null ;

/* SQLMan_Param Table */
drop table sqlman_param ;
create table sqlman_param
( oid         number( 12 ),
  sql_oid     number( 12 )   constraint sqlmp_sql_oid_nn not null,
  disp_order  number( 6 )    default 0 constraint sqlmp_disp_order_nn not null,
  param_name  varchar2( 20 ) constraint sqlmp_param_name_nn not null,
  param_type  varchar2( 20 ) constraint sqlmp_param_type_nn not null,
  param_value varchar2( 50 ) constraint sqlmp_param_value_nn not null,
  param_isnull varchar2(1)   constraint sqlmp_param_isnull_ch check (param_isnull in ('T','F')),
  constraint Sqlman_Param_PK
  Primary Key ( oid ) using index
  Tablespace Index_Data ) Tablespace User_Data;

alter table SQLMan_Param
add foreign key ( SQL_OID ) references SQLMan_SQL ( OID ) ;

create unique index I_SQLMan_Param
on SQLMan_Param
( SQL_OID, Param_Name ) ;

alter table SQLMan_Param modify Param_IsNull constraint sqlmp_param_isnull_nn not null ;

/* SQLMan_Interface Table */
drop table sqlman_interface ;
create table sqlman_interface
( oid         number( 12 ),
  sql_oid     number( 12 ),
  disp_order   number( 6 )    default 0 constraint sqlmi_disp_order_nn not null,
  field_name  varchar2( 20 )  constraint sqlmi_field_name_nn not null,
  field_type  varchar2( 20 )  constraint sqlmi_field_type_nn not null,
  constraint Sqlman_Insterface_PK
  Primary Key ( oid ) using index
  Tablespace Index_Data ) Tablespace User_Data;

alter table SQLMan_Interface
add foreign key ( SQL_OID ) references SQLMan_SQL ( OID ) ;

create unique index I_SQLMan_Interface
on SQLMan_Interface
( SQL_OID, Field_Name ) ;


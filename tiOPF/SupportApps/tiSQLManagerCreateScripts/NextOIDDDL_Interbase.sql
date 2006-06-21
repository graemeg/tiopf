/* Drop all tables to start off */
drop table  next_oid ;

create domain domain_oid
as integer not null ;

/* Create Next_OID table */
create table Next_OID
  ( OID domain_oid ) ;

insert into next_oid ( oid )
values ( 1000000 ) ;

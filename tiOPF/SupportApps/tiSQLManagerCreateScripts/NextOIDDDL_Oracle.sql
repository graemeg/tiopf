/* Drop table to start off */
drop table  next_oid ;

/* Create Next_OID table */
create table Next_OID
  ( oid number( 12 )) ;

insert into next_oid ( oid )
values ( 1000000 ) ;

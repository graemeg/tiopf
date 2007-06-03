Create Database 'AdrsBookSimple.fdb' user 'SYSDBA' password 'masterkey';

connect  "AdrsBookSimple.fdb" user "SYSDBA" password "masterkey" ;

create table person
  ( oid               varchar(36)    not null,
    first_name        varchar(60),
    last_name         varchar(60),
    title             varchar(10),
    initials          varchar(10),
    notes             BLOB SUB_TYPE TEXT
  );

alter table person add
constraint person_pk
primary key (oid);

create table eadrs
  ( oid               varchar(36)    not null,
    oid_person        varchar(36)    not null,
    eadrs_type        varchar(36)    not null,
    eadrs_text        varchar(60)
  );


alter table eadrs add
constraint eadrs_pk
primary key (oid);

alter table eadrs add
constraint eadrs_person_fk
foreign key (oid_person)
references person;

commit;

exit;

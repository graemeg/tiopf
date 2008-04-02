Create Database 'Adrs.fdb' user 'SYSDBA' password 'masterkey';

connect  "Adrs.fdb" user "SYSDBA" password "masterkey" ;

create table adrs_type
  ( oid               varchar(36)    not null,
    text              varchar(60)    not null
  );

alter table adrs_type add
constraint adrs_type_pk
primary key (oid);

create table eadrs_type
  ( oid               varchar(36)    not null,
    text              varchar(60)    not null
  );

alter table eadrs_type add
constraint eadrs_type_pk
primary key (oid);

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

create table adrs
  ( oid               varchar(36)    not null,
    oid_person        varchar(36)    not null,
    oid_adrs_type     varchar(36)    not null,
    lines             varchar(120),
    suburb            varchar(60),
    state             varchar(60),
    pcode             varchar(60),
    country           varchar(60)
  );

alter table adrs add
constraint adrs_pk
primary key (oid);

alter table adrs add
constraint adrs_person_fk
foreign key (oid_person)
references person;

alter table adrs add
constraint adrs_adrs_type_fk
foreign key (oid_adrs_type)
references adrs_type;

create table eadrs
  ( oid               varchar(36)    not null,
    oid_person        varchar(36)    not null,
    oid_eadrs_type     varchar(36)    not null,
    eadrs_text        varchar(60)
  );


alter table eadrs add
constraint eadrs_pk
primary key (oid);

alter table eadrs add
constraint eadrs_person_fk
foreign key (oid_person)
references person;

alter table eadrs add
constraint eadrs_adrs_type_fk
foreign key (oid_eadrs_type)
references eadrs_type;

exit;

drop table deploy_mgr ;

create table deploy_mgr
( id               number( 12 )    not null,
  file_group       varchar2( 20 )  not null,
  file_name        varchar2( 256 ) not null,
  deploy_from_path varchar2( 256 ),
  deploy_to_root   varchar2( 256 ),
  file_size        number( 7 )     not null,
  file_date        date            not null,
  file_version     varchar2( 20 )  not null,
  compression      varchar2( 20 ),
  launch           char( 1 )       not null,
  effective_start  date            not null,
  effective_end    date            not null,
  file_bin         long raw
) ;


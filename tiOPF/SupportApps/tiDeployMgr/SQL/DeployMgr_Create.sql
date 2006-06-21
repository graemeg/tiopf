drop table deploy_mgr_params ;
drop table deploy_mgr_files ;
drop table deploy_mgr_apps ;

create table deploy_mgr_apps
(  OID                Number( 15 )      not null
  ,LastChanged        Date              not null
  ,LastChanged_By     VarChar2( 20 )    not null
  ,App_Name           VarChar2( 20 )    not null
  ,DisplayText        VarChar2( 60 )    not null
  ,Description        VarChar2( 2000 )
  ,Compression        VarChar2( 20 )    Not Null
  ,Image              Long Raw
  ,constraint deploy_mgr_apps_PK
     Primary Key ( OID ) using index
  TableSpace Index_data
) TableSpace User_Data ;

-- Create a second unique index
Create unique index I_deploy_mgr_apps
on deploy_mgr_apps
  ( App_Name ) TableSpace Index_Data ;

insert into deploy_mgr_apps values
( 1, sysdate, user, 'Test', 'A test application', 'A test application', 'No Compression', null ) ;

create table deploy_mgr_files
(
   OID                Number( 15 )    NOT NULL
  ,LastChanged        Date            not null
  ,LastChanged_By     VarChar2( 20 )  not null
  ,OID_Deploy_Mgr_App Number( 15 )    not null
  ,Deploy_From        VarChar2( 256 ) NOT NULL
  ,Deploy_To_Root     VarChar2(256)
  ,File_Size          Number( 7 )     NOT NULL
  ,File_Date          Date            NOT NULL
  ,File_Version       VarChar2( 20 )  NOT NULL
  ,Launch             Char( 1 )       NOT NULL
  ,File_Bin           Long Raw
  ,constraint deploy_mgr_files_PK
     Primary Key ( OID ) using index
  TableSpace Index_data
) TableSpace User_Data ;

-- Create a second unique index
Create unique index I_deploy_mgr_files
on deploy_mgr_files
  ( OID_Deploy_Mgr_App, Deploy_To_Root, Deploy_From ) TableSpace Index_Data ;

-- Add a foreign key constraint
Alter Table deploy_mgr_files
add foreign key ( OID_Deploy_Mgr_App ) references deploy_mgr_apps ( OID ) ;

create table deploy_mgr_params
(
   OID                Number(    15  ) NOT NULL
  ,LastChanged        Date             not null
  ,LastChanged_By     VarChar2(  20  ) not null
  ,OID_Deploy_Mgr_App Number(    15  ) not null
  ,Params             VarChar2( 256  ) NOT NULL
  ,Description        VarChar2( 2000 )
  ,DisplayText        VarChar2(   60 )
  ,constraint deploy_mgr_params_PK
     Primary Key ( OID ) using index
  TableSpace Index_data
) TableSpace User_Data ;

-- Create a second unique index
Create unique index I_deploy_mgr_params
on deploy_mgr_params
  ( OID_Deploy_Mgr_App, params ) TableSpace Index_Data ;

-- Add a foreign key constraint
Alter Table deploy_mgr_params
add foreign key ( OID_Deploy_Mgr_App ) references deploy_mgr_apps ( OID ) ;


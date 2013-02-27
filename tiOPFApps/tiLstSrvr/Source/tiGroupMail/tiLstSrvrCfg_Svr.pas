unit tiLstSrvrCfg_Svr;

interface

implementation
uses
  tiLstSrvrCfg_BOM
  ,tiClassToDBMap_BOM
  ,tiPersist
  ;

initialization

  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiLstSrvrCfg,  'list_server_config',  'oid',                         'oid',      [pktDB]  ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiLstSrvrCfg,  'list_server_config',  'listName',                    'list_Name' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiLstSrvrCfg,  'list_server_config',  'listNameLong',                'list_Name_Long' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiLstSrvrCfg,  'list_server_config',  'ListEMailAdrs',               'List_EMail_Adrs' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiLstSrvrCfg,  'list_server_config',  'ListMasterEMailAdrs',         'List_Master_EMail_Adrs' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiLstSrvrCfg,  'list_server_config',  'ListMasterName',              'List_Master_Name' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiLstSrvrCfg,  'list_server_config',  'SenderAdrsTypeAsString',      'Sender_Adrs_Type' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiLstSrvrCfg,  'list_server_config',  'AcceptPostsFromAsString',     'Accept_Posts_From' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiLstSrvrCfg,  'list_server_config',  'MaxMessageSize',              'Max_Message_Size' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiLstSrvrCfg,  'list_server_config',  'Active',                      'Active' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiLstSrvrCfg,  'list_server_config',  'POPHost',                     'POP_Host' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiLstSrvrCfg,  'list_server_config',  'POPUserID',                   'POP_User_ID' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiLstSrvrCfg,  'list_server_config',  'POPPassword',                 'POP_Pass_word' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiLstSrvrCfg,  'list_server_config',  'SMTPHost',                    'SMTP_Host' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiLstSrvrCfg,  'list_server_config',  'ArchiveDir',                  'Archive_Dir' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiLstSrvrCfg,  'list_server_config',  'JoinMessage',                 'Join_Message' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiLstSrvrCfg,  'list_server_config',  'LeaveMessage',                'Leave_Message' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiLstSrvrCfg,  'list_server_config',  'RejectMessageSizeMessage',    'Reject_Message_Size_Message' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiLstSrvrCfg,  'list_server_config',  'NonListMemberMessage',        'Non_List_Member_Message' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiLstSrvrCfg,  'list_server_config',  'RejectAttachmentMessage',     'Reject_Attachment_Message' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiLstSrvrCfg,  'list_server_config',  'FooterMessage',               'Footer_Message' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TtiLstSrvrCfg,  'list_server_config',  'MessageIndex',                'Message_Index' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterCollection( TtiLstSrvrCfgs, TtiLstSrvrCfg  ) ;

  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(    TListMember,  'list_members',  'oid', 'oid', [pktDB]  ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(    TListMember,  'list_members',  'owner.oid', 'owner_oid', [pktFK]  ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping(    TListMember,  'list_members',  'EMailAddress', 'EMail_Address' ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterCollection( TListMembers, TListMember ) ;

end.

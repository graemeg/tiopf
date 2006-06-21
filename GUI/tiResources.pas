unit tiResources;

interface

const
  cResTI_16N = '_16N' ;
  cResTI_16H = '_16H' ;
  cResTI_16D = '_16D' ;

  cResTI_24N = '_24N' ;
  cResTI_24H = '_24H' ;
  cResTI_24D = '_24D' ;


  cResTI_Tick          = 'ti_Tick' ;
  cResTI_Tick16D       = 'ti_Tick_16D' ;
  cResTI_Tick16H       = 'ti_Tick_16H' ;
  cResTI_Tick16N       = 'ti_Tick_16N' ;
  cResTI_Tick16ND      = 'ti_Tick_16ND' ;
  cResTI_Cross         = 'ti_Cross' ;
  cResTI_Cross16D      = 'ti_Cross_16D' ;
  cResTI_Cross16H      = 'ti_Cross_16H' ;
  cResTI_Cross16N      = 'ti_Cross_16N' ;
  cResTI_Cross16ND     = 'ti_Cross_16ND' ;
  cResTI_Browse16D     = 'ti_Browse_16D' ;
  cResTI_Browse16H     = 'ti_Browse_16H' ;
  cResTI_Browse16N     = 'ti_Browse_16N' ;
  cResTI_CancelSave16D = 'ti_CancelSave_16D' ;
  cResTI_CancelSave16H = 'ti_CancelSave_16H' ;
  cResTI_CancelSave16N = 'ti_CancelSave_16N' ;

  cResTI_View          = 'ti_View' ;
  cResTI_Insert        = 'ti_Insert' ;
  cResTI_Edit          = 'ti_Edit' ;
  cResTI_Delete        = 'ti_Delete' ;
  cResTI_Deletex       = 'ti_Deletex';

  cResTI_View08D       = 'ti_View_08D' ;
  cResTI_View08H       = 'ti_View_08H' ;
  cResTI_View08N       = 'ti_View_08N' ;
  cResTI_Insert08D     = 'ti_Insert_08D' ;
  cResTI_Insert08H     = 'ti_Insert_08H' ;
  cResTI_Insert08N     = 'ti_Insert_08N' ;
  cResTI_Edit08D       = 'ti_Edit_08D' ;
  cResTI_Edit08H       = 'ti_Edit_08H' ;
  cResTI_Edit08N       = 'ti_Edit_08N' ;
  cResTI_Delete08D     = 'ti_Delete_08D' ;
  cResTI_Delete08H     = 'ti_Delete_08H' ;
  cResTI_Delete08N     = 'ti_Delete_08N' ;

  cResTI_View16D       = 'ti_View_16D' ;
  cResTI_View16H       = 'ti_View_16H' ;
  cResTI_View16N       = 'ti_View_16N' ;
  cResTI_Insert16D     = 'ti_Insert_16D' ;
  cResTI_Insert16H     = 'ti_Insert_16H' ;
  cResTI_Insert16N     = 'ti_Insert_16N' ;
  cResTI_Edit16D       = 'ti_Edit_16D' ;
  cResTI_Edit16H       = 'ti_Edit_16H' ;
  cResTI_Edit16N       = 'ti_Edit_16N' ;
  cResTI_Delete16D     = 'ti_Delete_16D' ;
  cResTI_Delete16H     = 'ti_Delete_16H' ;
  cResTI_Delete16N     = 'ti_Delete_16N' ;

  cResTI_View24D       = 'ti_View_24D' ;
  cResTI_View24H       = 'ti_View_24H' ;
  cResTI_View24N       = 'ti_View_24N' ;
  cResTI_Insert24D     = 'ti_Insert_24D' ;
  cResTI_Insert24H     = 'ti_Insert_24H' ;
  cResTI_Insert24N     = 'ti_Insert_24N' ;
  cResTI_Edit24D       = 'ti_Edit_24D' ;
  cResTI_Edit24H       = 'ti_Edit_24H' ;
  cResTI_Edit24N       = 'ti_Edit_24N' ;
  cResTI_Delete24D     = 'ti_Delete_24D' ;
  cResTI_Delete24H     = 'ti_Delete_24H' ;
  cResTI_Delete24N     = 'ti_Delete_24N' ;

  cResTI_Execute16D    = 'ti_Execute_16D' ;
  cResTI_Execute16H    = 'ti_Execute_16H' ;
  cResTI_Execute16N    = 'ti_Execute_16N' ;
  cResTI_FileOpen      = 'ti_FileOpen';
  cResTI_FileOpen16D   = 'ti_FileOpen_16D' ;
  cResTI_FileOpen16H   = 'ti_FileOpen_16H' ;
  cResTI_FileOpen16N   = 'ti_FileOpen_16N' ;
  cResTI_Find          = 'ti_Find' ;
  cResTI_Find16D       = 'ti_Find_16D' ;
  cResTI_Find16H       = 'ti_Find_16H' ;
  cResTI_Find16N       = 'ti_Find_16N' ;
  cResTI_FullScreen    = 'ti_FullScreen';
  cResTI_FullScreen16D = 'ti_FullScreen_16D' ;
  cResTI_FullScreen16H = 'ti_FullScreen_16H' ;
  cResTI_FullScreen16N = 'ti_FullScreen_16N' ;
  cResTI_GraphBar16D   = 'ti_GraphBar_16D' ;
  cResTI_GraphBar16H   = 'ti_GraphBar_16H' ;
  cResTI_GraphBar16N   = 'ti_GraphBar_16N' ;

  cResTI_OpenImage     = 'ti_OpenImage';
  cResTI_OpenImage16D  = 'ti_OpenImage_16D' ;
  cResTI_OpenImage16H  = 'ti_OpenImage_16H' ;
  cResTI_OpenImage16N  = 'ti_OpenImage_16N' ;

  cResTI_GraphLine     = 'ti_GraphLine'     ;
  cResTI_GraphLine16D  = 'ti_GraphLine_16D' ;
  cResTI_GraphLine16H  = 'ti_GraphLine_16H' ;
  cResTI_GraphLine16N  = 'ti_GraphLine_16N' ;

  cResTI_Query         = 'ti_Query' ;
  cResTI_Query16D      = 'ti_Query_16D' ;
  cResTI_Query16H      = 'ti_Query_16H' ;
  cResTI_Query16N      = 'ti_Query_16N' ;
  cResTI_ReDo16D       = 'ti_ReDo_16D' ;
  cResTI_ReDo16H       = 'ti_ReDo_16H' ;
  cResTI_ReDo16N       = 'ti_ReDo_16N' ;
  cResTI_SaveAll16D    = 'ti_SaveAll_16D' ;
  cResTI_SaveAll16H    = 'ti_SaveAll_16H' ;
  cResTI_SaveAll16N    = 'ti_SaveAll_16N' ;
  cResTI_Save          = 'ti_Save' ;
  cResTI_Save16D       = 'ti_Save_16D' ;
  cResTI_Save16H       = 'ti_Save_16H' ;
  cResTI_Save16N       = 'ti_Save_16N' ;
  cResTI_UnDo          = 'ti_UnDo' ;
  cResTI_UnDo16D       = 'ti_UnDo_16D' ;
  cResTI_UnDo16H       = 'ti_UnDo_16H' ;
  cResTI_UnDo16N       = 'ti_UnDo_16N' ;
  cResTI_Sort          = 'ti_Sort' ;
  cResTI_Sort16D       = 'ti_Sort_16D' ;
  cResTI_Sort16H       = 'ti_Sort_16H' ;
  cResTI_Sort16N       = 'ti_Sort_16N' ;

  cResTI_SelectCols            = 'ti_SelectCols' ;
  cResTI_SelectCols16D         = 'ti_SelectCols_16D' ;
  cResTI_SelectCols16H         = 'ti_SelectCols_16H' ;
  cResTI_SelectCols16N         = 'ti_SelectCols_16N' ;

  cResTI_Export     = 'ti_Export' ;
  cResTI_Export16D  = 'ti_Export_16D' ;
  cResTI_Export16H  = 'ti_Export_16H' ;
  cResTI_Export16N  = 'ti_Export_16N' ;

  cResTI_CopyToClipboard     = 'ti_CopyToClipboard' ;
  cResTI_CopyToClipboard16D  = 'ti_CopyToClipboard_16D' ;
  cResTI_CopyToClipboard16H  = 'ti_CopyToClipboard_16H' ;
  cResTI_CopyToClipboard16N  = 'ti_CopyToClipboard_16N' ;

  cResTI_PasteFromClipboard    = 'ti_PasteFromClipboard';
  cResTI_PasteFromClipboard16D = 'ti_PasteFromClipboard_16D';
  cResTI_PasteFromClipboard16H = 'ti_PasteFromClipboard_16H';
  cResTI_PasteFromClipboard16N = 'ti_PasteFromClipboard_16N';

  cResTI_ExportToCSV           = 'ti_ExportToCSV' ;
  cResTI_ExportToCSV16D        = 'ti_ExportToCSV_16D' ;
  cResTI_ExportToCSV16H        = 'ti_ExportToCSV_16H' ;
  cResTI_ExportToCSV16N        = 'ti_ExportToCSV_16N' ;

  cResTI_ExportToHTML          = 'ti_ExportToHTML' ;
  cResTI_ExportToHTML16D       = 'ti_ExportToHTML_16D' ;
  cResTI_ExportToHTML16H       = 'ti_ExportToHTML_16H' ;
  cResTI_ExportToHTML16N       = 'ti_ExportToHTML_16N' ;

  cResTI_ZoomIn                = 'ti_ZoomIn' ;
  cResTI_ZoomIn16D             = 'ti_ZoomIn_16D' ;
  cResTI_ZoomIn16H             = 'ti_ZoomIn_16H' ;
  cResTI_ZoomIn16N             = 'ti_ZoomIn_16N' ;

  cResTI_ZoomOut                = 'ti_ZoomOut' ;
  cResTI_ZoomOut16D             = 'ti_ZoomOut_16D' ;
  cResTI_ZoomOut16H             = 'ti_ZoomOut_16H' ;
  cResTI_ZoomOut16N             = 'ti_ZoomOut_16N' ;

  cResTI_Maximize               = 'ti_Maximize' ;
  cResTI_Maximize16D            = 'ti_Maximize_16D' ;
  cResTI_Maximize16H            = 'ti_Maximize_16H' ;
  cResTI_Maximize16N            = 'ti_Maximize_16N' ;

  cResTI_Exit                   = 'ti_Exit' ;
  cResTI_ArrowLeft              = 'ti_ArrowLeftBlue' ;
  cResTI_ArrowRight             = 'ti_ArrowRightBlue' ;
  cResTI_ArrowDoubleLeft        = 'ti_ArrowDoubleLeftBlue';
  cResTI_ArrowDoubleRight       = 'ti_ArrowDoubleRightBlue'; 
  cResTI_CloseWindow            = 'ti_CloseWindow' ;
  cResTI_Help                   = 'ti_Help' ;
  cResTI_HelpAbout              = 'ti_About' ;
  cResTI_HelpWhatsThis          = 'ti_WhatsThis' ;
  cResTI_WorkList               = 'ti_WorkList' ;
  cResTI_GoTo                   = 'ti_GoTo' ;

  cResTI_Copy1Left              = 'ti_Copy1Left' ;
  cResTI_CopyAllLeft            = 'ti_CopyAllLeft' ;
  cResTI_Copy1Right             = 'ti_Copy1Right' ;
  cResTI_CopyAllRight           = 'ti_CopyAllRight' ;

  cResTI_MoveUp                 = 'ti_MoveUp';
  cResTI_MoveDown               = 'ti_MoveDown';

  cResTI_CheckBoxChecked16N      = 'ti_CheckBoxChecked_16N';
  cResTI_CheckBoxUnChecked16N    = 'ti_CheckBoxUnChecked_16N';
  cResTI_RadioButtonChecked16N   = 'ti_RadioButtonChecked_16N';
  cResTI_RadioButtonUnChecked16N = 'ti_RadioButtonUnChecked_16N';
  cResTI_CheckBoxChecked16D      = 'ti_CheckBoxChecked_16D';
  cResTI_CheckBoxUnChecked16D    = 'ti_CheckBoxUnChecked_16D';
  cResTI_RadioButtonChecked16D   = 'ti_RadioButtonChecked_16D';
  cResTI_RadioButtonUnChecked16D = 'ti_RadioButtonUnChecked_16D';

  cResTI_Print                   = 'ti_Print';
  cResTI_Print16N                = 'ti_Print_16N';
  cResTI_Print16D                = 'ti_Print_16D';
  cResTI_Print16H                = 'ti_Print_16H';

  cResTI_PrintSetup              = 'ti_PrintSetup';
  cResTI_PrintSetup16N           = 'ti_PrintSetup_16N';
  cResTI_PrintSetup16D           = 'ti_PrintSetup_16D';
  cResTI_PrintSetup16H           = 'ti_PrintSetup_16H';

type
  TtiImageRes = (  tiRINone
                  ,tiRICopyToClipboard
                  ,tiRIZoomIn
                  ,tiRIZoomOut
                  ,tiRIMaximize
                  ,tiRIGraphLine
               ) ;
const
  cImageRes  : array[TtiImageRes] of string = (
    'None'
    ,cResTI_CopyToClipboard
    ,cResTI_ZoomIn
    ,cResTI_ZoomOut
    ,cResTI_Maximize
    ,cResTI_GraphLine
    ) ;



implementation
{$R tiImages.res}

// ToDo: Move the images from these res files into tiImages.res and add to the build script
{$R tiListView.res}
{$R tiListViewDif.res}
{$R tiPerAwareCtrls.res}

end.





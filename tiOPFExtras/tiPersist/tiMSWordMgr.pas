
{$I tiDefines.inc}

unit tiMSWordMgr;

interface
uses
  Classes
  ,Graphics
  ,OleServer
  ,Word97
  ,SysUtils
  ,tiStreams
  ,tiHTML
  ,Contnrs
  ;

type

  TParagraphStyle = (
     ParagraphStyle_Normal
    ,ParagraphStyle_Heading1
    ,ParagraphStyle_Heading2
    ,ParagraphStyle_Heading3
    ,ParagraphStyle_Heading4
    ,ParagraphStyle_NormalIndent
    ,ParagraphStyle_Caption
    ) ;

  TtiParagraphStyleFormat = class( TObject )
  private
    FFormatStr: string;
    FParagraphStyle: TParagraphStyle;
  public
    property ParagraphStyle : TParagraphStyle read FParagraphStyle write FParagraphStyle ;
    property FormatStr : string read FFormatStr write FFormatStr ;
  end ;

  TtiParagraphStyles = class( TObjectList )
  public
    procedure AddStyle( pStyle : TParagraphStyle ; const pText : string ) ;
    function  FindFormatStrByStyle( pStyle : TParagraphStyle ) : string ;
  end ;

  TtiMSWordMgrAbs = class( TObject )
  protected
    FParagraphStyles : TtiParagraphStyles ;
    procedure SetupStyles; virtual ; abstract ;
    function  GetPageNumbers: boolean; virtual ; abstract ;
    procedure SetPageNumbers(const Value: boolean); virtual ; abstract ;
  public
    constructor Create ; virtual ;
    destructor  Destroy ; override ;
    procedure   FileSaveAs( const pFileName : string ) ; virtual ; abstract ;
    function    NewParagraph( const pIndex : integer = -1 ) : Paragraph ; virtual ; abstract ;
    function    InsertText( const pText : string  ; const pStyle : TParagraphStyle = ParagraphStyle_Normal ) : variant ; virtual ; abstract ;
    function    InsertParagraph( const pText : string = '' ; const pStyle : TParagraphStyle = ParagraphStyle_Normal ) : variant ; virtual ; abstract ;
    procedure   InsertBookmark( const pText : string ) ; virtual ; abstract ;
    procedure   InsertHyperLink( const pDisplay, pURL, pNamedLocation : string ) ; virtual ; abstract ;
//    procedure   SetParagraphStyle( const pStyle : TParagraphStyle ) ; virtual ; abstract ;
//    procedure   InsertImage( const pGraphic  : TBitMap ;
//                             const pCaption : string = '' ) ; overload ; virtual ; abstract ;
    procedure   InsertImage( const pFileName : TFileName ;
                             const pCaption : string = '' ;
                             const pDiagramWidth : integer = 0 ) ; overload ; virtual ; abstract ;
    procedure   InsertTOC( pParagraphIndex    : byte = 1 ;
                           pUpperHeadingLevel : byte = 1 ;
                           pLowerHeadingLevel : byte = 9 ); virtual ; abstract ;
    procedure   InsertTable( pList    : TList ;
                             pCaption : string ;
                             aCols : array of string ; 
                             aHeadings : array of string ) ; overload ; virtual ; abstract ;
//    procedure   InsertTableCell( const pRow : integer ;
//                                 const pCol : integer ;
//                                 const pText : string ) ; virtual ; abstract ;
//    procedure   SetTableCellBorders( const pRow : integer ;
//                                     const pCol : integer ;
//                                     const pTop : boolean ;
//                                     const pBottom : boolean ;
//                                     const pLeft   : boolean ;
//                                     const pRight  : boolean ) ; virtual ; abstract ;
//    procedure   SetTableColWidth( const pCol : Byte ;
//                                  const pWidth : real ) ; virtual ; abstract ;
    property    PageNumbers : boolean read GetPageNumbers write SetPageNumbers ;
  end ;

  TtiMSWordMgr = class( TtiMSWordMgrAbs )
  private
    FWordDoc : TWordDocument ;
    FTable   : Table ;
    FDiagramIndex : integer ;
    FDiagramFileNameIndex : integer ;
    FTrue : OleVariant ;
    FFalse : OleVariant ;
    procedure   AutoFormatTable ;
    procedure   InsertTableCell( const pRow : integer ;
                                 const pCol : integer ;
                                 const pText : string ) ;
//    procedure   SetTableCellBorders( const pRow : integer ;
//                                     const pCol : integer ;
//                                     const pTop : boolean ;
//                                     const pBottom : boolean ;
//                                     const pLeft   : boolean ;
//                                     const pRight  : boolean ) ;
    procedure   SetParagraphStyle( const pStyle : TParagraphStyle = ParagraphStyle_Normal ) ;
  protected
    procedure SetupStyles; override ;
    function  GetPageNumbers: boolean; override ;
    procedure SetPageNumbers(const Value: boolean); override ;

  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    procedure   FileSaveAs( const pFileName : string ) ; override ;
    function    NewParagraph( const pIndex : integer = -1 ) : Paragraph ; override ;
    function    InsertText( const pText : string  ; const pStyle : TParagraphStyle = ParagraphStyle_Normal ) : variant ; override ;
    function    InsertParagraph( const pText : string = '' ; const pStyle : TParagraphStyle = ParagraphStyle_Normal) : variant ; override ;
    procedure   InsertBookmark( const pText : string ) ; override ;
    procedure   InsertHyperLink( const pDisplay, pURL, pNamedLocation : string ) ; override ;
//    procedure   InsertImage( const pGraphic  : TBitMap ;
//                             const pCaption : string = '' ) ; override ;
    procedure   InsertImage( const pFileName : TFileName ;
                             const pCaption : string = '' ;
                             const pDiagramWidth : integer = 0 ) ; override ;
    procedure   InsertTOC( pParagraphIndex    : byte = 1 ;
                           pUpperHeadingLevel : byte = 1 ;
                           pLowerHeadingLevel : byte = 9 ); override ;
    procedure   InsertTable( pList    : TList ;
                             pCaption : string ;
                             aCols : array of string ;
                             aHeadings : array of string ) ; override ;
//    procedure   SetTableColWidth( const pCol : Byte ;
//                                  const pWidth : real ) ; override ;
  end ;

  TtiHTMLDocMgr = class( TtiMSWordMgrAbs )
  private
    FFileName : String ;
    FStream   : TtiFileStream ;
    FHTMLBuilder : TtiHTMLBuilder ;
    function ImageDir( pAbsolute : boolean = false ) : string ;
//    FWordDoc : TWordDocument ;
//    FTable   : Table ;
//    FDiagramIndex : integer ;
//    FDiagramFileNameIndex : integer ;
//    FTrue : OleVariant ;
//    FFalse : OleVariant ;
  protected
    procedure SetupStyles; override ;
    function  GetPageNumbers: boolean; override ;
    procedure SetPageNumbers(const Value: boolean); override ;

  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    procedure   FileSaveAs( const pFileName : string ) ; override ;
    function    NewParagraph( const pIndex : integer = -1 ) : Paragraph ; override ;
    function    InsertText( const pText : string  ; const pStyle : TParagraphStyle = ParagraphStyle_Normal) : variant ; override ;
    function    InsertParagraph( const pText : string = '' ; const pStyle : TParagraphStyle = ParagraphStyle_Normal ) : variant ; override ;
    procedure   InsertBookmark( const pText : string ) ; override ;
    procedure   InsertHyperLink( const pDisplay, pURL, pNamedLocation : string ) ; override ;
//    procedure   SetParagraphStyle( const pStyle : TParagraphStyle ) ; override ;
//    procedure   InsertImage( const pGraphic  : TBitMap ;
//                             const pCaption : string = '' ) ; override ;
    procedure   InsertImage( const pFileName : TFileName ;
                             const pCaption : string = '' ;
                             const pDiagramWidth : integer = 0 ) ; override ;
    procedure   InsertTOC( pParagraphIndex    : byte = 1 ;
                           pUpperHeadingLevel : byte = 1 ;
                           pLowerHeadingLevel : byte = 9 ); override ;
//    procedure   InsertTableCell( const pRow : integer ;
//                                 const pCol : integer ;
//                                 const pText : string ) ; override ;
    procedure   InsertTable( pList    : TList ;
                             pCaption : string ;
                             aCols : array of string ;
                             aHeadings : array of string ) ; override ;
//    procedure   SetTableCellBorders( const pRow : integer ;
//                                     const pCol : integer ;
//                                     const pTop : boolean ;
//                                     const pBottom : boolean ;
//                                     const pLeft   : boolean ;
//                                     const pRight  : boolean ) ; override ;
//    procedure   SetTableColWidth( const pCol : Byte ;
//                                  const pWidth : real ) ; override ;
  end ;

procedure DocToHTML( const pFileNameFrom : TFileName ;
                     const pFileNameTo : TFileName ) ;



implementation
uses
  tiCom
  ,ComObj
  ,ClipBrd
  ,tiUtils
  ,tiDialogs
  ,FileCtrl
  ,tiLog
  ,TypInfo
  ;

const
  CaptionLabelDiagram = 'CaptionLabelDiagram' ;


procedure DocToHTML( const pFileNameFrom : TFileName ;
                    const pFileNameTo : TFileName ) ;
var
  lWordApplication    : TWordApplication ;
  lFileNameFrom       : OleVariant ;
  lFileNameTo         : OleVariant ;
  lDocFrom            : OleVariant ;
  lFileConverter      : FileConverter ;
  lFileConverterIndex : OleVariant ;
begin

  lFileNameFrom := pFileNameFrom ;
  lFileNameTo   := pFileNameTo ;

  lWordApplication := TWordApplication.Create( nil ) ;
  try
    lDocFrom :=
      lWordApplication.Documents.Open(
        lFileNameFrom,
        EmptyParam,
        EmptyParam,
        EmptyParam,
        EmptyParam,
        EmptyParam,
        EmptyParam,
        EmptyParam,
        EmptyParam,
        EmptyParam
      ) ;

    // Get the file converter
    lFileConverterIndex := 1 ;
    while lFileConverterIndex <= lWordApplication.FileConverters.Count - 1 do
    begin
      if SameText( lWordApplication.FileConverters.Item(lFileConverterIndex).Name,
                   'HTML32.CNV' ) then
      begin
        lFileConverter := lWordApplication.FileConverters.Item(lFileConverterIndex) ;
        Break ; //==>
      end ;
      lFileConverterIndex := lFileConverterIndex + 1 ;
    end ;

    lDocFrom.SaveAs( lFileNameTo,
                     lFileConverter.SaveFormat ) ;

    lDocFrom.Close ;

  finally
    lWordApplication.Quit;
    lWordApplication.Free;
  end;
end ;


constructor TtiMSWordMgr.Create;
begin
  inherited ;
  FParagraphStyles.AddStyle( ParagraphStyle_Normal,          'Normal' );
  FParagraphStyles.AddStyle( ParagraphStyle_Heading1,        'Heading 1' );
  FParagraphStyles.AddStyle( ParagraphStyle_Heading2,        'Heading 2' );
  FParagraphStyles.AddStyle( ParagraphStyle_Heading3,        'Heading 3' );
  FParagraphStyles.AddStyle( ParagraphStyle_Heading4,        'Heading 4' );
  FParagraphStyles.AddStyle( ParagraphStyle_NormalIndent,    'Normal Indent' );
  FParagraphStyles.AddStyle( ParagraphStyle_Caption,         'Caption' );
  tiCoInitialize ;
  try
    FWordDoc := TWordDocument.Create( nil ) ;
    FWordDoc.Application.CaptionLabels.Add( CaptionLabelDiagram ) ;
    FWordDoc.ShowSpellingErrors := false ;
    SetupStyles ;
  except
    on e:exception do
      tiFmtException( 'Unable to connect to MS Word. Message: ' + e.message,
                      ClassName, 'Create' ) ;
  end ;
  FDiagramIndex := 1 ;
  FDiagramFileNameIndex := 1 ;
  FTrue  := true ;
  FFalse := false ;
end;

destructor TtiMSWordMgr.Destroy;
begin
  FWordDoc.Close ;
  FWordDoc.DisConnect ;
  FWordDoc.Free ;
//  FWordApp.Quit(False) ;
  tiCoUnInitialize ;
  inherited;
end;

procedure TtiMSWordMgr.InsertTOC( pParagraphIndex    : byte = 1 ;
                                  pUpperHeadingLevel : byte = 1 ;
                                  pLowerHeadingLevel : byte = 9 ) ;
var
  lRange : Range ;
  lUpperHeadingLevel : OleVariant ;
  lLowerHeadingLevel : OleVariant ;
begin
  lRange := NewParagraph( pParagraphIndex ).Range ;
  lUpperHeadingLevel := pUpperHeadingLevel ;
  lLowerHeadingLevel := pLowerHeadingLevel ;
  FWordDoc.TablesOfContents.Add(
    lRange,
    EmptyParam,         // UseHeadingStyles
    lUpperHeadingLevel, // UpperHeadingLevel
    lLowerHeadingLevel, // LowerHeadingLevel
    EmptyParam,         // UseFields
    EmptyParam,         // TableID
    EmptyParam,         // RightAlignPageNumbers
    EmptyParam,         // IncludePageNumbers
    EmptyParam          // AddedStyles
  ) ;
end ;

procedure TtiMSWordMgr.SetupStyles ;
var
  lParagraphStyle : OleVariant ;
  lTrue : OleVariant ;
  lFalse : OleVariant ;
begin

  FWordDoc.Sections.Item(1).PageSetup.LeftMargin :=
    FWordDoc.Application.CentimetersToPoints( 2.0 ) ;
  FWordDoc.Sections.Item(1).PageSetup.RightMargin :=
    FWordDoc.Application.CentimetersToPoints( 2.0 ) ;

  lTrue  := True ;
  lFalse := False ;

  lParagraphStyle := FParagraphStyles.FindFormatStrByStyle( ParagraphStyle_Heading2 );
  FWordDoc.Styles.Item(lParagraphStyle).Font.Italic := lFalse ;

  lParagraphStyle := FParagraphStyles.FindFormatStrByStyle( ParagraphStyle_Heading3 ) ;
  FWordDoc.Styles.Item(lParagraphStyle).Font.Bold := lTrue ;
  FWordDoc.Styles.Item(lParagraphStyle).Font.Size := 10 ;
  FWordDoc.Styles.Item(lParagraphStyle).ParagraphFormat.SpaceBefore := 0 ;
  FWordDoc.Styles.Item(lParagraphStyle).ParagraphFormat.SpaceAfter := 12 ;
  FWordDoc.Styles.Item(lParagraphStyle).ParagraphFormat.PageBreakBefore := lTrue ;

  lParagraphStyle := FParagraphStyles.FindFormatStrByStyle( ParagraphStyle_Heading4 ) ;
  FWordDoc.Styles.Item(lParagraphStyle).Font.Name := 'Times New Roman' ;
  FWordDoc.Styles.Item(lParagraphStyle).Font.Bold := lTrue ;
  FWordDoc.Styles.Item(lParagraphStyle).Font.Size := 10 ;
  FWordDoc.Styles.Item(lParagraphStyle).ParagraphFormat.SpaceBefore := 0 ;
  FWordDoc.Styles.Item(lParagraphStyle).ParagraphFormat.SpaceAfter := 12 ;

  FWordDoc.Application.Options.AllowFastSave := true ;

end ;

procedure TtiMSWordMgr.FileSaveAs(const pFileName: string);
var
  lFileName : OleVariant ;
begin
  lFileName := pFileName ;
  FWordDoc.SaveAs( lFileName ) ;
end;

//procedure TtiMSWordMgr.InsertImage(const pGraphic : TBitMap ; const pCaption : string = '' );
//var
//  lFileName : string ;
//begin
//  lFileName := tiGetTempFile( 'BMP' ) ;
//  pGraphic.SaveToFile( lFileName ) ;
//  InsertImage( lFileName, pCaption ) ;
//end;

function TtiMSWordMgr.InsertParagraph(const pText : string = '' ; const pStyle : TParagraphStyle = ParagraphStyle_Normal ): variant;
begin
  NewParagraph ;
  if pText <> '' then
    InsertText( pText ) ;
  if pStyle <> ParagraphStyle_Normal then
    SetParagraphStyle( pStyle )
  else
    SetParagraphStyle( ParagraphStyle_Normal );
end;

function TtiMSWordMgr.InsertText(const pText: string ; const pStyle : TParagraphStyle = ParagraphStyle_Normal): variant;
//var
//  lStart : OleVariant ;
//  lEnd   : OleVariant ;
begin
//  lEnd   := FWordDoc.Characters.Count-1 ;
//  lStart := lEnd ;
//  FWordDoc.Range( lStart, lEnd ).InsertAfter( pText ) ;

//  lEnd   := FWordDoc.Characters.Count-1 ;
//  lStart := lEnd ;
  FWordDoc.Paragraphs.Last.Range.InsertAfter( pText ) ;

  if pStyle <> ParagraphStyle_Normal then
    SetParagraphStyle( pStyle ) ;
{
  lStart := 0 ;
  lEnd   := FWordDoc.Characters.Count-1 ;
  FWordDoc.Range( lStart, lEnd ).InsertAfter( pText ) ;
  if pStyle <> '' then
    SetParagraphStyle( pStyle ) ;
}
end;

function TtiMSWordMgr.NewParagraph( const pIndex : integer = -1 ) : Paragraph ;
var
  lIndex : OleVariant ;
  lRange : OleVariant ;
begin
  if ( FWordDoc.Paragraphs.Count = 1 ) and
     ( FWordDoc.Characters.Count = 1 ) then
    Exit ; //==>
  if pIndex = -1 then
  begin
    FWordDoc.Paragraphs.Add( EmptyParam ) ;
    result := FWordDoc.Paragraphs.Last ;
  end
  else
  begin
    lIndex := pIndex ;
    lRange := FWOrdDoc.Paragraphs.Item(lIndex).Range;
    FWordDoc.Paragraphs.Add( lRange ) ;
    result := FWordDoc.Paragraphs.Item(lIndex) ;
  end ;
end;


procedure TtiMSWordMgr.SetParagraphStyle(const pStyle: TParagraphStyle );
var
  lStyle : OleVariant ;
begin
  lStyle := FParagraphStyles.FindFormatStrByStyle( pStyle ) ;
  FWordDoc.Paragraphs.Last.Set_Style( lStyle ) ;
end;

(*

unit EMailServer_SRV;

interface

// This will need cleaning up...
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, OleServer, Word97, SMTPWinshoe, Pop3Winshoe, WinshoeMessage, Winshoes  ;

resourcestring
  crsDocumentHeader =
    'TechInsite Charting Service - pre beta release.' + #13 + #13 +
    'To request a chart, send an email to charting@techinsite.com.au with a list ' +
    'of ASX codes, separated by commas in the subject line. The TechInsite charting ' +
    'service robot will send you a word document containing 12 month, daily charts for ' +
    'the companies you requested.' + #13 + #13 +
    'Each vertical bar indicates one trading day. A red bar shows a day where the share ' +
    'price closed lower than it opened. A green bar shows a day where the share price ' +
    'closed higher than it opened. The small, horizontal bar to the left of each ' +
    'vertical bar shows the opening price for the day and the horizontal bar to the ' +
    'right of each vertical shows the closing price.' + #13 + #13 +

    'The green line shows the 20 day moving average and the blue line shows the 10 ' +
    'day moving average.' + #13 + #13 ;

  crsDocumentFooter =
    #13 + #13 +
    'The information contained in this document is provided as an example of the ' +
    'type of data processing that can be provided by TechInsite. ' +
    'No warranty is given as to the accuracy or reliability of this data. ' +
    'The TechInsite charting service is in pre-beta which means that it is under ' +
    'development and is not to be relied upon for making trading decisions. ' + #13 + #13 +

    'To comment on this service, or to request more information, please email ' +
    'info@techinsite.com.au' ;


type
  TSWEMailServer = class( TObject )
  private
    FPOP: TWinshoePOP3;
    FMsgFrom: TWinshoeMessage;
    FSMTP: TWinshoeSMTP;
    FMsgTo: TWinshoeMessage;
    procedure Connect;
    procedure Disconnect ;
    procedure PrepareMessage;
    procedure ReadNext;
    procedure SendNext;
    procedure LogRequest ;
  public
    constructor create ;
    destructor  destroy ; override ;
    procedure   Execute ;
    procedure   PrepareWordDoc( const psSecurityCodes : string ; psFileName : string ) ;
  end ;


  TWordWriter = class( TObject )
  private
    FWD : TWordDocument ;
    procedure ChartToClipBoard( const psSecurityCode : string ) ;
    procedure WriteHeader ;
    procedure WriteFooter;
  public
    constructor Create ;
    destructor  Destroy ; override ;
    procedure   SaveToFile( const psFileName : string ) ;
    procedure   WriteChart( const psSecurityCode : string ) ;
    procedure   PrepareDoc( const psSecurityCodes : string ; psFileName : string ) ;
  end ;


implementation
uses
  tiUtils
  ,FBarGraph
  ,tiLog
  ,tiNextOID_Cli
  ,ClosingPriceEMailHistory_BOM
  ,tiPtnVisMgr
  ,cTIStockWatch
  ,tiPtnVisPerObj
  ,FMain    // This is bad, using a client side component in a server side unit
  ;

const
  cgsPOP3Server = 'techinsite.com.au' ;
  cgsUserID     = 'mb10618d' ;
  cgsPassword   = 'trilby' ;
  cgiPort       = 110 ;
  cgsHostAddress = 'mail.aimtec.com.au' ;
  cgsEMailAddress = 'Charting@TechInsite.com.au' ;
  cgsChartFileName = 'c:\temp\TechInsiteCharting.doc' ;


procedure TWordWriter.ChartToClipBoard(const psSecurityCode: string);
var
  lForm : TFormBarGraph ;
begin
  lForm := TFormBarGraph.Create( nil ) ;
  try
    lForm.ChartToClipBoard( psSecurityCode ) ;
  finally
    lForm.Free ;
  end ;
end;

constructor TWordWriter.Create;
begin
  inherited ;
  FWD := TWordDocument.Create( nil ) ;
  FWD.Connect ;
  //FWD.Close ;
end;

destructor TWordWriter.Destroy;
begin
  inherited;
  FWD.Close ;
  FWD.DisConnect ;
  FWD.Free ;
end;

procedure TWordWriter.PrepareDoc(const psSecurityCodes: string ; psFileName : string );
var
  i : integer ;
begin
  WriteHeader ;
  for i := 1 to tiNumToken( psSecurityCodes, ',' ) do
    WriteChart( upperCase( tiToken( psSecurityCodes, ',', i ))) ;
  WriteFooter ;
  Log( 'About to save to file ' + psFileName ) ;
  SaveToFile( psFileName ) ;
  Log( 'Done save to file ' + psFileName ) ;
end;

procedure TWordWriter.SaveToFile(const psFileName: string);
var
  lFileName : OleVariant ;
begin
  if FileExists( psFileName ) then
    DeleteFile( psFileName ) ;
  lFileName := psFileName;
  FWD.SaveAs( lFileName ) ;
end;

procedure TWordWriter.WriteChart(const psSecurityCode: string);
var
  lStart : OleVariant ;
  lEnd   : OleVariant ;
begin
  Log('About to write chart for ' + psSecurityCode ) ;
  lStart := 0 ;
  lEnd   := FWD.Characters.Count ;
  Log('About to write heading for ' + psSecurityCode ) ;

  FWD.Range( lStart, lEnd ).InsertAfter(
    Cr + psSecurityCode +
    ' - 12 Month Daily Chart.' + Cr ) ;

  Log('Done writing heading for ' + psSecurityCode ) ;
  lStart := FWD.Characters.Count-1 ;
  lEnd   := FWD.Characters.Count ;
  Log('About to copy chart to clipboard for ' + psSecurityCode ) ;
  ChartToClipBoard( psSecurityCode ) ;
  Log('Done copying chart to clipboard for ' + psSecurityCode ) ;
  Log('About to paste chart to clipboard for ' + psSecurityCode ) ;
  FWD.Range( lStart, lEnd ).Paste ;
  Log( 'Done pasting chart to clipboard for ' + psSecurityCode ) ;

end;

procedure TSWEMailServer.Connect;
begin
  // Set the settings in POP3
  FPOP.Host := cgsPOP3Server ;
  FPOP.Port := cgiPort ;
  FPOP.UserID := cgsUserID ;
  FPOP.Password := cgsPassword ;
  FPOP.Connect ;
  if not FPOP.Connected then
    LogError( 'Unable to connect to POP server' )
end;

constructor TSWEMailServer.create;
begin
  inherited ;
  FPOP     := TWinshoePOP3.Create( nil ) ;
  FMsgFrom := TWinshoeMessage.Create( nil );
  FSMTP    := TWinshoeSMTP.Create( nil ) ;
  FMsgTo   := TWinshoeMessage.Create( nil ) ;
end;

destructor TSWEMailServer.destroy;
begin
  FPOP.Free ;
  FMsgFrom.Free ;
  FSMTP.Free ;
  FMsgTo.Free ;
  inherited;
end;

procedure TSWEMailServer.Disconnect;
begin
   if FPop.Connected then
     Fpop.DisConnect ;
end;

procedure TSWEMailServer.Execute ;
begin
    Connect ;
    Log( 'Message count: ' + IntToStr( FPOP.CheckMessages )) ;
    if FPOP.CheckMessages < 1 then
    begin
      Log( 'Nothing to process' ) ;
      Disconnect ;
      Exit ; //==>
    end ;

    ReadNext ;
    Disconnect ;
    LogRequest ;

    PrepareMessage ;
    SendNext ;
    //DeleteFile( cgsChartFileName ) ;
end;

procedure TSWEMailServer.LogRequest;
var
  lData : TClosingPriceEMailRequest ;
begin
  lData := TClosingPriceEMailRequest(
             gNextOID.NewPerObjAbs( TClosingPriceEMailRequest )) ;
  try
    lData.SubjectLine := FMsgFrom.Subject ;
    lData.ChangedDate := now ;
    lData.AppUser.EMailAddress := FMsgFrom.From ;
    gVisMgr.Execute( cgVisUserNameReadyEMailAddress, lData.AppUser ) ;
    if not lData.AppUser.Valid then
    begin
      lData.AppUser.ObjectState  := posCreate ;
      lData.AppUser.OID          := gNextOID.NextOID ;
      lData.AppUser.LastName     := cgsUnknown ;
      lData.AppUser.FirstName    := cgsUnknown ;
      lData.AppUser.UserPassword := cgsUnknown ;
      gVisMgr.Execute( cgVisUserNameInsert, lData.AppUser ) ;
    end ;
    gVisMgr.Execute( cgVisClosingPriceEMailRequestInsert, lData ) ;
    FormMain.Log( lData.Caption ) ;
  finally
    lData.Free ;
  end ;
end;

procedure TSWEMailServer.PrepareMessage ;
begin
  //FMSGTo.Too.Text  := 'peter_hinrichsen@techinsite.com.au' ;
  FMSGTo.Too.Text  := FMsgFrom.From ;
  FMSGTo.From      := cgsEMailAddress ;
  FMSGTo.Subject   := 'TechInsite charting service' ;
  FMSGTo.Text.Text := 'Thank you for using TechInsite''s charting service.' + Cr( 2 ) +
                      'The charts for ' +
                      FMSGFrom.Subject + ' are attached.' ;
  FMSGTo.Attachments.Clear;
  PrepareWordDoc( FMSGFrom.Subject, cgsChartFileName ) ;
  FMSGTo.Attachments.AddAttachment( cgsChartFileName ) ;
end ;

procedure TSWEMailServer.PrepareWordDoc(const psSecurityCodes: string; psFileName: string);
var
  lWordWriter : TWordWriter ;
begin
  lWordWriter := TWordWriter.Create ;
  try
    lWordWriter.PrepareDoc( psSecurityCodes, psFileName ) ;
  finally
    lWordWriter.Free ;
  end ;
end;

procedure TSWEMailServer.ReadNext ;
begin
    FMsgFrom.Clear ;
    FMsgFrom.ExtractAttachments := True ;
    FPOP.RetrieveHeader( 1, FMsgFrom ) ;
    Log([ FMsgFrom.Subject, FMsgFrom.From, DateToStr(FMsgFrom.Date)]) ;
    FPop.Delete( 1 ) ;
end ;

procedure TSWEMailServer.SendNext ;
begin
  Log( 'Sending <' + FMsgFrom.Subject + '> to ' + FMSGTo.Too.Text ) ;
  FSMTP.Host := cgsHostAddress ;
  FSMTP.Send( FMSGTo ) ;
end ;

procedure TWordWriter.WriteHeader;
var
  lStart : OleVariant ;
  lEnd   : OleVariant ;
begin

  Log('About to write document header.' ) ;

  lStart := 0 ;
  lEnd   := FWD.Characters.Count ;

  FWD.Range( lStart, lEnd ).InsertAfter( crsDocumentHeader ) ;

  Log('Done writing write document header.' ) ;

end;

procedure TWordWriter.WriteFooter;
var
  lStart : OleVariant ;
  lEnd   : OleVariant ;
begin

  Log('About to write document footer.' ) ;

  lStart := 0 ;
  lEnd   := FWD.Characters.Count ;

  FWD.Range( lStart, lEnd ).InsertAfter( crsDocumentFooter ) ;

  Log('Done writing write document footer.' ) ;

end;

end.

*)



procedure TtiMSWordMgr.InsertImage( const pFileName : TFileName ;
                                    const pCaption : string = '' ;
                                    const pDiagramWidth : integer = 0 );
var
  lRange : OleVariant ;
  lStart : OleVariant ;
  lEnd   : OleVariant ;
  lInlineShape : OleVariant ;
  lDiagramWidth : Single ;
  lPageWidth : Extended ;
begin

  if pDiagramWidth = 0 then
  begin
    lPageWidth :=
      FWordDoc.Sections.Item(1).PageSetup.PageWidth -
      FWordDoc.Sections.Item(1).PageSetup.LeftMargin -
      FWordDoc.Sections.Item(1).PageSetup.RightMargin ;
    lDiagramWidth :=
        lPageWidth -
        FWordDoc.Application.CentimetersToPoints( 0.5 ) ;
  end
  else
    lDiagramWidth :=
      FWordDoc.Application.CentimetersToPoints( pDiagramWidth ) ;

  InsertParagraph( ' ' ) ;
  lStart := FWordDoc.Characters.Count-1 ;
  lEnd   := FWordDoc.Characters.Count ;
  lRange := FWordDoc.Range( lStart, lEnd ) ;

  lInLineShape :=
    lRange.InlineShapes.AddPicture(
      pFileName,
      EmptyParam,
      EmptyParam,
      EmptyParam ) ;

  lInLineShape.LockAspectRatio := True ;
  if lInLineShape.Width > lDiagramWidth then
    lInLineShape.Width := lDiagramWidth ;

  if pCaption <> '' then
  begin
    InsertParagraph( 'Diagram ' +
                     IntToStr( FDiagramIndex ) +
                     '. ' + pCaption,
                     ParagraphStyle_Caption ) ;
    Inc( FDiagramIndex ) ;
  end ;

  SetParagraphStyle( ParagraphStyle_Normal ) ;

end;


procedure TtiMSWordMgr.InsertTableCell(const pRow, pCol: integer;
  const pText: string);
begin
  Assert( FTable <> nil, 'Table not assigned' ) ;
  FTable.Cell( pRow, pCol ).Range.Text := pText ;
end;

{
procedure TtiMSWordMgr.InsertPageBreak;
var
  lBreakType : OleVariant ;
  lCollapseDirection : OleVariant ;
  lRange : Range ;
  lStart, lEnd : OleVariant ;
begin

  InsertParagraph( ' ' ) ;
  lStart := FWordDoc.Characters.Count-1 ;
  lEnd   := FWordDoc.Characters.Count ;
  lRange := FWordDoc.Range( lStart, lEnd ) ;
  lRange.InsertBreak( lBreakType ) ;

end;
}

//procedure TtiMSWordMgr.SetTableColWidth(const pCol: Byte; const pWidth: real);
//begin
//  Assert( FTable <> nil, 'Table not assigned' ) ;
//  FTable.Columns.Item(pCol).Width := FWordDoc.Application.CentimetersToPoints( pWidth ) ;
//end;

//procedure TtiMSWordMgr.SetTableCellBorders(const pRow, pCol: integer;
//  const pTop, pBottom, pLeft, pRight: boolean);
//begin
//  Assert( FTable <> nil, 'Table not assigned' ) ;
//  FTable.Cell( pRow, pCol ).Borders.Item( wdBorderTop ).Visible    := pTop ;
//  FTable.Cell( pRow, pCol ).Borders.Item( wdBorderBottom ).Visible := pBottom ;
//  FTable.Cell( pRow, pCol ).Borders.Item( wdBorderLeft ).Visible   := pLeft ;
//  FTable.Cell( pRow, pCol ).Borders.Item( wdBorderRight ).Visible  := pRight ;
//end;

function TtiMSWordMgr.GetPageNumbers: boolean;
begin
  Assert( false, 'Under construction' ) ;
  result := false ;
end;

procedure TtiMSWordMgr.SetPageNumbers(const Value: boolean);
var
  lPageNoFormat : OleVariant ;
  lFirstPage : OleVariant ;
begin
  lPageNoFormat := wdAlignPageNumberRight ;
  lFirstPage := 1 ;
  if Value then
    FWordDoc.Sections.Item(1).Footers.Item( wdHeaderFooterPrimary ).PageNumbers.Add(lPageNoFormat, lFirstPage)
  else
    Assert( false, 'Under construction: Can not turn page number off once they have been set on' ) ;
end;

procedure TtiMSWordMgr.InsertBookmark(const pText: string);
var
  lRange : Range ;
  lStart, lEnd : OleVariant ;
begin
  lStart := FWordDoc.Characters.Count-1 ;
  lEnd   := FWordDoc.Characters.Count ;
  lRange :=
    FWordDoc.Range( lStart, lEnd ) ;
  lRange.Bookmarks.Add( pText, EmptyParam ) ;
end;

procedure TtiMSWordMgr.InsertHyperLink(const pDisplay, pURL, pNamedLocation : string);
var
  lRange : Range ;
//  lStart, lEnd : OleVariant ;
  lNamedLocation : OleVariant ;
  lURL : OleVariant ;
begin

//  FWordDoc.Paragraphs.Last.Range.InsertAfter( pDisplay ) ;
//  lStart := FWordDoc.Characters.Count-Length( pDisplay )-1 ;
//  lEnd   := FWordDoc.Characters.Count ;
//  lRange := FWordDoc.Range( lStart, lEnd ) ;

  InsertParagraph( pDisplay, ParagraphStyle_NormalIndent );
  lRange := FWordDoc.Paragraphs.Last.Range ;

  lNamedLocation := pNamedLocation ;
  lURL := pURL ;

  if ( pURL = '' ) and ( pNamedLocation = '' ) then
    tiFmtException( ClassName, 'InsertHyperlink', 'URL and location not assigned' )
  else if ( pURL <> '' ) and ( pNamedLocation = '' ) then
    FWordDoc.Hyperlinks.Add( lRange, lURL, EmptyParam )
  else if ( pURL = '' ) and ( pNamedLocation <> '' ) then
    FWordDoc.Hyperlinks.Add( lRange, EmptyParam, lNamedLocation )
  else if ( pURL <> '' ) and ( pNamedLocation <> '' ) then
    FWordDoc.Hyperlinks.Add( lRange, lURL, lNamedLocation )
  else
    tiFmtException( ClassName, 'InsertHyperlink', 'Invalid parameters' ) ;

end;

procedure TtiMSWordMgr.AutoFormatTable;
var
  lTableFormat : OleVariant ;
begin
  lTableFormat := wdTableFormatClassic1 ;
  FTable.AutoFormat(
      lTableFormat, // Format
      FTrue,        // ApplyBorders
      FFalse,       // ApplyShading
      FTrue,        // ApplyFont
      FTrue,        // ApplyColor
      FTrue,        // ApplyHeadingRows
      FFalse,       // ApplyLastRow
      FFalse,       // ApplyFirstColumn
      FFalse,       // ApplyLastColumn
      FTrue          //AutoFit
 ) ;

end;

{ TtiHTMLDocMgr }

constructor TtiHTMLDocMgr.Create;
begin
  inherited;
  FHTMLBuilder := TtiHTMLBuilder.Create ;
  FParagraphStyles.AddStyle( ParagraphStyle_Normal,       '' );
  FParagraphStyles.AddStyle( ParagraphStyle_Heading1,     'h1' );
  FParagraphStyles.AddStyle( ParagraphStyle_Heading2,     'h2' );
  FParagraphStyles.AddStyle( ParagraphStyle_Heading3,     'h3' );
  FParagraphStyles.AddStyle( ParagraphStyle_Heading4,     'h4' );
  FParagraphStyles.AddStyle( ParagraphStyle_NormalIndent, '' );
  FParagraphStyles.AddStyle( ParagraphStyle_Caption,      'b' );
end;

destructor TtiHTMLDocMgr.Destroy;
begin
  FHTMLBuilder.Free ;
  FStream.Free ;
  inherited;
end;

procedure TtiHTMLDocMgr.FileSaveAs(const pFileName: string);
var
  lDir : string ;
begin
  if FFileName <> '' then
    Exit ; //==>
  FFileName := pFileName ;
  FStream :=
    TtiFileStream.Create(
       pFileName,
       fmCreate or fmShareCompat ) ;
  lDir := tiRemoveTrailingSlash( ImageDir( true )) ;
  if DirectoryExists( lDir ) then
    tiForceRemoveDir( lDir ) ;
  ForceDirectories( lDir ) ;
end;

function TtiHTMLDocMgr.GetPageNumbers: boolean;
begin
  Assert( false, 'Under construction' ) ;
  result := false ;
end;

function TtiHTMLDocMgr.ImageDir(pAbsolute: boolean=false): string;
begin
  Assert( FFileName <> '', 'FFileName not assigned' ) ;
  if pAbsolute then
    result := FFileName
  else
    result := ExtractFileName( FFileName ) ;
  result := tiRemoveExtension( result ) + '_Images' ;
  result := tiAddTrailingSlash( result ) ;
end;

procedure TtiHTMLDocMgr.InsertBookmark(const pText: string);
begin
  Assert( FStream <> nil, 'FStream not assigned' ) ;
  FStream.Write( '<A NAME="' + pText + '"></A>' ) ;
//<P><A NAME="Line_1">Line 1</A></P>
end;

procedure TtiHTMLDocMgr.InsertHyperLink(const pDisplay, pURL, pNamedLocation: string);
begin
  Assert( FStream <> nil, 'FStream not assigned' ) ;
  FStream.WriteLn( '<P><A HREF="#' + pNamedLocation + '">' + pDisplay + '</A></P>' ) ;
end;

//procedure TtiHTMLDocMgr.InsertImage(const pGraphic: TBitMap; const pCaption: string);
//begin
//  Assert( FStream <> nil, 'FStream not assigned' ) ;
//end;

procedure TtiHTMLDocMgr.InsertImage(const pFileName: TFileName;
  const pCaption: string; const pDiagramWidth: integer);
var
  lFileName : string ;
begin
  Assert( FStream <> nil, 'FStream not assigned' ) ;
  lFileName :=
    tiAddTrailingSlash( ImageDir( true )) +
    ExtractFileName( pFileName ) ;
  tiCopyFile( pFileName, lFileName ) ;
  lFileName :=
    tiAddTrailingSlash( ImageDir( false )) +
    ExtractFileName( pFileName ) ;
  FStream.WriteLn( '<img src="' + lFileName + '">' ) ;
end;

function TtiHTMLDocMgr.InsertParagraph(const pText : string = '' ; const pStyle : TParagraphStyle = ParagraphStyle_Normal ): variant;
var
  lFormat : string ;
begin
  Assert( FStream <> nil, 'FStream not assigned' ) ;
  lFormat := FParagraphStyles.FindFormatStrByStyle( pStyle ) ;
  if lFormat <> '' then
    FStream.WriteLn( '<' + lFormat + '>' +
                     pText + '</' + lFormat + '>' + cHTMLNewLine )
  else
    FStream.WriteLn( pText + cHTMLNewLine ) ;
end;

procedure TtiHTMLDocMgr.InsertTable(pList: TList; pCaption: string;
  aCols: array of string ; aHeadings : array of string );
begin
  FHTMLBuilder.Stream := FStream ;
  FHTMLBuilder.ListToHTML( pList, pCaption, aCols, aHeadings ) ;
end;

function TtiHTMLDocMgr.InsertText(const pText : string  ; const pStyle : TParagraphStyle = ParagraphStyle_Normal): variant;
var
  lFormat : string ;
begin
  Assert( FStream <> nil, 'FStream not assigned' ) ;
  lFormat := FParagraphStyles.FindFormatStrByStyle( pStyle ) ;
  if lFormat <> '' then
    FStream.Write( '<' + lFormat + '>' +
                     pText + '</' + lFormat + '>' )
  else
    FStream.Write( pText ) ;
end;

procedure TtiHTMLDocMgr.InsertTOC(pParagraphIndex, pUpperHeadingLevel, pLowerHeadingLevel: byte);
begin
  Assert( FStream <> nil, 'FStream not assigned' ) ;
end;

function TtiHTMLDocMgr.NewParagraph(const pIndex: integer): Paragraph;
begin
  Assert( FStream <> nil, 'FStream not assigned' ) ;
  FStream.WriteLn(cHTMLNewLine) ;
end;

procedure TtiHTMLDocMgr.SetPageNumbers(const Value: boolean);
begin

end;

//procedure TtiHTMLDocMgr.SetParagraphStyle(const pStyle : TParagraphStyle );
//begin
//  Assert( FStream <> nil, 'FStream not assigned' ) ;
//end;

//procedure TtiHTMLDocMgr.SetTableColWidth(const pCol: Byte; const pWidth: real);
//begin
//  Assert( FStream <> nil, 'FStream not assigned' ) ;
//end;

procedure TtiHTMLDocMgr.SetupStyles;
begin
  Assert( FStream <> nil, 'FStream not assigned' ) ;
end;

{ TtiMSWordMgrAbs }

constructor TtiMSWordMgrAbs.Create;
begin
  inherited ;
  FParagraphStyles := TtiParagraphStyles.Create ;
end;

procedure TtiMSWordMgr.InsertTable(pList: TList; pCaption: string; aCols: array of string ; aHeadings: array of string);

  procedure WriteColHeadings(pslCols: TStringList);
  var
    i : integer ;
  begin
    for i := 0 to pslCols.Count - 1 do
      InsertTableCell(  1,  i+1, pslCols.Strings[i] ) ;
  end;

  procedure WriteRow( pIndex : integer ; pData: TPersistent;pslCols: TStringList);
  var
    i  : integer ;
    ls : string ;
  begin
    for i := 0 to pslCols.Count - 1 do
    begin
      ls := GetPropValue( pData, pslCols.Strings[i] ) ;
      InsertTableCell( pIndex+2, i+1, ls ) ;
    end ;
  end;

var
  lslCols : TStringList ;
  i : integer ;
  lRange : Range ;
begin

  Assert( pList <> nil, 'List not assigned' ) ;
  Assert( pList.Count > 0, 'List contains no elements' ) ;
  Assert( (TObject( pList.Items[0] ) is TPersistent ), 'List elements not TPersistent' ) ;

  lslCols := TStringList.Create ;

  try

    if High( aCols ) = 0 then
      tiGetPropertyNames( TPersistent( pList.Items[0] ), lslCols )
    else
      for i := Low( aCols ) to High( aCols ) do begin
        if not IsPublishedProp( TPersistent( pList.Items[0] ), aCols[i] ) then
          raise exception.create( aCols[i] + ' is not a published property of ' +
                                  TObject( pList.Items[i] ).ClassName +
                                  '. Called in TtiHTMLBuilder.ListToHTML' ) ;
        lslCols.add( aCols[i] ) ;
      end ;

    InsertParagraph( pCaption, ParagraphStyle_Heading4 ) ;

    // Insert the table
    lRange := NewParagraph.Range ;
    SetParagraphStyle( ParagraphStyle_NormalIndent ) ;
    FTable := FWordDoc.Tables.Add( lRange, pList.Count+1, lslCols.Count ) ;

    WriteColHeadings( lslCols ) ;
    for i := 0 to pList.Count - 1 do
      WriteRow( i, pList.Items[i], lslCols ) ;

  finally
    lslCols.Free ;
  end ;
  AutoFormatTable ;
  
end;

destructor TtiMSWordMgrAbs.Destroy;
begin
  FParagraphStyles.Free ;
  inherited;
end;

{ TtiParagraphStyles }

procedure TtiParagraphStyles.AddStyle(pStyle: TParagraphStyle; const pText: string);
var
  lPF : TtiParagraphStyleFormat ;
begin
  lPF := TtiParagraphStyleFormat.Create ;
  lPF.ParagraphStyle := pStyle ;
  lPF.FFormatStr := pText ;
  Add( lPF ) ;
end;

function TtiParagraphStyles.FindFormatStrByStyle( pStyle: TParagraphStyle): string;
var
  i : integer ;
begin
  result := '' ;
  for i := 0 to Count - 1 do
    if TtiParagraphStyleFormat( Items[i] ).ParagraphStyle = pStyle then
    begin
      result := TtiParagraphStyleFormat( Items[i] ).FormatStr ;
      Break ; //==>
    end ;
end;

end.

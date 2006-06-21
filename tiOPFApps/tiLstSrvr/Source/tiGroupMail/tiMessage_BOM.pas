unit tiMessage_BOM;

interface
uses
  tiObjAbs
  ,tiXMLToTIDataSet
  ;

type

  TtiMessage = class( TtiObjAbs )
  private
    FDiscussionID: string;
    FSendTo: string;
    FText: string;
    FFrom: string;
    FSubject: string;
    FMessageDate: TDateTime;
    function  GetAsXML: string;
    procedure SetAsXML(const Value: string);
  public
    property  From         : string    read FFrom         write FFrom ;
    property  SendTo       : string    read FSendTo       write FSendTo ;
    property  MessageDate  : TDateTime read FMessageDate  write FMessageDate ;
    property  Subject      : string    read FSubject      write FSubject ;
    property  DiscussionID : string    read FDiscussionID write FDiscussionID ;
    property  Text         : string    read FText         write FText ;
    property  AsXML        : string    read GetAsXML      write SetAsXML ;
    procedure LoadFromFile(const pFileName : string ) ;
    procedure SaveToFile(const pFileName : string ) ;
  end ;

  TtiMessageReader = class(TtiDataSetToXMLWriter)
  private
    FText    : string ;
    procedure DoOnMessageNodeEvent( const pNodeText : string ) ;
  public
    procedure XMLToMessage( const pXML : string ; const pMessage : TtiMessage ) ;
  end ;

implementation
uses
  tiUtils
  ,tiXML
  ,cTIPersist
  ,tiDialogs
  ;

const
  cgXMLTagTIMessageEmailArchive = 'tiOPFEmailArchive' ;
  cgXMLTagTIMessagesubject = 'subject' ;
  cgXMLTagTIMessagesender = 'sender' ;
  cgXMLTagTIMessagesendto = 'sendto' ;
  cgXMLTagTIMessagedate = 'date' ;
  cgXMLTagTIMessagediscussionid = 'discussionid' ;
  cgXMLTagTIMessagebody = 'body' ;

var
  uXMLTags: TtiXMLTags;

{ TtiMessage }

function TtiMessage.GetAsXML: string;
  function _WrapInXMLTag(const pText : string ; const pTag : string ) : string ;
  var
    lResChars: IXMLReservedCharsTranslator;
  begin
    lResChars := CreateXMLReservedCharsTranslator;
    result :=
    tiXMLTag(pTag) +
    lResChars.RemoveReserved(rcXML, pTag) + 
    tiXMLTagEnd(pTag);
  end ;
begin
  result :=
    _WrapInXMLTag( Subject, cgXMLTagTIMessagesubject ) + CrLf +
    _WrapInXMLTag( From , cgXMLTagTIMessagesender ) + CrLf +
    _WrapInXMLTag( SendTo , cgXMLTagTIMessagesendto ) + CrLf +
    _WrapInXMLTag( tiDateTimeAsXMLString(MessageDate), cgXMLTagTIMessagedate ) + CrLf +
    _WrapInXMLTag( DiscussionID , cgXMLTagTIMessagediscussionid ) + CrLf +
    _WrapInXMLTag( Text , cgXMLTagTIMessagebody ) + CrLf ;
  result :=
    uXMLTags.DocHeader + CrLf +
    tiXMLTag(cgXMLTagTIMessageEmailArchive) + CrLf +
    result +
    tiXMLTagEnd(cgXMLTagTIMessageEmailArchive);
end;

procedure TtiMessage.LoadFromFile(const pFileName: string);
var
  ls : string ;
begin
  ls := tiFileToString(pFileName);
end;

procedure TtiMessage.SaveToFile(const pFileName: string);
var
  lMessage : string ;
begin
  lMessage := AsXML ;
  tiStringToFile(lMessage, pFileName ) ;
end;

procedure TtiMessage.SetAsXML(const Value: string);
var
  ls : string ;
  lReader : TtiMessageReader;
begin
  lReader := TtiMessageReader.Create;
  try
    lReader.XMLToMessage(Value,Self);
  finally
    lReader.Free ;
  end ;
end;

{ TtiMessageReader }

procedure TtiMessageReader.DoOnMessageNodeEvent(const pNodeText: string);
begin
  FText := pNodeText ;
end;

procedure TtiMessageReader.XMLToMessage(const pXML: string;const pMessage: TtiMessage);
begin
  Assert(pMessage.TestValid(TtiMessage), cTIInvalidObjectError ) ;
//  ParseForNode( pXML, tiXMLTag(cgXMLTagTIMessagesubject), tiXMLTagEnd(cgXMLTagTIMessagesubject), DoOnMessageNodeEvent);
Assert(false, 'Under construction');
//  tiShowMessage(FText);
//    _WrapInXMLTag( Subject,  ) + CrLf +
//    _WrapInXMLTag( From , cgXMLTagTIMessagesender ) + CrLf +
//    _WrapInXMLTag( SendTo , cgXMLTagTIMessagesendto ) + CrLf +
//    _WrapInXMLTag( tiDateTimeAsXMLString(MessageDate), cgXMLTagTIMessagedate ) + CrLf +
//    _WrapInXMLTag( DiscussionID , cgXMLTagTIMessagediscussionid ) + CrLf +
//    _WrapInXMLTag( Text , cgXMLTagTIMessagebody ) + CrLf ;


end;

initialization
  uXMLTags := TtiXMLTags.Create;
finalization
  uXMLTags.Free;

end.

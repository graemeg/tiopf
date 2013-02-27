unit tiMessage_TST;

interface
uses
   TestFramework
  ,tiMessage_BOM
  ;

Type
  TTestMessage = class( TTestCase )
  private
    function  MessageAsXML(const pMessage : TtiMessage): string ;
  published
    procedure AsString ;
    procedure TestSaveToFile ;
    procedure TestLoadFromFile;
  end;

procedure RegisterTests ;

implementation
uses
  tiXML
  ,SysUtils
  ,tiDialogs
  ;

procedure RegisterTests ;
begin
  RegisterTest(TTestMessage.Suite);
end;

{ TTestMessage }

procedure TTestMessage.AsString;
var
  lMessage : TtiMessage;
  ls : string ;
begin
  lMessage := TtiMessage.Create;
  try
    lMessage.From        := 'email1@domain.com' ;
    lMessage.SendTo      := 'email2@domain.com' ;
    lMessage.MessageDate := EncodeDate( 2004, 01, 02 ) ;
    lMessage.Subject     := 'my first test' ; ;
    lMessage.DiscussionID  := 'discussion id 1';
    lMessage.Text := 'a test message'  ;
    ls := lMessage.AsXML ;
  finally
    lMessage.Free;
  end;

  lMessage := TtiMessage.Create;
  try
//    lMessage.From        := 'email1@domain.com' ;
//    lMessage.SendTo      := 'email2@domain.com' ;
//    lMessage.MessageDate := EncodeDate( 2004, 01, 02 ) ;
//    lMessage.Subject     := 'my first test' ; ;
//    lMessage.DiscussionID  := 'discussion id 1';
//    lMessage.Text := 'a test message'  ;
    lMessage.AsXML := ls ;
  finally
    lMessage.Free;
  end;


end;

function TTestMessage.MessageAsXML(const pMessage: TtiMessage): string;
begin
{
  lMessage :=
    '<?xml version="1.0"?>' + CrLf +
    '<tiOPFEmailArchive>' + CrLf +
    '<subject>' +
    tiXMLRemoveReservedChars( pSubject, cTIXMLReservedChr ) +
    '</subject>' + CrLf +
    '<sender>' +
    tiXMLRemoveReservedChars( pSender, cTIXMLReservedChr ) +
    '</sender>' + CrLf +
    '<date>' +
    tiDateTimeToStr( Now ) +
    '</date>' + CrLf +
    '<discussionid>' +
    pDiscussionID +
    '</discussionid>' + CrLf +
    '<body>' +
    tiXMLRemoveReservedChars( lMessage, cTIXMLReservedChr ) +
    '</body>' + CrLf +
    '</tiOPFEmailArchive>' ;
}
end;

procedure TTestMessage.TestLoadFromFile;
var
  lMessage : TtiMessage;
begin
  lMessage := TtiMessage.Create;
  try
    CheckEquals(lMessage.From, '', 'From');
    CheckEquals(lMessage.SendTo, '', 'SendTo');
    CheckEquals(lMessage.MessageDate, 0, 'Date');
    CheckEquals(lMessage.Subject, '', 'Subject');
    CheckEquals(lMessage.DiscussionID, '', 'DiscussionID');
    CheckEquals(lMessage.Text, '', 'Text');
  finally
    lMessage.Free;
  end;
end;

procedure TTestMessage.TestSaveToFile;
var
  lMessage : TtiMessage;
begin
  lMessage := TtiMessage.Create;
  try
    lMessage.From        := 'email1@domain.com' ;
    lMessage.SendTo      := 'email2@domain.com' ;
    lMessage.MessageDate := EncodeDate( 2004, 01, 02 ) ;
    lMessage.Subject     := 'my first test' ; ;
    lMessage.DiscussionID  := 'discussion id 1';
    lMessage.Text := 'a test message'  ;

    tiShowMessage(lMessage.AsXML);

  finally
    lMessage.Free;
  end;
end;

end.

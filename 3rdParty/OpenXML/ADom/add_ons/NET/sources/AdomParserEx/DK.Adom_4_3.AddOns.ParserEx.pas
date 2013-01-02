// Delphi 8/2005/2006/2007 for .NET Implementation
// January 2010
//
//
// LICENSE
//
// The contents of this file are subject to the Mozilla Public License Version
// 1.1 (the "License"); you may not use this file except in compliance with
// the License. You may obtain a copy of the License at
// "http://www.mozilla.org/MPL/"
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
// the specific language governing rights and limitations under the License.
//
// The Original Code is "DK.Adom_4_3.AddOns.ParserEx.pas".
//
// The Initial Developer of the Original Code is Dieter Köhler (Heidelberg,
// Germany, "http://www.philo.de/"). Portions created by the Initial Developer
// are Copyright (C) 2006-2010 Dieter Köhler. All Rights Reserved.
//
// Alternatively, the contents of this file may be used under the terms of the
// GNU General Public License Version 2 or later (the "GPL"), in which case the
// provisions of the GPL are applicable instead of those above. If you wish to
// allow use of your version of this file only under the terms of the GPL, and
// not to allow others to use your version of this file under the terms of the
// MPL, indicate your decision by deleting the provisions above and replace them
// with the notice and other provisions required by the GPL. If you do not delete
// the provisions above, a recipient may use your version of this file under the
// terms of any one of the MPL or the GPL.

unit DK.Adom_4_3.AddOns.ParserEx;

interface

uses
  DK.Adom_4_3.AdomCore_4_3, DK.Utilities.dkCodecUtilsRTL, DK.Utilities.dkWideStringUtils,
  Classes, SysUtils;

type
  TXmlToDomParserEx = class(TXmlToDomParser)
    function StreamToDom(const Stream: TStream;
                         const SysId: WideString;
                         const CodecClass: TUnicodeCodecClass;
                         const InclDecl: Boolean): TDomDocument; virtual;
    function StringToDom(const S: string;
                         const SysId: WideString;
                         const CodecClass: TUnicodeCodecClass;
                         const InclDecl: Boolean): TDomDocument; virtual;
   function WideStringToDom(const S: WideString;
                             const SysId: WideString;
                             const CodecClass: TUnicodeCodecClass;
                             const InclDecl: Boolean): TDomDocument; virtual;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ADOM 4.3 Add-Ons',[ TXmlToDomParserEx ]);
end;

{ TXmlToDomParserEx }

function TXmlToDomParserEx.StreamToDom(const Stream: TStream;
                                       const SysId: WideString;
                                       const CodecClass: TUnicodeCodecClass;
                                       const InclDecl: Boolean): TDomDocument;
var
  InputSrc: TXmlInputSource;
begin
  if not Assigned(Stream) then
    raise ArgumentNullException.Create('Stream not specified.');
  try
    InputSrc := TXmlInputSource.Create(Stream, '', SysId, FBufferSize, CodecClass,
                  InclDecl, 0, 0, 0, 0, 1);  {TODO 5 -cLocation : Allow other location parameters?}
  except
    on ENot_Supported_Err do begin
      SendErrorNotification(ET_ENCODING_NOT_SUPPORTED);
      raise EParserException.Create('Parser error.');
    end;
    on EConvertError do begin
      SendErrorNotification(ET_BYTE_ORDER_MARK_ENCODING_MISMATCH);
      raise EParserException.Create('Parser error.');
    end;
  end;
  try
    Result := Parse(InputSrc);
  finally
    InputSrc.Free;
  end;
end;

function TXmlToDomParserEx.StringToDom(const S: string;
                                       const SysId: WideString;
                                       const CodecClass: TUnicodeCodecClass;
                                       const InclDecl: Boolean): TDomDocument;
var
  StrStream: TStringStream;
begin
  if S = '' then
    raise ArgumentNullException.Create('Empty string.');
  StrStream := TStringStream.Create(S);
  try
    Result := StreamToDom(StrStream, SysId, CodecClass, InclDecl);
  finally
    StrStream.Free;
  end;
end;

function TXmlToDomParserEx.WideStringToDom(const S: WideString;
                                           const SysId: WideString;
                                           const CodecClass: TUnicodeCodecClass;
                                           const InclDecl: Boolean): TDomDocument;
var
  WStrStream: TUtilsWideStringStream;
begin
  if S = '' then
    raise ArgumentNullException.Create('Empty string.');
  WStrStream := TUtilsWideStringStream.Create(S);
  try
    Result := StreamToDom(WStrStream, SysId, CodecClass, InclDecl);
  finally
    WStrStream.Free;
  end;
end;

end.

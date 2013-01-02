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
// The Original Code is "DK.Adom_4_3.AddOns.Tanscoder.pas".
//
// The Initial Developer of the Original Code is Dieter Köhler (Heidelberg,
// Germany, "http://www.philo.de/"). Portions created by the Initial Developer
// are Copyright (C) 2004-2010 Dieter Köhler. All Rights Reserved.
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

unit DK.Adom_4_3.AddOns.Tanscoder;

interface

uses
  DK.Adom_4_3.AdomCore_4_3, DK.Utilities.dkCodecUtilsRTL, DK.Utilities.dkParserUtilsRTL,
  {$IFDEF MSWINDOWS}
    {$IFDEF VER140+} Types,
    {$ELSE} Windows, {$ENDIF}
  {$ENDIF}
  {$IFDEF LINUX}
    Types,
  {$ENDIF}
  SysUtils, Classes;

type
  TXmlTranscodeOpt = (
      toEnforceXmlDecl,
      toCopyXmlDecl,
      toNoXmlDecl );

  TXmlTranscoder = class
  private
    FBusy: Boolean;
    FEscapeAmpersand: Boolean;
    FInputSource: TXmlInputSource;
    FOutputSource: TXmlOutputSource;
    FUseCharRefs: Boolean;
    FXmlDeclOpt: TXmlTranscodeOpt;
    FOnProgress: TNotifyEvent;
    function GetOutputEncoding: WideString;
    function GetWriteLFOption: TCodecWriteLFOption;
    procedure SetOutputEncoding(const Value: WideString);
    procedure SetWriteLFOption(const Value: TCodecWriteLFOption);
  protected
    procedure SetXmlDeclOpt(const Value: TXmlTranscodeOpt); virtual;
    procedure DoProgress; virtual;
  public
    constructor Create(const InputStream,
                             OutputStream: TStream;
                       const BufSize: Integer;
                       const InputCodecClass: TUnicodeCodecClass;
                       const SkipXmlOrTextDecl: Boolean);
    destructor Destroy; override;
    function Transcode: Integer; virtual;   // Returns the number of bytes written to the output stream.

    property Busy: Boolean read FBusy;
    property EscapeAmpersand: Boolean read FEscapeAmpersand write FEscapeAmpersand default False;
    property OutputEncoding: WideString read GetOutputEncoding write SetOutputEncoding;
    property UseCharRefs: Boolean read FUseCharRefs write FUseCharRefs default False;
    property XmlDeclOpt: TXmlTranscodeOpt read FXmlDeclOpt write SetXmlDeclOpt default toEnforceXmlDecl;
    property WriteLFOption: TCodecWriteLFOption read GetWriteLFOption write SetWriteLFOption default lwCRLF;

    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  end;

{ Helper methods }

function PcdataToStr(const S: WideString): string;
function StrToPcdata(const S: string): WideString;

implementation

uses
  DK.Utilities.dkWideStringUtils;

{ Helper methods }

function PcdataToStr(const S: WideString): string;
var
  InputStream: TUtilsWideStringStream;
  OutputStream: TStringStream;
  XmlTranscoder: TXmlTranscoder;
begin
  XmlTranscoder := nil;
  OutputStream := nil;
  InputStream := TUtilsWideStringStream.Create(S);
  try
    OutputStream := TStringStream.Create('');
    XmlTranscoder := TXmlTranscoder.Create(InputStream, OutputStream, 4096, TUTF16LECodec, True);
    with XmlTranscoder do begin
      EscapeAmpersand := True;
      OutputEncoding := GetSystemEncodingName;
      UseCharRefs := True;
      XmlDeclOpt := toNoXmlDecl;
      WriteLFOption := lwCRLF;

      Transcode;

      Result := OutputStream.DataString;
    end;
  finally
    XmlTranscoder.Free;
    OutputStream.Free;
    InputStream.Free;
  end;
end;

function StrToPcdata(const S: string): WideString;
begin
  // xxx Problem: The following conversion raises an exception if a character
  //              reference is invalid:
  try
    Result := ResolveCharRefs(EncodingToUTF16(GetSystemEncodingCodecClass, S));
  except
    Result := EncodingToUTF16(GetSystemEncodingCodecClass, S);
  end;
end;

{ TXmlTranscoder }

constructor TXmlTranscoder.Create(const InputStream, OutputStream: TStream;
  const BufSize: Integer; const InputCodecClass: TUnicodeCodecClass;
  const SkipXmlOrTextDecl: Boolean);
begin
  inherited Create;
  FInputSource := TXmlInputSource.Create(InputStream, '', '', BufSize,
    InputCodecClass, SkipXmlOrTextDecl, 0, 0, 0, 0, 1);
  FOutputSource := TXmlOutputSource.Create(OutputStream, BufSize);
  FXmlDeclOpt := toEnforceXmlDecl;
  FBusy := False;
  FEscapeAmpersand := False;
  FUseCharRefs := False;
  SetWriteLFOption(lwCRLF);
end;

destructor TXmlTranscoder.Destroy;
begin
  FInputSource.Free;
  FOutputSource.Free;
  inherited;
end;

procedure TXmlTranscoder.DoProgress;
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self);
end;

function TXmlTranscoder.GetOutputEncoding: WideString;
begin
  Result := GetEncodingName(FOutputSource.codecClass);
end;

function TXmlTranscoder.GetWriteLFOption: TCodecWriteLFOption;
begin
  Result := FOutputSource.WriteLFOption;
end;

procedure TXmlTranscoder.SetOutputEncoding(const Value: WideString);
begin
  FOutputSource.codecClass := GetCodecClassByAlias(Value);
end;

procedure TXmlTranscoder.SetWriteLFOption(const Value: TCodecWriteLFOption);
begin
  if not FBusy then
    FOutputSource.WriteLFOption := Value;
end;

procedure TXmlTranscoder.SetXmlDeclOpt(const value: TXmlTranscodeOpt);
begin
  FXmlDeclOpt := value;
end;

function TXmlTranscoder.Transcode: Integer;
// Returns the number of bytes written to the output stream.

  procedure WriteXmlDeclaration(const AXmlVersion, AXmlEncoding: WideString;
     AXmlStandalone: TdomStandalone; out ByteCount: Integer);
  var
    I: Integer;
    S: WideString;

  begin
    if AXmlVersion = ''
      then S := '<?xml version="1.0"'
      else S := '<?xml version="' + AXmlVersion + '"';
    if AXmlEncoding <> '' then
      S := S + ' encoding="' + AXmlEncoding + '"';
    case AXmlStandalone of
      STANDALONE_YES: S := S + ' standalone="yes"';
      STANDALONE_NO:  S := S + ' standalone="no"';
    end;
    S := S + '?>';
    for I := 1 to Length(S) do begin
      FOutputSource.WriteUCS4Char(Ord(S[I]), ByteCount);
      DoProgress;
    end;
  end;

const
  AMP: UCS4Char   = $26;      // '&'
  AMP_REF: string = '&#x26;';
var
  BytesUsed, BytesUsed_2: Integer;
  C: UCS4Char;
  CharRef: WideString;
  I: Integer;
begin
  FBusy := True;
  try
    if (xmlDeclOpt = toEnforceXmlDecl) or
       ( (xmlDeclOpt = toCopyXmlDecl) and
         (FInputSource.declType in [DT_XML_DECLARATION, DT_XML_OR_TEXT_DECLARATION]) ) then begin  // xxx What about text declarations?
      WriteXmlDeclaration(FInputSource.xmlVersion, OutputEncoding, FInputSource.XmlStandalone, Result);
    end;

    while True do begin
      FInputSource.Next;
      C := FInputSource.CurrentCharInfo.CodePoint;
      if C = $9C then Break; // $9C = STRING TERMINATOR
      if EscapeAmpersand and (C = AMP) then begin
        BytesUsed := 0;
        for I := 1 to Length(AMP_REF) do begin
          FOutputSource.WriteUCS4Char(Ord(AMP_REF[I]), BytesUsed_2);
          BytesUsed := BytesUsed + BytesUsed_2;
        end;
      end else
        try
          FOutputSource.WriteUCS4Char(C, BytesUsed);
        except
          on EConvertError do
            if UseCharRefs then begin
              CharRef := XmlIntToCharRefHex(C);
              BytesUsed := 0;
              for I := 1 to Length(CharRef) do begin
                FOutputSource.WriteUCS4Char(Ord(CharRef[I]), BytesUsed_2);
                BytesUsed := BytesUsed + BytesUsed_2;
              end;
            end else
              raise;
        end;
      Result := Result + BytesUsed;
      DoProgress;
    end;

  finally
    FBusy := False;
    FInputSource.Reset;
    FOutputSource.FlushBuffer;
  end;
end;

end.

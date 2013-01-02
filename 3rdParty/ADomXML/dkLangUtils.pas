unit dkLangUtils;

{$I ADOMXMLWarn.inc}

// dkLangUtils 1.0.4
// Delphi 4 to 2009 and Kylix 3 Implementation
// February 2009
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
// The Original Code is "dkLangUtils.pas".
//
// The Initial Developer of the Original Code is Dieter Köhler (Heidelberg,
// Germany, "http://www.philo.de/"). Portions created by the Initial Developer
// are Copyright (C) 2003-2009 Dieter Köhler. All Rights Reserved.
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

// HISTORY
//
// 2009-02-23 1.0.4 Internal modifications.
// 2008-09-28 1.0.3 Internal modifications.
// 2007-12-03 1.0.2 Made .NET compliant.
// 2003-09-20 1.0.1 ISO 639 language constants added.
//                  TIso639Info class added.
// 2003-08-03 1.0.0

interface

uses
  Classes;

type
  TIso639LanguageCode = (iso639_aa, // Afar
    iso639_ab, // Abkhazian
    iso639_af, // Afrikaans
    iso639_am, // Amharic
    iso639_ar, // Arabic
    iso639_as, // Assamese
    iso639_ay, // Aymara
    iso639_az, // Azerbaijani

    iso639_ba, // Bashkir
    iso639_be, // Byelorussian
    iso639_bg, // Bulgarian
    iso639_bh, // Bihari
    iso639_bi, // Bislama
    iso639_bn, // Bengali; Bangla
    iso639_bo, // Tibetan
    iso639_br, // Breton

    iso639_ca, // Catalan
    iso639_co, // Corsican
    iso639_cs, // Czech
    iso639_cy, // Welsh

    iso639_da, // Danish
    iso639_de, // German
    iso639_dz, // Bhutani

    iso639_el, // Greek
    iso639_en, // English
    iso639_eo, // Esperanto
    iso639_es, // Spanish
    iso639_et, // Estonian
    iso639_eu, // Basque

    iso639_fa, // Persian
    iso639_fi, // Finnish
    iso639_fj, // Fiji
    iso639_fo, // Faeroese
    iso639_fr, // French
    iso639_fy, // Frisian

    iso639_ga, // Irish
    iso639_gd, // Scots Gaelic
    iso639_gl, // Galician
    iso639_gn, // Guarani
    iso639_gu, // Gujarati

    iso639_ha, // Hausa
    iso639_hi, // Hindi
    iso639_hr, // Croatian
    iso639_hu, // Hungarian
    iso639_hy, // Armenian

    iso639_ia, // Interlingua
    iso639_ie, // Interlingue
    iso639_ik, // Inupiak
    iso639_in, // Indonesian
    iso639_is, // Icelandic
    iso639_it, // Italian
    iso639_iw, // Hebrew

    iso639_ja, // Japanese
    iso639_ji, // Yiddish
    iso639_jw, // Javanese

    iso639_ka, // Georgian
    iso639_kk, // Kazakh
    iso639_kl, // Greenlandic
    iso639_km, // Cambodian
    iso639_kn, // Kannada
    iso639_ko, // Korean
    iso639_ks, // Kashmiri
    iso639_ku, // Kurdish
    iso639_ky, // Kirghiz

    iso639_la, // Latin
    iso639_ln, // Lingala
    iso639_lo, // Laothian
    iso639_lt, // Lithuanian
    iso639_lv, // Latvian; Lettish

    iso639_mg, // Malagasy
    iso639_mi, // Maori
    iso639_mk, // Macedonian
    iso639_ml, // Malayalam
    iso639_mn, // Mongolian
    iso639_mo, // Moldavian
    iso639_mr, // Marathi
    iso639_ms, // Malay
    iso639_mt, // Maltese
    iso639_my, // Burmese

    iso639_na, // Nauru
    iso639_ne, // Nepali
    iso639_nl, // Dutch
    iso639_no, // Norwegian

    iso639_oc, // Occitan
    iso639_om, // Afan; Oromo
    iso639_or, // Oriya

    iso639_pa, // Punjabi
    iso639_pl, // Polish
    iso639_ps, // Pashto; Pushto
    iso639_pt, // Portuguese

    iso639_qu, // Quechua

    iso639_rm, // Rhaeto-Romance
    iso639_rn, // Kirundi
    iso639_ro, // Romanian
    iso639_ru, // Russian
    iso639_rw, // Kinyarwanda

    iso639_sa, // Sanskrit
    iso639_sd, // Sindhi
    iso639_sg, // Sangro
    iso639_sh, // Serbo-Croatian
    iso639_si, // Singhalese
    iso639_sk, // Slovak
    iso639_sl, // Slovenian
    iso639_sm, // Samoan
    iso639_sn, // Shona
    iso639_so, // Somali
    iso639_sq, // Albanian
    iso639_sr, // Serbian
    iso639_ss, // Siswati
    iso639_st, // Sesotho
    iso639_su, // Sundanese
    iso639_sv, // Swedish
    iso639_sw, // Swahili

    iso639_ta, // Tamil
    iso639_te, // Tegulu
    iso639_tg, // Tajik
    iso639_th, // Thai
    iso639_ti, // Tigrinya
    iso639_tk, // Turkmen
    iso639_tl, // Tagalog
    iso639_tn, // Setswana
    iso639_to, // Tonga
    iso639_tr, // Turkish
    iso639_ts, // Tsonga
    iso639_tt, // Tatar
    iso639_tw, // Twi

    iso639_uk, // Ukrainian
    iso639_ur, // Urdu
    iso639_uz, // Uzbek

    iso639_vi, // Vietnamese
    iso639_vo, // Volapuk

    iso639_wo, // Wolof

    iso639_xh, // Xhosa

    iso639_yo, // Yoruba

    iso639_zh, // Chinese
    iso639_zu // Zulu
    );

  TIso639LanguageCodeSet = set of TIso639LanguageCode;

  TIso639Info = class(TPersistent)
  private
    function CodeToName_en(Value: TIso639LanguageCode): WideString;
    function NameToCode_en(Value: WideString): TIso639LanguageCode;
  protected
    FAppendSymbolToName: Boolean;
    FNameLanguage: TIso639LanguageCode;
    FSupportedLanguages: TIso639LanguageCodeSet;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetNameLanguage(const Value: TIso639LanguageCode); virtual;
  public
    constructor Create;
    function CodeToName(const Value: TIso639LanguageCode): WideString; virtual;
    function CodeToSymbol(const Value: TIso639LanguageCode): WideString;
      virtual;
    function NameToCode(const Value: WideString): TIso639LanguageCode; virtual;
    function SymbolToCode(const Value: WideString): TIso639LanguageCode;
      virtual;
    property AppendSymbolToName: Boolean read FAppendSymbolToName write
      FAppendSymbolToName default false;
    property NameLanguage: TIso639LanguageCode read FNameLanguage write
      SetNameLanguage default iso639_en;
    property SupportedLanguages: TIso639LanguageCodeSet read
      FSupportedLanguages;
  end;

function IsRFC3066LanguageTag(const S: string): Boolean;
function IsSubLanguage(const Sublanguage, Superlanguage: WideString): Boolean;

implementation

uses
{$IFDEF CLR}
  dkParserUtilsRTL,
{$ELSE}
  dkParserUtilsWin32,
{$ENDIF}
  dkWideStringUtils, dkAbnfUtils, SysUtils;

{ Helper functions}

function IsRFC3066LanguageTag(const S: string): Boolean;
var
  I: Integer;
  LetterCount: Integer;
  PrimaryTag: Boolean;
  TagStart: Boolean;
begin
  LetterCount := 0;
  PrimaryTag := True;
  TagStart := True;
  for I := 1 to Length(S) do
  begin
    if IsAbnfALPHAChar(S[I]) then
    begin
      TagStart := False;
      Inc(LetterCount);
      if LetterCount > 8 then
      begin
        Result := False;
        Exit;
      end;
    end
    else if IsAbnfDIGITChar(S[I]) then
    begin
      if PrimaryTag then
      begin
        Result := False;
        Exit;
      end;
      TagStart := False;
      Inc(LetterCount);
      if LetterCount > 8 then
      begin
        Result := False;
        Exit;
      end;
    end
    else if S[I] = '-' then
    begin
      if TagStart then
      begin
        Result := False;
        Exit;
      end;
      PrimaryTag := False;
      TagStart := True;
      LetterCount := 0;
    end;
  end;
  if TagStart then
    Result := False
  else
    Result := True;
end;

function IsSubLanguage(const Sublanguage, Superlanguage: WideString): Boolean;
begin
  if Length(Sublanguage) = Length(Superlanguage) then
  begin
    Result := CompareText(Sublanguage, Superlanguage) = 0
  end
  else if Length(Sublanguage) > Length(Superlanguage) then
  begin
    Result := CompareText(copy(Sublanguage, 1, Length(Superlanguage) + 1),
      Superlanguage + '-') = 0
  end
  else
    Result := False;
end;

{ TIso639Info }

constructor TIso639Info.Create;
begin
  inherited;
  FSupportedLanguages := [iso639_en];
  FNameLanguage := iso639_en;
  FAppendSymbolToName := False;
end;

procedure TIso639Info.AssignTo(Dest: TPersistent);
var
  I: Integer;
begin
  if Dest is TStrings then
  begin

    with TStrings(Dest) do
    begin
      BeginUpdate;
      try
        Clear;
        try
          for I := Ord(Low(TIso639LanguageCode)) to
            Ord(High(TIso639LanguageCode)) do
            {$IFDEF CLR}
            AddObject(ConvertFromUTF16('US-ASCII',
              CodeToName(TIso639LanguageCode(I))), TObject(I));
            {$ELSE}
              {$IFDEF UNICODE}
            AddObject(
              CodeToName(TIso639LanguageCode(I)), Pointer(I));
              {$ELSE}
            AddObject(ConvertFromUTF16('US-ASCII',
              CodeToName(TIso639LanguageCode(I))), Pointer(I));
              {$ENDIF}
            {$ENDIF}
        except
          Clear;
          raise;
        end;
      finally
        EndUpdate;
      end;
    end;

  end
  else if Dest is TUtilsWideStringList then
  begin

    with TUtilsWideStringList(Dest) do
    begin
      BeginUpdate;
      try
        Clear;
        try
          for I := Ord(Low(TIso639LanguageCode)) to
            Ord(High(TIso639LanguageCode)) do
            AddObject(CodeToName(TIso639LanguageCode(I)), {$IFDEF CLR}TObject(I){$ELSE}Pointer(I){$ENDIF});
        except
          Clear;
          raise;
        end;
      finally
        EndUpdate;
      end;
    end;

  end
  else
    inherited AssignTo(Dest);
end;

function TIso639Info.CodeToName(const Value: TIso639LanguageCode): WideString;
begin
  case NameLanguage of
    iso639_en: Result := CodeToName_en(Value);
  else
    raise EConvertError.Create('Conversion not supported');
  end;
end;

function TIso639Info.CodeToName_en(Value: TIso639LanguageCode): WideString;
begin
  if Value = iso639_aa then
    Result := 'Afar'
  else if Value = iso639_ab then
    Result := 'Abkhazian'
  else if Value = iso639_af then
    Result := 'Afrikaans'
  else if Value = iso639_am then
    Result := 'Amharic'
  else if Value = iso639_ar then
    Result := 'Arabic'
  else if Value = iso639_as then
    Result := 'Assamese'
  else if Value = iso639_ay then
    Result := 'Aymara'
  else if Value = iso639_az then
    Result := 'Azerbaijani'
  else if Value = iso639_ba then
    Result := 'Bashkir'
  else if Value = iso639_be then
    Result := 'Byelorussian'
  else if Value = iso639_bg then
    Result := 'Bulgarian'
  else if Value = iso639_bh then
    Result := 'Bihari'
  else if Value = iso639_bi then
    Result := 'Bislama'
  else if Value = iso639_bn then
    Result := 'Bengali; Bangla'
  else if Value = iso639_bo then
    Result := 'Tibetan'
  else if Value = iso639_br then
    Result := 'Breton'
  else if Value = iso639_ca then
    Result := 'Catalan'
  else if Value = iso639_co then
    Result := 'Corsican'
  else if Value = iso639_cs then
    Result := 'Czech'
  else if Value = iso639_cy then
    Result := 'Welsh'
  else if Value = iso639_da then
    Result := 'Danish'
  else if Value = iso639_de then
    Result := 'German'
  else if Value = iso639_dz then
    Result := 'Bhutani'
  else if Value = iso639_el then
    Result := 'Greek'
  else if Value = iso639_en then
    Result := 'English'
  else if Value = iso639_eo then
    Result := 'Esperanto'
  else if Value = iso639_es then
    Result := 'Spanish'
  else if Value = iso639_et then
    Result := 'Estonian'
  else if Value = iso639_eu then
    Result := 'Basque'
  else if Value = iso639_fa then
    Result := 'Persian'
  else if Value = iso639_fi then
    Result := 'Finnish'
  else if Value = iso639_fj then
    Result := 'Fiji'
  else if Value = iso639_fo then
    Result := 'Faeroese'
  else if Value = iso639_fr then
    Result := 'French'
  else if Value = iso639_fy then
    Result := 'Frisian'
  else if Value = iso639_ga then
    Result := 'Irish'
  else if Value = iso639_gd then
    Result := 'Scots Gaelic'
  else if Value = iso639_gl then
    Result := 'Galician'
  else if Value = iso639_gn then
    Result := 'Guarani'
  else if Value = iso639_gu then
    Result := 'Gujarati'
  else if Value = iso639_ha then
    Result := 'Hausa'
  else if Value = iso639_hi then
    Result := 'Hindi'
  else if Value = iso639_hr then
    Result := 'Croatian'
  else if Value = iso639_hu then
    Result := 'Hungarian'
  else if Value = iso639_hy then
    Result := 'Armenian'
  else if Value = iso639_ia then
    Result := 'Interlingua'
  else if Value = iso639_ie then
    Result := 'Interlingue'
  else if Value = iso639_ik then
    Result := 'Inupiak'
  else if Value = iso639_in then
    Result := 'Indonesian'
  else if Value = iso639_is then
    Result := 'Icelandic'
  else if Value = iso639_it then
    Result := 'Italian'
  else if Value = iso639_iw then
    Result := 'Hebrew'
  else if Value = iso639_ja then
    Result := 'Japanese'
  else if Value = iso639_ji then
    Result := 'Yiddish'
  else if Value = iso639_jw then
    Result := 'Javanese'
  else if Value = iso639_ka then
    Result := 'Georgian'
  else if Value = iso639_kk then
    Result := 'Kazakh'
  else if Value = iso639_kl then
    Result := 'Greenlandic'
  else if Value = iso639_km then
    Result := 'Cambodian'
  else if Value = iso639_kn then
    Result := 'Kannada'
  else if Value = iso639_ko then
    Result := 'Korean'
  else if Value = iso639_ks then
    Result := 'Kashmiri'
  else if Value = iso639_ku then
    Result := 'Kurdish'
  else if Value = iso639_ky then
    Result := 'Kirghiz'
  else if Value = iso639_la then
    Result := 'Latin'
  else if Value = iso639_ln then
    Result := 'Lingala'
  else if Value = iso639_lo then
    Result := 'Laothian'
  else if Value = iso639_lt then
    Result := 'Lithuanian'
  else if Value = iso639_lv then
    Result := 'Latvian; Lettish'
  else if Value = iso639_mg then
    Result := 'Malagasy'
  else if Value = iso639_mi then
    Result := 'Maori'
  else if Value = iso639_mk then
    Result := 'Macedonian'
  else if Value = iso639_ml then
    Result := 'Malayalam'
  else if Value = iso639_mn then
    Result := 'Mongolian'
  else if Value = iso639_mo then
    Result := 'Moldavian'
  else if Value = iso639_mr then
    Result := 'Marathi'
  else if Value = iso639_ms then
    Result := 'Malay'
  else if Value = iso639_mt then
    Result := 'Maltese'
  else if Value = iso639_my then
    Result := 'Burmese'
  else if Value = iso639_na then
    Result := 'Nauru'
  else if Value = iso639_ne then
    Result := 'Nepali'
  else if Value = iso639_nl then
    Result := 'Dutch'
  else if Value = iso639_no then
    Result := 'Norwegian'
  else if Value = iso639_oc then
    Result := 'Occitan'
  else if Value = iso639_om then
    Result := 'Afan; Oromo'
  else if Value = iso639_or then
    Result := 'Oriya'
  else if Value = iso639_pa then
    Result := 'Punjabi'
  else if Value = iso639_pl then
    Result := 'Polish'
  else if Value = iso639_ps then
    Result := 'Pashto; Pushto'
  else if Value = iso639_pt then
    Result := 'Portuguese'
  else if Value = iso639_qu then
    Result := 'Quechua'
  else if Value = iso639_rm then
    Result := 'Rhaeto-Romance'
  else if Value = iso639_rn then
    Result := 'Kirundi'
  else if Value = iso639_ro then
    Result := 'Romanian'
  else if Value = iso639_ru then
    Result := 'Russian'
  else if Value = iso639_rw then
    Result := 'Kinyarwanda'
  else if Value = iso639_sa then
    Result := 'Sanskrit'
  else if Value = iso639_sd then
    Result := 'Sindhi'
  else if Value = iso639_sg then
    Result := 'Sangro'
  else if Value = iso639_sh then
    Result := 'Serbo-Croatian'
  else if Value = iso639_si then
    Result := 'Singhalese'
  else if Value = iso639_sk then
    Result := 'Slovak'
  else if Value = iso639_sl then
    Result := 'Slovenian'
  else if Value = iso639_sm then
    Result := 'Samoan'
  else if Value = iso639_sn then
    Result := 'Shona'
  else if Value = iso639_so then
    Result := 'Somali'
  else if Value = iso639_sq then
    Result := 'Albanian'
  else if Value = iso639_sr then
    Result := 'Serbian'
  else if Value = iso639_ss then
    Result := 'Siswati'
  else if Value = iso639_st then
    Result := 'Sesotho'
  else if Value = iso639_su then
    Result := 'Sundanese'
  else if Value = iso639_sv then
    Result := 'Swedish'
  else if Value = iso639_sw then
    Result := 'Swahili'
  else if Value = iso639_ta then
    Result := 'Tamil'
  else if Value = iso639_te then
    Result := 'Tegulu'
  else if Value = iso639_tg then
    Result := 'Tajik'
  else if Value = iso639_th then
    Result := 'Thai'
  else if Value = iso639_ti then
    Result := 'Tigrinya'
  else if Value = iso639_tk then
    Result := 'Turkmen'
  else if Value = iso639_tl then
    Result := 'Tagalog'
  else if Value = iso639_tn then
    Result := 'Setswana'
  else if Value = iso639_to then
    Result := 'Tonga'
  else if Value = iso639_tr then
    Result := 'Turkish'
  else if Value = iso639_ts then
    Result := 'Tsonga'
  else if Value = iso639_tt then
    Result := 'Tatar'
  else if Value = iso639_tw then
    Result := 'Twi'
  else if Value = iso639_uk then
    Result := 'Ukrainian'
  else if Value = iso639_ur then
    Result := 'Urdu'
  else if Value = iso639_uz then
    Result := 'Uzbek'
  else if Value = iso639_vi then
    Result := 'Vietnamese'
  else if Value = iso639_vo then
    Result := 'Volapuk'
  else if Value = iso639_wo then
    Result := 'Wolof'
  else if Value = iso639_xh then
    Result := 'Xhosa'
  else if Value = iso639_yo then
    Result := 'Yoruba'
  else if Value = iso639_zh then
    Result := 'Chinese'
  else if Value = iso639_zu then
    Result := 'Zulu'
      ;

  if FAppendSymbolToName then
    Result := Concat(Result, ' [', CodeToSymbol(Value), ']');
end;

function TIso639Info.codeToSymbol(const Value: TIso639LanguageCode): WideString;
begin
  if Value = iso639_aa then
    Result := 'aa' // Afar
  else if Value = iso639_ab then
    Result := 'ab' // Abkhazian
  else if Value = iso639_af then
    Result := 'af' // Afrikaans
  else if Value = iso639_am then
    Result := 'am' // Amharic
  else if Value = iso639_ar then
    Result := 'ar' // Arabic
  else if Value = iso639_as then
    Result := 'as' // Assamese
  else if Value = iso639_ay then
    Result := 'ay' // Aymara
  else if Value = iso639_az then
    Result := 'az' // Azerbaijani
  else if Value = iso639_ba then
    Result := 'ba' // Bashkir
  else if Value = iso639_be then
    Result := 'be' // Byelorussian
  else if Value = iso639_bg then
    Result := 'bg' // Bulgarian
  else if Value = iso639_bh then
    Result := 'bh' // Bihari
  else if Value = iso639_bi then
    Result := 'bi' // Bislama
  else if Value = iso639_bn then
    Result := 'bn' // Bengali; Bangla
  else if Value = iso639_bo then
    Result := 'bo' // Tibetan
  else if Value = iso639_br then
    Result := 'br' // Breton
  else if Value = iso639_ca then
    Result := 'ca' // Catalan
  else if Value = iso639_co then
    Result := 'co' // Corsican
  else if Value = iso639_cs then
    Result := 'cs' // Czech
  else if Value = iso639_cy then
    Result := 'cy' // Welsh
  else if Value = iso639_da then
    Result := 'da' // Danish
  else if Value = iso639_de then
    Result := 'de' // German
  else if Value = iso639_dz then
    Result := 'dz' // Bhutani
  else if Value = iso639_el then
    Result := 'el' // Greek
  else if Value = iso639_en then
    Result := 'en' // English
  else if Value = iso639_eo then
    Result := 'eo' // Esperanto
  else if Value = iso639_es then
    Result := 'es' // Spanish
  else if Value = iso639_et then
    Result := 'et' // Estonian
  else if Value = iso639_eu then
    Result := 'eu' // Basque
  else if Value = iso639_fa then
    Result := 'fa' // Persian
  else if Value = iso639_fi then
    Result := 'fi' // Finnish
  else if Value = iso639_fj then
    Result := 'fj' // Fiji
  else if Value = iso639_fo then
    Result := 'fo' // Faeroese
  else if Value = iso639_fr then
    Result := 'fr' // French
  else if Value = iso639_fy then
    Result := 'fy' // Frisian
  else if Value = iso639_ga then
    Result := 'ga' // Irish
  else if Value = iso639_gd then
    Result := 'gd' // Scots Gaelic
  else if Value = iso639_gl then
    Result := 'gl' // Galician
  else if Value = iso639_gn then
    Result := 'gn' // Guarani
  else if Value = iso639_gu then
    Result := 'gu' // Gujarati
  else if Value = iso639_ha then
    Result := 'ha' // Hausa
  else if Value = iso639_hi then
    Result := 'hi' // Hindi
  else if Value = iso639_hr then
    Result := 'hr' // Croatian
  else if Value = iso639_hu then
    Result := 'hu' // Hungarian
  else if Value = iso639_hy then
    Result := 'hy' // Armenian
  else if Value = iso639_ia then
    Result := 'ia' // Interlingua
  else if Value = iso639_ie then
    Result := 'ie' // Interlingue
  else if Value = iso639_ik then
    Result := 'ik' // Inupiak
  else if Value = iso639_in then
    Result := 'in' // Indonesian
  else if Value = iso639_is then
    Result := 'is' // Icelandic
  else if Value = iso639_it then
    Result := 'it' // Italian
  else if Value = iso639_iw then
    Result := 'iw' // Hebrew
  else if Value = iso639_ja then
    Result := 'ja' // Japanese
  else if Value = iso639_ji then
    Result := 'ji' // Yiddish
  else if Value = iso639_jw then
    Result := 'jw' // Javanese
  else if Value = iso639_ka then
    Result := 'ka' // Georgian
  else if Value = iso639_kk then
    Result := 'kk' // Kazakh
  else if Value = iso639_kl then
    Result := 'kl' // Greenlandic
  else if Value = iso639_km then
    Result := 'km' // Cambodian
  else if Value = iso639_kn then
    Result := 'kn' // Kannada
  else if Value = iso639_ko then
    Result := 'ko' // Korean
  else if Value = iso639_ks then
    Result := 'ks' // Kashmiri
  else if Value = iso639_ku then
    Result := 'ku' // Kurdish
  else if Value = iso639_ky then
    Result := 'ky' // Kirghiz
  else if Value = iso639_la then
    Result := 'la' // Latin
  else if Value = iso639_ln then
    Result := 'ln' // Lingala
  else if Value = iso639_lo then
    Result := 'lo' // Laothian
  else if Value = iso639_lt then
    Result := 'lt' // Lithuanian
  else if Value = iso639_lv then
    Result := 'lv' // Latvian; Lettish
  else if Value = iso639_mg then
    Result := 'mg' // Malagasy
  else if Value = iso639_mi then
    Result := 'mi' // Maori
  else if Value = iso639_mk then
    Result := 'mk' // Macedonian
  else if Value = iso639_ml then
    Result := 'ml' // Malayalam
  else if Value = iso639_mn then
    Result := 'mn' // Mongolian
  else if Value = iso639_mo then
    Result := 'mo' // Moldavian
  else if Value = iso639_mr then
    Result := 'mr' // Marathi
  else if Value = iso639_ms then
    Result := 'ms' // Malay
  else if Value = iso639_mt then
    Result := 'mt' // Maltese
  else if Value = iso639_my then
    Result := 'my' // Burmese
  else if Value = iso639_na then
    Result := 'na' // Nauru
  else if Value = iso639_ne then
    Result := 'ne' // Nepali
  else if Value = iso639_nl then
    Result := 'nl' // Dutch
  else if Value = iso639_no then
    Result := 'no' // Norwegian
  else if Value = iso639_oc then
    Result := 'oc' // Occitan
  else if Value = iso639_om then
    Result := 'om' // Afan; Oromo
  else if Value = iso639_or then
    Result := 'or' // Oriya
  else if Value = iso639_pa then
    Result := 'pa' // Punjabi
  else if Value = iso639_pl then
    Result := 'pl' // Polish
  else if Value = iso639_ps then
    Result := 'ps' // Pashto; Pushto
  else if Value = iso639_pt then
    Result := 'pt' // Portuguese
  else if Value = iso639_qu then
    Result := 'qu' // Quechua
  else if Value = iso639_rm then
    Result := 'rm' // Rhaeto-Romance
  else if Value = iso639_rn then
    Result := 'rn' // Kirundi
  else if Value = iso639_ro then
    Result := 'ro' // Romanian
  else if Value = iso639_ru then
    Result := 'ru' // Russian
  else if Value = iso639_rw then
    Result := 'rw' // Kinyarwanda
  else if Value = iso639_sa then
    Result := 'sa' // Sanskrit
  else if Value = iso639_sd then
    Result := 'sd' // Sindhi
  else if Value = iso639_sg then
    Result := 'sg' // Sangro
  else if Value = iso639_sh then
    Result := 'sh' // Serbo-Croatian
  else if Value = iso639_si then
    Result := 'si' // Singhalese
  else if Value = iso639_sk then
    Result := 'sk' // Slovak
  else if Value = iso639_sl then
    Result := 'sl' // Slovenian
  else if Value = iso639_sm then
    Result := 'sm' // Samoan
  else if Value = iso639_sn then
    Result := 'sn' // Shona
  else if Value = iso639_so then
    Result := 'so' // Somali
  else if Value = iso639_sq then
    Result := 'sq' // Albanian
  else if Value = iso639_sr then
    Result := 'sr' // Serbian
  else if Value = iso639_ss then
    Result := 'ss' // Siswati
  else if Value = iso639_st then
    Result := 'st' // Sesotho
  else if Value = iso639_su then
    Result := 'su' // Sundanese
  else if Value = iso639_sv then
    Result := 'sv' // Swedish
  else if Value = iso639_sw then
    Result := 'sw' // Swahili
  else if Value = iso639_ta then
    Result := 'ta' // Tamil
  else if Value = iso639_te then
    Result := 'te' // Tegulu
  else if Value = iso639_tg then
    Result := 'tg' // Tajik
  else if Value = iso639_th then
    Result := 'th' // Thai
  else if Value = iso639_ti then
    Result := 'ti' // Tigrinya
  else if Value = iso639_tk then
    Result := 'tk' // Turkmen
  else if Value = iso639_tl then
    Result := 'tl' // Tagalog
  else if Value = iso639_tn then
    Result := 'tn' // Setswana
  else if Value = iso639_to then
    Result := 'to' // Tonga
  else if Value = iso639_tr then
    Result := 'tr' // Turkish
  else if Value = iso639_ts then
    Result := 'ts' // Tsonga
  else if Value = iso639_tt then
    Result := 'tt' // Tatar
  else if Value = iso639_tw then
    Result := 'tw' // Twi
  else if Value = iso639_uk then
    Result := 'uk' // Ukrainian
  else if Value = iso639_ur then
    Result := 'ur' // Urdu
  else if Value = iso639_uz then
    Result := 'uz' // Uzbek
  else if Value = iso639_vi then
    Result := 'vi' // Vietnamese
  else if Value = iso639_vo then
    Result := 'vo' // Volapuk
  else if Value = iso639_wo then
    Result := 'wo' // Wolof
  else if Value = iso639_xh then
    Result := 'xh' // Xhosa
  else if Value = iso639_yo then
    Result := 'yo' // Yoruba
  else if Value = iso639_zh then
    Result := 'zh' // Chinese
  else if Value = iso639_zu then
    Result := 'zu' // Zulu
    ;
end;

function TIso639Info.NameToCode(const Value: WideString): TIso639LanguageCode;
begin
  case NameLanguage of
    iso639_en: Result := NameToCode_en(Value);
  else
    raise EConvertError.Create('Conversion not supported');
  end;
end;

function TIso639Info.NameToCode_en(Value: WideString): TIso639LanguageCode;
var
  I, J: integer;
  Dummy: WideString;
begin
  I := Pos(';', Value);
  if I > 0 then
  begin
    Dummy := Copy(Value, 1, I - 1);
    Value := Dummy;
  end
  else
  begin
    J := Pos('[', Value);
    if J > 1 then
    begin
      if Value[J] = ' ' then
      begin
        Dummy := Copy(Value, 1, J - 2);
        Value := Dummy;
      end;
    end;
  end;

  if Value = 'Afar' then
    Result := iso639_aa
  else if Value = 'Abkhazian' then
    Result := iso639_ab
  else if Value = 'Afrikaans' then
    Result := iso639_af
  else if Value = 'Amharic' then
    Result := iso639_am
  else if Value = 'Arabic' then
    Result := iso639_ar
  else if Value = 'Assamese' then
    Result := iso639_as
  else if Value = 'Aymara' then
    Result := iso639_ay
  else if Value = 'Azerbaijani' then
    Result := iso639_az
  else if Value = 'Bashkir' then
    Result := iso639_ba
  else if Value = 'Byelorussian' then
    Result := iso639_be
  else if Value = 'Bulgarian' then
    Result := iso639_bg
  else if Value = 'Bihari' then
    Result := iso639_bh
  else if Value = 'Bislama' then
    Result := iso639_bi
  else if Value = 'Bengali' then
    Result := iso639_bn
  else if Value = 'Bangla' then
    Result := iso639_bn
  else if Value = 'Tibetan' then
    Result := iso639_bo
  else if Value = 'Breton' then
    Result := iso639_br
  else if Value = 'Catalan' then
    Result := iso639_ca
  else if Value = 'Corsican' then
    Result := iso639_co
  else if Value = 'Czech' then
    Result := iso639_cs
  else if Value = 'Welsh' then
    Result := iso639_cy
  else if Value = 'Danish' then
    Result := iso639_da
  else if Value = 'German' then
    Result := iso639_de
  else if Value = 'Bhutani' then
    Result := iso639_dz
  else if Value = 'Greek' then
    Result := iso639_el
  else if Value = 'English' then
    Result := iso639_en
  else if Value = 'Esperanto' then
    Result := iso639_eo
  else if Value = 'Spanish' then
    Result := iso639_es
  else if Value = 'Estonian' then
    Result := iso639_et
  else if Value = 'Basque' then
    Result := iso639_eu
  else if Value = 'Persian' then
    Result := iso639_fa
  else if Value = 'Finnish' then
    Result := iso639_fi
  else if Value = 'Fiji' then
    Result := iso639_fj
  else if Value = 'Faeroese' then
    Result := iso639_fo
  else if Value = 'French' then
    Result := iso639_fr
  else if Value = 'Frisian' then
    Result := iso639_fy
  else if Value = 'Irish' then
    Result := iso639_ga
  else if Value = 'Scots Gaelic' then
    Result := iso639_gd
  else if Value = 'Galician' then
    Result := iso639_gl
  else if Value = 'Guarani' then
    Result := iso639_gn
  else if Value = 'Gujarati' then
    Result := iso639_gu
  else if Value = 'Hausa' then
    Result := iso639_ha
  else if Value = 'Hindi' then
    Result := iso639_hi
  else if Value = 'Croatian' then
    Result := iso639_hr
  else if Value = 'Hungarian' then
    Result := iso639_hu
  else if Value = 'Armenian' then
    Result := iso639_hy
  else if Value = 'Interlingua' then
    Result := iso639_ia
  else if Value = 'Interlingue' then
    Result := iso639_ie
  else if Value = 'Inupiak' then
    Result := iso639_ik
  else if Value = 'Indonesian' then
    Result := iso639_in
  else if Value = 'Icelandic' then
    Result := iso639_is
  else if Value = 'Italian' then
    Result := iso639_it
  else if Value = 'Hebrew' then
    Result := iso639_iw
  else if Value = 'Japanese' then
    Result := iso639_ja
  else if Value = 'Yiddish' then
    Result := iso639_ji
  else if Value = 'Javanese' then
    Result := iso639_jw
  else if Value = 'Georgian' then
    Result := iso639_ka
  else if Value = 'Kazakh' then
    Result := iso639_kk
  else if Value = 'Greenlandic' then
    Result := iso639_kl
  else if Value = 'Cambodian' then
    Result := iso639_km
  else if Value = 'Kannada' then
    Result := iso639_kn
  else if Value = 'Korean' then
    Result := iso639_ko
  else if Value = 'Kashmiri' then
    Result := iso639_ks
  else if Value = 'Kurdish' then
    Result := iso639_ku
  else if Value = 'Kirghiz' then
    Result := iso639_ky
  else if Value = 'Latin' then
    Result := iso639_la
  else if Value = 'Lingala' then
    Result := iso639_ln
  else if Value = 'Laothian' then
    Result := iso639_lo
  else if Value = 'Lithuanian' then
    Result := iso639_lt
  else if Value = 'Latvian' then
    Result := iso639_lv
  else if Value = 'Lettish' then
    Result := iso639_lv
  else if Value = 'Malagasy' then
    Result := iso639_mg
  else if Value = 'Maori' then
    Result := iso639_mi
  else if Value = 'Macedonian' then
    Result := iso639_mk
  else if Value = 'Malayalam' then
    Result := iso639_ml
  else if Value = 'Mongolian' then
    Result := iso639_mn
  else if Value = 'Moldavian' then
    Result := iso639_mo
  else if Value = 'Marathi' then
    Result := iso639_mr
  else if Value = 'Malay' then
    Result := iso639_ms
  else if Value = 'Maltese' then
    Result := iso639_mt
  else if Value = 'Burmese' then
    Result := iso639_my
  else if Value = 'Nauru' then
    Result := iso639_na
  else if Value = 'Nepali' then
    Result := iso639_ne
  else if Value = 'Dutch' then
    Result := iso639_nl
  else if Value = 'Norwegian' then
    Result := iso639_no
  else if Value = 'Occitan' then
    Result := iso639_oc
  else if Value = 'Afan' then
    Result := iso639_om
  else if Value = 'Oromo' then
    Result := iso639_om
  else if Value = 'Oriya' then
    Result := iso639_or
  else if Value = 'Punjabi' then
    Result := iso639_pa
  else if Value = 'Polish' then
    Result := iso639_pl
  else if Value = 'Pashto' then
    Result := iso639_ps
  else if Value = 'Pushto' then
    Result := iso639_ps
  else if Value = 'Portuguese' then
    Result := iso639_pt
  else if Value = 'Quechua' then
    Result := iso639_qu
  else if Value = 'Rhaeto-Romance' then
    Result := iso639_rm
  else if Value = 'Kirundi' then
    Result := iso639_rn
  else if Value = 'Romanian' then
    Result := iso639_ro
  else if Value = 'Russian' then
    Result := iso639_ru
  else if Value = 'Kinyarwanda' then
    Result := iso639_rw
  else if Value = 'Sanskrit' then
    Result := iso639_sa
  else if Value = 'Sindhi' then
    Result := iso639_sd
  else if Value = 'Sangro' then
    Result := iso639_sg
  else if Value = 'Serbo-Croatian' then
    Result := iso639_sh
  else if Value = 'Singhalese' then
    Result := iso639_si
  else if Value = 'Slovak' then
    Result := iso639_sk
  else if Value = 'Slovenian' then
    Result := iso639_sl
  else if Value = 'Samoan' then
    Result := iso639_sm
  else if Value = 'Shona' then
    Result := iso639_sn
  else if Value = 'Somali' then
    Result := iso639_so
  else if Value = 'Albanian' then
    Result := iso639_sq
  else if Value = 'Serbian' then
    Result := iso639_sr
  else if Value = 'Siswati' then
    Result := iso639_ss
  else if Value = 'Sesotho' then
    Result := iso639_st
  else if Value = 'Sundanese' then
    Result := iso639_su
  else if Value = 'Swedish' then
    Result := iso639_sv
  else if Value = 'Swahili' then
    Result := iso639_sw
  else if Value = 'Tamil' then
    Result := iso639_ta
  else if Value = 'Tegulu' then
    Result := iso639_te
  else if Value = 'Tajik' then
    Result := iso639_tg
  else if Value = 'Thai' then
    Result := iso639_th
  else if Value = 'Tigrinya' then
    Result := iso639_ti
  else if Value = 'Turkmen' then
    Result := iso639_tk
  else if Value = 'Tagalog' then
    Result := iso639_tl
  else if Value = 'Setswana' then
    Result := iso639_tn
  else if Value = 'Tonga' then
    Result := iso639_to
  else if Value = 'Turkish' then
    Result := iso639_tr
  else if Value = 'Tsonga' then
    Result := iso639_ts
  else if Value = 'Tatar' then
    Result := iso639_tt
  else if Value = 'Twi' then
    Result := iso639_tw
  else if Value = 'Ukrainian' then
    Result := iso639_uk
  else if Value = 'Urdu' then
    Result := iso639_ur
  else if Value = 'Uzbek' then
    Result := iso639_uz
  else if Value = 'Vietnamese' then
    Result := iso639_vi
  else if Value = 'Volapuk' then
    Result := iso639_vo
  else if Value = 'Wolof' then
    Result := iso639_wo
  else if Value = 'Xhosa' then
    Result := iso639_xh
  else if Value = 'Yoruba' then
    Result := iso639_yo
  else if Value = 'Chinese' then
    Result := iso639_zh
  else if Value = 'Zulu' then
    Result := iso639_zu
  else
    raise EConvertError.Create('Invalid ISO 639 language name');
end;

procedure TIso639Info.setNameLanguage(const Value: TIso639LanguageCode);
begin
  if not (Value in FSupportedLanguages) then
    raise EConvertError.Create('Language not supported');
end;

function TIso639Info.symbolToCode(const Value: WideString): TIso639LanguageCode;
begin
  if Value = 'aa' then
    Result := iso639_aa // Afar
  else if Value = 'ab' then
    Result := iso639_ab // Abkhazian
  else if Value = 'af' then
    Result := iso639_af // Afrikaans
  else if Value = 'am' then
    Result := iso639_am // Amharic
  else if Value = 'ar' then
    Result := iso639_ar // Arabic
  else if Value = 'as' then
    Result := iso639_as // Assamese
  else if Value = 'ay' then
    Result := iso639_ay // Aymara
  else if Value = 'az' then
    Result := iso639_az // Azerbaijani
  else if Value = 'ba' then
    Result := iso639_ba // Bashkir
  else if Value = 'be' then
    Result := iso639_be // Byelorussian
  else if Value = 'bg' then
    Result := iso639_bg // Bulgarian
  else if Value = 'bh' then
    Result := iso639_bh // Bihari
  else if Value = 'bi' then
    Result := iso639_bi // Bislama
  else if Value = 'bn' then
    Result := iso639_bn // Bengali; Bangla
  else if Value = 'bo' then
    Result := iso639_bo // Tibetan
  else if Value = 'br' then
    Result := iso639_br // Breton
  else if Value = 'ca' then
    Result := iso639_ca // Catalan
  else if Value = 'co' then
    Result := iso639_co // Corsican
  else if Value = 'cs' then
    Result := iso639_cs // Czech
  else if Value = 'cy' then
    Result := iso639_cy // Welsh
  else if Value = 'da' then
    Result := iso639_da // Danish
  else if Value = 'de' then
    Result := iso639_de // German
  else if Value = 'dz' then
    Result := iso639_dz // Bhutani
  else if Value = 'el' then
    Result := iso639_el // Greek
  else if Value = 'en' then
    Result := iso639_en // English
  else if Value = 'eo' then
    Result := iso639_eo // Esperanto
  else if Value = 'es' then
    Result := iso639_es // Spanish
  else if Value = 'et' then
    Result := iso639_et // Estonian
  else if Value = 'eu' then
    Result := iso639_eu // Basque
  else if Value = 'fa' then
    Result := iso639_fa // Persian
  else if Value = 'fi' then
    Result := iso639_fi // Finnish
  else if Value = 'fj' then
    Result := iso639_fj // Fiji
  else if Value = 'fo' then
    Result := iso639_fo // Faeroese
  else if Value = 'fr' then
    Result := iso639_fr // French
  else if Value = 'fy' then
    Result := iso639_fy // Frisian
  else if Value = 'ga' then
    Result := iso639_ga // Irish
  else if Value = 'gd' then
    Result := iso639_gd // Scots Gaelic
  else if Value = 'gl' then
    Result := iso639_gl // Galician
  else if Value = 'gn' then
    Result := iso639_gn // Guarani
  else if Value = 'gu' then
    Result := iso639_gu // Gujarati
  else if Value = 'ha' then
    Result := iso639_ha // Hausa
  else if Value = 'hi' then
    Result := iso639_hi // Hindi
  else if Value = 'hr' then
    Result := iso639_hr // Croatian
  else if Value = 'hu' then
    Result := iso639_hu // Hungarian
  else if Value = 'hy' then
    Result := iso639_hy // Armenian
  else if Value = 'ia' then
    Result := iso639_ia // Interlingua
  else if Value = 'ie' then
    Result := iso639_ie // Interlingue
  else if Value = 'ik' then
    Result := iso639_ik // Inupiak
  else if Value = 'in' then
    Result := iso639_in // Indonesian
  else if Value = 'is' then
    Result := iso639_is // Icelandic
  else if Value = 'it' then
    Result := iso639_it // Italian
  else if Value = 'iw' then
    Result := iso639_iw // Hebrew
  else if Value = 'ja' then
    Result := iso639_ja // Japanese
  else if Value = 'ji' then
    Result := iso639_ji // Yiddish
  else if Value = 'jw' then
    Result := iso639_jw // Javanese
  else if Value = 'ka' then
    Result := iso639_ka // Georgian
  else if Value = 'kk' then
    Result := iso639_kk // Kazakh
  else if Value = 'kl' then
    Result := iso639_kl // Greenlandic
  else if Value = 'km' then
    Result := iso639_km // Cambodian
  else if Value = 'kn' then
    Result := iso639_kn // Kannada
  else if Value = 'ko' then
    Result := iso639_ko // Korean
  else if Value = 'ks' then
    Result := iso639_ks // Kashmiri
  else if Value = 'ku' then
    Result := iso639_ku // Kurdish
  else if Value = 'ky' then
    Result := iso639_ky // Kirghiz
  else if Value = 'la' then
    Result := iso639_la // Latin
  else if Value = 'ln' then
    Result := iso639_ln // Lingala
  else if Value = 'lo' then
    Result := iso639_lo // Laothian
  else if Value = 'lt' then
    Result := iso639_lt // Lithuanian
  else if Value = 'lv' then
    Result := iso639_lv // Latvian; Lettish
  else if Value = 'mg' then
    Result := iso639_mg // Malagasy
  else if Value = 'mi' then
    Result := iso639_mi // Maori
  else if Value = 'mk' then
    Result := iso639_mk // Macedonian
  else if Value = 'ml' then
    Result := iso639_ml // Malayalam
  else if Value = 'mn' then
    Result := iso639_mn // Mongolian
  else if Value = 'mo' then
    Result := iso639_mo // Moldavian
  else if Value = 'mr' then
    Result := iso639_mr // Marathi
  else if Value = 'ms' then
    Result := iso639_ms // Malay
  else if Value = 'mt' then
    Result := iso639_mt // Maltese
  else if Value = 'my' then
    Result := iso639_my // Burmese
  else if Value = 'na' then
    Result := iso639_na // Nauru
  else if Value = 'ne' then
    Result := iso639_ne // Nepali
  else if Value = 'nl' then
    Result := iso639_nl // Dutch
  else if Value = 'no' then
    Result := iso639_no // Norwegian
  else if Value = 'oc' then
    Result := iso639_oc // Occitan
  else if Value = 'om' then
    Result := iso639_om // Afan; Oromo
  else if Value = 'or' then
    Result := iso639_or // Oriya
  else if Value = 'pa' then
    Result := iso639_pa // Punjabi
  else if Value = 'pl' then
    Result := iso639_pl // Polish
  else if Value = 'ps' then
    Result := iso639_ps // Pashto; Pushto
  else if Value = 'pt' then
    Result := iso639_pt // Portuguese
  else if Value = 'qu' then
    Result := iso639_qu // Quechua
  else if Value = 'rm' then
    Result := iso639_rm // Rhaeto-Romance
  else if Value = 'rn' then
    Result := iso639_rn // Kirundi
  else if Value = 'ro' then
    Result := iso639_ro // Romanian
  else if Value = 'ru' then
    Result := iso639_ru // Russian
  else if Value = 'rw' then
    Result := iso639_rw // Kinyarwanda
  else if Value = 'sa' then
    Result := iso639_sa // Sanskrit
  else if Value = 'sd' then
    Result := iso639_sd // Sindhi
  else if Value = 'sg' then
    Result := iso639_sg // Sangro
  else if Value = 'sh' then
    Result := iso639_sh // Serbo-Croatian
  else if Value = 'si' then
    Result := iso639_si // Singhalese
  else if Value = 'sk' then
    Result := iso639_sk // Slovak
  else if Value = 'sl' then
    Result := iso639_sl // Slovenian
  else if Value = 'sm' then
    Result := iso639_sm // Samoan
  else if Value = 'sn' then
    Result := iso639_sn // Shona
  else if Value = 'so' then
    Result := iso639_so // Somali
  else if Value = 'sq' then
    Result := iso639_sq // Albanian
  else if Value = 'sr' then
    Result := iso639_sr // Serbian
  else if Value = 'ss' then
    Result := iso639_ss // Siswati
  else if Value = 'st' then
    Result := iso639_st // Sesotho
  else if Value = 'su' then
    Result := iso639_su // Sundanese
  else if Value = 'sv' then
    Result := iso639_sv // Swedish
  else if Value = 'sw' then
    Result := iso639_sw // Swahili
  else if Value = 'ta' then
    Result := iso639_ta // Tamil
  else if Value = 'te' then
    Result := iso639_te // Tegulu
  else if Value = 'tg' then
    Result := iso639_tg // Tajik
  else if Value = 'th' then
    Result := iso639_th // Thai
  else if Value = 'ti' then
    Result := iso639_ti // Tigrinya
  else if Value = 'tk' then
    Result := iso639_tk // Turkmen
  else if Value = 'tl' then
    Result := iso639_tl // Tagalog
  else if Value = 'tn' then
    Result := iso639_tn // Setswana
  else if Value = 'to' then
    Result := iso639_to // Tonga
  else if Value = 'tr' then
    Result := iso639_tr // Turkish
  else if Value = 'ts' then
    Result := iso639_ts // Tsonga
  else if Value = 'tt' then
    Result := iso639_tt // Tatar
  else if Value = 'tw' then
    Result := iso639_tw // Twi
  else if Value = 'uk' then
    Result := iso639_uk // Ukrainian
  else if Value = 'ur' then
    Result := iso639_ur // Urdu
  else if Value = 'uz' then
    Result := iso639_uz // Uzbek
  else if Value = 'vi' then
    Result := iso639_vi // Vietnamese
  else if Value = 'vo' then
    Result := iso639_vo // Volapuk
  else if Value = 'wo' then
    Result := iso639_wo // Wolof
  else if Value = 'xh' then
    Result := iso639_xh // Xhosa
  else if Value = 'yo' then
    Result := iso639_yo // Yoruba
  else if Value = 'zh' then
    Result := iso639_zh // Chinese
  else if Value = 'zu' then
    Result := iso639_zu // Zulu
  else
    raise EConvertError.Create('Invalid ISO 639 language symbol');
end;

end.


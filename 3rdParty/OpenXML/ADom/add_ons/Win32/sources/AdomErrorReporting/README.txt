The AdomErrorReporting unit implements several components which provide basic translations from TdomError objects into specific strings describing the error that occurred.


REQUIREMENTS
------------

The Open XML Dynamic Controls package must be installed on you Delphi system.


INSTALLATION
------------

The installation procedure is as follows:

   1. Select the Option "Install Component" from the "Component" menu.
   2. Add the file ending with ".pas" to a new or to an already existing package.
   3. Click on OK, and next confirm that the components should be compiled and installed.
   4. Close the package window, and next confirm that the modifications should be saved.


COMPONENT OVERVIEW
------------------

TDomCustomErrorTranslator
-------------------------

The abstract base class for all error translation components is the TDomCustomErrorTranslator class.  Derived classes must override the abstract methods to implement language specific translation functions.

property Language: TIso639LanguageCode 
  The language for the error message.  An ENot_Supported_Err is raised if an attempt is made to specify a language which is not supported.

property SupportedLanguages: TIso639LanguageCodeSet (readonly) 
  SupportedLanguages indicates the languages supported by the error translation component.

property TabWidth: Integer
  Specifies for how many columns a TAB character (#0009) counts.  TabWidth is especially useful for adjusting XML error reports to the settings of some XML editor.  The default value is 4.

function Translate(const Error: TdomError): WideString; virtual; abstract;
  Returns a WideString encoded in UTF-16 that contains a detailed error message in the language specified in the Language property.
  Hint: To use this UTF-16 WideString in Delphi's native VCL components, you must convert it from UTF-16 to the code page used on your system; this usually is Windows-1250 for Polish resp. Windows-1252 for the other supported languages.


Error Translation Components for Individual Languages
-----------------------------------------------------

The AdomErrorReporting_4_3 unit comes with a couple of simple error translation components for different languages.  Note that usually only the German and the English versions are perfectly up-to-date.  The other error translators were donated by ADOM users.  Since I do not speak those languages I cannot myself update the translation if a modification in ADOM would require an update.  So in case of the other languages you must, if necessary, update the translations yourself.  Your updates, enhancements and new error translators are always welcome.  Please sent them to <service@philo.de>.

Currently supported languages:
  Language       Class                               Author
-------------------------------------------------------------------------
- Dutch        TDomDutchErrorTranslator        Erik van der Poll [1]
- English      TDomEnglishErrorTranslator      Dieter Köhler
- French       TDomFrenchErrorTranslator       Bernard Eblin
- German       TDomGermanErrorTranslator       Dieter Köhler
- Italian      TDomItalianErrorTranslator      Massimo Maria Ghisalberti
- Polish       TDomPolishErrorTranslator       Piotr Kuczynski
- Portuguese   TDomPortugueseErrorTranslator   Ricardo Albuquerque [2]
- Spanish      TDomSpanishErrorTranslator      Pedro de Paz


[1] Locus Warehouse Management Systems b.v., Almere
[2] Recife, Brazil



TDomErrorAutoStrings
--------------------

TDomErrorAutoStrings is a class derived from TUtilsCustomAliasedStrings (specified in the AutoListUtils.pas unit from the Open XML Dynamic Controls package).  It is used to maintaine a list of (internal) copies of TDomError objects.  

property Capacity : Integer
  Indicates the number of errors the list has allocated memory to hold.

property Count: Integer (readonly)
  Indicates the number of errors in the list.

property Errors[Index: Integer]: TdomError (readonly)
  Lists the errors in the list, referenced by a 0-based index.

property Strings[Index: Integer]: string (readonly)
  Lists a set of strings associated one with each of the erros in the Errors property.

property Enabled: Boolean (published)
  Determines whether the iterators associated with the TDomErrorAutoStrings component are active.
  
property ErrorTranslator: TDomCustomErrorTranslator (published)
  Indicates the Error Translation Component which is used to calculate the Strings property.

function AddError(AError: TDomError): Integer; virtual;
  Adds a copy of the specified TDomError object to the end of the list.

procedure Clear; override;
  Removes (and frees) all the errors from the list.

procedure Delete(Index: Integer); override;
  Removes (and frees) the error at the specified position from the list.

function InsertError(Index: Integer; AError: TDomError): Boolean; virtual;
  Inserts a copy of the specified TDomError object into the list at the specified position.

procedure DisableControls; virtual;
  Disables list-aware controls associated with this TDomErrorAutoStrings component.

procedure EnableControls; virtual;
  Enables list-aware controls associated with this TDomErrorAutoStrings component.
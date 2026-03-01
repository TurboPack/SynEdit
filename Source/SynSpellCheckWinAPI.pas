{-------------------------------------------------------------------------------
TurboPack SynEdit

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
-------------------------------------------------------------------------------}

{ Windows Spell Checking API COM interface declarations.
  Shared by both VCL and FMX spell check implementations.
  Requires Windows 8+ at runtime. }

unit SynSpellCheckWinAPI;

{$I SynEdit.inc}

{$IFDEF MSWINDOWS}

interface

uses
  Winapi.Windows,
  Winapi.ActiveX;

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:
//   Type Libraries     : LIBID_xxxx
//   CoClasses          : CLASS_xxxx
//   DISPInterfaces     : DIID_xxxx
//   Non-DISP interfaces: IID_xxxx
// *********************************************************************//
const
  IID_ISpellCheckerFactory: TGUID = '{8E018A9D-2415-4677-BF08-794EA61F94BB}';
  IID_IUserDictionariesRegistrar: TGUID = '{AA176B85-0E12-4844-8E1A-EEF1DA77F586}';
  IID_ISpellChecker: TGUID = '{B6FD0B71-E2BC-4653-8D05-F197E412770B}';
  IID_IEnumSpellingError: TGUID = '{803E3BD4-2828-4410-8290-418D1D73C762}';
  IID_ISpellingError: TGUID = '{B7C82D61-FBE8-4B47-9B27-6C0D2E0DE0A3}';
  IID_ISpellCheckerChangedEventHandler: TGUID = '{0B83A5B0-792F-4EAB-9799-ACF52C5ED08A}';
  IID_IOptionDescription: TGUID = '{432E5F85-35CF-4606-A801-6F70277E1D7A}';
  CLASS_SpellCheckerFactory: TGUID = '{7AB36653-1796-484B-BDFA-E74F1DB7C1DC}';

// Constants for enum CORRECTIVE_ACTION
type
  CORRECTIVE_ACTION = TOleEnum;
  TCorrectiveAction = (secaNone, secaSuggestions, secaReplace, secaDelete);

const
  CORRECTIVE_ACTION_NONE = $00000000;
  CORRECTIVE_ACTION_GET_SUGGESTIONS = $00000001;
  CORRECTIVE_ACTION_REPLACE = $00000002;
  CORRECTIVE_ACTION_DELETE = $00000003;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary
// *********************************************************************//
  ISpellCheckerFactory = interface;
  IUserDictionariesRegistrar = interface;
  ISpellChecker = interface;
  IEnumSpellingError = interface;
  ISpellingError = interface;
  ISpellCheckerChangedEventHandler = interface;
  IOptionDescription = interface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library
// (NOTE: Here we map each CoClass to its Default Interface)
// *********************************************************************//
  SpellCheckerFactory = ISpellCheckerFactory;

// *********************************************************************//
// Interface: ISpellCheckerFactory
// GUID:      {8E018A9D-2415-4677-BF08-794EA61F94BB}
// *********************************************************************//
  ISpellCheckerFactory = interface(IUnknown)
    ['{8E018A9D-2415-4677-BF08-794EA61F94BB}']
    function Get_SupportedLanguages(out value: IEnumString): HResult; stdcall;
    function IsSupported(languageTag: PWideChar; out value: Integer): HResult; stdcall;
    function CreateSpellChecker(languageTag: PWideChar; out value: ISpellChecker): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IUserDictionariesRegistrar
// GUID:      {AA176B85-0E12-4844-8E1A-EEF1DA77F586}
// *********************************************************************//
  IUserDictionariesRegistrar = interface(IUnknown)
    ['{AA176B85-0E12-4844-8E1A-EEF1DA77F586}']
    function RegisterUserDictionary(dictionaryPath: PWideChar; languageTag: PWideChar): HResult; stdcall;
    function UnregisterUserDictionary(dictionaryPath: PWideChar; languageTag: PWideChar): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpellChecker
// GUID:      {B6FD0B71-E2BC-4653-8D05-F197E412770B}
// *********************************************************************//
  ISpellChecker = interface(IUnknown)
    ['{B6FD0B71-E2BC-4653-8D05-F197E412770B}']
    function Get_languageTag(out value: PWideChar): HResult; stdcall;
    function Check(text: PWideChar; out value: IEnumSpellingError): HResult; stdcall;
    function Suggest(word: PWideChar; out value: IEnumString): HResult; stdcall;
    function Add(word: PWideChar): HResult; stdcall;
    function Ignore(word: PWideChar): HResult; stdcall;
    function AutoCorrect(from: PWideChar; to_: PWideChar): HResult; stdcall;
    function GetOptionValue(optionId: PWideChar; out value: Byte): HResult; stdcall;
    function Get_OptionIds(out value: IEnumString): HResult; stdcall;
    function Get_Id(out value: PWideChar): HResult; stdcall;
    function Get_LocalizedName(out value: PWideChar): HResult; stdcall;
    function add_SpellCheckerChanged(const handler: ISpellCheckerChangedEventHandler;
                                     out eventCookie: LongWord): HResult; stdcall;
    function remove_SpellCheckerChanged(eventCookie: LongWord): HResult; stdcall;
    function GetOptionDescription(optionId: PWideChar; out value: IOptionDescription): HResult; stdcall;
    function ComprehensiveCheck(text: PWideChar; out value: IEnumSpellingError): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpellChecker2
// GUID:      {E7ED1C71-87F7-4378-A840-C9200DACEE47}
// *********************************************************************//
  ISpellChecker2 = interface(ISpellChecker)
    ['{E7ED1C71-87F7-4378-A840-C9200DACEE47}']
    function Remove(word: PWideChar): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IEnumSpellingError
// GUID:      {803E3BD4-2828-4410-8290-418D1D73C762}
// *********************************************************************//
  IEnumSpellingError = interface(IUnknown)
    ['{803E3BD4-2828-4410-8290-418D1D73C762}']
    function Next(out value: ISpellingError): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpellingError
// GUID:      {B7C82D61-FBE8-4B47-9B27-6C0D2E0DE0A3}
// *********************************************************************//
  ISpellingError = interface(IUnknown)
    ['{B7C82D61-FBE8-4B47-9B27-6C0D2E0DE0A3}']
    function Get_StartIndex(out value: LongWord): HResult; stdcall;
    function Get_Length(out value: LongWord): HResult; stdcall;
    function Get_CorrectiveAction(out value: CORRECTIVE_ACTION): HResult; stdcall;
    function Get_Replacement(out value: PWideChar): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: ISpellCheckerChangedEventHandler
// GUID:      {0B83A5B0-792F-4EAB-9799-ACF52C5ED08A}
// *********************************************************************//
  ISpellCheckerChangedEventHandler = interface(IUnknown)
    ['{0B83A5B0-792F-4EAB-9799-ACF52C5ED08A}']
    function Invoke(const sender: ISpellChecker): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IOptionDescription
// GUID:      {432E5F85-35CF-4606-A801-6F70277E1D7A}
// *********************************************************************//
  IOptionDescription = interface(IUnknown)
    ['{432E5F85-35CF-4606-A801-6F70277E1D7A}']
    function Get_Id(out value: PWideChar): HResult; stdcall;
    function Get_Heading(out value: PWideChar): HResult; stdcall;
    function Get_Description(out value: PWideChar): HResult; stdcall;
    function Get_Labels(out value: IEnumString): HResult; stdcall;
  end;

{$ENDIF MSWINDOWS}

implementation

end.

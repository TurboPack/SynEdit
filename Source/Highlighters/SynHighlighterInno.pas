{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterInno.pas, released 2000-05-01.
The Initial Author of this file is Satya.
Portions created by Satya are Copyright 2000 Satya.
Unicode translation by Maël Hörz.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.
-------------------------------------------------------------------------------}
{
@abstract(Provides an Inno Setup script highlighter for SynEdit)
@author(Satya)
@created(2000-05-01)
@lastmod(2026-06-05)
The SynHighlighterInno unit provides an Inno script file highlighter for SynEdit.
Check out http://www.jrsoftware.org for the free Inno Setup program,
and http://www.wintax.nl/isx/ for My Inno Setup Extensions.
}

unit SynHighlighterInno;

{$I SynEdit.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Defaults,
  System.Generics.Collections,
  Vcl.Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynFunc;

type
  TtkTokenKind = (tkComment, tkConstant, tkFunction, tkIdentifier, tkKey,
    tkKeyOrParameter, tkNull, tkNumber, tkParameter, tkPreprocessor, tkSection,
    tkSpace, tkString, tkSymbol, tkUnknown);

  // rsUnknown        : normal Inno directive section (e.g. [Setup], [Files])
  // rsCode           : inside the [Code] section, Pascal mode
  // rsCodeBraceComment / rsCodeParenComment : multi-line Pascal comments inside
  //                    the [Code] section ({ ... } and (* ... *) respectively)
  TRangeState = (rsUnknown, rsCode, rsCodeBraceComment, rsCodeParenComment);

  TSynInnoSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fTokenID: TtkTokenKind;
    fConstantAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fSectionAttri: TSynHighlighterAttributes;
    fParamAttri: TSynHighlighterAttributes;
    fFunctionAttri: TSynHighlighterAttributes;
    fPreprocessorAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fInvalidAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    FKeywords: TDictionary<string, TtkTokenKind>;
    FCodeKeywords: TDictionary<string, TtkTokenKind>;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    function InCodeSection: Boolean;
    // shared
    procedure CRProc;
    procedure LFProc;
    procedure NullProc;
    procedure SpaceProc;
    procedure NumberProc;
    procedure PreprocessorProc;
    procedure SectionProc;
    procedure UnknownProc;
    // Inno directive mode
    procedure NextInno;
    procedure IdentProc;
    procedure SymbolProc;
    procedure EqualProc;
    procedure ConstantProc;
    procedure SemiColonProc;
    procedure StringProc;
    // Pascal [Code] mode
    procedure NextCode;
    procedure CodeIdentProc;
    procedure CodeSlashProc;
    procedure CodeBraceProc;
    procedure CodeParenProc;
    procedure CodeStringProc;
    procedure CodeHexProc;
    procedure CodeSymbolProc;
    procedure BraceCommentProc;
    procedure ParenCommentProc;
    procedure DoAddKeyword(AKeyword: string; AKind: TSynNativeInt);
    procedure DoAddCodeKeyword(AKeyword: string; AKind: TSynNativeInt);
  protected
    function GetSampleSource: string; override;
    function IsCurrentToken(const Token: string): Boolean; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenKind: TSynNativeInt; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
  published
    property ConstantAttri: TSynHighlighterAttributes read fConstantAttri
      write fConstantAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property FunctionAttri: TSynHighlighterAttributes read fFunctionAttri
      write fFunctionAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property InvalidAttri: TSynHighlighterAttributes read fInvalidAttri
      write fInvalidAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property ParameterAttri: TSynHighlighterAttributes read fParamAttri
      write fParamAttri;
    property PreprocessorAttri: TSynHighlighterAttributes read fPreprocessorAttri
      write fPreprocessorAttri;
    property SectionAttri: TSynHighlighterAttributes read fSectionAttri
      write fSectionAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
  end;

implementation

uses
  SynEditMiscProcs,
  SynEditStrConst;

const
  // Section directive keys ('Key=' in [Setup], 'Key:' in the list sections).
  // Updated for Inno Setup 6.x. New section names and {constants} need not be
  // listed: sections are highlighted by position and constants by syntax.
  Keywords: string =
    'adminprivilegesrequired,afterinstall,allowcancelduringinstall,' +
    'allownetworkdrive,allownoicons,allowrootdirectory,allowuncpath,' +
    'alwaysrestart,alwaysshowcomponentslist,alwaysshowdironreadypage,' +
    'alwaysshowgrouponreadypage,alwaysusepersonalgroup,appcomments,' +
    'appcontact,appcopyright,appid,appmodifypath,appmutex,appname,' +
    'apppublisher,apppublisherurl,appreadmefile,appsupportphone,' +
    'appsupporturl,appupdatesurl,appvername,appversion,' +
    'architecturesallowed,architecturesinstallin64bitmode,attribs,' +
    'backcolor,backcolor2,backcolordirection,backsolid,beforeinstall,bits,' +
    'changesassociations,changesenvironment,check,closeapplications,' +
    'closeapplicationsfilter,codefile,comment,components,compression,' +
    'compressionthreads,compresslevel,copymode,createallsubdirs,createappdir,' +
    'createuninstallregkey,defaultdirname,defaultgroupname,description,' +
    'destdir,destname,direxistswarning,disableappenddir,' +
    'disabledirexistswarning,disabledirpage,disablefinishedpage,' +
    'disableprogramgrouppage,disablereadymemo,disablereadypage,' +
    'disablestartupprompt,disablewelcomepage,diskclustersize,diskslicesize,' +
    'diskspacemblabel,diskspanning,dontmergeduplicatefiles,emptydircheck,' +
    'enabledirdoesntexistwarning,encryption,excludes,extradiskspacerequired,' +
    'externalsize,filename,flags,flatcomponentslist,fontinstall,' +
    'groupdescription,hotkey,iconfilename,iconindex,infoafterfile,' +
    'infobeforefile,internalcompresslevel,key,languagedetectionmethod,' +
    'languages,licensefile,lzmaalgorithm,lzmadictionarysize,lzmamatchfinder,' +
    'lzmanumblockthreads,lzmanumfastbytes,lzmauseseparateprocess,' +
    'mergeduplicatefiles,messagesfile,minversion,name,onlybelowversion,output,' +
    'outputbasefilename,outputdir,outputmanifestfile,parameters,password,' +
    'permissions,privilegesrequired,privilegesrequiredoverridesallowed,' +
    'reservebytes,restartapplications,restartifneededbyrun,root,runonceid,' +
    'section,setupiconfile,setupmutex,sharedfilelocation,showcomponentsizes,' +
    'showlanguagedialog,signtool,signtoolretrycount,signtoolretrydelay,' +
    'signeduninstaller,slicesperdisk,solidcompression,source,sourcedir,' +
    'statusmsg,strongassemblyname,subkey,tasks,timestamprounding,' +
    'timestampsinutc,touchdate,touchtime,type,types,uninstalldisplayicon,' +
    'uninstalldisplayname,uninstalldisplaysize,uninstallable,' +
    'uninstallfilesdir,uninstalliconname,uninstalllogmode,' +
    'uninstallrestartcomputer,updateuninstalllogappname,useapppaths,' +
    'usepreviousappdir,usepreviousgroup,usepreviouslanguage,' +
    'usepreviousprivileges,useprevioussetuptype,useprevioustasks,' +
    'useprevioususerinfo,usesetupldr,valuedata,valuename,valuetype,' +
    'versioninfocompany,versioninfocopyright,versioninfodescription,' +
    'versioninfooriginalfilename,versioninfoproductname,' +
    'versioninfoproducttextversion,versioninfoproductversion,' +
    'versioninfotextversion,versioninfoversion,windowresizable,' +
    'windowshowcaption,windowstartmaximized,windowvisible,' +
    'wizardimagealphaformat,wizardimagefile,wizardimagestretch,' +
    'wizardresizable,wizardsizepercent,wizardsmallimagefile,wizardstyle,' +
    'workingdir';

  // Flag and enumeration values used as parameters.
  Parameters: string =
    'hkcc,hkcr,hkcu,hklm,hku,hka,hkcc32,hkcr32,hkcu32,hklm32,hku32,hkcc64,' +
    'hkcr64,hkcu64,hklm64,hku64,' +
    'admin,arm64,auto,binary,bzip,classic,commandline,dialog,dword,expandsz,' +
    'force,ia64,lowest,lzma,lzma2,modern,multisz,none,poweruser,qword,string,' +
    'x64,x64compatible,x64os,x86,x86compatible,x86os,yes,no,zip,' +
    '32bit,64bit,allowunsafefiles,append,checkablealone,checkedonce,' +
    'closeonexit,comparetimestamp,comparetimestampalso,confirmoverwrite,' +
    'createkeyifdoesntexist,createonlyiffileexists,createvalueifdoesntexist,' +
    'deleteafterinstall,deletekey,deletevalue,dirifempty,dontcloseonexit,' +
    'dontcreatekey,dontinheritcheck,dontverifychecksum,' +
    'excludefromshowinnewinstall,exclusive,external,files,filesandordirs,fixed,' +
    'fontisnttruetype,foldershortcut,gacinstall,hidewizard,ignoreversion,' +
    'iscustom,isreadme,noerror,nowait,onlyifdestfileexists,onlyifdoesntexist,' +
    'overwrite,overwritereadonly,postinstall,preservestringtype,preventpinning,' +
    'promptifolder,recursesubdirs,regserver,regtypelib,replacesameversion,' +
    'restart,restartreplace,runascurrentuser,runasoriginaluser,runhidden,' +
    'runmaximized,runminimized,setntfscompression,sharedfile,shellexec,sign,' +
    'signonce,silent,skipifdoesntexist,skipifnotsilent,skipifsilent,' +
    'skipifsourcedoesntexist,solidbreak,sortfilesbyextension,sortfilesbyname,' +
    'touch,unchecked,uninsalwaysuninstall,uninsclearvalue,uninsdeleteentry,' +
    'uninsdeletekey,uninsdeletekeyifempty,uninsdeletesection,' +
    'uninsdeletesectionifempty,uninsdeletevalue,uninsneveruninstall,' +
    'uninsremovereadonly,uninsrestartdelete,unsetntfscompression,useapppaths,' +
    'verysilent,waituntilidle,waituntilterminated';

  KeyOrParameter: string = 'string';

  // Pascal Script keywords for the [Code] section.
  CodeKeywords: string =
    'and,array,as,asm,begin,case,class,const,constructor,destructor,div,do,' +
    'downto,else,end,except,exit,export,exports,external,false,finalization,' +
    'finally,for,forward,function,goto,if,implementation,in,inherited,' +
    'initialization,inline,interface,is,label,library,mod,nil,not,object,of,' +
    'on,or,out,overload,override,packed,private,procedure,program,property,' +
    'protected,public,published,raise,record,repeat,resourcestring,set,shl,shr,' +
    'string,then,threadvar,to,try,type,unit,until,uses,var,virtual,while,with,' +
    'xor,true,break,continue,result,self,' +
    // common types
    'boolean,byte,char,ansichar,widechar,word,integer,cardinal,longint,' +
    'longword,smallint,shortint,int64,uint64,single,double,extended,real,' +
    'currency,ansistring,widestring,unicodestring,pchar,pansichar,variant,' +
    'olevariant,pointer,tobject,tstream,tstrings,tstringlist,tarrayofstring,' +
    'tarrayofinteger,tarrayofchar,tsetupstep,tuninstallstep,' +
    'tsetupprocessorarchitecture';

  // A selection of Inno Setup [Code] support functions and the common event
  // function names.
  CodeFunctions: string =
    'abort,abs,addbackslash,addperiod,addquotes,activelanguage,ansilowercase,' +
    'ansiuppercase,assigned,beep,changefileext,charlength,chr,comparestr,' +
    'comparetext,convertpercentstr,copy,copyfile,createcustomform,' +
    'createcustompage,createdir,createinputdirpage,createinputfilepage,' +
    'createinputoptionpage,createinputquerypage,createoleobject,' +
    'createoutputmsgmemopage,createoutputmsgpage,createoutputprogresspage,' +
    'custommessage,delayms,deletefile,deleteinientry,deleteinisection,deltree,' +
    'direxists,enablefsredirection,exec,execandcapturetext,execandlogoutput,' +
    'execasoriginaluser,exitsetupmsgbox,exp,expandconstant,expandconstantex,' +
    'expandfilename,extractfiledir,extractfiledrive,extractfileext,' +
    'extractfilename,extractfilepath,extractrelativepath,extracttemporaryfile,' +
    'extracttemporaryfiles,fileclose,filecopy,filecreate,fileexists,fileopen,' +
    'fileread,filesearch,filesetattr,filesize,filesize64,fmtmessage,' +
    'floattostr,forcedirectories,format,getarraylength,getcmdtail,' +
    'getcomputernamestring,getcurrentdir,getdatetimestring,getenv,' +
    'getexceptionmessage,getinibool,getiniint,getinistring,getmd5offile,' +
    'getmd5ofstring,getsha1offile,getsha1ofstring,getsha256offile,' +
    'getsha256ofstring,getshortname,getspaceondisk,getspaceondisk64,' +
    'getsysnativedir,getsystemdir,getsyswow64dir,gettempdir,getuilanguage,' +
    'getusernamestring,getversionnumbers,getversionnumbersstring,getwindir,' +
    'getwindowsversion,getwindowsversionex,getwindowsversionstring,' +
    'idispatchinvoke,inc,inisectionexists,inivaluexists,inputbox,instr,' +
    'inttostr,isadmin,isadmininstallmode,isarm64,iswin64,isx64,' +
    'isx64compatible,isx64os,isx86,length,loadstringfromfile,' +
    'loadstringsfromfile,log,lowercase,maxint,minimizepathname,msgbox,now,ord,' +
    'paramcount,paramstr,pos,random,randomize,registerpreviousdata,' +
    'regdeletekeyincludingsubkeys,regdeletekeyifempty,regdeletevalue,' +
    'regkeyexists,regquerybinaryvalue,regquerydwordvalue,' +
    'regquerymultistringvalue,regquerystringvalue,regvalueexists,' +
    'regwritebinaryvalue,regwritedwordvalue,regwriteexpandstringvalue,' +
    'regwritemultistringvalue,regwritestringvalue,removebackslash,' +
    'removebackslashunlessroot,removequotes,renamefile,round,samestr,sametext,' +
    'savestringtofile,savestringstofile,scalex,scaley,setarraylength,' +
    'setcurrentdir,setinibool,setiniint,setinistring,setlength,setpreviousdata,' +
    'shellexec,shellexecasoriginaluser,sleep,sqrt,stringchange,stringchangeex,' +
    'strtofloat,strtoint,strtoint64,strtoint64def,strtointdef,' +
    'suppressiblemsgbox,terminated,trim,trimleft,trimright,trunc,uppercase,' +
    'varisempty,varisnull,wizarddirvalue,wizardform,wizardgroupvalue,' +
    'wizardiscomponentselected,wizardistaskselected,wizardnoicons,' +
    'wizardselectedcomponents,wizardselectedtasks,wizardsetuptype,wizardsilent,' +
    // common event functions
    'initializesetup,deinitializesetup,initializewizard,curstepchanged,' +
    'curuninstallstepchanged,nextbuttonclick,backbuttonclick,cancelbuttonclick,' +
    'shouldskippage,curpagechanged,checkpassword,needrestart,updatereadymemo,' +
    'getcustomsetupexitcode,preparetoinstall,registerextrafiles,' +
    'getpreviousdata,checkserial,initializeuninstall,uninstallneedrestart,' +
    'initializeuninstallprogressform';

function TSynInnoSyn.InCodeSection: Boolean;
begin
  Result := fRange <> rsUnknown;
end;

function TSynInnoSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  S: string;
begin
  fToIdent := MayBe;
  while IsIdentChar(MayBe^) do
    Inc(Maybe);
  fStringLen := Maybe - fToIdent;
  SetString(S, fToIdent, fStringLen);
  if InCodeSection then
  begin
    if not FCodeKeywords.TryGetValue(S, Result) then
      Result := tkIdentifier;
  end
  else
  begin
    if not FKeywords.TryGetValue(S, Result) then
      Result := tkIdentifier;
  end;
end;

function TSynInnoSyn.IsCurrentToken(const Token: string): Boolean;
var
  I: NativeInt;
  Temp: PWideChar;
begin
  Temp := fToIdent;
  if Length(Token) = fStringLen then
  begin
    Result := True;
    for i := 1 to fStringLen do
    begin
      if AnsiLowerCase(Temp^)[1] <> AnsiLowerCase(Token[i])[1] then
      begin
        Result := False;
        Break;
      end;
      Inc(Temp);
    end;
  end
  else
    Result := False;
end;

constructor TSynInnoSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCaseSensitive := False;

  FKeywords := TDictionary<string, TtkTokenKind>.Create(TIStringComparer.Ordinal);
  FCodeKeywords := TDictionary<string, TtkTokenKind>.Create(TIStringComparer.Ordinal);

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := clGreen;
  AddAttribute(fCommentAttri);

  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fInvalidAttri := TSynHighlighterAttributes.Create(SYNS_AttrIllegalChar, SYNS_FriendlyAttrIllegalChar);
  AddAttribute(fInvalidAttri);

  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style := [fsBold];
  fKeyAttri.Foreground := clNavy;
  AddAttribute(fKeyAttri);

  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  fNumberAttri.Foreground := clMaroon;
  AddAttribute(fNumberAttri);

  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  fStringAttri.Foreground := clBlue;
  AddAttribute(fStringAttri);

  fConstantAttri := TSynHighlighterAttributes.Create(SYNS_AttrDirective, SYNS_FriendlyAttrDirective);
  fConstantAttri.Style := [fsBold, fsItalic];
  fConstantAttri.Foreground := clTeal;
  AddAttribute(fConstantAttri);

  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(fSymbolAttri);

  // Parameters / flag values
  fParamAttri := TSynHighlighterAttributes.Create(SYNS_AttrPreprocessor, SYNS_FriendlyAttrPreprocessor);
  fParamAttri.Style := [fsBold];
  fParamAttri.Foreground := clOlive;
  AddAttribute(fParamAttri);

  // [Code] support functions
  fFunctionAttri := TSynHighlighterAttributes.Create(SYNS_AttrFunction, SYNS_FriendlyAttrFunction);
  fFunctionAttri.Foreground := clTeal;
  AddAttribute(fFunctionAttri);

  // ISPP preprocessor directives (#define, #include, ...)
  fPreprocessorAttri := TSynHighlighterAttributes.Create(SYNS_AttrPragma, SYNS_FriendlyAttrPragma);
  fPreprocessorAttri.Style := [fsBold];
  fPreprocessorAttri.Foreground := clPurple;
  AddAttribute(fPreprocessorAttri);

  fSectionAttri := TSynHighlighterAttributes.Create(SYNS_AttrSection, SYNS_FriendlyAttrSection);
  fSectionAttri.Style := [fsBold];
  fSectionAttri.Foreground := clRed;
  AddAttribute(fSectionAttri);

  SetAttributesOnChange(DefHighlightChange);
  EnumerateKeywords(Ord(tkKey), Keywords, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkParameter), Parameters, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkKeyOrParameter), KeyOrParameter, IsIdentChar,
    DoAddKeyword);
  EnumerateKeywords(Ord(tkKey), CodeKeywords, IsIdentChar, DoAddCodeKeyword);
  EnumerateKeywords(Ord(tkFunction), CodeFunctions, IsIdentChar,
    DoAddCodeKeyword);
  fDefaultFilter := SYNS_FilterInno;
  fRange := rsUnknown;
end;

destructor TSynInnoSyn.Destroy;
begin
  fKeywords.Free;
  fCodeKeywords.Free;
  inherited Destroy;
end;

procedure TSynInnoSyn.DoAddKeyword(AKeyword: string; AKind: TSynNativeInt);
begin
  if not FKeywords.ContainsKey(AKeyword) then
    FKeywords.Add(AKeyword, TtkTokenKind(AKind));
end;

procedure TSynInnoSyn.DoAddCodeKeyword(AKeyword: string; AKind: TSynNativeInt);
begin
  if not FCodeKeywords.ContainsKey(AKeyword) then
    FCodeKeywords.Add(AKeyword, TtkTokenKind(AKind));
end;

{ Shared procedures }

procedure TSynInnoSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then Inc(Run);
end;

procedure TSynInnoSyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynInnoSyn.NullProc;
begin
  fTokenID := tkNull;
  Inc(Run);
end;

procedure TSynInnoSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    Inc(Run);
  until (fLine[Run] > #32) or IsLineEnd(Run);
end;

procedure TSynInnoSyn.NumberProc;
begin
  fTokenID := tkNumber;
  while CharInSet(fLine[Run], ['0'..'9']) do
    Inc(Run);
  if (fLine[Run] = '.') and CharInSet(fLine[Run + 1], ['0'..'9']) then
  begin
    Inc(Run);
    while CharInSet(fLine[Run], ['0'..'9']) do
      Inc(Run);
  end;
  if CharInSet(fLine[Run], ['e', 'E']) then
  begin
    Inc(Run);
    if CharInSet(fLine[Run], ['+', '-']) then
      Inc(Run);
    while CharInSet(fLine[Run], ['0'..'9']) do
      Inc(Run);
  end;
end;

procedure TSynInnoSyn.PreprocessorProc;
begin
  if CharInSet(fLine[Run + 1], ['0'..'9', '$']) then
  begin
    // Pascal character-code literal: #13, #$0D
    fTokenID := tkString;
    Inc(Run);
    if fLine[Run] = '$' then
    begin
      Inc(Run);
      while CharInSet(fLine[Run], ['0'..'9', 'a'..'f', 'A'..'F']) do
        Inc(Run);
    end
    else
      while CharInSet(fLine[Run], ['0'..'9']) do
        Inc(Run);
  end
  else
  begin
    // ISPP preprocessor directive: #define, #include, #if, #emit, #sub, ...
    fTokenID := tkPreprocessor;
    Inc(Run);
    while IsIdentChar(fLine[Run]) do
      Inc(Run);
  end;
end;

procedure TSynInnoSyn.SectionProc;
var
  Name: string;
begin
  // A section header is only valid at column 0; elsewhere '[' is a symbol
  // (e.g. an array subscript in [Code]).
  if Run > 0 then
  begin
    fTokenID := tkSymbol;
    Inc(Run);
    Exit;
  end;

  fTokenID := tkSection;
  repeat
    Inc(Run);
  until CharInSet(fLine[Run], [']', #0, #10, #13]);
  SetString(Name, fLine + 1, Run - 1);
  if fLine[Run] = ']' then
    Inc(Run);
  if SameText(Name, 'Code') then
    fRange := rsCode
  else
    fRange := rsUnknown;
end;

procedure TSynInnoSyn.UnknownProc;
begin
  Inc(Run);
  fTokenID := tkUnknown;
end;

{ Inno directive mode }

procedure TSynInnoSyn.SymbolProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
end;

procedure TSynInnoSyn.EqualProc;
begin
  // In [Setup] a 'Key=Value' value runs to the end of the line (or a ';'
  // comment). It is shown as a string even without quotes.
  fTokenID := tkString;
  repeat
    Inc(Run);
    if fLine[Run] = ';' then
    begin
      Inc(Run);
      Break;
    end;
  until IsLineEnd(Run);
end;

procedure TSynInnoSyn.IdentProc;
var
  LookAhead: TSynNativeInt;
begin
  fTokenID := IdentKind((fLine + Run));
  Inc(Run, fStringLen);
  if fTokenID = tkKeyOrParameter then
  begin
    LookAhead := Run;
    while CharInSet(fLine[LookAhead], [#9, ' ']) do
      Inc(LookAhead);
    if fLine[LookAhead] = ':' then
      fTokenID := tkKey
    else
      fTokenID := tkParameter;
  end;
end;

procedure TSynInnoSyn.ConstantProc;
var
  BraceLevel, LastOpenBrace: TSynNativeInt;
begin
  { Based on the SkipPastConst function from IS's CmnFunc2 unit. }
  if fLine[Run + 1] = '{' then
  begin
    { '{{' is not a constant }
    fTokenID := tkUnknown;
    Inc(Run, 2);
    Exit;
  end;
  fTokenID := tkConstant;
  BraceLevel := 1;
  LastOpenBrace := Low(TSynNativeInt);
  repeat
    Inc(Run);
    case fLine[Run] of
      '{':
        begin
          if LastOpenBrace <> Run - 1 then
          begin
            Inc(BraceLevel);
            LastOpenBrace := Run;
          end
          else
            Dec(BraceLevel);
        end;
      '}':
        begin
          Dec(BraceLevel);
          if BraceLevel = 0 then
          begin
            Inc(Run);
            Break;
          end;
        end;
    end;
  until IsLineEnd(Run);
end;

procedure TSynInnoSyn.SemiColonProc;
var
  I: TSynNativeInt;
begin
  for I := Run - 1 downto 0 do
    if fLine[I] > ' ' then
    begin
      // Not the first non-whitespace character: not a comment.
      fTokenID := tkUnknown;
      Inc(Run);
      Exit;
    end;
  fTokenID := tkComment;
  repeat
    Inc(Run);
  until IsLineEnd(Run);
end;

procedure TSynInnoSyn.StringProc;
begin
  fTokenID := tkString;
  repeat
    Inc(Run);
    if fLine[Run] = '"' then
    begin
      Inc(Run);
      if fLine[Run] <> '"' then
        Break;
    end;
  until IsLineEnd(Run);
end;

procedure TSynInnoSyn.NextInno;
begin
  case fLine[Run] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    'A'..'Z', 'a'..'z', '_': IdentProc;
    '0'..'9': NumberProc;
    ';': SemiColonProc;
    '=': EqualProc;
    '"': StringProc;
    '#': PreprocessorProc;
    '{': ConstantProc;
    '[': SectionProc;
    ':', ',', '(', ')': SymbolProc;
  else
    UnknownProc;
  end;
end;

{ Pascal [Code] mode }

procedure TSynInnoSyn.CodeIdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  Inc(Run, fStringLen);
end;

procedure TSynInnoSyn.CodeSlashProc;
begin
  if fLine[Run + 1] = '/' then
  begin
    fTokenID := tkComment;
    Inc(Run, 2);
    while not IsLineEnd(Run) do
      Inc(Run);
  end
  else
    CodeSymbolProc;
end;

procedure TSynInnoSyn.BraceCommentProc;
begin
  if (Run = fTokenPos) and CharInSet(fLine[Run], [#0, #10, #13]) then
  begin
    case fLine[Run] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
    end;
    Exit;
  end;

  fTokenID := tkComment;
  while not IsLineEnd(Run) do
  begin
    if fLine[Run] = '}' then
    begin
      Inc(Run);
      fRange := rsCode;
      Exit;
    end;
    Inc(Run);
  end;
end;

procedure TSynInnoSyn.ParenCommentProc;
begin
  if (Run = fTokenPos) and CharInSet(fLine[Run], [#0, #10, #13]) then
  begin
    case fLine[Run] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
    end;
    Exit;
  end;

  fTokenID := tkComment;
  while not IsLineEnd(Run) do
  begin
    if (fLine[Run] = '*') and (fLine[Run + 1] = ')') then
    begin
      Inc(Run, 2);
      fRange := rsCode;
      Exit;
    end;
    Inc(Run);
  end;
end;

procedure TSynInnoSyn.CodeBraceProc;
begin
  // Pascal '{ ... }' comment (multi-line).
  fRange := rsCodeBraceComment;
  Inc(Run);
  BraceCommentProc;
end;

procedure TSynInnoSyn.CodeParenProc;
begin
  if fLine[Run + 1] = '*' then
  begin
    // Pascal '(* ... *)' comment (multi-line).
    fRange := rsCodeParenComment;
    Inc(Run, 2);
    ParenCommentProc;
  end
  else
    CodeSymbolProc;
end;

procedure TSynInnoSyn.CodeStringProc;
begin
  // Pascal single-quoted string; a doubled '' is an escaped quote.
  fTokenID := tkString;
  repeat
    Inc(Run);
    if fLine[Run] = '''' then
    begin
      Inc(Run);
      if fLine[Run] <> '''' then
        Break;
    end;
  until IsLineEnd(Run);
end;

procedure TSynInnoSyn.CodeHexProc;
begin
  fTokenID := tkNumber;
  Inc(Run);
  while CharInSet(fLine[Run], ['0'..'9', 'a'..'f', 'A'..'F']) do
    Inc(Run);
end;

procedure TSynInnoSyn.CodeSymbolProc;
begin
  fTokenID := tkSymbol;
  case fLine[Run] of
    ':': if fLine[Run + 1] = '=' then Inc(Run);
    '<': if CharInSet(fLine[Run + 1], ['=', '>']) then Inc(Run);
    '>': if fLine[Run + 1] = '=' then Inc(Run);
    '.': if fLine[Run + 1] = '.' then Inc(Run);
  end;
  Inc(Run);
end;

procedure TSynInnoSyn.NextCode;
begin
  case fLine[Run] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    #1..#9, #11, #12, #14..#32: SpaceProc;
    'A'..'Z', 'a'..'z', '_': CodeIdentProc;
    '0'..'9': NumberProc;
    '''': CodeStringProc;
    '/': CodeSlashProc;
    '{': CodeBraceProc;
    '(': CodeParenProc;
    '#': PreprocessorProc;
    '$': CodeHexProc;
    '[': SectionProc;
    ':', '=', '<', '>', '.', '+', '-', '*', ';', ',', ')', ']', '@', '^':
      CodeSymbolProc;
  else
    UnknownProc;
  end;
end;

procedure TSynInnoSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsCodeBraceComment: BraceCommentProc;
    rsCodeParenComment: ParenCommentProc;
    rsCode: NextCode;
  else
    NextInno;
  end;
  inherited;
end;

function TSynInnoSyn.GetDefaultAttribute(Index: Integer):
  TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynInnoSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynInnoSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynInnoSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkConstant: Result := fConstantAttri;
    tkFunction: Result := fFunctionAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkKeyOrParameter: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkParameter: Result := fParamAttri;
    tkPreprocessor: Result := fPreprocessorAttri;
    tkSection: Result := fSectionAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynInnoSyn.GetTokenKind: TSynNativeInt;
begin
  Result := Ord(fTokenId);
end;

function TSynInnoSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynInnoSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterInno;
end;

class function TSynInnoSyn.GetLanguageName: string;
begin
  Result := SYNS_LangInno;
end;

procedure TSynInnoSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynInnoSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynInnoSyn.GetSampleSource: string;
begin
  Result :=
    '; -- Example.iss --'#13#10 +
    '#define MyAppName "My Program"'#13#10 +
    '#define MyAppVersion "1.5"'#13#10 +
    ''#13#10 +
    '[Setup]'#13#10 +
    'AppName={#MyAppName}'#13#10 +
    'AppVersion={#MyAppVersion}'#13#10 +
    'WizardStyle=modern'#13#10 +
    'DefaultDirName={autopf}\{#MyAppName}'#13#10 +
    'ArchitecturesInstallIn64BitMode=x64compatible'#13#10 +
    'Compression=lzma2'#13#10 +
    'SolidCompression=yes'#13#10 +
    ''#13#10 +
    '[Files]'#13#10 +
    'Source: "MyProg.exe"; DestDir: "{app}"; Flags: ignoreversion'#13#10 +
    ''#13#10 +
    '[Code]'#13#10 +
    'function InitializeSetup(): Boolean;'#13#10 +
    'var'#13#10 +
    '  Confirmed: Boolean;'#13#10 +
    'begin'#13#10 +
    '  // ask the user before continuing'#13#10 +
    '  Confirmed := MsgBox(''Continue the install?'', mbConfirmation, MB_YESNO) = IDYES;'#13#10 +
    '  { a Pascal block comment }'#13#10 +
    '  Result := Confirmed;'#13#10 +
    'end;';
end;

class function TSynInnoSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangInno;
end;

initialization
  RegisterPlaceableHighlighter(TSynInnoSyn);
end.

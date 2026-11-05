{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Description: x86-64 Assembly Syntax Highlighter (x64 registers, AVX,
separate attribute classes for registers, jumps, conditionals and labels).
The initial author of this file is TetzkatLipHoka.
Copyright (c) 2026, all rights reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

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
unit SynHighlighterAsmX86_64;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Defaults,
  System.Generics.Collections,
  Vcl.Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynUnicode,
  SynFunc;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace,
                  tkString, tkSymbol, tkBracket, tkMath,
                  tkUnknown, tkRegister, tkFloatRegister, tkTypeSize, tkJump,
                  tkConditional, tkAVX, tkLabelDeclaration, tkLabel, tkChar, tkFloat, tkHex, tkOctal);

type
  TSynAsmX86_64 = class(TSynCustomHighlighter)
  private
    FTokenID: TtkTokenKind;
    FCommentAttri               : TSynHighlighterAttributes;
    FIdentifierAttri            : TSynHighlighterAttributes;
    FNumberAttri                : TSynHighlighterAttributes;
    FSpaceAttri                 : TSynHighlighterAttributes;
    FStringAttri                : TSynHighlighterAttributes;
    FSymbolAttri                : TSynHighlighterAttributes;
    FBracketAttri               : TSynHighlighterAttributes;
    FMathAttri                  : TSynHighlighterAttributes;

    FFloatAttri                 : TSynHighlighterAttributes;
    FHexAttri                   : TSynHighlighterAttributes;
    FOctalAttri                 : TSynHighlighterAttributes;
    FCharAttri                  : TSynHighlighterAttributes;

    // One dictionary for all seven keyword classes: each entry carries its own
    // token kind, and DoAddKeyword keeps the first insertion, so a keyword that
    // appears in two lists resolves to the earlier one - the same precedence the
    // previous seven separate lists gave by search order.
    FKeywords                   : TDictionary<string, TtkTokenKind>;
    FKeyAttri                   : TSynHighlighterAttributes;

    FRegisterAttributes         : TSynHighlighterAttributes;
    FFloatRegisterAttributes    : TSynHighlighterAttributes;
    FTypeSizeAttributes         : TSynHighlighterAttributes;
    FJumpAttributes             : TSynHighlighterAttributes;
    FConditionalAttributes      : TSynHighlighterAttributes;
    FAVXAttributes              : TSynHighlighterAttributes;

    FLabelAttributes            : TSynHighlighterAttributes;
    FLabelDeclarationAttributes : TSynHighlighterAttributes;

    procedure NumberProc;
    procedure DoAddKeyword(AKeyword: string; AKind: TSynNativeInt);
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    function IsLabel(MayBe: PWideChar): Boolean;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;    
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: TSynNativeInt; override;
    procedure Next; override;
  published
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri write FCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;

    property RegisterAttri: TSynHighlighterAttributes read FRegisterAttributes write FRegisterAttributes;
    property FloatRegisterAttri: TSynHighlighterAttributes read FFloatRegisterAttributes write FFloatRegisterAttributes;
    property TypeSizeAttri: TSynHighlighterAttributes read FTypeSizeAttributes write FTypeSizeAttributes;
    property JumpAttri: TSynHighlighterAttributes read FJumpAttributes write FJumpAttributes;
    property ConditionalAttri: TSynHighlighterAttributes read FConditionalAttributes write FConditionalAttributes;
    property AVXAttri: TSynHighlighterAttributes read FAVXAttributes write FAVXAttributes;
    property LabelDeclarationAttri: TSynHighlighterAttributes read FLabelDeclarationAttributes write FLabelDeclarationAttributes;
    property LabelAttri: TSynHighlighterAttributes read FLabelAttributes write FLabelAttributes;

    property NumberAttri: TSynHighlighterAttributes read FNumberAttri write FNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri write FStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri write FSymbolAttri;
    property MathAttri: TSynHighlighterAttributes read FMathAttri write FMathAttri;
    property BracketAttri: TSynHighlighterAttributes read FBracketAttri write FBracketAttri;
    property FloatAttri: TSynHighlighterAttributes read FFloatAttri write FFloatAttri;
    property HexAttri: TSynHighlighterAttributes read FHexAttri write FHexAttri;
    property OctalAttri: TSynHighlighterAttributes read FOctalAttri write FOctalAttri;
    property CharAttri: TSynHighlighterAttributes read FCharAttri write FCharAttri;
  end;

implementation

uses
  SynEdit, SynEditMiscProcs, SynEditStrConst;

const
  Registers: UnicodeString =
    'rax,rbx,rcx,rdx,rsi,rdi,rbp,rsp,r8,r9,r10,r11,r12,r13,r14,r15,' +
    'eax,ebx,ecx,edx,esi,edi,ebp,esp,r8d,r9d,r10d,r11d,r12d,r13d,r14d,r15d,' +
    'ax,bx,cx,dx,ah,bh,ch,dh,si,di,bp,sp,r8w,r9w,r10w,r11w,r12w,r13w,r14w,r15w,' +
    'al,bl,cl,dl,sil,dil,bpl,spl,r8b,r9b,r10b,r11b,r12b,r13b,r14b,r15b,' +
    'ip,eip,rip,' +
    'cs,ds,ss,es,fs,gs,' +
    'cr0,cr1,cr2,cr3,cr4,cr5,cr6,cr7,cr8,cr9,cr10,cr11,cr12,cr13,cr14,cr15,' +
    'dr0,dr1,dr2,dr3,dr6,dr7';

  FloatRegisters: UnicodeString =
    'xmm0,xmm1,xmm2,xmm3,xmm4,xmm5,xmm6,xmm7,xmm8,xmm9,xmm10,xmm11,xmm12,xmm13,xmm14,xmm15,' +
    'xmm16,xmm17,xmm18,xmm19,xmm20,xmm21,xmm22,xmm23,xmm24,xmm25,xmm26,xmm27,xmm28,xmm29,xmm30,xmm31' +
    'ymm0,ymm1,ymm2,ymm3,ymm4,ymm5,ymm6,ymm7,ymm8,ymm9,ymm10,ymm11,ymm12,ymm13,ymm14,ymm15,' +
    'ymm16,ymm17,ymm18,ymm19,ymm20,ymm21,ymm22,ymm23,ymm24,ymm25,ymm26,ymm27,ymm28,ymm29,ymm30,ymm31' +
    'zmm0,zmm1,zmm2,zmm3,zmm4,zmm5,zmm6,zmm7,zmm8,zmm9,zmm10,zmm11,zmm12,zmm13,zmm14,zmm15' +
    'zmm16,zmm17,zmm18,zmm19,zmm20,zmm21,zmm22,zmm23,zmm24,zmm25,zmm26,zmm27,zmm28,zmm29,zmm30,zmm31';

  TypeSizes: UnicodeString =
    'qword,dword,word,byte,ptr';

  Jumps: UnicodeString =
    'ja,jae,jb,jbe,jc,jcxz,je,jecxz,jg,jge,jl,jle,jmp,jna,jnae,jnb,jnbe,jnc,jne,jng,' +
    'jnge,jnl,jnle,jno,jnp,jns,jnz,jo,jp,jpe,jpo,js,jz';

  Conditionals: UnicodeString =
    'seta,setae,setb,setbe,setc,sete,setg,setge,setl,setle,setna,setnae,setnb,setnbe,setnc,' +
    'setne,setng,setnge,setnl,setnle,setno,setnp,setns,setnz,seto,setp,setpe,setpo,sets,setz,' +

    'cmova,cmovae,cmovaeq,cmovaq,cmovb,cmovbe,cmovbeq,cmovbq,cmovc,cmovcq,cmove,' +
    'cmoveq,cmovg,cmovge,cmovgeq,cmovgq,cmovl,cmovleq,cmovle,cmovlq,cmovna,cmovnae,' +
    'cmovnaeq,cmovnaq,cmovnb,cmovnbe,cmovnbeq,cmovnbq,cmovnc,cmovncq,cmovne,cmovneq,' +
    'cmovng,cmovnge,cmovngeq,cmovngq,cmovnl,cmovnle,cmovnleq,cmovnlq,cmovno,cmovnoq,' +
    'cmovnp,cmovnpq,cmovns,cmovnsq,cmovnz,cmovnzq,cmovo,cmovoq,cmovp,cmovpe,cmovpeq,' +
    'cmovpo,cmovpoq,cmovpq,cmovs,cmovsq,cmovz,cmovzq,fcmovb,fcmovbe,fcmove,fcmovnb,' +
    'fcmovnbe,fcmovne,fcmovnu,fcmovu';

  AVX: UnicodeString =
    'kand,kandn,kmov,kunpck,knot,kor,kortest,kshiftl,kshiftr,kxnor,kxor,kadd,ktest,vblendmpd,' +
    'vblendmps,vpblendmd,vpblendmq,vpblendmb,vpblendmw,vpcmpd,vpcmpud,vpcmpq,vpcmpuq,vpcmpb,' +
    'vpcmpub,vpcmpw,vpcmpuw,vptestmd,vptestmq,vptestnmd,vptestnmq,vptestmb,vptestmw,vptestnmb,' +
    'vptestnmw,vcompresspd,vcompressps,vpcompressd,vpcompressq,vexpandpd,vexpandps,vpexpandd,' +
    'vpexpandq,vpermb,vpermw,vpermt2b,vpermt2w,vpermi2pd,vpermi2ps,vpermi2d,vpermi2q,vpermi2b,' +
    'vpermi2w,vpermt2ps,vpermt2pd,vpermt2d,vpermt2q,vshuff32x4,vshuff64x2,vshufi32x4,vshufi64x2,' +
    'vpmultishiftqb,vpternlogd,vpternlogq,vpmovqd,vpmovsqd,vpmovusqd,vpmovqw,vpmovsqw,vpmovusqw,' +
    'vpmovqb,vpmovsqb,vpmovusqb,vpmovdw,vpmovsdw,vpmovusdw,vpmovdb,vpmovsdb,vpmovusdb,vpmovwb,' +
    'vpmovswb,vpmovuswb,vcvtps2udq,vcvtpd2udq,vcvttps2udq,vcvttpd2udq,vcvtss2usi,vcvtsd2usi,' +
    'vcvttss2usi,vcvttsd2usi,vcvtps2qq,vcvtpd2qq,vcvtps2uqq,vcvtpd2uqq,vcvttps2qq,vcvttpd2qq,' +
    'vcvttps2uqq,vcvttpd2uqq,vcvtudq2ps,vcvtudq2pd,vcvtusi2ps,vcvtusi2pd,vcvtusi2sd,vcvtusi2ss,' +
    'vcvtuqq2ps,vcvtuqq2pd,vcvtqq2pd,vcvtqq2ps,vgetexppd,vgetexpps,vgetexpsd,vgetexpss,vgetmantpd,' +
    'vgetmantps,vgetmantsd,vgetmantss,vfixupimmpd,vfixupimmps,vfixupimmsd,vfixupimmss,vrcp14pd,' +
    'vrcp14ps,vrcp14sd,vrcp14ss,vrndscaleps,vrndscalepd,vrndscaless,vrndscalesd,vrsqrt14pd,vrsqrt14ps,' +
    'vrsqrt14sd,vrsqrt14ss,vscalefps,vscalefpd,vscalefss,vscalefsd,vbroadcastss,vbroadcastsd,vpbroadcastb,' +
    'vpbroadcastw,vpbroadcastd,vpbroadcastq,vbroadcasti32x2,vbroadcasti64x2,vbroadcasti32x4,' +
    'vbroadcasti32x8,vbroadcasti64x4,valignd,valignq,vdbpsadbw,' +
    'vprold,vprolvd,vprolq,vprolvq,vprord,vprorvd,vprorq,vprorvq,vpscatterdd,vpscatterdq,vpscatterqd,' +
    'vpscatterqq,vscatterdps,vscatterdpd,vscatterqps,vscatterqpd,vpconflictd,vpconflictq,vplzcntd,vplzcntq,' +
    'vpbroadcastmb2q,vpbroadcastmw2d,vexp2pd,vexp2ps,vrcp28pd,vrcp28ps,vrcp28sd,vrcp28ss,vrsqrt28pd,' +
    'vrsqrt28ps,vrsqrt28sd,vrsqrt28ss,vgatherpf0dps,vgatherpf0qps,vgatherpf0dpd,vgatherpf0qpd,vgatherpf1dps,' +
    'vgatherpf1qps,vgatherpf1dpd,vgatherpf1qpd,vscatterpf0dps,vscatterpf0qps,vscatterpf0dpd,vscatterpf0qpd,' +
    'vscatterpf1dps,vscatterpf1qps,vscatterpf1dpd,vscatterpf1qpd,v4fmaddps,v4fmaddss,v4fnmaddps,' +
    'v4fnmaddss,vp4dpwssd,vp4dpwssds,vfpclassps,vfpclasspd,vfpclassss,vfpclasssd,vrangeps,vrangepd,' +
    'vrangess,vrangesd,vreduceps,vreducepd,vreducess,vreducesd,vpmovm2d,vpmovm2q,vpmovm2b,vpmovm2w,' +
    'vpmovd2m,vpmovq2m,vpmovb2m,vpmovw2m,vpcompressb,vpcompressw,vpexpandb,vpexpandw,vpshld,' +
    'vpshldv,vpshrd,vpshrdv,vpdpbusd,vpdpbusds,vpdpwssd,vpdpwssds,vpmadd52luq,vpmadd52huq,vpopcntd,' +
    'vpopcntq,vpopcntb,vpopcntw,vpshufbitqmb,vp2intersectd,vp2intersectq,vgf2p8affineinvqb,vgf2p8affineqb,' +
    'vgf2p8mulb,vpclmulqdq,vaesdec,vaesdeclast,vaesenc,vaesenclast,vcvtne2ps2bf16,vcvtneps2bf16,vdpbf16ps,' +
    'vaddph,vaddsh,vsubph,vsubsh,vmulph,vmulsh,vdivph,vdivsh,vsqrtph,vsqrtsh,vfmadd132ph,vfmadd213ph,' +
    'vfmadd231ph,vfmadd132sh,vfmadd213sh,vfmadd231sh,vfnmadd132ph,vfnmadd213ph,vfnmadd231ph,vfnmadd132sh,' +
    'vfnmadd213sh,vfnmadd231sh,vfmsub132ph,vfmsub213ph,vfmsub231ph,vfmsub132sh,vfmsub213sh,vfmsub231sh,' +
    'vfnmsub132ph,vfnmsub213ph,vfnmsub231ph,vfnmsub132sh,vfnmsub213sh,vfnmsub231sh,vfmaddsub132ph,vfmaddsub213ph,' +
    'vfmaddsub231ph,vfmsubadd132ph,vfmsubadd213ph,vfmsubadd231ph,vreduceph,vreducesh,vrndscaleph,vrndscalesh,' +
    'vscalefph,vscalefsh,vfmulcph,vfmulcsh,vfcmulcph,vfcmulcsh,vfmaddcph,vfmaddcsh,vfcmaddcph,' +
    'vfcmaddcsh,vrcpph,vrcpsh,vrsqrtph,vrsqrtsh,vcmpph,vcmpsh,vcomish,vucomish,vmaxph,vmaxsh,' +
    'vminph,vminsh,vfpclassph,vfpclasssh,vcvtw2ph,vcvtuw2ph,vcvtdq2ph,vcvtudq2ph,vcvtqq2ph,vcvtuqq2ph,' +
    'vcvtps2phx,vcvtpd2ph,vcvtsi2sh,vcvtusi2sh,vcvtss2sh,vcvtsd2sh,vcvtph2w,vcvttph2w,vcvtph2uw,vcvttph2uw,' +
    'vcvtph2dq,vcvttph2dq,vcvtph2udq,vcvttph2udq,vcvtph2qq,vcvttph2qq,vcvtph2uqq,vcvttph2uqq,vcvtph2psx,' +
    'vcvtph2pd,vcvtsh2si,vcvttsh2si,vcvtsh2usi,vcvttsh2usi,vcvtsh2ss,vcvtsh2sd,vgetexpph,vgetexpsh,vgetmantph,' +
    'vgetmantsh,vmovsh,vmovw'+

    'vaddpd,vaddps,vaddsd,vaddss,vandpd,vandps,vandnpd,vandnps,vcmppd,vcmpps,vcmpsd,' +
    'vcmpss,vcomisd,vcomiss,vdivpd,vdivps,vdivsd,vdivss,vcvtdq2pd,vcvtdq2ps,vcvtpd2dq,' +
    'vcvtpd2ps,vcvtph2ps,vcvtps2ph,vcvtps2dq,vcvtps2pd,vcvtsd2si,vcvtsd2ss,vcvtsi2sd,' +
    'vcvtsi2ss,vcvtss2sd,vcvtss2si,vcvttpd2dq,vcvttps2dq,vcvttsd2si,vcvttss2si,vmaxpd,' +
    'vmaxps,vmaxsd,vmaxss,vminpd,vminps,vminsd,vminss,vmovapd,vmovaps,vmovd,vmovq,' +
    'vmovddup,vmovhlps,vmovhpd,vmovhps,vmovlhps,vmovlpd,vmovlps,vmovntdqa,vmovntdq,' +
    'vmovntpd,vmovntps,vmovsd,vmovshdup,vmovsldup,vmovss,vmovupd,vmovups,vmovdqa32,' +
    'vmovdqa64,vmovdqu8,vmovdqu16,vmovdqu32,vmovdqu64,vmulpd,vmulps,vmulsd,vmulss,' +
    'vorpd,vorps,vsqrtpd,vsqrtps,vsqrtsd,vsqrtss,vsubpd,vsubps,vsubsd,vsubss,' +
    'vucomisd,vucomiss,vunpckhpd,vunpckhps,vunpcklpd,vunpcklps,vxorpd,vxorps,' +
    'vextractps,vinsertps,vpextrb,vpextrw,vpextrd,vpextrq,vpinsrb,vpinsrw,vpinsrd,' +
    'vpinsrq,vpacksswb,vpackssdw,vpackusdw,vpackuswb,vpaddb,vpaddw,vpaddd,vpaddq,' +
    'vpaddsb,vpaddsw,vpaddusb,vpaddusw,vpandd,vpandq,vpandnd,vpandnq,vpavgb,vpavgw,' +
    'vpcmpeqb,vpcmpeqw,vpcmpeqd,vpcmpeqq,vpcmpgtb,vpcmpgtw,vpcmpgtd,vpcmpgtq,' +
    'vpmaxsb,vpmaxsw,vpmaxsd,vpmaxsq,vpmaxub,vpmaxuw,vpmaxud,vpmaxuq,vpminsb,' +
    'vpminsw,vpminsd,vpminsq,vpminub,vpminuw,vpminud,vpminuq,vpmovsxbw,vpmovsxbd,' +
    'vpmovsxbq,vpmovsxwd,vpmovsxwq,vpmovsxdq,vpmovzxbw,vpmovzxbd,vpmovzxbq,vpmovzxwd,' +
    'vpmovzxwq,vpmovzxdq,vpmuldq,vpmuludq,vpmulhrsw,vpmulhuw,vpmulhw,vpmulld,vpmullq,' +
    'vpmullw,vpord,vporq,vpsubb,vpsubw,vpsubd,vpsubq,vpsubsb,vpsubsw,vpsubusb,vpsubusw,' +
    'vpunpckhbw,vpunpckhwd,vpunpckhdq,vpunpckhqdq,vpunpcklbw,vpunpcklwd,vpunpckldq,vpunpcklqdq,' +
    'vpxord,vpxorq,vpsadbw,vpshufb,vpshufhw,vpshuflw,vpshufd,vpslldq,vpsllw,vpslld,vpsllq,' +
    'vpsraw,vpsrad,vpsraq,vpsrldq,vpsrlw,vpsrld,vpsrlq,vpsllvw,vpsllvd,vpsllvq,vpsrlvw,vpsrlvd,' +
    'vpsrlvq,vpshufpd,vpshufps,vextractf32x4,vextractf64x2,vextractf32x8,vextractf64x4,' +
    'vextracti32x4,vextracti64x2,vextracti32x8,vextracti64x4,vinsertf32x4,vinsertf64x2,' +
    'vinsertf32x8,vinsertf64x4,vinserti32x4,vinserti64x2,vinserti32x8,vinserti64x4,vpabsb,' +
    'vpabsw,vpabsd,vpabsq,vpalignr,vpermd,vpermilpd,vpermilps,vpermpd,vpermps,vpermq,' +
    'vpmaddubsw,vpmaddwd,vfmadd132pd,vfmadd213pd,vfmadd231pd,vfmadd132ps,vfmadd213ps,' +
    'vfmadd231ps,vfmadd132sd,vfmadd213sd,vfmadd231sd,vfmadd132ss,vfmadd213ss,vfmadd231ss,' +
    'vfmaddsub132pd,vfmaddsub213pd,vfmaddsub231pd,vfmaddsub132ps,vfmaddsub213ps,vfmaddsub231ps,' +
    'vfmsubadd132pd,vfmsubadd213pd,vfmsubadd231pd,vfmsubadd132ps,vfmsubadd213ps,vfmsubadd231ps,' +
    'vfmsub132pd,vfmsub213pd,vfmsub231pd,vfmsub132ps,vfmsub213ps,vfmsub231ps,vfmsub132sd,' +
    'vfmsub213sd,vfmsub231sd,vfmsub132ss,vfmsub213ss,vfmsub231ss,vfnmadd132pd,vfnmadd213pd,' +
    'vfnmadd231pd,vfnmadd132ps,vfnmadd213ps,vfnmadd231ps,vfnmadd132sd,vfnmadd213sd,' +
    'vfnmadd231sd,vfnmadd132ss,vfnmadd213ss,vfnmadd231ss,vfnmsub132pd,vfnmsub213pd,' +
    'vfnmsub231pd,vfnmsub132ps,vfnmsub213ps,vfnmsub231ps,vfnmsub132sd,vfnmsub213sd,' +
    'vfnmsub231sd,vfnmsub132ss,vfnmsub213ss,vfnmsub231ss,vgatherdps,vgatherdpd,vgatherqps,' +
    'vgatherqpd,vpgatherdd,vpgatherdq,vpgatherqd,vpgatherqq,vpsravw,vpsravd,vpsravq' +

    'vaddsubpd,vaddsubps,vblendpd,vblendps,vblendvpd,vblendvps,vcmpeq_ospd,vcmpeq_uqpd,' +
    'vcmpeq_uspd,vcmpeqpd,vcmpfalse_ospd,vcmpfalsepd,vcmpge_oqpd,vcmpgepd,vcmpgt_oqpd,' +
    'vcmpgtpd,vcmple_oqpd,vcmplepd,vcmplt_oqpd,vcmpltpd,vcmpneq_oqpd,vcmpneq_ospd,vcmpneq_uspd,' +
    'vcmpneqpd,vcmpnge_uqpd,vcmpngepd,vcmpngt_uqpd,vcmpngtpd,vcmpnle_uqpd,vcmpnlepd,' +
    'vcmpnlt_uqpd,vcmpnltpd,vcmpord_spd,vcmpordpd,vcmptrue_uspd,vcmptruepd,vcmpunord_spd,' +
    'vcmpunordpd,vcmpeq_osps,vcmpeq_uqps,vcmpeq_usps,vcmpeqps,vcmpfalse_osps,vcmpfalseps,' +
    'vcmpge_oqps,vcmpgeps,vcmpgt_oqps,vcmpgtps,vcmple_oqps,vcmpleps,vcmplt_oqps,vcmpltps,' +
    'vcmpneq_oqps,vcmpneq_osps,vcmpneq_usps,vcmpneqps,vcmpnge_uqps,vcmpngeps,vcmpngt_uqps,' +
    'vcmpngtps,vcmpnle_uqps,vcmpnleps,vcmpnlt_uqps,vcmpnltps,vcmpord_sps,vcmpordps,' +
    'vcmptrue_usps,vcmptrueps,vcmpunord_sps,vcmpunordps,vcmpeq_ossd,vcmpeq_uqsd,vcmpeq_ussd,' +
    'vcmpeqsd,vcmpfalse_ossd,vcmpfalsesd,vcmpge_oqsd,vcmpgesd,vcmpgt_oqsd,vcmpgtsd,' +
    'vcmple_oqsd,vcmplesd,vcmplt_oqsd,vcmpltsd,vcmpneq_oqsd,vcmpneq_ossd,vcmpneq_ussd,vcmpneqsd,' +
    'vcmpnge_uqsd,vcmpngesd,vcmpngt_uqsd,vcmpngtsd,vcmpnle_uqsd,vcmpnlesd,vcmpnlt_uqsd,' +
    'vcmpnltsd,vcmpord_ssd,vcmpordsd,vcmptrue_ussd,vcmptruesd,vcmpunord_ssd,vcmpunordsd,' +
    'vcmpeq_osss,vcmpeq_uqss,vcmpeq_usss,vcmpeqss,vcmpfalse_osss,vcmpfalsess,vcmpge_oqss,' +
    'vcmpgess,vcmpgt_oqss,vcmpgtss,vcmple_oqss,vcmpless,vcmplt_oqss,vcmpltss,vcmpneq_oqss,' +
    'vcmpneq_osss,vcmpneq_usss,vcmpneqss,vcmpnge_uqss,vcmpngess,vcmpngt_uqss,vcmpngtss,' +
    'vcmpnle_uqss,vcmpnless,vcmpnlt_uqss,vcmpnltss,vcmpord_sss,vcmpordss,vcmptrue_usss,' +
    'vcmptruess,vcmpunord_sss,vcmpunordss,vcvtpd2dqx,vcvtpd2dqy,vcvtpd2psx,vcvtpd2psy,' +
    'vcvtsd2siq,vcvtsd2sil,vcvtsi2sdq,vcvtsi2sdl,vcvtsi2ssq,vcvtsi2ssl,vcvtss2siq,vcvtss2sil,' +
    'vcvttpd2dqx,vcvttpd2dqy,vcvttsd2siq,vcvttsd2sil,vcvttss2siq,vcvttss2sil,vdppd,vdpps,' +
    'vhaddpd,vhaddps,vhsubpd,vhsubps,vlddqu,vldmxcsr,vmaskmovdqu,vmovdqa,vmovdqu,vmpsadbw,' +
    'vpand,vpandn,vpblendvb,vpblendw,vpcmpestri,vpcmpestrm,vpcmpistri,vpcmpistrm,vphaddsw,' +
    'vphaddw,vphaddd,vphminposuw,vphsubsw,vphsubw,vphsubd,vpmovmskb,vpor,vpsignw,vpsignb,' +
    'vpsignd,vptest,vpxor,vroundpd,vroundps,vroundsd,vroundss,vstmxcsr,vbroadcastf128,' +
    'vextractf128,vinsertf128,vmaskmovpd,vmaskmovps,vperm2f128,vtestpd,vtestps,vzeroall,' +
    'vzeroupper';

  Mnemonics: UnicodeString =
    'aaa,aad,aam,aas,adc,adcq,add,addpd,addps,addq,addsd,addss,and,andn,andnpd,andnps,andpd,' +
    'andps,andq,arpl,bound,bsf,bsfq,bsr,bsrq,bswap,bswapq,bt,btc,btcq,btq,btr,btrq,bts,btsq,' +
    'call,cbw,cdq,cdqe,clc,cld,clflush,cli,clts,cmc,cmp,cmppd,cmpps,cmpq,cmps,cmpsb,cmpsd,cmpsq,' +
    'cmpss,cmpsw,cmpxchg,cmpxchg8b,cmpxchgq,comisd,comiss,cpuid,cqo,cvtdq2pd,cvtdq2ps,cvtpd2dq,cvtpd2pi,' +
    'vcvtpd2pi,cvtpd2ps,cvtpi2pd,vcvtpi2pd,cvtpi2ps,vcvtpi2ps,cvtps2dq,cvtps2pd,cvtps2pi,vcvtps2pi,' +
    'cvtsd2si,cvtsd2ss,cvtsi2sd,cvtsi2ss,cvtss2sd,cvtss2si,cvttpd2dq,cvttpd2pi,vcvttpd2pi,' +
    'cvttps2dq,cvttps2pi,vcvttps2pi,cvttsd2si,cvttss2si,cwd,cwde,daa,das,dec,decq,div,divpd,divps,' +
    'divq,divsd,divss,emms,enter,f2xm1,fabs,fadd,vfadd,faddp,vfaddp,fbld,fbstp,fchs,fclex,fcom,' +
    'fcomi,fcomip,fcomp,fcompp,fcos,fdecstp,fdiv,vfdiv,fdivp,fdivr,fdivrp,ffree,fiadd,ficom,ficomp,' +
    'fidiv,fidivr,fild,fimul,fincstp,finit,fist,fistp,fisub,fisubr,fld,fld1,fldl2t,fldl2e,fldpi,fldlg2,' +
    'fldln2,fldz,fldcw,fldenv,fmul,vfmul,fmulp,fnclex,fninit,fnop,fnsave,fnstcw,fnstenv,fnstsw,fpatan,' +
    'fprem,fprem1,fptan,frndint,frstor,fsave,fscale,fsin,fsincos,fsqrt,fst,fstcw,fstenv,fstp,fstsw,fsub,' +
    'vfsub,fsubp,fsubr,fsubrp,ftst,fucom,fucomi,fucomip,fucomp,fucompp,fwait,fxam,fxch,fxrstor,' +
    'fxsave,fxtract,fyl2x,fyl2xp1,hlt,idiv,idivq,imul,imulq,in,inc,incq,ins,insb,insd,insw,int,' +
    'into,invd,invlpg,iret,iretd,iretw,lahf,lar,larq,ldmxcsr,lds,lea,leaq,leave,les,lfence,lfs,lgdt,' +
    'lgs,lidt,lldt,lmsw,lock,lods,lodsb,lodsd,lodsq,lodsw,loop,loope,loopne,loopnz,loopz,lsl,lslq,lss,ltr,' +
    'maskmovdqu,maskmovq,maxpd,maxps,maxsd,maxss,mfence,minpd,minps,minsd,minss,mov,movabs,movapd,movaps,' +
    'movd,movdq,movdq2q,movdqa,movdqu,movhlps,movhpd,movhps,movlhps,movlpd,movlps,movmskpd,vmovmskpd,movmskps,' +
    'vmovmskps,movntdq,movnti,movntiq,movntpd,movntps,movntq,movq,movq2dq,movs,movsb,movsbq,movswq,movsd,' +
    'movsq,movss,movsw,movsx,movupd,movups,movzbq,movzwq,movzx,mul,mulpd,mulps,mulq,mulsd,vmulsd,' +
    'mulss,vmulss,neg,negq,nop,not,notq,or,orpd,orps,orq,out,outs,outsb,outsd,outsw,packssdw,packsswb,' +
    'packuswb,paddb,paddd,paddq,paddsb,paddsw,paddusb,paddusw,paddw,pand,pandn,pause,pavgb,pavgw,pcmpeqb,' +
    'pcmpeqd,pcmpeqw,pcmpgtb,pcmpgtd,pcmpgtw,pextrw,pinsrw,pmaddwd,pmaxsw,pmaxub,pminsw,pminub,pmovmskb,' +
    'pmulhuw,pmulhw,pmullw,pmuludq,pop,popa,popad,popaw,popf,popfl,popfq,popfd,popfw,popq,por,prefetch,' +
    'prefetchw,prefetchnta,prefetcht0,prefetcht1,prefetcht2,psadbw,pshufd,pshufhw,pshuflw,pshufw,pslld,' +
    'pslldq,psllq,psllw,psrad,psraw,psrld,psrldq,psrlq,psrlw,psubb,psubd,psubq,psubsb,psubsw,psubusb,psubusw,' +
    'psubw,punpckhbw,punpckhdq,punpckhqdq,punpckhwd,punpcklbw,punpckldq,punpcklqdq,punpcklwd,push,pusha,' +
    'pushad,pushaw,pushf,pushfd,pushfw,pushfl,pushfq,pushq,pxor,rcl,rclq,rcpps,vrcpps,rcpss,vrcpss,rcr,rcrq,' +
    'rdmsr,rdpmc,rdtsc,rep,repe,repne,repnz,repz,ret,rol,rolq,ror,rorq,rsm,rsqrtps,vrsqrtps,rsqrtss,' +
    'vrsqrtss,sahf,sal,salq,sar,sarq,sbb,sbbq,scas,scasb,scasd,scasq,scasw,sfence,sgdt,shl,shld,shldq,' +
    'shlq,shr,shrd,shrdq,shrq,shufpd,vshufpd,shufps,vshufps,sidt,sldt,sldtq,smsw,smswq,sqrtpd,sqrtps,sqrtsd,' +
    'sqrtss,stc,std,sti,stmxcsr,stos,stosb,stosd,stosq,stosw,str,strq,sub,subpd,subps,subq,subsd,subss,' +
    'sysenter,sysexit,test,testq,ucomisd,ucomiss,ud2,unpckhpd,unpckhps,unpcklpd,unpcklps,verr,verw,wait,' +
    'vwait,wbinvd,wrmsr,xadd,xaddq,xchg,xchgq,xchgqa,xlat,xlatb,xor,xorpd,xorps,xorq,int3' +


    'addsubpd,addsubps,blendpd,blendps,blendvpd,blendvps,dppd,dpps,extractps,' +

    'haddpd,haddps,hsubpd,hsubps,insertps,lddqu,movdmovq,movddup,movntdqa,' +
    'movshdup,movsldup,mpsadbw,pabsb,pabsw,pabsd,pabsq,packusdw,palignr,pblendvb,' +
    'pblendw,pcmpeqq,pcmpestri,pcmpestrm,pcmpgtq,pcmpistri,pcmpistrm,pextrb,pextrd,' +
    'pextrq,phaddsw,phaddw,phaddd,phminposuw,phsubsw,phsubw,phsubd,pinsrb,pinsrd,' +
    'pinsrq,pmaddubsw,pmaxsb,pmaxsd,pmaxsq,pmaxud,pmaxuq,pmaxuw,pminsb,pminsd,pminsq,' +
    'pminud,pminuq,pminuw,pmovsx,pmovzx,pmuldq,pmulhrsw,pmulld,pshufb,psignb,psignw,' +
    'psignd,ptest,pxord,pxorq,roundpd,roundps,roundsd,roundss,vbroadcast,vextractf128,' +
    'vinsertf128,vmaskmov,vperm2f128,vtestpdvtestps,vzeroall,vzeroupper,pclmulqdq';

procedure TSynAsmX86_64.DoAddKeyword(AKeyword: string; AKind: TSynNativeInt);
begin
  if not FKeywords.ContainsKey(AKeyword) then
    FKeywords.Add(AKeyword, TtkTokenKind(AKind));
end;

function TSynAsmX86_64.IsLabel(MayBe: PWideChar): Boolean;
var
  i : Integer;
  LabelText : UnicodeString; // D7: PWideChar + ':' is no implicit concat there
begin
  result := False;
  LabelText := MayBe;
  LabelText := LabelText + ':';
  for i := 0 to TSynEdit( Owner ).Lines.Count-1 do
    begin
    if ( CompareText( TSynEdit( Owner ).Lines[ i ], LabelText ) = 0 ) then
      begin
      result := True;
      break;
      end;
    end;
end;

function TSynAsmX86_64.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  S: string;
begin
  FToIdent := MayBe;
  while IsIdentChar(MayBe^) do
    Inc(MayBe);
  FStringLen := MayBe - FToIdent;
  SetString(S, FToIdent, FStringLen);

  if FKeywords.TryGetValue(S, Result) then
    Exit;

  // Not a keyword: a trailing colon declares a label, otherwise it is only a
  // label if a declaration for it exists somewhere in the text.
  if Pos(' ', S) > 0 then
    Result := tkIdentifier
  else if (S <> '') and (S[Length(S)] = ':') then
    Result := tkLabelDeclaration
  else if IsLabel(FToIdent) then
    Result := tkLabel
  else
    Result := tkIdentifier;
end;

constructor TSynAsmX86_64.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaseSensitive := False;

  // Case-insensitive, like the other assembler highlighters
  FKeywords := TDictionary<string, TtkTokenKind>.Create(TIStringComparer.Ordinal);

  FCommentAttri       := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  AddAttribute(FCommentAttri);
  FIdentifierAttri    := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(FIdentifierAttri);
  FKeyAttri           := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style     := [ fsBold ];
  AddAttribute(FKeyAttri);
  FNumberAttri        := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  AddAttribute(FNumberAttri);
  FSpaceAttri         := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(FSpaceAttri);
  FStringAttri        := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  AddAttribute(FStringAttri);
  FSymbolAttri        := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  AddAttribute(FSymbolAttri);
  FBracketAttri       := TSynHighlighterAttributes.Create('Brackets', 'Brackets');
  AddAttribute(FBracketAttri);
  FMathAttri          := TSynHighlighterAttributes.Create('Math', 'Math');
  AddAttribute(FMathAttri);

  FCharAttri := TSynHighlighterAttributes.Create(SYNS_AttrCharacter, SYNS_FriendlyAttrCharacter);
  AddAttribute(FCharAttri);
  FFloatAttri := TSynHighlighterAttributes.Create(SYNS_AttrFloat, SYNS_FriendlyAttrFloat);
  AddAttribute(FFloatAttri);
  FHexAttri := TSynHighlighterAttributes.Create(SYNS_AttrHexadecimal, SYNS_FriendlyAttrHexadecimal);
  AddAttribute(FHexAttri);
  FOctalAttri := TSynHighlighterAttributes.Create(SYNS_AttrOctal, SYNS_FriendlyAttrOctal);
  AddAttribute(FOctalAttri);

  EnumerateKeywords(Ord(tkKey), Mnemonics, IsIdentChar, DoAddKeyword);

  FRegisterAttributes := TSynHighlighterAttributes.Create( 'Register', 'Register' );
  AddAttribute(FRegisterAttributes);
  EnumerateKeywords(Ord(tkRegister), Registers, IsIdentChar, DoAddKeyword);

  FFloatRegisterAttributes := TSynHighlighterAttributes.Create( 'FloatRegister', 'Floating-Point Register' );
  AddAttribute(FFloatRegisterAttributes);
  EnumerateKeywords(Ord(tkFloatRegister), FloatRegisters, IsIdentChar, DoAddKeyword);

  FTypeSizeAttributes := TSynHighlighterAttributes.Create( 'TypeSize', 'Type Size' );
  AddAttribute(FTypeSizeAttributes);
  EnumerateKeywords(Ord(tkTypeSize), TypeSizes, IsIdentChar, DoAddKeyword);

  FJumpAttributes := TSynHighlighterAttributes.Create( 'Jump', 'Jump' );
  FJumpAttributes.Style := [ fsBold ];
  AddAttribute(FJumpAttributes);
  EnumerateKeywords(Ord(tkJump), Jumps, IsIdentChar, DoAddKeyword);

  FConditionalAttributes := TSynHighlighterAttributes.Create( 'Conditional', 'Conditional' );
  FConditionalAttributes.Style := [ fsBold ];
  AddAttribute(FConditionalAttributes);
  EnumerateKeywords(Ord(tkConditional), Conditionals, IsIdentChar, DoAddKeyword);

  FAVXAttributes := TSynHighlighterAttributes.Create( 'AVX', 'AVX' );
  FAVXAttributes.Style := [ fsBold ];
  AddAttribute(FAVXAttributes);
  EnumerateKeywords(Ord(tkAVX), AVX, IsIdentChar, DoAddKeyword);

  FLabelDeclarationAttributes := TSynHighlighterAttributes.Create( 'LabelDeclaration', 'Label Declaration' );
  AddAttribute(FLabelDeclarationAttributes);

  FLabelAttributes := TSynHighlighterAttributes.Create( 'Label', 'Label' );
  AddAttribute(FLabelAttributes);

  SetAttributesOnChange(DefHighlightChange);
  FDefaultFilter      := SYNS_FilterX86_64Assembly;
end;

destructor TSynAsmX86_64.Destroy;
begin
  FKeywords.Free;
  inherited;
end;

procedure TSynAsmX86_64.NumberProc;
var
  idx1: Integer; // token[1]
  i: Integer;
begin
  idx1 := Run;
  Inc(Run);
  FTokenID := tkNumber;
  while CharInSet( FLine[ Run ], [ '0'..'9', 'A'..'F', 'a'..'f', '.', 'u', 'U', 'l', 'L', 'x', 'X', '-', '+' ] ) do
    begin
    case FLine[Run] of
      '.':
        begin
        if FLine[Succ(Run)] = '.' then
          Break
        else
          begin
          if (FTokenID <> tkHex) then
            FTokenID := tkFloat
          else // invalid
            begin
            FTokenID := tkUnknown;
            Exit;
            end;
          end;
        end;
      '-', '+':
        begin
        if FTokenID <> tkFloat then // number <> float. an arithmetic operator
          Exit;
        if not CharInSet(FLine[Pred(Run)], ['e', 'E']) then
          Exit; // number = float, but no exponent. an arithmetic operator
        if not CharInSet( FLine[ Succ(Run) ], [ '0'..'9', '+', '-' ] ) then // IsDigitPlusMinusChar, invalid
          begin
          Inc(Run);
          FTokenID := tkUnknown;
          Exit;
          end
        end;
      '0'..'7':
        begin
        if (Run = Succ(idx1)) and (FLine[idx1] = '0') then // octal number
          FTokenID := tkOctal;
        end;
      '8', '9':
        begin
        if (FLine[idx1] = '0') and
           ((FTokenID <> tkHex) and (FTokenID <> tkFloat)) then // invalid octal char
             FTokenID := tkUnknown;
        end;
      'a'..'d', 'A'..'D':
        begin
        if FTokenID <> tkHex then // invalid char
          Break;
        end;
      'e', 'E':
        begin
        if (FTokenID <> tkHex) then
          begin
          if CharInSet(FLine[Pred(Run)], ['0'..'9']) then // exponent
            begin
            for i := idx1 to Pred(Run) do
              begin
              if CharInSet(FLine[i], ['e', 'E']) then // too many exponents
                begin
                FTokenID := tkUnknown;
                Exit;
                end;
              end;
            if not CharInSet( FLine[ Succ(Run) ], [ '0'..'9', '+', '-' ] ) then // IsDigitPlusMinusChar
              Break
            else
              FTokenID := tkFloat
            end
          else // invalid char
            Break;
          end;
        end;
      'f', 'F':
        if FTokenID <> tkHex then
          begin
          for i := idx1 to Pred(Run) do
            begin
            if CharInSet(FLine[i], ['f', 'F']) then // declaration syntax error
              begin
              FTokenID := tkUnknown;
              Exit;
              end;
            end;
          if FTokenID = tkFloat then
            begin
            if CharInSet(FLine[Pred(Run)], ['l', 'L']) then // can't mix
              Break;
            end
          else
            FTokenID := tkFloat;
          end;
      'l', 'L':
        begin
        for i := idx1 to Run - 2 do
          begin
          if CharInSet(FLine[i], ['l', 'L']) then // declaration syntax error
            begin
            FTokenID := tkUnknown;
            Exit;
            end;
          end;
        if FTokenID = tkFloat then
          if CharInSet(FLine[Pred(Run)], ['f', 'F']) then // can't mix
            Break;
        end;
      'u', 'U':
        begin
        if FTokenID = tkFloat then // not allowed
          Break
        else
          begin
          for i := idx1 to Pred(Run) do
            begin
            if CharInSet(FLine[i], ['u', 'U']) then // declaration syntax error
              begin
              FTokenID := tkUnknown;
              Exit;
              end;
            end;
          end;
        end;
      'x', 'X':
        if (Run = Succ(idx1)) and   // 0x... 'x' must be second char
           (FLine[idx1] = '0') and  // 0x...
           CharInSet( FLine[Succ(Run) ], [ '0'..'9', 'a'..'f', 'A'..'F' ] ) then // IsHexDigit, 0x... must be continued with a number
             FTokenID := tkHex
        else // invalid char
          begin
          if not IsIdentChar(FLine[Succ(Run)]) and
             CharInSet(FLine[Succ(idx1)], ['x', 'X']) then
            begin
            Inc(Run); // highlight 'x' too
            FTokenID := tkUnknown;
            end;
          Break;
          end;
    end; // case
    Inc(Run);
    end; // while
  if CharInSet( FLine[Run], [ 'A'..'Z', 'a'..'z', '_' ] ) then
    FTokenID := tkUnknown;
end;

procedure TSynAsmX86_64.Next;
begin
  FTokenPos := Run;
  case FLine[Run] of
     #0: begin
         FTokenID := tkNull;
         Inc(Run);
         end;

    #10: begin // LF
         FTokenID := tkSpace;
         Inc(Run);
         end;

    #13: begin // CR
         FTokenID := tkSpace;
         Inc(Run);
         if FLine[Run] = #10 then
           Inc(Run);
         end;

    #34: begin
         FTokenID := tkString;
         if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then
           Inc(Run, 2);
         repeat
           case FLine[Run] of
             #0, #10, #13:
               Break;
           end;
           Inc(Run);
         until FLine[Run] = #34;
         if FLine[Run] <> #0 then
           Inc(Run);
         end;

    #39: begin
         FTokenID := tkChar;
         repeat
           if FLine[Run] = '\' then
             begin
             if CharInSet(FLine[Run + 1], [#39, '\']) then
               Inc(Run);
             end;
           Inc(Run);
         until IsLineEnd(Run) or (FLine[Run] = #39);
         if FLine[Run] = #39 then
           Inc(Run);

         // SingleQuoteStringProc
//         FTokenID := tkString;
//         if (FLine[Run + 1] = #39) and (FLine[Run + 2] = #39) then
//           Inc(Run, 2);
//         repeat
//           case FLine[Run] of
//             #0, #10, #13:
//               Break;
//           end;
//           Inc(Run);
//         until FLine[Run] = #39;
//         if FLine[Run] <> #0 then
//           Inc(Run);
         end;

    '>': begin
         Inc(Run);
         FTokenID := tkMath;
         if FLine[Run] = '=' then
           Inc(Run);
         end;

    '<': begin
         Inc(Run);
         FTokenID := tkMath;
         if CharInSet(FLine[Run], ['=', '>']) then
           Inc(Run);
         end;

    '/': begin
         Inc(Run);
         if FLine[Run] = '/' then
           begin
            FTokenID := tkComment;
           repeat
             Inc(Run);
           until IsLineEnd(Run);
           end
         else
           FTokenID := tkSymbol;
         end;

    'A'..'Z', 'a'..'z', '_':
      begin
      FTokenID := IdentKind((FLine + Run));
      Inc(Run, FStringLen);
      while IsIdentChar(FLine[Run]) do
        Inc(Run);
      end;

    '0'..'9':
      NumberProc;

    #1..#9, #11, #12, #14..#32:
      begin
      FTokenID := tkSpace;
      repeat
        Inc(Run);
      until (FLine[Run] > #32) or IsLineEnd(Run);
      end;

    '#', ';':
      begin
      FTokenID := tkComment;
      repeat
        Inc(Run);
      until IsLineEnd(Run);
      end;

    '&',  '=', '^', '-', '+', '*':
      begin
      Inc(Run);
      FTokenID := tkMath;
      end;

    '{', '}', '(', ')', '[', ']':
      begin
      Inc(Run);
      FTokenID := tkBracket;
      end;

    '.', ',', ':':
      begin
      Inc(Run);
      if ( Copy( FToIdent, Length( FToIdent ), 1 ) = ':' ) AND ( Pos( ' ', FToIdent ) < 1 ) then
        FTokenID := tkLabelDeclaration
      else
        FTokenID := tkSymbol;
      end;
  else
    begin
    Inc(Run);
    FTokenID := tkIdentifier;
    end;
  end;
  inherited;
end;

function TSynAsmX86_64.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynAsmX86_64.GetEol: Boolean;
begin
  Result := Run = FLineLen + 1;
end;

function TSynAsmX86_64.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case FTokenID of
    tkComment          : Result := FCommentAttri;
    tkIdentifier       : Result := FIdentifierAttri;
    tkKey              : Result := FKeyAttri;
    tkNumber           : Result := FNumberAttri;
    tkSpace            : Result := FSpaceAttri;
    tkString           : Result := FStringAttri;
    tkSymbol           : Result := FSymbolAttri;
    tkBracket          : Result := FBracketAttri;
    tkMath             : Result := FMathAttri;
    tkUnknown          : Result := FIdentifierAttri;
    tkRegister         : Result := FRegisterAttributes;
    tkFloatRegister    : Result := FFloatRegisterAttributes;
    tkTypeSize         : Result := FTypeSizeAttributes;
    tkJump             : Result := FJumpAttributes;
    tkConditional      : Result := FConditionalAttributes;
    tkAVX              : Result := FAVXAttributes;
    tkLabelDeclaration : Result := FLabelDeclarationAttributes;
    tkLabel            : Result := FLabelAttributes;
    tkFloat            : Result := FFloatAttri;
    tkHex              : Result := FHexAttri;
    tkOctal            : Result := FOctalAttri;
    tkChar             : Result := FCharAttri;
  else
    Result := nil;
  end;
end;

function TSynAsmX86_64.GetTokenKind: TSynNativeInt;
begin
  Result := Ord(FTokenID);
end;

function TSynAsmX86_64.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

class function TSynAsmX86_64.GetLanguageName: string;
begin
  Result := SYNS_LangX86_64Asm;
end;

function TSynAsmX86_64.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterX86_64Assembly;
end;

function TSynAsmX86_64.GetSampleSource: UnicodeString;
begin
  Result :=
    '; x86 assembly sample source'#13#10 +
    '  CODE	SEGMENT	BYTE PUBLIC'#13#10 +
    '    ASSUME	CS:CODE'#13#10 +
    #13#10 +
    '    PUSH SS'#13#10 +
    '    POP DS'#13#10 +
    '    MOV AX, AABBh'#13#10 +
    '    MOV	BYTE PTR ES:[DI], 255'#13#10 +
    '    JMP SHORT AsmEnd'#13#10 +
    #13#10 +
    '  welcomeMsg DB ''Hello World'', 0'#13#10 +
    #13#10 +
    '  AsmEnd:'#13#10 +
    '    MOV AX, 0'#13#10 +
    #13#10 +
    '  CODE	ENDS'#13#10 +
    'END';
end;

class function TSynAsmX86_64.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangX86_64Asm;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynAsmX86_64);
{$ENDIF}
end.


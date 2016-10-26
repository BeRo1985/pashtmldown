(******************************************************************************
 *                         PasHTMLDown MarkDown Libary                        *
 ******************************************************************************
 *                        Version 2016-10-26-12-36-0000                       *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016, Benjamin Rosseaux (benjamin@rosseaux.de)               *
 *                                                                            *
 * This software is provided 'as-is', without any express or implied          *
 * warranty. In no event will the authors be held liable for any damages      *
 * arising from the use of this software.                                     *
 *                                                                            *
 * Permission is granted to anyone to use this software for any purpose,      *
 * including commercial applications, and to alter it and redistribute it     *
 * freely, subject to the following restrictions:                             *
 *                                                                            *
 * 1. The origin of this software must not be misrepresented; you must not    *
 *    claim that you wrote the original software. If you use this software    *
 *    in a product, an acknowledgement in the product documentation would be  *
 *    appreciated but is not required.                                        *
 * 2. Altered source versions must be plainly marked as such, and must not be *
 *    misrepresented as being the original software.                          *
 * 3. This notice may not be removed or altered from any source distribution. *
 *                                                                            *
 ******************************************************************************
 *                  General guidelines for code contributors                  *
 *============================================================================*
 *                                                                            *
 * 1. Make sure you are legally allowed to make a contribution under the zlib *
 *    license.                                                                *
 * 2. The zlib license header goes at the top of each source file, with       *
 *    appropriate copyright notice.                                           *
 * 3. After a pull request, check the status of your pull request on          *
      http://github.com/BeRo1985/pashtmldown                                  *
 * 4. Write code, which is compatible with Delphi 7-XE7 and FreePascal >= 3.0 *
 *    so don't use generics/templates, operator overloading and another newer *
 *    syntax features than Delphi 7 has support for that, but if needed, make *
 *    it out-ifdef-able.                                                      *
 * 5. Don't use Delphi-only, FreePascal-only or Lazarus-only libraries/units, *
 *    but if needed, make it out-ifdef-able.                                  *
 * 6. No use of third-party libraries/units as possible, but if needed, make  *
 *    it out-ifdef-able.                                                      *
 * 7. Try to use const when possible.                                         *
 * 8. Make sure to comment out writeln, used while debugging.                 *
 * 9. Make sure the code compiles on 32-bit and 64-bit platforms (x86-32,     *
 *    x86-64, ARM, ARM64, etc.).                                              *
 *                                                                            *
 ******************************************************************************)
unit PasHTMLDown;
{$ifdef fpc}
 {$mode delphi}
 {$ifdef cpui386}
  {$define cpu386}
 {$endif}
 {$ifdef cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef cpuamd64}
  {$asmmode intel}
 {$endif}
 {$ifdef FPC_LITTLE_ENDIAN}
  {$define LITTLE_ENDIAN}
 {$else}
  {$ifdef FPC_BIG_ENDIAN}
   {$define BIG_ENDIAN}
  {$endif}
 {$endif}
 {-$pic off}
 {$define CanInline}
 {$ifdef FPC_HAS_TYPE_EXTENDED}
  {$define HAS_TYPE_EXTENDED}
 {$else}
  {$undef HAS_TYPE_EXTENDED}
 {$endif}
 {$ifdef FPC_HAS_TYPE_DOUBLE}
  {$define HAS_TYPE_DOUBLE}
 {$else}
  {$undef HAS_TYPE_DOUBLE}
 {$endif}
 {$ifdef FPC_HAS_TYPE_SINGLE}
  {$define HAS_TYPE_SINGLE}
 {$else}
  {$undef HAS_TYPE_SINGLE}
 {$endif}
 {$if declared(RawByteString)}
  {$define HAS_TYPE_RAWBYTESTRING}
 {$else}
  {$undef HAS_TYPE_RAWBYTESTRING}
 {$ifend}
 {$if declared(UTF8String)}
  {$define HAS_TYPE_UTF8STRING}
 {$else}
  {$undef HAS_TYPE_UTF8STRING}
 {$ifend}
 {$if declared(UnicodeString)}
  {$define HAS_TYPE_UNICODESTRING}
 {$else}
  {$undef HAS_TYPE_UNICODESTRING}
 {$ifend}
{$else}
 {$realcompatibility off}
 {$localsymbols on}
 {$define LITTLE_ENDIAN}
 {$ifndef cpu64}
  {$define cpu32}
 {$endif}
 {$define HAS_TYPE_EXTENDED}
 {$define HAS_TYPE_DOUBLE}
 {$define HAS_TYPE_SINGLE}
 {$ifdef conditionalexpressions}
  {$if declared(RawByteString)}
   {$define HAS_TYPE_RAWBYTESTRING}
  {$else}
   {$undef HAS_TYPE_RAWBYTESTRING}
  {$ifend}
  {$if declared(UTF8String)}
   {$define HAS_TYPE_UTF8STRING}
  {$else}
   {$undef HAS_TYPE_UTF8STRING}
  {$ifend}
  {$if declared(UnicodeString)}
   {$define HAS_TYPE_UNICODESTRING}
  {$else}
   {$undef HAS_TYPE_UNICODESTRING}
  {$ifend}
 {$else}
  {$undef HAS_TYPE_RAWBYTESTRING}
  {$undef HAS_TYPE_UTF8STRING}
  {$undef HAS_TYPE_UNICODESTRING}
 {$endif}
 {$ifndef BCB}
  {$ifdef ver120}
   {$define Delphi4or5}
  {$endif}
  {$ifdef ver130}
   {$define Delphi4or5}
  {$endif}
  {$ifdef ver140}
   {$define Delphi6}
  {$endif}
  {$ifdef ver150}
   {$define Delphi7}
  {$endif}
  {$ifdef ver170}
   {$define Delphi2005}
  {$endif}
 {$else}
  {$ifdef ver120}
   {$define Delphi4or5}
   {$define BCB4}
  {$endif}
  {$ifdef ver130}
   {$define Delphi4or5}
  {$endif}
 {$endif}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24}
   {$legacyifend on}
  {$ifend}
  {$if CompilerVersion>=14.0}
   {$if CompilerVersion=14.0}
    {$define Delphi6}
   {$ifend}
   {$define Delphi6AndUp}
  {$ifend}
  {$if CompilerVersion>=15.0}
   {$if CompilerVersion=15.0}
    {$define Delphi7}
   {$ifend}
   {$define Delphi7AndUp}
  {$ifend}
  {$if CompilerVersion>=17.0}
   {$if CompilerVersion=17.0}
    {$define Delphi2005}
   {$ifend}
   {$define Delphi2005AndUp}
  {$ifend}
  {$if CompilerVersion>=18.0}
   {$if CompilerVersion=18.0}
    {$define BDS2006}
    {$define Delphi2006}
   {$ifend}
   {$define Delphi2006AndUp}
  {$ifend}
  {$if CompilerVersion>=18.5}
   {$if CompilerVersion=18.5}
    {$define Delphi2007}
   {$ifend}
   {$define Delphi2007AndUp}
  {$ifend}
  {$if CompilerVersion=19.0}
   {$define Delphi2007Net}
  {$ifend}
  {$if CompilerVersion>=20.0}
   {$if CompilerVersion=20.0}
    {$define Delphi2009}
   {$ifend}
   {$define Delphi2009AndUp}
  {$ifend}
  {$if CompilerVersion>=21.0}
   {$if CompilerVersion=21.0}
    {$define Delphi2010}
   {$ifend}
   {$define Delphi2010AndUp}
  {$ifend}
  {$if CompilerVersion>=22.0}
   {$if CompilerVersion=22.0}
    {$define DelphiXE}
   {$ifend}
   {$define DelphiXEAndUp}
  {$ifend}
  {$if CompilerVersion>=23.0}
   {$if CompilerVersion=23.0}
    {$define DelphiXE2}
   {$ifend}
   {$define DelphiXE2AndUp}
  {$ifend}
  {$if CompilerVersion>=24.0}
   {$if CompilerVersion=24.0}
    {$define DelphiXE3}
   {$ifend}
   {$define DelphiXE3AndUp}
  {$ifend}
  {$if CompilerVersion>=25.0}
   {$if CompilerVersion=25.0}
    {$define DelphiXE4}
   {$ifend}
   {$define DelphiXE4AndUp}
  {$ifend}
  {$if CompilerVersion>=26.0}
   {$if CompilerVersion=26.0}
    {$define DelphiXE5}
   {$ifend}
   {$define DelphiXE5AndUp}
  {$ifend}
  {$if CompilerVersion>=27.0}
   {$if CompilerVersion=27.0}
    {$define DelphiXE6}
   {$ifend}
   {$define DelphiXE6AndUp}
  {$ifend}
  {$if CompilerVersion>=28.0}
   {$if CompilerVersion=28.0}
    {$define DelphiXE7}
   {$ifend}
   {$define DelphiXE7AndUp}
  {$ifend}
  {$if CompilerVersion>=29.0}
   {$if CompilerVersion=29.0}
    {$define DelphiXE8}
   {$ifend}
   {$define DelphiXE8AndUp}
  {$ifend}
  {$if CompilerVersion>=30.0}
   {$if CompilerVersion=30.0}
    {$define Delphi10Seattle}
   {$ifend}
   {$define Delphi10SeattleAndUp}
  {$ifend}
  {$if CompilerVersion>=31.0}
   {$if CompilerVersion=31.0}
    {$define Delphi10Berlin}
   {$ifend}
   {$define Delphi10BerlinAndUp}
  {$ifend}
 {$endif}
 {$ifndef Delphi4or5}
  {$ifndef BCB}
   {$define Delphi6AndUp}
  {$endif}
   {$ifndef Delphi6}
    {$define BCB6OrDelphi7AndUp}
    {$ifndef BCB}
     {$define Delphi7AndUp}
    {$endif}
    {$ifndef BCB}
     {$ifndef Delphi7}
      {$ifndef Delphi2005}
       {$define BDS2006AndUp}
      {$endif}
     {$endif}
    {$endif}
   {$endif}
 {$endif}
 {$ifdef Delphi6AndUp}
  {$warn symbol_platform off}
  {$warn symbol_deprecated off}
 {$endif}
{$endif}
{$ifdef win32}
 {$define windows}
{$endif}
{$ifdef win64}
 {$define windows}
{$endif}
{$ifdef wince}
 {$define windows}
{$endif}
{$rangechecks off}
{$extendedsyntax on}
{$writeableconst on}
{$hints off}
{$booleval off}
{$typedaddress off}
{$stackframes off}
{$varstringchecks on}
{$typeinfo on}
{$overflowchecks off}
{$longstrings on}
{$openstrings on}

interface

uses Classes,SysUtils;

type THTMLCharset=(ISO_8859_1,ISO_8859_2,ISO_8859_3,ISO_8859_4,ISO_8859_5,
                   ISO_8859_6,ISO_8859_7,ISO_8859_8,ISO_8859_9,ISO_8859_10,
                   CP1250,CP1251,CP1252,CP1253,CP1254,CP1255,CP1256,CP1257,CP1258,
                   KOI8_R,UCS_2,UCS_4,UTF_8,UTF_7);

     THTMLNodeType=(ntROOT,ntTAG,ntTEXT);

     PHTMLTagParameter=^THTMLTagParameter;
     THTMLTagParameter=record
      Name:ansistring;
      Value:ansistring;
     end;

     THTMLTagParameters=array of THTMLTagParameter;

     PHTMLNode=^THTMLNode;

     TPHTMLNodes=array of PHTMLNode;

     THTMLNode=record
      NodeType:THTMLNodeType;
      TagName:ansistring;
      Text:ansistring;
      TagParameters:THTMLTagParameters;
      Children:TPHTMLNodes;
     end;

     THTML=class
      public
       RootNode:THTMLNode;
       constructor Create(Input:ansistring;Charset:THTMLCharset=UTF_8);
       destructor Destroy; override;
       function GetPlainText:ansistring;
       function GetMarkDown:ansistring;
       function GetHTML(AllowedTags:TStringList=nil):ansistring;
     end;

function MarkDownToHTML(const pInputText:ansistring):ansistring;

implementation

const Base64Chars='ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

      Base64Table:array[0..63] of ansichar='ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

var Base64DecoderTable:array[ansichar] of byte;

procedure InitBase64;
var i:longint;
begin
 for i:=0 to 63 do begin
  Base64DecoderTable[Base64Table[i]]:=i;
 end;
end;

function DecodeBase64(s:ansistring):ansistring;
var i,j,l:longint;
    c:longword;
begin
 setlength(result,((length(s)*3) shr 2)+3);
{while (length(s) and 3)<>0 do begin
  s:=s+'=';
 end;}
 l:=0;
 i:=1;
 while i<=length(s) do begin
  c:=0;
  for j:=1 to 4 do begin
   if i<=length(s) then begin
    case s[i] of
     'A'..'Z','a'..'z','0'..'9','+','/':begin
      c:=c or (Base64DecoderTable[s[i]] shl (24-((j shl 2)+(j shl 1))));
     end;
     '=':begin
      c:=(c and $00ffffff) or (((c shr 24)+1) shl 24);
     end;
     else begin
      c:=c or $f0000000;
      break;
     end;
    end;
   end else begin
    c:=(c and $00ffffff) or (((c shr 24)+1) shl 24);
   end;
   inc(i);
  end;
  if (c shr 24)<3 then begin
   inc(l);
   result[l]:=chr((c shr 16) and $ff);
   if (c shr 24)<2 then begin
    inc(l);
    result[l]:=chr((c shr 8) and $ff);
    if (c shr 24)<1 then begin
     inc(l);
     result[l]:=chr(c and $ff);
    end;
   end;
  end else begin
   break;
  end;
 end;
 setlength(result,l);
end;

function EncodeBase64(s:ansistring):ansistring;
var i,l:longint;
    c:longword;
begin
 if length(s)=0 then begin
  result:='';
 end else begin
  setlength(result,((length(s)*4) div 3)+4);
  l:=1;
  i:=1;
  while (i+2)<=length(s) do begin
   c:=(byte(s[i]) shl 16) or (byte(s[i+1]) shl 8) or byte(s[i+2]);
   result[l]:=Base64Table[(c shr 18) and $3f];
   result[l+1]:=Base64Table[(c shr 12) and $3f];
   result[l+2]:=Base64Table[(c shr 6) and $3f];
   result[l+3]:=Base64Table[c and $3f];
   inc(i,3);
   inc(l,4);
  end;
  if (i+1)<=length(s) then begin
   c:=(byte(s[i]) shl 16) or (byte(s[i+1]) shl 8);
   result[l]:=Base64Table[(c shr 18) and $3f];
   result[l+1]:=Base64Table[(c shr 12) and $3f];
   result[l+2]:=Base64Table[(c shr 6) and $3f];
   result[l+3]:='=';
   inc(l,4);
  end else if i<=length(s) then begin
   c:=byte(s[i]) shl 16;
   result[l]:=Base64Table[(c shr 18) and $3f];
   result[l+1]:=Base64Table[(c shr 12) and $3f];
   result[l+2]:='=';
   result[l+3]:='=';
   inc(l,4);
  end;
  if l>1 then begin
   setlength(result,l-1);
  end else begin
   result:=trim(result);
  end;
 end;
end;

function Dequote(s:ansistring):ansistring;
const hexa:array[1..$F] of ansichar='123456789ABCDEF';
var p:longint;
    encode:ansistring;
begin
 if s='' then begin
  result:=#13#10;
 end else begin
  result:='';
  if s[length(s)]='=' then begin
   setlength(s,Length(s)-1)
  end else begin
   if (length(s)>=3) and (s[length(s)-2]<>'=') then begin
    s:=s+#13#10;
   end;
  end;
  p:=pos('=',s);
  while p>0 do begin
   encode:=chr((pos(s[p+1],hexa) shl 4) or pos(s[p+2],hexa));
   if encode=#0 then begin
    encode:=#13#10;
   end;
   result:=result+copy(s,1,p-1)+encode;
   delete(s,1,p+2);
   p:=pos('=',s);
  end;
  result:=result+s;
  p:=pos('_',result);
  while p>0 do begin
   result[p]:=' ';
   p:=pos('_',result);
  end;
 end;
end;

type TCharsetSet=set of THTMLCharset;

     TCharsetTable=array[128..255] of word;

     TCharsetTableCasted=array[128..255] of widechar;

const AllCharsets:TCharsetSet=[ISO_8859_1,ISO_8859_2,ISO_8859_3,ISO_8859_4,
                                   ISO_8859_5,ISO_8859_6,ISO_8859_7,ISO_8859_8,
                                   ISO_8859_9,ISO_8859_10,CP1250,CP1251,CP1252,
                                   CP1253,CP1254,CP1255,CP1256,CP1257,CP1258,
                                   KOI8_R,UCS_2,UCS_4,UTF_8,UTF_7];

      CharISO_8859_1:TCharsetTable=
       ($0080,$0081,$0082,$0083,$0084,$0085,$0086,$0087,
        $0088,$0089,$008A,$008B,$008C,$008D,$008E,$008F,
        $0090,$0091,$0092,$0093,$0094,$0095,$0096,$0097,
        $0098,$0099,$009A,$009B,$009C,$009D,$009E,$009F,
        $00A0,$00A1,$00A2,$00A3,$00A4,$00A5,$00A6,$00A7,
        $00A8,$00A9,$00AA,$00AB,$00AC,$00AD,$00AE,$00AF,
        $00B0,$00B1,$00B2,$00B3,$00B4,$00B5,$00B6,$00B7,
        $00B8,$00B9,$00BA,$00BB,$00BC,$00BD,$00BE,$00BF,
        $00C0,$00C1,$00C2,$00C3,$00C4,$00C5,$00C6,$00C7,
        $00C8,$00C9,$00CA,$00CB,$00CC,$00CD,$00CE,$00CF,
        $00D0,$00D1,$00D2,$00D3,$00D4,$00D5,$00D6,$00D7,
        $00D8,$00D9,$00DA,$00DB,$00DC,$00DD,$00DE,$00DF,
        $00E0,$00E1,$00E2,$00E3,$00E4,$00E5,$00E6,$00E7,
        $00E8,$00E9,$00EA,$00EB,$00EC,$00ED,$00EE,$00EF,
        $00F0,$00F1,$00F2,$00F3,$00F4,$00F5,$00F6,$00F7,
        $00F8,$00F9,$00FA,$00FB,$00FC,$00FD,$00FE,$00FF);

      CharISO_8859_2:TCharsetTable=
       ($0080,$0081,$0082,$0083,$0084,$0085,$0086,$0087,
        $0088,$0089,$008A,$008B,$008C,$008D,$008E,$008F,
        $0090,$0091,$0092,$0093,$0094,$0095,$0096,$0097,
        $0098,$0099,$009A,$009B,$009C,$009D,$009E,$009F,
        $00a0,$0104,$02d8,$0141,$00a4,$013d,$015a,$00a7,
        $00a8,$0160,$015e,$0164,$0179,$00ad,$017d,$017b,
        $00b0,$0105,$02db,$0142,$00b4,$013e,$015b,$02c7,
        $00b8,$0161,$015f,$0165,$017a,$02dd,$017e,$017c,
        $0154,$00c1,$00c2,$0102,$00c4,$0139,$0106,$00c7,
        $010c,$00c9,$0118,$00cb,$011a,$00cd,$00ce,$010e,
        $0110,$0143,$0147,$00d3,$00d4,$0150,$00d6,$00d7,
        $0158,$016e,$00da,$0170,$00dc,$00dd,$0162,$00df,
        $0155,$00e1,$00e2,$0103,$00e4,$013a,$0107,$00e7,
        $010d,$00e9,$0119,$00eb,$011b,$00ed,$00ee,$010f,
        $0111,$0144,$0148,$00f3,$00f4,$0151,$00f6,$00f7,
        $0159,$016f,$00fa,$0171,$00fc,$00fd,$0163,$02d9);

      CharISO_8859_3:TCharsetTable=
       ($0080,$0081,$0082,$0083,$0084,$0085,$0086,$0087,
        $0088,$0089,$008A,$008B,$008C,$008D,$008E,$008F,
        $0090,$0091,$0092,$0093,$0094,$0095,$0096,$0097,
        $0098,$0099,$009A,$009B,$009C,$009D,$009E,$009F,
        $00a0,$0126,$02d8,$00a3,$00a4,$fffd,$0124,$00a7,
        $00a8,$0130,$015e,$011e,$0134,$00ad,$fffd,$017b,
        $00b0,$0127,$00b2,$00b3,$00b4,$00b5,$0125,$00b7,
        $00b8,$0131,$015f,$011f,$0135,$00bd,$fffd,$017c,
        $00c0,$00c1,$00c2,$fffd,$00c4,$010a,$0108,$00c7,
        $00c8,$00c9,$00ca,$00cb,$00cc,$00cd,$00ce,$00cf,
        $fffd,$00d1,$00d2,$00d3,$00d4,$0120,$00d6,$00d7,
        $011c,$00d9,$00da,$00db,$00dc,$016c,$015c,$00df,
        $00e0,$00e1,$00e2,$fffd,$00e4,$010b,$0109,$00e7,
        $00e8,$00e9,$00ea,$00eb,$00ec,$00ed,$00ee,$00ef,
        $fffd,$00f1,$00f2,$00f3,$00f4,$0121,$00f6,$00f7,
        $011d,$00f9,$00fa,$00fb,$00fc,$016d,$015d,$02d9);

      CharISO_8859_4:TCharsetTable=
       ($0080,$0081,$0082,$0083,$0084,$0085,$0086,$0087,
        $0088,$0089,$008A,$008B,$008C,$008D,$008E,$008F,
        $0090,$0091,$0092,$0093,$0094,$0095,$0096,$0097,
        $0098,$0099,$009A,$009B,$009C,$009D,$009E,$009F,
        $00a0,$0104,$0138,$0156,$00a4,$0128,$013b,$00a7,
        $00a8,$0160,$0112,$0122,$0166,$00ad,$017d,$00af,
        $00b0,$0105,$02db,$0157,$00b4,$0129,$013c,$02c7,
        $00b8,$0161,$0113,$0123,$0167,$014a,$017e,$014b,
        $0100,$00c1,$00c2,$00c3,$00c4,$00c5,$00c6,$012e,
        $010c,$00c9,$0118,$00cb,$0116,$00cd,$00ce,$012a,
        $0110,$0145,$014c,$0136,$00d4,$00d5,$00d6,$00d7,
        $00d8,$0172,$00da,$00db,$00dc,$0168,$016a,$00df,
        $0101,$00e1,$00e2,$00e3,$00e4,$00e5,$00e6,$012f,
        $010d,$00e9,$0119,$00eb,$0117,$00ed,$00ee,$012b,
        $0111,$0146,$014d,$0137,$00f4,$00f5,$00f6,$00f7,
        $00f8,$0173,$00fa,$00fb,$00fc,$0169,$016b,$02d9);

      CharISO_8859_5:TCharsetTable=
       ($0080,$0081,$0082,$0083,$0084,$0085,$0086,$0087,
        $0088,$0089,$008A,$008B,$008C,$008D,$008E,$008F,
        $0090,$0091,$0092,$0093,$0094,$0095,$0096,$0097,
        $0098,$0099,$009A,$009B,$009C,$009D,$009E,$009F,
        $00a0,$0401,$0402,$0403,$0404,$0405,$0406,$0407,
        $0408,$0409,$040a,$040b,$040c,$00ad,$040e,$040f,
        $0410,$0411,$0412,$0413,$0414,$0415,$0416,$0417,
        $0418,$0419,$041a,$041b,$041c,$041d,$041e,$041f,
        $0420,$0421,$0422,$0423,$0424,$0425,$0426,$0427,
        $0428,$0429,$042a,$042b,$042c,$042d,$042e,$042f,
        $0430,$0431,$0432,$0433,$0434,$0435,$0436,$0437,
        $0438,$0439,$043a,$043b,$043c,$043d,$043e,$043f,
        $0440,$0441,$0442,$0443,$0444,$0445,$0446,$0447,
        $0448,$0449,$044a,$044b,$044c,$044d,$044e,$044f,
        $2116,$0451,$0452,$0453,$0454,$0455,$0456,$0457,
        $0458,$0459,$045a,$045b,$045c,$00a7,$045e,$045f);

      CharISO_8859_6:TCharsetTable=
       ($0080,$0081,$0082,$0083,$0084,$0085,$0086,$0087,
        $0088,$0089,$008A,$008B,$008C,$008D,$008E,$008F,
        $0090,$0091,$0092,$0093,$0094,$0095,$0096,$0097,
        $0098,$0099,$009A,$009B,$009C,$009D,$009E,$009F,
        $00a0,$fffd,$fffd,$fffd,$00a4,$fffd,$fffd,$fffd,
        $fffd,$fffd,$fffd,$fffd,$060c,$00ad,$fffd,$fffd,
        $fffd,$fffd,$fffd,$fffd,$fffd,$fffd,$fffd,$fffd,
        $fffd,$fffd,$fffd,$061b,$fffd,$fffd,$fffd,$061f,
        $fffd,$0621,$0622,$0623,$0624,$0625,$0626,$0627,
        $0628,$0629,$062a,$062b,$062c,$062d,$062e,$062f,
        $0630,$0631,$0632,$0633,$0634,$0635,$0636,$0637,
        $0638,$0639,$063a,$fffd,$fffd,$fffd,$fffd,$fffd,
        $0640,$0641,$0642,$0643,$0644,$0645,$0646,$0647,
        $0648,$0649,$064a,$064b,$064c,$064d,$064e,$064f,
        $0650,$0651,$0652,$fffd,$fffd,$fffd,$fffd,$fffd,
        $fffd,$fffd,$fffd,$fffd,$fffd,$fffd,$fffd,$fffd);

      CharISO_8859_7:TCharsetTable=
       ($0080,$0081,$0082,$0083,$0084,$0085,$0086,$0087,
        $0088,$0089,$008A,$008B,$008C,$008D,$008E,$008F,
        $0090,$0091,$0092,$0093,$0094,$0095,$0096,$0097,
        $0098,$0099,$009A,$009B,$009C,$009D,$009E,$009F,
        $00a0,$2018,$2019,$00a3,$fffd,$fffd,$00a6,$00a7,
        $00a8,$00a9,$fffd,$00ab,$00ac,$00ad,$fffd,$2015,
        $00b0,$00b1,$00b2,$00b3,$0384,$0385,$0386,$00b7,
        $0388,$0389,$038a,$00bb,$038c,$00bd,$038e,$038f,
        $0390,$0391,$0392,$0393,$0394,$0395,$0396,$0397,
        $0398,$0399,$039a,$039b,$039c,$039d,$039e,$039f,
        $03a0,$03a1,$fffd,$03a3,$03a4,$03a5,$03a6,$03a7,
        $03a8,$03a9,$03aa,$03ab,$03ac,$03ad,$03ae,$03af,
        $03b0,$03b1,$03b2,$03b3,$03b4,$03b5,$03b6,$03b7,
        $03b8,$03b9,$03ba,$03bb,$03bc,$03bd,$03be,$03bf,
        $03c0,$03c1,$03c2,$03c3,$03c4,$03c5,$03c6,$03c7,
        $03c8,$03c9,$03ca,$03cb,$03cc,$03cd,$03ce,$fffd);

      CharISO_8859_8:TCharsetTable=
       ($0080,$0081,$0082,$0083,$0084,$0085,$0086,$0087,
        $0088,$0089,$008A,$008B,$008C,$008D,$008E,$008F,
        $0090,$0091,$0092,$0093,$0094,$0095,$0096,$0097,
        $0098,$0099,$009A,$009B,$009C,$009D,$009E,$009F,
        $00a0,$fffd,$00a2,$00a3,$00a4,$00a5,$00a6,$00a7,
        $00a8,$00a9,$00d7,$00ab,$00ac,$00ad,$00ae,$00af,
        $00b0,$00b1,$00b2,$00b3,$00b4,$00b5,$00b6,$00b7,
        $00b8,$00b9,$00f7,$00bb,$00bc,$00bd,$00be,$fffd,
        $fffd,$fffd,$fffd,$fffd,$fffd,$fffd,$fffd,$fffd,
        $fffd,$fffd,$fffd,$fffd,$fffd,$fffd,$fffd,$fffd,
        $fffd,$fffd,$fffd,$fffd,$fffd,$fffd,$fffd,$fffd,
        $fffd,$fffd,$fffd,$fffd,$fffd,$fffd,$fffd,$2017,
        $05d0,$05d1,$05d2,$05d3,$05d4,$05d5,$05d6,$05d7,
        $05d8,$05d9,$05da,$05db,$05dc,$05dd,$05de,$05df,
        $05e0,$05e1,$05e2,$05e3,$05e4,$05e5,$05e6,$05e7,
        $05e8,$05e9,$05ea,$fffd,$fffd,$200e,$200f,$fffd);

      CharISO_8859_9:TCharsetTable=
       ($0080,$0081,$0082,$0083,$0084,$0085,$0086,$0087,
        $0088,$0089,$008A,$008B,$008C,$008D,$008E,$008F,
        $0090,$0091,$0092,$0093,$0094,$0095,$0096,$0097,
        $0098,$0099,$009A,$009B,$009C,$009D,$009E,$009F,
        $00a0,$0104,$02d8,$0141,$00a4,$013d,$015a,$00a7,
        $00a8,$0160,$015e,$0164,$0179,$00ad,$017d,$017b,
        $00b0,$0105,$02db,$0142,$00b4,$013e,$015b,$02c7,
        $00b8,$0161,$015f,$0165,$017a,$02dd,$017e,$017c,
        $0154,$00c1,$00c2,$0102,$00c4,$0139,$0106,$00c7,
        $010c,$00c9,$0118,$00cb,$011a,$00cd,$00ce,$010e,
        $011e,$00d1,$00d2,$00d3,$00d4,$00d5,$00d6,$00d7,
        $00d8,$00d9,$00da,$00db,$00dc,$0130,$015e,$00df,
        $00e0,$00e1,$00e2,$00e3,$00e4,$00e5,$00e6,$00e7,
        $00e8,$00e9,$00ea,$00eb,$00ec,$00ed,$00ee,$00ef,
        $011f,$00f1,$00f2,$00f3,$00f4,$00f5,$00f6,$00f7,
        $00f8,$00f9,$00fa,$00fb,$00fc,$0131,$015f,$00ff);

      CharISO_8859_10:TCharsetTable=
       ($0080,$0081,$0082,$0083,$0084,$0085,$0086,$0087,
        $0088,$0089,$008A,$008B,$008C,$008D,$008E,$008F,
        $0090,$0091,$0092,$0093,$0094,$0095,$0096,$0097,
        $0098,$0099,$009A,$009B,$009C,$009D,$009E,$009F,
        $00a0,$0104,$0112,$0122,$012a,$0128,$0136,$00a7,
        $013b,$0110,$0160,$0166,$017d,$00ad,$016a,$014a,
        $00b0,$0105,$0113,$0123,$012b,$0129,$0137,$00b7,
        $013c,$0111,$0161,$0167,$017e,$2015,$016b,$014b,
        $0100,$00c1,$00c2,$00c3,$00c4,$00c5,$00c6,$012e,
        $010c,$00c9,$0118,$00cb,$0116,$00cd,$00ce,$00cf,
        $00d0,$0145,$014c,$00d3,$00d4,$00d5,$00d6,$0168,
        $00d8,$0172,$00da,$00db,$00dc,$00dd,$00de,$00df,
        $0101,$00e1,$00e2,$00e3,$00e4,$00e5,$00e6,$012f,
        $010d,$00e9,$0119,$00eb,$0117,$00ed,$00ee,$00ef,
        $00f0,$0146,$014d,$00f3,$00f4,$00f5,$00f6,$0169,
        $00f8,$0173,$00fa,$00fb,$00fc,$00fd,$00fe,$0138);

      CharCP_1250:TCharsetTable=
       ($20ac,$fffd,$201a,$fffd,$201e,$2026,$2020,$2021,
        $fffd,$2030,$0160,$2039,$015a,$0164,$017d,$0179,
        $fffd,$2018,$2019,$201c,$201d,$2022,$2013,$2014,
        $fffd,$2122,$0161,$203a,$015b,$0165,$017e,$017a,
        $00a0,$02c7,$02d8,$0141,$00a4,$0104,$00a6,$00a7,
        $00a8,$00a9,$015e,$00ab,$00ac,$00ad,$00ae,$017b,
        $00b0,$00b1,$02db,$0142,$00b4,$00b5,$00b6,$00b7,
        $00b8,$0105,$015f,$00bb,$013d,$02dd,$013e,$017c,
        $0154,$00c1,$00c2,$0102,$00c4,$0139,$0106,$00c7,
        $010c,$00c9,$0118,$00cb,$011a,$00cd,$00ce,$010e,
        $0110,$0143,$0147,$00d3,$00d4,$0150,$00d6,$00d7,
        $0158,$016e,$00da,$0170,$00dc,$00dd,$0162,$00df,
        $0155,$00e1,$00e2,$0103,$00e4,$013a,$0107,$00e7,
        $010d,$00e9,$0119,$00eb,$011b,$00ed,$00ee,$010f,
        $0111,$0144,$0148,$00f3,$00f4,$0151,$00f6,$00f7,
        $0159,$016f,$00fa,$0171,$00fc,$00fd,$0163,$02d9);

      CharCP_1251:TCharsetTable=
       ($0402,$0403,$201a,$0453,$201e,$2026,$2020,$2021,
        $20ac,$2030,$0409,$2039,$040a,$040c,$040b,$040f,
        $0452,$2018,$2019,$201c,$201d,$2022,$2013,$2014,
        $fffd,$2122,$0459,$203a,$045a,$045c,$045b,$045f,
        $00a0,$040e,$045e,$0408,$00a4,$0490,$00a6,$00a7,
        $0401,$00a9,$0404,$00ab,$00ac,$00ad,$00ae,$0407,
        $00b0,$00b1,$0406,$0456,$0491,$00b5,$00b6,$00b7,
        $0451,$2116,$0454,$00bb,$0458,$0405,$0455,$0457,
        $0410,$0411,$0412,$0413,$0414,$0415,$0416,$0417,
        $0418,$0419,$041a,$041b,$041c,$041d,$041e,$041f,
        $0420,$0421,$0422,$0423,$0424,$0425,$0426,$0427,
        $0428,$0429,$042a,$042b,$042c,$042d,$042e,$042f,
        $0430,$0431,$0432,$0433,$0434,$0435,$0436,$0437,
        $0438,$0439,$043a,$043b,$043c,$043d,$043e,$043f,
        $0440,$0441,$0442,$0443,$0444,$0445,$0446,$0447,
        $0448,$0449,$044a,$044b,$044c,$044d,$044e,$044f);

      CharCP_1252:TCharsetTable=
       ($20ac,$fffd,$201a,$0192,$201e,$2026,$2020,$2021,
        $02c6,$2030,$0160,$2039,$0152,$fffd,$017d,$fffd,
        $fffd,$2018,$2019,$201c,$201d,$2022,$2013,$2014,
        $02dc,$2122,$0161,$203a,$0153,$fffd,$017e,$0178,
        $00A0,$00A1,$00A2,$00A3,$00A4,$00A5,$00A6,$00A7,
        $00A8,$00A9,$00AA,$00AB,$00AC,$00AD,$00AE,$00AF,
        $00B0,$00B1,$00B2,$00B3,$00B4,$00B5,$00B6,$00B7,
        $00B8,$00B9,$00BA,$00BB,$00BC,$00BD,$00BE,$00BF,
        $00C0,$00C1,$00C2,$00C3,$00C4,$00C5,$00C6,$00C7,
        $00C8,$00C9,$00CA,$00CB,$00CC,$00CD,$00CE,$00CF,
        $00D0,$00D1,$00D2,$00D3,$00D4,$00D5,$00D6,$00D7,
        $00D8,$00D9,$00DA,$00DB,$00DC,$00DD,$00DE,$00DF,
        $00E0,$00E1,$00E2,$00E3,$00E4,$00E5,$00E6,$00E7,
        $00E8,$00E9,$00EA,$00EB,$00EC,$00ED,$00EE,$00EF,
        $00F0,$00F1,$00F2,$00F3,$00F4,$00F5,$00F6,$00F7,
        $00F8,$00F9,$00FA,$00FB,$00FC,$00FD,$00FE,$00FF);

      CharCP_1253:TCharsetTable=
       ($20ac,$fffd,$201a,$0192,$201e,$2026,$2020,$2021,
        $fffd,$2030,$fffd,$2039,$fffd,$fffd,$fffd,$fffd,
        $fffd,$2018,$2019,$201c,$201d,$2022,$2013,$2014,
        $fffd,$2122,$fffd,$203a,$fffd,$fffd,$fffd,$fffd,
        $00a0,$0385,$0386,$00a3,$00a4,$00a5,$00a6,$00a7,
        $00a8,$00a9,$fffd,$00ab,$00ac,$00ad,$00ae,$2015,
        $00b0,$00b1,$00b2,$00b3,$0384,$00b5,$00b6,$00b7,
        $0388,$0389,$038a,$00bb,$038c,$00bd,$038e,$038f,
        $0390,$0391,$0392,$0393,$0394,$0395,$0396,$0397,
        $0398,$0399,$039a,$039b,$039c,$039d,$039e,$039f,
        $03a0,$03a1,$fffd,$03a3,$03a4,$03a5,$03a6,$03a7,
        $03a8,$03a9,$03aa,$03ab,$03ac,$03ad,$03ae,$03af,
        $03b0,$03b1,$03b2,$03b3,$03b4,$03b5,$03b6,$03b7,
        $03b8,$03b9,$03ba,$03bb,$03bc,$03bd,$03be,$03bf,
        $03c0,$03c1,$03c2,$03c3,$03c4,$03c5,$03c6,$03c7,
        $03c8,$03c9,$03ca,$03cb,$03cc,$03cd,$03ce,$fffd);

      CharCP_1254:TCharsetTable=
       ($20ac,$fffd,$201a,$0192,$201e,$2026,$2020,$2021,
        $02c6,$2030,$0160,$2039,$0152,$fffd,$fffd,$fffd,
        $fffd,$2018,$2019,$201c,$201d,$2022,$2013,$2014,
        $02dc,$2122,$0161,$203a,$0153,$fffd,$fffd,$0178,
        $00A0,$00A1,$00A2,$00A3,$00A4,$00A5,$00A6,$00A7,
        $00A8,$00A9,$00AA,$00AB,$00AC,$00AD,$00AE,$00AF,
        $00B0,$00B1,$00B2,$00B3,$00B4,$00B5,$00B6,$00B7,
        $00B8,$00B9,$00BA,$00BB,$00BC,$00BD,$00BE,$00BF,
        $00C0,$00C1,$00C2,$00C3,$00C4,$00C5,$00C6,$00C7,
        $00C8,$00C9,$00CA,$00CB,$00CC,$00CD,$00CE,$00CF,
        $011e,$00d1,$00d2,$00d3,$00d4,$00d5,$00d6,$00d7,
        $00d8,$00d9,$00da,$00db,$00dc,$0130,$015e,$00df,
        $00E0,$00E1,$00E2,$00E3,$00E4,$00E5,$00E6,$00E7,
        $00E8,$00E9,$00EA,$00EB,$00EC,$00ED,$00EE,$00EF,
        $011f,$00f1,$00f2,$00f3,$00f4,$00f5,$00f6,$00f7,
        $00f8,$00f9,$00fa,$00fb,$00fc,$0131,$015f,$00ff);

      CharCP_1255:TCharsetTable=
       ($20ac,$fffd,$201a,$0192,$201e,$2026,$2020,$2021,
        $02c6,$2030,$fffd,$2039,$fffd,$fffd,$fffd,$fffd,
        $fffd,$2018,$2019,$201c,$201d,$2022,$2013,$2014,
        $02dc,$2122,$fffd,$203a,$fffd,$fffd,$fffd,$fffd,
        $00a0,$00a1,$00a2,$00a3,$20aa,$00a5,$00a6,$00a7,
        $00a8,$00a9,$00d7,$00ab,$00ac,$00ad,$00ae,$00af,
        $00b0,$00b1,$00b2,$00b3,$00b4,$00b5,$00b6,$00b7,
        $00b8,$00b9,$00f7,$00bb,$00bc,$00bd,$00be,$00bf,
        $05b0,$05b1,$05b2,$05b3,$05b4,$05b5,$05b6,$05b7,
        $05b8,$05b9,$fffd,$05bb,$05bc,$05bd,$05be,$05bf,
        $05c0,$05c1,$05c2,$05c3,$05f0,$05f1,$05f2,$05f3,
        $05f4,$fffd,$fffd,$fffd,$fffd,$fffd,$fffd,$fffd,
        $05d0,$05d1,$05d2,$05d3,$05d4,$05d5,$05d6,$05d7,
        $05d8,$05d9,$05da,$05db,$05dc,$05dd,$05de,$05df,
        $05e0,$05e1,$05e2,$05e3,$05e4,$05e5,$05e6,$05e7,
        $05e8,$05e9,$05ea,$fffd,$fffd,$200e,$200f,$fffd);

      CharCP_1256:TCharsetTable=
       ($20ac,$067e,$201a,$0192,$201e,$2026,$2020,$2021,
        $02c6,$2030,$0679,$2039,$0152,$0686,$0698,$0688,
        $06af,$2018,$2019,$201c,$201d,$2022,$2013,$2014,
        $06a9,$2122,$0691,$203a,$0153,$200c,$200d,$06ba,
        $00a0,$060c,$00a2,$00a3,$00a4,$00a5,$00a6,$00a7,
        $00a8,$00a9,$06be,$00ab,$00ac,$00ad,$00ae,$00af,
        $00b0,$00b1,$00b2,$00b3,$00b4,$00b5,$00b6,$00b7,
        $00b8,$00b9,$061b,$00bb,$00bc,$00bd,$00be,$061f,
        $06c1,$0621,$0622,$0623,$0624,$0625,$0626,$0627,
        $0628,$0629,$062a,$062b,$062c,$062d,$062e,$062f,
        $0630,$0631,$0632,$0633,$0634,$0635,$0636,$00d7,
        $0637,$0638,$0639,$063a,$0640,$0641,$0642,$0643,
        $00e0,$0644,$00e2,$0645,$0646,$0647,$0648,$00e7,
        $00e8,$00e9,$00ea,$00eb,$0649,$064a,$00ee,$00ef,
        $064b,$064c,$064d,$064e,$00f4,$064f,$0650,$00f7,
        $0651,$00f9,$0652,$00fb,$00fc,$200e,$200f,$06d2);

      CharCP_1257:TCharsetTable=
       ($20ac,$fffd,$201a,$fffd,$201e,$2026,$2020,$2021,
        $fffd,$2030,$fffd,$2039,$fffd,$00a8,$02c7,$00b8,
        $fffd,$2018,$2019,$201c,$201d,$2022,$2013,$2014,
        $fffd,$2122,$fffd,$203a,$fffd,$00af,$02db,$fffd,
        $00a0,$fffd,$00a2,$00a3,$00a4,$fffd,$00a6,$00a7,
        $00d8,$00a9,$0156,$00ab,$00ac,$00ad,$00ae,$00c6,
        $00b0,$00b1,$00b2,$00b3,$00b4,$00b5,$00b6,$00b7,
        $00f8,$00b9,$0157,$00bb,$00bc,$00bd,$00be,$00e6,
        $0104,$012e,$0100,$0106,$00c4,$00c5,$0118,$0112,
        $010c,$00c9,$0179,$0116,$0122,$0136,$012a,$013b,
        $0160,$0143,$0145,$00d3,$014c,$00d5,$00d6,$00d7,
        $0172,$0141,$015a,$016a,$00dc,$017b,$017d,$00df,
        $0105,$012f,$0101,$0107,$00e4,$00e5,$0119,$0113,
        $010d,$00e9,$017a,$0117,$0123,$0137,$012b,$013c,
        $0161,$0144,$0146,$00f3,$014d,$00f5,$00f6,$00f7,
        $0173,$0142,$015b,$016b,$00fc,$017c,$017e,$02d9);

      CharCP_1258:TCharsetTable=
       ($20ac,$fffd,$201a,$0192,$201e,$2026,$2020,$2021,
        $02c6,$2030,$fffd,$2039,$0152,$fffd,$fffd,$fffd,
        $fffd,$2018,$2019,$201c,$201d,$2022,$2013,$2014,
        $02dc,$2122,$fffd,$203a,$0153,$fffd,$fffd,$0178,
        $00a0,$00a1,$00a2,$00a3,$00a4,$00a5,$00a6,$00a7,
        $00a8,$00a9,$00aa,$00ab,$00ac,$00ad,$00ae,$00af,
        $00b0,$00b1,$00b2,$00b3,$00b4,$00b5,$00b6,$00b7,
        $00b8,$00b9,$00ba,$00bb,$00bc,$00bd,$00be,$00bf,
        $00c0,$00c1,$00c2,$0102,$00c4,$00c5,$00c6,$00c7,
        $00c8,$00c9,$00ca,$00cb,$0300,$00cd,$00ce,$00cf,
        $0110,$00d1,$0309,$00d3,$00d4,$01a0,$00d6,$00d7,
        $00d8,$00d9,$00da,$00db,$00dc,$01af,$0303,$00df,
        $00e0,$00e1,$00e2,$0103,$00e4,$00e5,$00e6,$00e7,
        $00e8,$00e9,$00ea,$00eb,$0301,$00ed,$00ee,$00ef,
        $0111,$00f1,$0323,$00f3,$00f4,$01a1,$00f6,$00f7,
        $00f8,$00f9,$00fa,$00fb,$00fc,$01b0,$20ab,$00ff);

      CharKOI8_R:TCharsetTable=
       ($2500,$2502,$250c,$2510,$2514,$2518,$251c,$2524,
        $252c,$2534,$253c,$2580,$2584,$2588,$258c,$2590,
        $2591,$2592,$2593,$2320,$25a0,$2219,$221a,$2248,
        $2264,$2265,$00a0,$2321,$00b0,$00b2,$00b7,$00f7,
        $2550,$2551,$2552,$0451,$2553,$2554,$2555,$2556,
        $2557,$2558,$2559,$255a,$255b,$255c,$255d,$255e,
        $255f,$2560,$2561,$0401,$2562,$2563,$2564,$2565,
        $2566,$2567,$2568,$2569,$256a,$256b,$256c,$00a9,
        $044e,$0430,$0431,$0446,$0434,$0435,$0444,$0433,
        $0445,$0438,$0439,$043a,$043b,$043c,$043d,$043e,
        $043f,$044f,$0440,$0441,$0442,$0443,$0436,$0432,
        $044c,$044b,$0437,$0448,$044d,$0449,$0447,$044a,
        $042e,$0410,$0411,$0426,$0414,$0415,$0424,$0413,
        $0425,$0418,$0419,$041a,$041b,$041c,$041d,$041e,
        $041f,$042f,$0420,$0421,$0422,$0423,$0416,$0412,
        $042c,$042b,$0417,$0428,$042d,$0429,$0427,$042a);

const UnknownChar='_';

function GetCharsetTable(CharSet:THTMLCharset):TCharsetTable;
begin
 case CharSet of
  ISO_8859_1:result:=CharISO_8859_1;
  ISO_8859_2:result:=CharISO_8859_2;
  ISO_8859_3:result:=CharISO_8859_3;
  ISO_8859_4:result:=CharISO_8859_4;
  ISO_8859_5:result:=CharISO_8859_5;
  ISO_8859_6:result:=CharISO_8859_6;
  ISO_8859_7:result:=CharISO_8859_7;
  ISO_8859_8:result:=CharISO_8859_8;
  ISO_8859_9:result:=CharISO_8859_9;
  ISO_8859_10:result:=CharISO_8859_10;
  CP1250:result:=CharCP_1250;
  CP1251:result:=CharCP_1251;
  CP1252:result:=CharCP_1252;
  CP1253:result:=CharCP_1253;
  CP1254:result:=CharCP_1254;
  CP1255:result:=CharCP_1255;
  CP1256:result:=CharCP_1256;
  CP1257:result:=CharCP_1257;
  CP1258:result:=CharCP_1258;
  KOI8_R:result:=CharKOI8_R;
 end;
end;

function UTF8ToUCS4(Value:ansistring):ansistring;
var i,j:longint;
    b:byte;
    Buffer:array of longword;
    v:longword;
begin
 j:=0;
 i:=1;
 while i<=length(Value) do begin
  b:=byte(Value[i]);
  if (b and $80)=0 then begin
   inc(i);
   inc(j);
  end else if ((i+1)<length(Value)) and ((b and $e0)=$c0) then begin
   if (byte(Value[i+1]) and $c0)<>$80 then begin
    j:=0;
    break;
   end;
   inc(i,2);
   inc(j);
  end else if ((i+2)<length(Value)) and ((b and $f0)=$e0) then begin
   if (byte(Value[i+1]) and $c0)<>$80 then begin
    j:=0;
    break;
   end;
   if (byte(Value[i+2]) and $c0)<>$80 then begin
    j:=0;
    break;
   end;
   inc(i,3);
   inc(j);
  end else if ((i+3)<length(Value)) and ((b and $f8)=$f0) then begin
   if (byte(Value[i+1]) and $c0)<>$80 then begin
    j:=0;
    break;
   end;
   if (byte(Value[i+2]) and $c0)<>$80 then begin
    j:=0;
    break;
   end;
   if (byte(Value[i+3]) and $c0)<>$80 then begin
    j:=0;
    break;
   end;
   inc(i,4);
   inc(j);
  end else if ((i+4)<length(Value)) and ((b and $fc)=$f8) then begin
   if (byte(Value[i+1]) and $c0)<>$80 then begin
    j:=0;
    break;
   end;
   if (byte(Value[i+2]) and $c0)<>$80 then begin
    j:=0;
    break;
   end;
   if (byte(Value[i+3]) and $c0)<>$80 then begin
    j:=0;
    break;
   end;
   if (byte(Value[i+4]) and $c0)<>$80 then begin
    j:=0;
    break;
   end;
   inc(i,5);
   inc(j);
  end else if ((i+5)<length(Value)) and ((b and $fe)=$fc)then begin
   if (byte(Value[i+1]) and $c0)<>$80 then begin
    j:=0;
    break;
   end;
   if (byte(Value[i+2]) and $c0)<>$80 then begin
    j:=0;
    break;
   end;
   if (byte(Value[i+3]) and $c0)<>$80 then begin
    j:=0;
    break;
   end;
   if (byte(Value[i+4]) and $c0)<>$80 then begin
    j:=0;
    break;
   end;
   if (byte(Value[i+5]) and $c0)<>$80 then begin
    j:=0;
    break;
   end;
   inc(i,6);
   inc(j);
  end else begin
   j:=0;
   break;
  end;
 end;
 setlength(Buffer,j);
 if j=0 then begin
  exit;
 end;
 j:=0;
 i:=1;
 while i<=length(Value) do begin
  b:=byte(Value[i]);
  if (b and $80)=0 then begin
   Buffer[j]:=b;
   inc(i);
   inc(j);
  end else if ((i+1)<length(Value)) and ((b and $e0)=$c0) then begin
   Buffer[j]:=((byte(Value[i]) and $3f) shl 6) or (byte(Value[i+1]) and $3f);
   inc(i,2);
   inc(j);
  end else if ((i+2)<length(Value)) and ((b and $f0)=$e0) then begin
   Buffer[j]:=((byte(Value[i]) and $3f) shl 12) or ((byte(Value[i+1]) and $3f) shl 6) or (byte(Value[i+2]) and $3f);
   inc(i,3);
   inc(j);
  end else if ((i+3)<length(Value)) and ((b and $f8)=$f0) then begin
   Buffer[j]:=((byte(Value[i]) and $3f) shl 18) or ((byte(Value[i+1]) and $3f) shl 12) or ((byte(Value[i+2]) and $3f) shl 6) or (byte(Value[i+3]) and $3f);
   inc(i,4);
   inc(j);
  end else if ((i+4)<length(Value)) and ((b and $fc)=$f8) then begin
   Buffer[j]:=((byte(Value[i]) and $3f) shl 24) or ((byte(Value[i+1]) and $3f) shl 18) or ((byte(Value[i+2]) and $3f) shl 12) or ((byte(Value[i+3]) and $3f) shl 6) or (byte(Value[i+4]) and $3f);
   inc(i,5);
   inc(j);
  end else if ((i+5)<length(Value)) and ((b and $fe)=$fc) then begin
   Buffer[j]:=((byte(Value[i]) and $3f) shl 30) or ((byte(Value[i+1]) and $3f) shl 24) or ((byte(Value[i+2]) and $3f) shl 18) or ((byte(Value[i+3]) and $3f) shl 12) or ((byte(Value[i+4]) and $3f) shl 6) or (byte(Value[i+5]) and $3f);
   inc(i,6);
   inc(j);
  end else begin
   break;
  end;
 end;
 setlength(result,length(Buffer)*4);
 j:=1;
 for i:=0 to length(Buffer)-1 do begin
  v:=Buffer[i];
  result[j]:=chr((v shr 24) and $ff);
  inc(j);
  result[j]:=chr((v shr 16) and $ff);
  inc(j);
  result[j]:=chr((v shr 8) and $ff);
  inc(j);
  result[j]:=chr(v and $ff);
  inc(j);
 end;
 setlength(Buffer,0);
end;

function UCS4toUTF8(Value:ansistring):ansistring;
var i,j:longint;
    u4c:longword;
    s:array of longword;
begin
 setlength(s,(length(Value)+3) shr 2);
 for i:=0 to length(s)-1 do begin
  j:=(i shl 2)+1;
  u4c:=0;
  if j<=length(Value) then begin
   u4c:=u4c or (byte(Value[j]) shl 24);
   inc(j);
   if j<=length(Value) then begin
    u4c:=u4c or (byte(Value[j]) shl 16);
    inc(j);
    if j<=length(Value) then begin
     u4c:=u4c or (byte(Value[j]) shl 8);
     inc(j);
     if j<=length(Value) then begin
      u4c:=u4c or byte(Value[j]);
     end;
    end;
   end;
  end;
  s[i]:=u4c;
 end;
 result:='';
 j:=0;
 for i:=0 to length(s)-1 do begin
  u4c:=s[i];
  if u4c<=$7f then begin
   inc(j);
  end else if u4c<=$7ff then begin
   inc(j,2);
  end else if u4c<=$ffff then begin
   inc(j,3);
  end else if u4c<=$1fffff then begin
   inc(j,4);
  end else if u4c<=$3ffffff then begin
   inc(j,5);
  end else if u4c<=$7fffffff then begin
   inc(j,6);
  end;
 end;
 setlength(result,j);
 j:=1;
 for i:=0 to length(s)-1 do begin
  u4c:=s[i];
  if u4c<=$7f then begin
   result[j]:=chr(u4c);
   inc(j);
  end else if u4c<=$7ff then begin
   result[j]:=chr($c0 or (u4c shr 6));
   result[j+1]:=chr($80 or (u4c and $3f));
   inc(j,2);
  end else if u4c<=$ffff then begin
   result[j]:=chr($e0 or (u4c shr 12));
   result[j+1]:=chr($80 or ((u4c shr 6) and $3f));
   result[j+2]:=chr($80 or (u4c and $3f));
   inc(j,3);
  end else if u4c<=$1fffff then begin
   result[j]:=chr($f0 or (u4c shr 18));
   result[j+1]:=chr($80 or ((u4c shr 12) and $3f));
   result[j+2]:=chr($80 or ((u4c shr 6) and $3f));
   result[j+3]:=chr($80 or (u4c and $3f));
   inc(j,4);
  end else if u4c<=$3ffffff then begin
   result[j]:=chr($f8 or (u4c shr 24));
   result[j+1]:=chr($80 or ((u4c shr 18) and $3f));
   result[j+2]:=chr($80 or ((u4c shr 12) and $3f));
   result[j+3]:=chr($80 or ((u4c shr 6) and $3f));
   result[j+4]:=chr($80 or (u4c and $3f));
   inc(j,5);
  end else if u4c<=$7fffffff then begin
   result[j]:=chr($fc or (u4c shr 30));
   result[j+1]:=chr($80 or ((u4c shr 24) and $3f));
   result[j+2]:=chr($80 or ((u4c shr 18) and $3f));
   result[j+3]:=chr($80 or ((u4c shr 12) and $3f));
   result[j+4]:=chr($80 or ((u4c shr 6) and $3f));
   result[j+5]:=chr($80 or (u4c and $3f));
   inc(j,6);
  end;
 end;
 setlength(s,0);
end;

function UTF7toUCS2(Value:ansistring):ansistring;
var i:longint;
    c:ansichar;
    s:ansistring;
begin
 result:='';
 i:=1;
 while i<=length(Value) do begin
  c:=Value[i];
  inc(i);
  if c<>'+' then begin
   result:=result+#0+c;
  end else begin
   s:='';
   while i<=length(Value) do begin
    c:=Value[i];
    inc(i);
    if c='-' then begin
     break;
    end else if (c='=') or (pos(c,Base64Chars)<1) then begin
     dec(i);
     break;
    end;
    s:=s+c;
   end;
   if s='' then begin
    s:='+';
   end else begin
    s:=DecodeBase64(s);
   end;
   result:=result+s;
  end;
 end;
end;

function UCS2toUTF7(Value:ansistring):ansistring;
var s:ansistring;
    c1,c2:ansichar;
    i,j:longint;
begin
 result:='';
 i:=1;
 while i<=length(Value) do begin
  c2:=Value[i];
  if (i+1)<=length(Value) then begin
   c1:=Value[i+1];
  end else begin
   c1:=#0;
  end;
  inc(i,2);
  if (c2=#0) and (c1<#128) then begin
   if c1='+' then begin
    result:=result+'+-';
   end else begin
    result:=result+ansichar(c1);
   end;
  end else begin
   s:=c2+c1;
   while i<=length(Value) do begin
    c2:=Value[i];
    if (i+1)<=length(Value) then begin
     c1:=Value[i+1];
    end else begin
     c1:=#0;
    end;
    if c2=#0 then begin
     break;
    end else begin
     inc(i,2);
     s:=s+c2+c1;
    end;
   end;
   s:=EncodeBase64(s);
   j:=pos('=',s);
   if j>0 then begin
    s:=copy(s,1,j-1);
   end;
   result:=result+'+'+s+'-';
  end;
 end;
end;

function EncodeString(Value:ansistring;CharFrom:THTMLCharset;CharTo:THTMLCharset):ansistring;
var Unicode:word;
    i,j:longint;
    b:byte;
    c1,c2,c3,c4:ansichar;
    SourceTable,TargetTable:TCharsetTable;
    FromByteCount,ToByteCount:byte;
begin
 if CharFrom=CharTo then begin
  result:=Value;
 end else begin
  SourceTable:=GetCharsetTable(CharFrom);
  TargetTable:=GetCharsetTable(CharTo);
  if CharFrom in [UCS_2,UTF_7] then begin
   FromByteCount:=2;
  end else if CharFrom in [UCS_4,UTF_8] then begin
   FromByteCount:=4;
  end else begin
   FromByteCount:=1;
  end;
  if CharTo in [UCS_2,UTF_7] then begin
   ToByteCount:=2;
  end else if CharTo in [UCS_4,UTF_8] then begin
   ToByteCount:=4;
  end else begin
   ToByteCount:=1;
  end;
  case CharFrom of
   UTF_7:begin
    Value:=UTF7toUCS2(Value);
   end;
   UTF_8:begin
    Value:=UTF8ToUCS4(Value);
   end;
  end;
  c1:=#0;
  c2:=#0;
  c3:=#0;
  c4:=#0;
  result:='';
  i:=1;
  while i<=length(Value) do begin
   case FromByteCount of
    1:begin
     c1:=Value[i];
     if c1>#127 then begin
      Unicode:=SourceTable[byte(c1)];
      c1:=chr(Unicode and $ff);
      c2:=chr(Unicode shr 8);
     end;
     inc(i);
    end;
    2:begin
     c2:=Value[i];
     if (i+1)<=length(Value) then begin
      c1:=Value[i+1];
     end else begin
      c1:=#0;
     end;
     inc(i,2);
    end;
    3:begin
     c3:=Value[i];
     if (i+1)<=length(Value) then begin
      c2:=Value[i+1];
     end else begin
      c2:=#0;
     end;
     if (i+2)<=length(Value) then begin
      c1:=Value[i+2];
     end else begin
      c1:=#0;
     end;
     inc(i,3);
    end;
    4:begin
     c4:=Value[i];
     if (i+1)<=length(Value) then begin
      c3:=Value[i+1];
     end else begin
      c3:=#0;
     end;
     if (i+2)<=length(Value) then begin
      c2:=Value[i+2];
     end else begin
      c2:=#0;
     end;
     if (i+3)<=length(Value) then begin
      c1:=Value[i+3];
     end else begin
      c1:=#0;
     end;
     inc(i,4);
    end;
   end;
   Unicode:=(byte(c2) shl 8) or byte(c1);
   if (c3<>#0) or (c4<>#0) then begin
    c1:=UnknownChar;
    c2:=#0;
    c3:=#0;
    c4:=#0;
   end else if ToByteCount=1 then begin
    if Unicode>127 then begin
     b:=ord(UnknownChar);
     for j:=128 to 255 do begin
      if TargetTable[j]=Unicode then begin
       b:=j;
       break;
      end;
     end;
     c1:=chr(b);
     c2:=#0;
    end else begin
     c1:=chr(Unicode and $ff);
    end;
   end;
   case ToByteCount of
    1:begin
     result:=result+c1;
    end;
    2:begin
     result:=result+c2+c1;
    end;
    3:begin
     result:=result+c3+c2+c1;
    end;
    4:begin
     result:=result+c4+c3+c2+c1;
    end;
   end;
  end;
  case CharTo of
   UTF_7:begin
    result:=UCS2toUTF7(result);
   end;
   UTF_8:begin
    result:=UCS4toUTF8(result);
   end;
  end;
 end;
end;

function GetCodePage(Value:ansistring):THTMLCharset;
begin
 Value:=uppercase(Value);
 if pos('ISO-8859-10',Value)>0 then begin
  result:=ISO_8859_10;
 end else if pos('ISO-8859-1',Value)>0 then begin
  result:=ISO_8859_1;
 end else if pos('ISO-8859-2',Value)>0 then begin
  result:=ISO_8859_2;
 end else if pos('ISO-8859-3',Value)>0 then begin
  result:=ISO_8859_3;
 end else if pos('ISO-8859-4',Value)>0 then begin
  result:=ISO_8859_4;
 end else if pos('ISO-8859-5',Value)>0 then begin
  result:=ISO_8859_5;
 end else if pos('ISO-8859-6',Value)>0 then begin
  result:=ISO_8859_6;
 end else if pos('ISO-8859-7',Value)>0 then begin
  result:=ISO_8859_7;
 end else if pos('ISO-8859-8',Value)>0 then begin
  result:=ISO_8859_8;
 end else if pos('ISO-8859-9',Value)>0 then begin
  result:=ISO_8859_9;
 end else if (pos('WINDOWS-1250',Value)>0) or (pos('X-CP1250',Value)>0) then begin
  result:=CP1250;
 end else if (pos('WINDOWS-1251',Value)>0) or (pos('X-CP1251',Value)>0) then begin
  result:=CP1251;
 end else if (pos('WINDOWS-1252',Value)>0) or (pos('X-CP1252',Value)>0) then begin
  result:=CP1252;
 end else if (pos('WINDOWS-1253',Value)>0) or (pos('X-CP1253',Value)>0) then begin
  result:=CP1253;
 end else if (pos('WINDOWS-1254',Value)>0) or (pos('X-CP1254',Value)>0) then begin
  result:=CP1254;
 end else if (pos('WINDOWS-1255',Value)>0) or (pos('X-CP1255',Value)>0) then begin
  result:=CP1255;
 end else if (pos('WINDOWS-1256',Value)>0) or (pos('X-CP1256',Value)>0) then begin
  result:=CP1256;
 end else if (pos('WINDOWS-1257',Value)>0) or (pos('X-CP1257',Value)>0) then begin
  result:=CP1257;
 end else if (pos('WINDOWS-1258',Value)>0) or (pos('X-CP1258',Value)>0) then begin
  result:=CP1258;
 end else if pos('KOI8-R',Value)>0 then begin
  result:=KOI8_R;
 end else if pos('UTF-7',Value)>0 then begin
  result:=UTF_7;
 end else if pos('UTF-8',Value)>0 then begin
  result:=UTF_8;
 end else if pos('UCS-4',Value)>0 then begin
  result:=UCS_4;
 end else if pos('UCS-2',Value)>0 then begin
  result:=UCS_2;
 end else if pos('UNICODE',Value)>0 then begin
  result:=UCS_2;
 end else begin
  result:=ISO_8859_1;
 end;
end;

function GetCodePageID(Value:THTMLCharset):ansistring;
begin
 case Value of
  ISO_8859_2:result:='ISO-8859-2';
  ISO_8859_3:result:='ISO-8859-3';
  ISO_8859_4:result:='ISO-8859-4';
  ISO_8859_5:result:='ISO-8859-5';
  ISO_8859_6:result:='ISO-8859-6';
  ISO_8859_7:result:='ISO-8859-7';
  ISO_8859_8:result:='ISO-8859-8';
  ISO_8859_9:result:='ISO-8859-9';
  ISO_8859_10:result:='ISO-8859-10';
  CP1250:result:='WINDOWS-1250';
  CP1251:result:='WINDOWS-1251';
  CP1252:result:='WINDOWS-1252';
  CP1253:result:='WINDOWS-1253';
  CP1254:result:='WINDOWS-1254';
  CP1255:result:='WINDOWS-1255';
  CP1256:result:='WINDOWS-1256';
  CP1257:result:='WINDOWS-1257';
  CP1258:result:='WINDOWS-1258';
  KOI8_R:result:='KOI8-R';
  UCS_2:result:='Unicode-1-1-UCS-2';
  UCS_4:result:='Unicode-1-1-UCS-4';
  UTF_8:result:='UTF-8';
  UTF_7:result:='UTF-7';
  else result:='ISO-8859-1';
 end;
end;

function DoNeedEncoding(Value:ansistring):boolean;
var i:longint;
begin
 result:=false;
 for i:=1 to length(Value) do begin
  if ord(Value[i])>127 then begin
   result:=true;
   break;
  end;
 end;
end;

function FindIdealCoding(Value:ansistring;CharFrom:THTMLCharset;CharTo:TCharsetSet):THTMLCharset;
var cs:THTMLCharset;
    i,j,k:longint;
    s,t:ansistring;
begin
 result:=ISO_8859_1;
 s:='';
 for i:=1 to length(Value) do begin
  if ord(Value[i])>127 then begin
   s:=s+Value[i];
  end;
 end;
 j:=128;
 for cs:=low(THTMLCharset) to high(THTMLCharset) do begin
  if cs in CharTo then begin
   t:=EncodeString(s,CharFrom,cs);
   k:=0;
   for i:=1 to length(t) do begin
    if t[i]=UnknownChar then begin
     inc(k);
    end;
   end;
   if k<j then begin
    j:=k;
    result:=cs;
    if k=0 then begin
     break;
    end;
   end;
  end;
 end;
end;

function ISOToUTF8(s:ansistring):ansistring;
var q,us,e:ansistring;
    encode:ansichar;
    p1,p2,p3:longint;
    cs:THTMLCharset;
begin
 result:='';
 us:=uppercase(s);
 p1:=pos('=?ISO',us);
 while p1>0 do begin
  q:=copy(s,p1+2,length(s));
  p2:=pos('?',q);
  if (p2=0) or (p2>=length(q)-2) or (q[p2+2]<>'?') then begin
   break;
  end;
  e:=copy(q,1,p2-1);
  cs:=GetCodePage(e);
  encode:=ansichar(upcase(ansichar(q[p2+1])));
  q:=copy(q,p2+3,length(q));
  p3:=pos('?=',q);
  if p3=0 then begin
   break;
  end;
  setlength(q,p3-1);
  if encode='B' then begin
   q:=DecodeBase64(q);
  end else if encode='Q' then begin
   q:=Dequote(q+'=');
  end else begin
   break;
  end;
  q:=EncodeString(q,cs,UTF_8);
  result:=result+copy(s,1,p1-1)+q;
  inc(p1,2+p2+2+p3);
  delete(s,1,p1);
  delete(us,1,p1);
  p1:=pos('=?ISO',us);
 end;
 p1:=pos('=?UTF-7',us);
 while p1>0 do begin
  q:=copy(s,p1+2,length(s));
  p2:=pos('?',q);
  if (p2=0) or (p2>=length(q)-2) or (q[p2+2]<>'?') then begin
   break;
  end;
  encode:=ansichar(upcase(ansichar(q[p2+1])));
  q:=copy(q,p2+3,length(q));
  p3:=pos('?=',q); if p3=0 then break;
  setlength(q,p3-1);
  if encode='B' then begin
   q:=DecodeBase64(q);
  end else if encode='Q' then begin
   q:=Dequote(q+'=');
  end else begin
   break;
  end;
  q:=EncodeString(s,UTF_7,UTF_8);
  result:=result+copy(s,1,p1-1)+q;
  inc(p1,2+p2+2+p3);
  delete(s,1,p1);
  delete(us,1,p1);
  p1:=pos('=?UTF-7',us);
 end;
 p1:=pos('=?UTF-8',us);
 while p1>0 do begin
  q:=copy(s,p1+2,length(s));
  p2:=pos('?',q);
  if (p2=0) or (p2>=length(q)-2) or (q[p2+2]<>'?') then begin
   break;
  end;
  encode:=ansichar(upcase(ansichar(q[p2+1])));
  q:=copy(q,p2+3,length(q));
  p3:=pos('?=',q); if p3=0 then break;
  setlength(q,p3-1);
  if encode='B' then begin
   q:=DecodeBase64(q);
  end else if encode='Q' then begin
   q:=Dequote(q+'=');
  end else begin
   break;
  end;
  result:=result+copy(s,1,p1-1)+q;
  inc(p1,2+p2+2+p3);
  delete(s,1,p1);
  delete(us,1,p1);
  p1:=pos('=?UTF-8',us);
 end;
 p1:=pos('=?',us);
 while p1>0 do begin
  q:=copy(s,p1+2,length(s));
  p2:=pos('?',q);
  if (p2=0) or (p2>=length(q)-2) or (q[p2+2]<>'?') then begin
   break;
  end;
  e:=copy(q,1,p2-1);
  cs:=GetCodePage(e);
  if cs=ISO_8859_1 then begin
   result:=result+'=?';
   delete(s,1,p1);
   delete(us,1,p1);
   p1:=pos('=?',us);
   continue;
  end;
  encode:=ansichar(upcase(ansichar(q[p2+1])));
  q:=copy(q,p2+3,length(q));
  p3:=pos('?=',q);
  if p3=0 then begin
   break;
  end;
  setlength(q,p3-1);
  if encode='B' then begin
   q:=DecodeBase64(q);
  end else if encode='Q' then begin
   q:=Dequote(q+'=');
  end else begin
   break;
  end;
  q:=EncodeString(q,cs,UTF_8);
  result:=result+copy(s,1,p1-1)+q;
  inc(p1,2+p2+2+p3);
  delete(s,1,p1);
  delete(us,1,p1);
  p1:=pos('=?',us);
 end;
 result:=result+EncodeString(s,ISO_8859_1,UTF_8);
end;

const ustNOUTF8=0;
      ustPOSSIBLEUTF8=1;
      ustISUTF8=2;

      usmcACCEPT=0;
      usmcERROR=16;

type TUCS4Char=longint;

     TUTF8String=ansistring;

     TUTF8Chars=array[ansichar] of byte;

     TUTF8Bytes=array[byte] of byte;

{$ifdef StrictUTF8}
                              //0 1 2 3 4 5 6 7 8 9 a b c d e f
const UTF8CharSteps:TUTF8Chars=(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 0
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 1
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 2
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 3
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 4
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 5
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 6
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 7
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 8
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 9
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // a
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // b
                                1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  // c
                                2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  // d
                                3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,  // e
                                4,4,4,4,4,1,1,1,1,1,1,1,1,1,1,1); // f
                              //0 1 2 3 4 5 6 7 8 9 a b c d e f

{$else}
                              //0 1 2 3 4 5 6 7 8 9 a b c d e f
const UTF8CharSteps:TUTF8Chars=(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 0
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 1
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 2
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 3
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 4
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 5
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 6
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 7
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 8
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 9
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // a
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // b
                                2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  // c
                                2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  // d
                                3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,  // e
                                4,4,4,4,4,4,4,4,5,5,5,5,6,6,1,1); // f
                              //0 1 2 3 4 5 6 7 8 9 a b c d e f

{$endif}

var UTF8DFACharClasses:TUTF8Chars;
    UTF8DFATransitions:TUTF8Bytes;

function UTF32CharToUTF8(CharValue:longword):ansistring;
var Data:array[0..{$ifdef strictutf8}3{$else}5{$endif}] of ansichar;
    ResultLen:longint;
begin
 if CharValue=0 then begin
  result:=#0;
 end else begin
  if CharValue<=$7f then begin
   Data[0]:=ansichar(byte(CharValue));
   ResultLen:=1;
  end else if CharValue<=$7ff then begin
   Data[0]:=ansichar(byte($c0 or ((CharValue shr 6) and $1f)));
   Data[1]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=2;
{$ifdef strictutf8}
  end else if CharValue<=$d7ff then begin
   Data[0]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[2]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=3;
  end else if CharValue<=$dfff then begin
   Data[0]:=#$ef; // $fffd
   Data[1]:=#$bf;
   Data[2]:=#$bd;
   ResultLen:=3;
{$endif}
  end else if CharValue<=$ffff then begin
   Data[0]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[2]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=3;
  end else if CharValue<=$1fffff then begin
   Data[0]:=ansichar(byte($f0 or ((CharValue shr 18) and $07)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
   Data[2]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[3]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=4;
{$ifndef strictutf8}
  end else if CharValue<=$3ffffff then begin
   Data[0]:=ansichar(byte($f8 or ((CharValue shr 24) and $03)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
   Data[2]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
   Data[3]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[4]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=5;
  end else if CharValue<=$7fffffff then begin
   Data[0]:=ansichar(byte($fc or ((CharValue shr 30) and $01)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 24) and $3f)));
   Data[2]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
   Data[3]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
   Data[4]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[5]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=6;
{$endif}
  end else begin
   Data[0]:=#$ef; // $fffd
   Data[1]:=#$bf;
   Data[2]:=#$bd;
   ResultLen:=3;
  end;
  SetString(result,pansichar(@Data[0]),ResultLen);
 end;
end;

function UTF32CharToUTF8At(CharValue:longword;var s:ansistring;Index:longint):longint;
var Data:array[0..{$ifdef strictutf8}3{$else}5{$endif}] of ansichar;
    ResultLen:longint;
begin
 if CharValue=0 then begin
  result:=0;
 end else begin
  if CharValue<=$7f then begin
   Data[0]:=ansichar(byte(CharValue));
   ResultLen:=1;
  end else if CharValue<=$7ff then begin
   Data[0]:=ansichar(byte($c0 or ((CharValue shr 6) and $1f)));
   Data[1]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=2;
{$ifdef strictutf8}
  end else if CharValue<=$d7ff then begin
   Data[0]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[2]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=3;
  end else if CharValue<=$dfff then begin
   Data[0]:=#$ef; // $fffd
   Data[1]:=#$bf;
   Data[2]:=#$bd;
   ResultLen:=3;
{$endif}
  end else if CharValue<=$ffff then begin
   Data[0]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[2]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=3;
  end else if CharValue<=$1fffff then begin
   Data[0]:=ansichar(byte($f0 or ((CharValue shr 18) and $07)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
   Data[2]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[3]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=4;
{$ifndef strictutf8}
  end else if CharValue<=$3ffffff then begin
   Data[0]:=ansichar(byte($f8 or ((CharValue shr 24) and $03)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
   Data[2]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
   Data[3]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[4]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=5;
  end else if CharValue<=$7fffffff then begin
   Data[0]:=ansichar(byte($fc or ((CharValue shr 30) and $01)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 24) and $3f)));
   Data[2]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
   Data[3]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
   Data[4]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[5]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=6;
{$endif}
  end else begin
   Data[0]:=#$ef; // $fffd
   Data[1]:=#$bf;
   Data[2]:=#$bd;
   ResultLen:=3;
  end;
  if (Index+ResultLen)>length(s) then begin
   SetLength(s,Index+ResultLen);
  end;
  Move(Data[0],s[Index],ResultLen);
  result:=ResultLen;
 end;
end;

function UTF32CharToUTF8Len(CharValue:longword):longint;
begin
 if CharValue<=$7f then begin
  result:=1;
 end else if CharValue<=$7ff then begin
  result:=2;
 end else if CharValue<=$ffff then begin
  result:=3;
 end else if CharValue<=$1fffff then begin
  result:=4;
{$ifndef strictutf8}
 end else if CharValue<=$3ffffff then begin
  result:=5;
 end else if CharValue<=$7fffffff then begin
  result:=6;
{$endif}
 end else begin
  result:=3;
 end;
end;

function IsUTF8(const s:ansistring):boolean;
var CodeUnit,CodePoints:longint;
    State:longword;
begin
 State:=usmcACCEPT;
 CodePoints:=0;
 for CodeUnit:=1 to length(s) do begin
  State:=UTF8DFATransitions[State+UTF8DFACharClasses[s[CodeUnit]]];
  case State of
   usmcACCEPT:begin
    inc(CodePoints);
   end;
   usmcERROR:begin
    result:=false;
    exit;
   end;
  end;
 end;
 result:=(State=usmcACCEPT) and (length(s)<>CodePoints);
end;

function ValidateUTF8(const s:ansistring):boolean;
var CodeUnit:longint;
    State:longword;
begin
 State:=usmcACCEPT;
 for CodeUnit:=1 to length(s) do begin
  State:=UTF8DFATransitions[State+UTF8DFACharClasses[s[CodeUnit]]];
  if State=usmcERROR then begin
   result:=false;
   exit;
  end;
 end;
 result:=State=usmcACCEPT;
end;

function GetUTF8(const s:ansistring):longint;
var CodeUnit,CodePoints:longint;
    State:longword;
begin
 State:=usmcACCEPT;
 CodePoints:=0;
 for CodeUnit:=1 to length(s) do begin
  State:=UTF8DFATransitions[State+UTF8DFACharClasses[s[CodeUnit]]];
  case State of
   usmcACCEPT:begin
    inc(CodePoints);
   end;
   usmcERROR:begin
    result:=ustNOUTF8;
    exit;
   end;
  end;
 end;
 if State=usmcACCEPT then begin
  if length(s)<>CodePoints then begin
   result:=ustISUTF8;
  end else begin
   result:=ustPOSSIBLEUTF8;
  end;
 end else begin
  result:=ustNOUTF8;
 end;
end;

procedure UTF8SafeInc(const s:ansistring;var CodeUnit:longint);
var Len:longint;
    StartCodeUnit,State:longword;
begin
 Len:=length(s);
 if CodeUnit>0 then begin
  StartCodeUnit:=CodeUnit;
  State:=usmcACCEPT;
  while CodeUnit<=Len do begin
   State:=UTF8DFATransitions[State+UTF8DFACharClasses[s[CodeUnit]]];
   inc(CodeUnit);
   if State<=usmcERROR then begin
    break;
   end;
  end;
  if State<>usmcACCEPT then begin
   CodeUnit:=StartCodeUnit+1;
  end;
 end;
end;

procedure UTF8Inc(const s:ansistring;var CodeUnit:longint);
begin
 if (CodeUnit>0) and (CodeUnit<=length(s)) then begin
  inc(CodeUnit,UTF8CharSteps[s[CodeUnit]]);
 end;
end;

procedure UTF8Dec(const s:ansistring;var CodeUnit:longint);
begin
 if (CodeUnit>=1) and (CodeUnit<=(length(s)+1)) then begin
  dec(CodeUnit);
  while CodeUnit>0 do begin
   if s[CodeUnit] in [#$80..#$bf] then begin
    dec(CodeUnit);
   end else begin
    break;
   end;
  end;
 end;
end;

procedure UTF8Delete(var s:ansistring;CodeUnit:longint);
begin
 if (CodeUnit>=1) and (CodeUnit<=length(s)) then begin
  Delete(s,CodeUnit,1);
  while ((CodeUnit>=1) and (CodeUnit<=length(s))) and (s[CodeUnit] in [#$80..#$bf]) do begin
   Delete(s,CodeUnit,1);
  end;
 end;
end;

function UTF8Length(const s:ansistring):longint;
{$ifdef cpu386} assembler; register;
asm
 test eax,eax
 jz @End
  push esi
   cld
   mov esi,eax
   mov ecx,dword ptr [esi-4]
   xor edx,edx
   jecxz @LoopEnd
    @Loop:
      lodsb
      shl al,1
      js @IsASCIICharOrUTF8Begin
      jc @IsUTF8Part
      @IsASCIICharOrUTF8Begin:
       inc edx
      @IsUTF8Part:
     dec ecx
    jnz @Loop
   @LoopEnd:
   mov eax,edx
  pop esi
 @End:
end;
{$else}
var CodeUnit:longint;
begin
 result:=0;
 for CodeUnit:=1 to length(s) do begin
  if (byte(s[CodeUnit]) and $c0)<>$80 then begin
   inc(result);
  end;
 end;
end;
{$endif}

function UTF8LengthEx(const s:ansistring):longint;
var State:longword;
    CodeUnit:longint;
begin
 result:=0;
 State:=usmcACCEPT;
 for CodeUnit:=1 to length(s) do begin
  State:=UTF8DFATransitions[State+UTF8DFACharClasses[s[CodeUnit]]];
  case State of
   usmcACCEPT:begin
    inc(result);
   end;
   usmcERROR:begin
    result:=0;
    exit;
   end;
  end;
 end;
 if State=usmcERROR then begin
  result:=0;
 end;
end;

function UTF8GetCodePoint(const s:ansistring;CodeUnit:longint):longint;
var CurrentCodeUnit,Len:longint;
begin
 if CodeUnit<1 then begin
  result:=-1;
 end else begin
  result:=0;
  CurrentCodeUnit:=1;
  Len:=length(s);
  while (CurrentCodeUnit<=Len) and (CurrentCodeUnit<>CodeUnit) do begin
   inc(result);
   inc(CurrentCodeUnit,UTF8CharSteps[s[CurrentCodeUnit]]);
  end;
 end;
end;

function UTF8GetCodeUnit(const s:ansistring;CodePoint:longint):longint;
var CurrentCodePoint,Len:longint;
begin
 if CodePoint<0 then begin
  result:=0;
 end else begin
  result:=1;
  CurrentCodePoint:=0;
  Len:=length(s);
  while (result<=Len) and (CurrentCodePoint<>CodePoint) do begin
   inc(CurrentCodePoint);
   inc(result,UTF8CharSteps[s[result]]);
  end;
 end;
end;

function UTF8CodeUnitGetChar(const s:ansistring;CodeUnit:longint):longword;
var Value,CharClass,State:longword;
begin
 result:=0;
 if (CodeUnit>0) and (CodeUnit<=length(s)) then begin
  State:=usmcACCEPT;
  for CodeUnit:=CodeUnit to length(s) do begin
   Value:=byte(ansichar(s[CodeUnit]));
   CharClass:=UTF8DFACharClasses[ansichar(Value)];
   if State=usmcACCEPT then begin
    result:=Value and ($ff shr CharClass);
   end else begin
    result:=(result shl 6) or (Value and $3f);
   end;
   State:=UTF8DFATransitions[State+CharClass];
   if State<=usmcERROR then begin
    break;
   end;
  end;
  if State<>usmcACCEPT then begin
   result:=$fffd;
  end;
 end;
end;

function UTF8CodeUnitGetCharAndInc(const s:ansistring;var CodeUnit:longint):longword;
var Len:longint;
    Value,CharClass,State:longword;
begin
 result:=0;
 Len:=length(s);
 if (CodeUnit>0) and (CodeUnit<=Len) then begin
  State:=usmcACCEPT;
  while CodeUnit<=Len do begin
   Value:=byte(ansichar(s[CodeUnit]));
   inc(CodeUnit);
   CharClass:=UTF8DFACharClasses[ansichar(Value)];
   if State=usmcACCEPT then begin
    result:=Value and ($ff shr CharClass);
   end else begin
    result:=(result shl 6) or (Value and $3f);
   end;
   State:=UTF8DFATransitions[State+CharClass];
   if State<=usmcERROR then begin
    break;
   end;
  end;
  if State<>usmcACCEPT then begin
   result:=$fffd;
  end;
 end;
end;

function UTF8CodeUnitGetCharFallback(const s:ansistring;CodeUnit:longint):longword;
var Len:longint;
    StartCodeUnit,Value,CharClass,State:longword;
begin
 result:=0;
 Len:=length(s);
 if (CodeUnit>0) and (CodeUnit<=Len) then begin
  StartCodeUnit:=CodeUnit;
  State:=usmcACCEPT;
  while CodeUnit<=Len do begin
   Value:=byte(ansichar(s[CodeUnit]));
   inc(CodeUnit);
   CharClass:=UTF8DFACharClasses[ansichar(Value)];
   if State=usmcACCEPT then begin
    result:=Value and ($ff shr CharClass);
   end else begin
    result:=(result shl 6) or (Value and $3f);
   end;
   State:=UTF8DFATransitions[State+CharClass];
   if State<=usmcERROR then begin
    break;
   end;
  end;
  if State<>usmcACCEPT then begin
   result:=byte(ansichar(s[StartCodeUnit]));
  end;
 end;
end;

function UTF8CodeUnitGetCharAndIncFallback(const s:ansistring;var CodeUnit:longint):longword;
var Len:longint;
    StartCodeUnit,Value,CharClass,State:longword;
begin
 result:=0;
 Len:=length(s);
 if (CodeUnit>0) and (CodeUnit<=Len) then begin
  StartCodeUnit:=CodeUnit;
  State:=usmcACCEPT;
  while CodeUnit<=Len do begin
   Value:=byte(ansichar(s[CodeUnit]));
   inc(CodeUnit);
   CharClass:=UTF8DFACharClasses[ansichar(Value)];
   if State=usmcACCEPT then begin
    result:=Value and ($ff shr CharClass);
   end else begin
    result:=(result shl 6) or (Value and $3f);
   end;
   State:=UTF8DFATransitions[State+CharClass];
   if State<=usmcERROR then begin
    break;
   end;
  end;
  if State<>usmcACCEPT then begin
   result:=byte(ansichar(s[StartCodeUnit]));
   CodeUnit:=StartCodeUnit+1;
  end;
 end;
end;

function UTF8CodePointGetChar(const s:ansistring;CodePoint:longint;Fallback:boolean=false):longword;
begin
 result:=UTF8CodeUnitGetChar(s,UTF8GetCodeUnit(s,CodePoint));
end;

function UTF8GetCharLen(const s:ansistring;i:longint):longword;
begin
 if (i>0) and (i<=length(s)) then begin
  result:=UTF8CharSteps[s[i]];
 end else begin
  result:=0;
 end;
end;

function UTF8Pos(const FindStr,InStr:ansistring):longint;
var i,j,l:longint;
    ok:boolean;
begin
 result:=0;
 i:=1;
 while i<=length(InStr) do begin
  l:=i+length(FindStr)-1;
  if l>length(InStr) then begin
   exit;
  end;
  ok:=true;
  for j:=1 to length(FindStr) do begin
   if InStr[i+j-1]<>FindStr[j] then begin
    ok:=false;
    break;
   end;
  end;
  if ok then begin
   result:=i;
   exit;
  end;
  inc(i,UTF8CharSteps[InStr[i]]);
 end;
end;

function UTF8Copy(const Str:ansistring;Start,Len:longint):ansistring;
var CodeUnit:longint;
begin
 result:='';
 CodeUnit:=1;
 while (CodeUnit<=length(Str)) and (Start>0) do begin
  inc(CodeUnit,UTF8CharSteps[Str[CodeUnit]]);
  dec(Start);
 end;
 if Start=0 then begin
  Start:=CodeUnit;
  while (CodeUnit<=length(Str)) and (Len>0) do begin
   inc(CodeUnit,UTF8CharSteps[Str[CodeUnit]]);
   dec(Len);
  end;
  if Start<CodeUnit then begin
   result:=copy(Str,Start,CodeUnit-Start);
  end;
 end;
end;

const Entities:array[0..100,0..1] of ansistring=
       (('&quot;','&#34;'),('&amp;','&#38;'),('&lt;','&#60;'),('&gt;', '&#62;'),
        ('&nbsp;','&#160;'),('&iexcl;','&#161;'),('&cent;','&#162;'),
        ('&pound;','&#163;'),('&curren;','&#164;'),('&yen;','&#165;'),
        ('&brvbar;','&#166;'),('&sect;','&#167;'),('&uml;','&#168;'),
        ('&copy;','&#169;'),('&ordf;','&#170;'),('&laquo;','&#171;'),
        ('&not;','&#172;'),('&shy;','&#173;'),
        ('&reg;','&#174;'),('&macr;','&#175;'),('&deg;','&#176;'),
        ('&plusmn;','&#177;'),('&sup2;','&#178;'),('&sup3;','&#179;'),
        ('&acute;','&#180;'),('&micro;','&#181;'),('&para;','&#182;'),
        ('&middot;','&#183;'),('&cedil;','&#184;'),('&sup1;','&#185;'),
        ('&ordm;','&#186;'),('&raquo;','&#187;'),('&frac14;','&#188;'),
        ('&frac12;','&#189;'),('&frac34;','&#190;'),('&iquest;','&#191;'),
        ('&Agrave;','&#192;'),('&Aacute;','&#193;'),('&Acirc;','&#194;'),
        ('&Atilde;','&#195;'),('&Auml;','&#196;'),('&Aring;','&#197;'),
        ('&AElig;','&#198;'),('&Ccedil;','&#199;'),('&Egrave;','&#200;'),
        ('&Eacute;','&#201;'),('&Ecirc;','&#202;'),('&Euml;','&#203;'),
        ('&Igrave;','&#204;'),('&Iacute;','&#205;'),('&Icirc;','&#206;'),
        ('&Iuml;','&#207;'),('&ETH;','&#208;'),('&Ntilde;','&#209;'),
        ('&Ograve;','&#210;'),('&Oacute;','&#211;'),('&Ocirc;','&#212;'),
        ('&Otilde;','&#213;'),('&Ouml;','&#214;'),('&times;','&#215;'),
        ('&Oslash;','&#216;'),('&Ugrave;','&#217;'),('&Uacute;','&#218;'),
        ('&Ucirc;','&#219;'),('&Uuml;','&#220;'),('&Yacute;','&#221;'),
        ('&THORN;','&#222;'),('&szlig;','&#223;'),('&agrave;','&#224;'),
        ('&aacute;','&#225;'),('&acirc;','&#226;'),('&atilde;','&#227;'),
        ('&auml;','&#228;'),('&aring;','&#229;'),('&aelig;','&#230;'),
        ('&ccedil;','&#231;'),('&egrave;','&#232;'),('&eacute;','&#233;'),
        ('&ecirc;','&#234;'),('&euml;','&#235;'),('&igrave;','&#236;'),
        ('&iacute;','&#237;'),('&icirc;','&#238;'),('&iuml;','&#239;'),
        ('&eth;','&#240;'),('&ntilde;','&#241;'),('&ograve;','&#242;'),
        ('&oacute;','&#243;'),('&ocirc;','&#244;'),('&otilde;','&#245;'),
        ('&ouml;','&#246;'),('&divide;','&#247;'),('&oslash;','&#248;'),
        ('&ugrave;','&#249;'),('&uacute;','&#250;'),('&ucirc;','&#251;'),
        ('&uuml;','&#252;'),('&yacute;','&#253;'),('&thorn;','&#254;'),
        ('&yuml;','&#255;'),('&euro;','&#x20ac;'));

      Charset8bit:array[byte] of ansichar=
       (' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',
        ' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ','!',
        '"','#','$','%','&','''','(',')','*','+',',','-','.','/','0','1','2',
        '3','4','5','6','7','8','9',':',';','<','=','>','?','@','A','B','C',
        'D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T',
        'U','V','W','X','Y','Z','[','\',']','^','_','`','a','b','c','d','e',
        'f','g','h','i','j','k','l','m','n','o','p','q','r','S','t','u','v',
        'w','X','Y','z','{','|','}','~','','Ä','Å','Ç','É','Ñ','Ö','Ü','á','à','â','ä','ã','å','ç','é',
        'è','ê','ë','í','ì','î','ï','ñ','ó','ò','ô','ö','õ','ú','ù','û','ü',' ','°',
        '¢','£','§','•','¶','ß','®','©','™','´','¨','≠','Æ','Ø','∞','±','≤',
        '≥','¥','µ','∂','∑','∏','π','∫','ª','º','Ω','æ','ø','¿','¡','¬','√','ƒ','≈','∆','«',
        '»','…',' ','À','Ã','Õ','Œ','œ','–','—','“','”','‘','’','÷','◊','ÿ','Ÿ','⁄',
        '€','‹','›','ﬁ','ﬂ','‡','·','‚','„','‰','Â','Ê','Á','Ë','È','Í','Î','Ï','Ì',
        'Ó','Ô','','Ò','Ú','Û','Ù','ı','ˆ','˜','¯','˘','˙','˚','¸','˝','˛','ˇ');

const EntityChars:array[1..102,1..2] of ansistring=(('&quot;',#34),('&amp;',#38),('&apos;',''''),
                                                    ('&lt;',#60),('&gt;',#62),('&euro;',#128),('&nbsp;',#160),('&iexcl;',#161),
                                                    ('&cent;',#162),('&pound;',#163),('&curren;',#164),('&yen;',#165),
                                                    ('&brvbar;',#166),('&sect;',#167),('&uml;',#168),('&copy;',#169),
                                                    ('&ordf;',#170),('&laquo;',#171),('&not;',#172),('&shy;',#173),
                                                    ('&reg;',#174),('&macr;',#175),('&deg;',#176),('&plusmn;',#177),
                                                    ('&sup2;',#178),('&sup3;',#179),('&acute;',#180),('&micro;',#181),
                                                    ('&para;',#182),('&middot;',#183),('&cedil;',#184),('&sup1;',#185),
                                                    ('&ordm;',#186),('&raquo;',#187),('&frac14;',#188),('&frac12;',#189),
                                                    ('&frac34;',#190),('&iquest;',#191),('&Agrave;',#192),('&Aacute;',#193),
                                                    ('&Acirc;',#194),('&Atilde;',#195),('&Auml;',#196),('&Aring;',#197),
                                                    ('&AElig;',#198),('&Ccedil;',#199),('&Egrave;',#200),('&Eacute;',#201),
                                                    ('&Ecirc;',#202),('&Euml;',#203),('&Igrave;',#204),('&Iacute;',#205),
                                                    ('&Icirc;',#206),('&Iuml;',#207),('&ETH;',#208),('&Ntilde;',#209),
                                                    ('&Ograve;',#210),('&Oacute;',#211),('&Ocirc;',#212),('&Otilde;',#213),
                                                    ('&Ouml;',#214),('&times;',#215),('&Oslash;',#216),('&Ugrave;',#217),
                                                    ('&Uacute;',#218),('&Ucirc;',#219),('&Uuml;',#220),('&Yacute;',#221),
                                                    ('&THORN;',#222),('&szlig;',#223),('&agrave;',#224),('&aacute;',#225),
                                                    ('&acirc;',#226),('&atilde;',#227),('&auml;',#228),('&aring;',#229),
                                                    ('&aelig;',#230),('&ccedil;',#231),('&egrave;',#232),('&eacute;',#233),
                                                    ('&ecirc;',#234),('&euml;',#235),('&igrave;',#236),('&iacute;',#237),
                                                    ('&icirc;',#238),('&iuml;',#239),('&eth;',#240),('&ntilde;',#241),
                                                    ('&ograve;',#242),('&oacute;',#243),('&ocirc;',#244),('&otilde;',#245),
                                                    ('&ouml;',#246),('&divide;',#247),('&oslash;',#248),('&ugrave;',#249),
                                                    ('&uacute;',#250),('&ucirc;',#251),('&uuml;',#252),('&yacute;',#253),
                                                    ('&thorn;',#254),('&yuml;',#255));

type TEntitiesCharLookUpItem=record
      IsEntity:boolean;
      Entity:ansistring;
     end;

     TEntitiesCharLookUpTable=array[0..{$ifdef UNICODE}65535{$else}255{$endif}] of TEntitiesCharLookUpItem;

var EntitiesCharLookUp:TEntitiesCharLookUpTable;

const EntityInitialized:boolean=false;

procedure InitializeEntites;
var EntityCounter:longint;
begin
 if not EntityInitialized then begin
  EntityInitialized:=true;
  FillChar(EntitiesCharLookUp,sizeof(TEntitiesCharLookUpTable),#0);
  for EntityCounter:=low(EntityChars) to high(EntityChars) do begin
   with EntitiesCharLookUp[ord(EntityChars[EntityCounter,2][1])] do begin
    IsEntity:=true;
    Entity:=EntityChars[EntityCounter,1];
   end;
  end;
 end;
end;

procedure FinalizeEntites;
begin
 EntityInitialized:=false;
end;

function ConvertToEntities(AString:ansistring;IdentLevel:longint=0):ansistring;
var Counter,IdentCounter:longint;
    c:longword;
begin
 result:='';
 Counter:=1;
 while Counter<=length(AString) do begin
  c:=UTF8CodeUnitGetCharAndInc(AString,Counter);
  if c=13 then begin
   if ((Counter+1)<=length(AString)) and (AString[Counter+1]=#10) then begin
    continue;
   end;
   c:=10;
  end else if c=10 then begin
   if ((Counter+1)<=length(AString)) and (AString[Counter+1]=#13) then begin
    inc(Counter);
   end;
  end;
  if EntitiesCharLookUp[ord(c)].IsEntity then begin
   result:=result+EntitiesCharLookUp[ord(c)].Entity;
  end else if (c=9) or (c=10) or (c=13) or ((c>=32) and (c<=127)) then begin
   result:=result+ansichar(byte(c));
   if c=10 then begin
    for IdentCounter:=1 to IdentLevel do begin
     result:=result+' ';
    end;
   end;
  end else begin
   if c<255 then begin
    result:=result+'&#'+IntToStr(c)+';';
   end else begin
    result:=result+'&#x'+IntToHex(c,4)+';';
   end;
  end;
 end;
end;

function ConvertEntities(s:ansistring;Charset:THTMLCharset):ansistring;
var i,j,d,c,EntityLength,EntityPosition:longint;
    IsItAnEntity:boolean;
    Entity:ansistring;
begin
 result:=EncodeString(s,Charset,UTF_8);
 i:=pos(#13#10,result);
 while i>0 do begin
  result[i]:=' ';
  delete(result,i+1,1);
  i:=pos(#13#10,result);
 end;
 for i:=1 to length(result) do begin
  if result[i] in [#9,#13,#10] then begin
   result[i]:=' ';
  end;
 end;
 i:=pos('  ',result);
 while i>0 do begin
  delete(result,i,1);
  i:=pos('  ',result);
 end;
 i:=1;
 IsItAnEntity:=false;
 EntityPosition:=0;
 while i<=length(result) do begin
  if result[i]='&' then begin
   EntityPosition:=i;
   IsItAnEntity:=true;
   Entity:='';
  end;
  if IsItAnEntity then begin
   Entity:=Entity+result[i];
   if (result[i]=';') or (result[i]=' ') then begin
    EntityLength:=length(Entity);
    if (EntityLength>2) and (Entity[2]='#') then begin
     delete(Entity,EntityLength,1);
     delete(Entity,1,2);
     if upcase(Entity[1])='X' then Entity[1]:='$';
     val(Entity,d,c);
     if c=0 then begin
      delete(result,EntityPosition,EntityLength);
      if d<128 then begin
       insert(Charset8bit[d],result,EntityPosition);
      end else begin
       insert(UTF32CharToUTF8(d),result,EntityPosition);
      end;
      i:=EntityPosition;
     end;
    end else begin
     i:=EntityPosition;
     for j:=low(Entities) to high(Entities) do begin
      if Entity=Entities[j,0] then begin
       delete(result,EntityPosition,EntityLength);
       insert(Entities[j,1],result,EntityPosition);
       i:=EntityPosition-1;
       break;
      end;
     end;
    end;
    IsItAnEntity:=false;
   end;
  end;
  inc(i);
 end;
end;

procedure FreeHTMLNode(var Node:THTMLNode);
var i:longint;
begin
 for i:=0 to length(Node.Children)-1 do begin
  FreeHTMLNode(Node.Children[i]^);
  freemem(Node.Children[i]);
  Node.Children[i]:=nil;
 end;
 SetLength(Node.Children,0);
 for i:=0 to length(Node.TagParameters)-1 do begin
  Node.TagParameters[i].Name:='';
  Node.TagParameters[i].Value:='';
 end;
 SetLength(Node.TagParameters,0);
 Node.TagName:='';
 Node.Text:='';
 Finalize(Node);
end;

constructor THTML.Create(Input:ansistring;Charset:THTMLCharset=UTF_8);
var i,j:longint;
    c,tc:ansichar;
    Text,TagName,ParameterName,ParameterValue:ansistring;
    Stack:array of PHTMLNode;
    StackPointer:longint;
    IsCloseTag,IsAloneTag:boolean;
    TagParameters:THTMLTagParameters;
 procedure FlushText;
 var TextNode,StackNode:PHTMLNode;
 begin
  if (length(trim(Text))>0) and (StackPointer>=0) and (StackPointer<length(Stack)) then begin
   StackNode:=Stack[StackPointer];
   new(TextNode);
   fillchar(TextNode^,sizeof(THTMLNode),#0);
   TextNode^.NodeType:=ntTEXT;
   TextNode^.Text:=ConvertEntities(Text,Charset);
   SetLength(StackNode.Children,length(StackNode.Children)+1);
   StackNode.Children[length(StackNode.Children)-1]:=TextNode;
  end;
  Text:='';
 end;
 procedure FlushTag;
 var TagNode,StackNode:PHTMLNode;
     i:longint;
 begin
  if (StackPointer>=0) and (StackPointer<length(Stack)) then begin
   if TagName='LI' then begin
    for i:=StackPointer downto 1 do begin
     if (i<length(Stack)) and ((Stack[i]^.TagName='OL') or (Stack[i]^.TagName='UL') or (Stack[i]^.TagName='DIR') or (Stack[i]^.TagName='MENU')) then begin
      StackPointer:=i;
      break;
     end;
    end;
   end else if TagName='FRAME' then begin
    for i:=StackPointer downto 1 do begin
     if (i<length(Stack)) and (Stack[i]^.TagName='FRAMESET') then begin
      StackPointer:=i;
      break;
     end;
    end;
   end else if TagName='FRAMESET' then begin
    for i:=StackPointer downto 1 do begin
     if (i<length(Stack)) and (Stack[i]^.TagName='HTML') then begin
      StackPointer:=i;
      break;
     end;
    end;
   end else if TagName='HEAD' then begin
    for i:=StackPointer downto 1 do begin
     if (i<length(Stack)) and (Stack[i]^.TagName='HTML') then begin
      StackPointer:=i;
      break;
     end;
    end;
   end else if TagName='BODY' then begin
    for i:=StackPointer downto 1 do begin
     if (i<length(Stack)) and (Stack[i]^.TagName='HTML') then begin
      StackPointer:=i;
      break;
     end;
    end;
   end else if TagName='TR' then begin
    for i:=StackPointer downto 1 do begin
     if (i<length(Stack)) and (Stack[i]^.TagName='TABLE') then begin
      StackPointer:=i;
      break;
     end;
    end;
   end else if TagName='TD' then begin
    for i:=StackPointer downto 1 do begin
     if (i<length(Stack)) and (Stack[i]^.TagName='TR') then begin
      StackPointer:=i;
      break;
     end;
    end;
   end else if TagName='TH' then begin
    for i:=StackPointer downto 1 do begin
     if (i<length(Stack)) and (Stack[i]^.TagName='TR') then begin
      StackPointer:=i;
      break;
     end;
    end;
   end else if TagName='DD' then begin
    for i:=StackPointer downto 1 do begin
     if (i<length(Stack)) and (Stack[i]^.TagName='DL') then begin
      StackPointer:=i;
      break;
     end;
    end;
   end else if TagName='DT' then begin
    for i:=StackPointer downto 1 do begin
     if (i<length(Stack)) and (Stack[i]^.TagName='DL') then begin
      StackPointer:=i;
      break;
     end;
    end;
   end;
   StackNode:=Stack[StackPointer];
   new(TagNode);
   fillchar(TagNode^,sizeof(THTMLNode),#0);
   TagNode^.NodeType:=ntTAG;
   TagNode^.TagName:=TagName;
   TagNode^.TagParameters:=copy(TagParameters,0,length(TagParameters));
   SetLength(StackNode.Children,length(StackNode.Children)+1);
   StackNode.Children[length(StackNode.Children)-1]:=TagNode;
   if (TagName<>'BR') and (TagName<>'HR') and (TagName<>'IMG') and
      (TagName<>'IFRAME') and (TagName<>'FRAME') and
      (TagName<>'META') and (TagName<>'LINK') then begin
    inc(StackPointer);
    if StackPointer>=length(Stack) then begin
     SetLength(Stack,StackPointer+1);
    end;
    Stack[StackPointer]:=TagNode;
    if IsAloneTag then begin
     dec(StackPointer);
    end;
   end;
  end;
  Text:='';
 end;
begin
 try
  if Charset in [UCS_2,UCS_4] then begin
   Input:=EncodeString(Input,Charset,UTF_8);
   Charset:=UTF_8;
  end;
  FillChar(RootNode,sizeof(THTMLNode),#0);
  TagParameters:=nil;
  SetLength(Stack,1);
  Stack[0]:=@RootNode;
  StackPointer:=0;
  i:=1;
  while i<=length(Input) do begin
   c:=Input[i];
   inc(i);
   if c='<' then begin
    IsAloneTag:=false;
    if length(Text)>0 then begin
     FlushText;
    end;
    if ((i+2)<=length(Input)) and ((Input[i]='!') and (Input[i+1]='-') and (Input[i+2]='-')) then begin
     inc(i,3);
     while i<=length(Input) do begin
      if ((i+2)<=length(Input)) and ((Input[i]='-') and (Input[i+1]='-') and (Input[i+2]='>')) then begin
       inc(i,3);
       break;
      end else begin
       inc(i);
      end;
     end;
    end else if (i<=length(Input)) and (Input[i] in ['!','?','&','%']) then begin
     while (i<=length(Input)) and (Input[i]<>'>') do begin
      inc(i);
     end;
     if (i<=length(Input)) and (Input[i]='>') then begin
      inc(i);
     end;
    end else begin
     if (i<=length(Input)) and (Input[i]='/') then begin
      IsCloseTag:=true;
      inc(i);
     end else begin
      IsCloseTag:=false;
     end;
     TagName:='';
     while i<=length(Input) do begin
      c:=Input[i];
      case c of
       'a'..'z','A'..'Z','0'..'9','-':begin
        TagName:=TagName+upcase(c);
        inc(i);
       end;
       else begin
        break;
       end;
      end;
     end;
     SetLength(TagParameters,0);
     while i<=length(Input) do begin
      while (i<=length(Input)) and (Input[i] in [#0..#32]) do begin
       inc(i);
      end;
      if i<=length(Input) then begin
       c:=Input[i];
       case c of
        '/':begin
         IsAloneTag:=true;
         inc(i);
        end;
        '>':begin
         inc(i);
         break;
        end;
        'a'..'z','A'..'Z','0'..'9','-':begin
         ParameterName:='';
         ParameterValue:='';
         while i<=length(Input) do begin
          c:=Input[i];
          case c of
           'a'..'z','A'..'Z','0'..'9','-':begin
            ParameterName:=ParameterName+upcase(c);
            inc(i);
           end;
           else begin
            break;
           end;
          end;
         end;
         while (i<=length(Input)) and (Input[i] in [#0..#32]) do begin
          inc(i);
         end;
         if (i<=length(Input)) and (Input[i]='=') then begin
          inc(i);
          while (i<=length(Input)) and (Input[i] in [#0..#32]) do begin
           inc(i);
          end;
          if (i<=length(Input)) and (Input[i] in ['''','"']) then begin
           tc:=Input[i];
           inc(i);
           while i<=length(Input) do begin
            c:=Input[i];
            if c=tc then begin
             break;
            end else if c='\' then begin
             inc(i);
             if i<=length(Input) then begin
              c:=Input[i];
              inc(i);
              case c of
               '''','"':begin
                ParameterValue:=ParameterValue+c;
               end;
               'r','R':begin
                ParameterValue:=ParameterValue+#13;
               end;
               'n','N':begin
                ParameterValue:=ParameterValue+#10;
               end;
               't','T':begin
                ParameterValue:=ParameterValue+#9;
               end;
               'b','B':begin
                ParameterValue:=ParameterValue+#8;
               end;
               else begin
                ParameterValue:=ParameterValue+'\'+c;
               end;
              end;
             end;
            end else begin
             ParameterValue:=ParameterValue+c;
             inc(i);
            end;
           end;
          end else begin
           while i<=length(Input) do begin
            c:=Input[i];
            case c of
             #0..#32:begin
              break;
             end;
             else begin
              ParameterValue:=ParameterValue+c;
              inc(i);
             end;
            end;
           end;
          end;
         end;
         SetLength(TagParameters,length(TagParameters)+1);
         with TagParameters[length(TagParameters)-1] do begin
          Name:=ParameterName;
          Value:=ParameterValue;
         end;
        end;
        else begin
         inc(i);
        end;
       end;
      end else begin
       break;
      end;
     end;
     if IsCloseTag then begin
      for j:=StackPointer downto 1 do begin
       if (j<length(Stack)) and (Stack[j]^.TagName=TagName) then begin
        StackPointer:=j-1;
        break;
       end;
      end;
     end else begin
      FlushTag;
     end;
     SetLength(TagParameters,0);
    end;
   end else begin
    Text:=Text+c;
   end;
  end;
  if length(Text)>0 then begin
   FlushText;
  end;
  SetLength(Stack,0);
  inherited Create;
 except
  FreeHTMLNode(RootNode);
 end;
end;

destructor THTML.Destroy;
begin
 FreeHTMLNode(RootNode);
 inherited Destroy;
end;

function THTML.GetPlainText:ansistring;
var Charset:THTMLCharset;
 function Build(var Node:THTMLNode):ansistring;
 type TCellAlign=(caLEFT,caCENTER,caRIGHT);
      TCell=record
       Text:ansistring;
       Align:TCellAlign;
      end;
 var i,j,k,h,w,x,y,xx:longint;
     t,s1,s2:ansistring;
     Cells:array of array of TCell;
     cw:array of longint;
     ch:array of longint;
     Rows:array of ansistring;
 begin
  case Node.NodeType of
   ntROOT:begin
    result:='';
    for i:=0 to length(Node.Children)-1 do begin
     result:=result+Build(Node.Children[i]^);
    end;
   end;
   ntTAG:begin
    result:='';
    if Node.TagName='META' then begin
     for i:=0 to length(Node.TagParameters)-1 do begin
      if Node.TagParameters[i].Name='HTTP-EQUIV' then begin
       if trim(uppercase(Node.TagParameters[i].Value))='CONTENT-TYPE' then begin
        for j:=0 to length(Node.TagParameters)-1 do begin
         if Node.TagParameters[j].Name='CONTENT' then begin
          Charset:=GetCodepage(Node.TagParameters[j].Value);
          break;
         end;
        end;
       end;
       break;
      end;
     end;
    end else if Node.TagName='HEAD' then begin
     for i:=0 to length(Node.Children)-1 do begin
      Build(Node.Children[i]^);
     end;
    end else if Node.TagName='BR' then begin
     result:=result+#13#10;
    end else if Node.TagName='HR' then begin
     result:=result+#13#10+'---------------------------------------'+#13#10;
    end else if (Node.TagName='B') or (Node.TagName='EM') then begin
     result:=result+'*';
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+'*';
    end else if Node.TagName='STRONG' then begin
     result:=result+'!';
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+'!';
    end else if Node.TagName='SAMP' then begin
     result:=result+'"';
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+'"';
    end else if Node.TagName='U' then begin
     result:=result+'_';
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+'_';
    end else if Node.TagName='I' then begin
     result:=result+'\';
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+'/';
    end else if Node.TagName='H1' then begin
     result:=result+#13#10+'- ';
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+' -'+#13#10;
    end else if Node.TagName='H2' then begin
     result:=result+#13#10+'-- ';
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+' --'+#13#10;
    end else if Node.TagName='H3' then begin
     result:=result+#13#10+'--- ';
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+' ---'+#13#10;
    end else if Node.TagName='H4' then begin
     result:=result+#13#10+'---- ';
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+' ----'+#13#10;
    end else if Node.TagName='H5' then begin
     result:=result+#13#10+'----- ';
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+' -----'+#13#10;
    end else if Node.TagName='H5' then begin
     result:=result+#13#10+'----- ';
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+' -----'+#13#10;
    end else if Node.TagName='H6' then begin
     result:=result+#13#10+'------ ';
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+' ------'+#13#10;
    end else if Node.TagName='H7' then begin
     result:=result+#13#10+'------- ';
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+' -------'+#13#10;
    end else if Node.TagName='H8' then begin
     result:=result+#13#10+'-------- ';
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+' --------'+#13#10;
    end else if Node.TagName='H9' then begin
     result:=result+#13#10+'--------- ';
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+' ---------'+#13#10;
    end else if Node.TagName='OL' then begin
     result:=result+#13#10;
     h:=1;
     for i:=0 to length(Node.Children)-1 do begin
      if Node.Children[i]^.NodeType=ntTAG then begin
       if Node.Children[i]^.TagName='LI' then begin
        str(h,s1);
        s1:=s1+'.';
        SetLength(s2,length(s1));
        fillchar(s2[1],length(s2),#32);
        j:=0;
        t:=TrimLeft(Build(Node.Children[i]^));
        k:=pos(#13#10,t);
        if k>0 then begin
         while k>0 do begin
          if j=0 then begin
           result:=result+s1+' '+copy(t,1,k-1)+#13#10;
          end else begin
           result:=result+s2+' '+copy(t,1,k-1)+#13#10;
          end;
          delete(t,1,k+1);
          inc(j);
          k:=pos(#13#10,t);
         end;
         if length(t)>0 then begin
          result:=result+s2+' '+t+#13#10;
         end;
        end else begin
         result:=result+s1+' '+t+#13#10;
        end;
        inc(h);
       end else begin
        result:=result+Build(Node.Children[i]^);
       end;
      end else begin
       result:=result+Build(Node.Children[i]^);
      end;
     end;
     result:=result+#13#10;
    end else if Node.TagName='UL' then begin
     result:=result+#13#10;
     for i:=0 to length(Node.Children)-1 do begin
      if Node.Children[i]^.NodeType=ntTAG then begin
       if Node.Children[i]^.TagName='LI' then begin
        j:=0;
        t:=TrimLeft(Build(Node.Children[i]^));
        k:=pos(#13#10,t);
        if k>0 then begin
         while k>0 do begin
          if j=0 then begin
           result:=result+'- '+copy(t,1,k-1)+#13#10;
          end else begin
           result:=result+'  '+copy(t,1,k-1)+#13#10;
          end;
          delete(t,1,k+1);
          inc(j);
          k:=pos(#13#10,t);
         end;
         if length(t)>0 then begin
          result:=result+'  '+t+#13#10;
         end;
        end else begin
         result:=result+'- '+t+#13#10;
        end;
       end else begin
        result:=result+Build(Node.Children[i]^);
       end;
      end else begin
       result:=result+Build(Node.Children[i]^);
      end;
     end;
     result:=result+#13#10;
    end else if (Node.TagName='DIR') or (Node.TagName='MENU') then begin
     result:=result+#13#10;
     for i:=0 to length(Node.Children)-1 do begin
      if Node.Children[i]^.NodeType=ntTAG then begin
       if Node.Children[i]^.TagName='LI' then begin
        j:=0;
        t:=TrimLeft(Build(Node.Children[i]^));
        k:=pos(#13#10,t);
        if k>0 then begin
         while k>0 do begin
          if j=0 then begin
           result:=result+'* '+copy(t,1,k-1)+#13#10;
          end else begin
           result:=result+'  '+copy(t,1,k-1)+#13#10;
          end;
          delete(t,1,k+1);
          inc(j);
          k:=pos(#13#10,t);
         end;
         if length(t)>0 then begin
          result:=result+'  '+t+#13#10;
         end;
        end else begin
         result:=result+'* '+t+#13#10;
        end;
       end else begin
        result:=result+Build(Node.Children[i]^);
       end;
      end else begin
       result:=result+Build(Node.Children[i]^);
      end;
     end;
     result:=result+#13#10;
    end else if Node.TagName='DL' then begin
     result:=result+#13#10;
     for i:=0 to length(Node.Children)-1 do begin
      if Node.Children[i]^.NodeType=ntTAG then begin
       if Node.Children[i]^.TagName='DT' then begin
        j:=0;
        t:=TrimLeft(Build(Node.Children[i]^));
        k:=pos(#13#10,t);
        if k>0 then begin
         while k>0 do begin
          if j=0 then begin
           result:=result+'= '+copy(t,1,k-1)+#13#10;
          end else begin
           result:=result+'  '+copy(t,1,k-1)+#13#10;
          end;
          delete(t,1,k+1);
          inc(j);
          k:=pos(#13#10,t);
         end;
         if length(t)>0 then begin
          result:=result+'  '+t+#13#10;
         end;
        end else begin
         result:=result+'= '+t+#13#10;
        end;
       end else if Node.Children[i]^.TagName='DD' then begin
        t:=TrimLeft(Build(Node.Children[i]^));
        k:=pos(#13#10,t);
        if k>0 then begin
         while k>0 do begin
          result:=result+'    '+copy(t,1,k-1)+#13#10;
          delete(t,1,k+1);
          k:=pos(#13#10,t);
         end;
         if length(t)>0 then begin
          result:=result+'    '+t+#13#10;
         end;
        end else begin
         result:=result+'    '+t+#13#10;
        end;
       end else begin
        result:=result+Build(Node.Children[i]^);
       end;
      end else begin
       result:=result+Build(Node.Children[i]^);
      end;
     end;
     result:=result+#13#10;
    end else if (Node.TagName='BLOCKQUOTE') or (Node.TagName='Q') then begin
     result:=result+#13#10;
     for i:=0 to length(Node.TagParameters)-1 do begin
      if Node.TagParameters[i].Name='CITE' then begin
       result:=result+Node.TagParameters[i].Value+' wrote:'+#13#10;
       break;
      end;
     end;
     t:='';
     for i:=0 to length(Node.Children)-1 do begin
      t:=t+Build(Node.Children[i]^);
     end;
     t:=TrimLeft(t);
     k:=pos(#13#10,t);
     if k>0 then begin
      while k>0 do begin
       result:=result+'>'+copy(t,1,k-1)+#13#10;
       delete(t,1,k+1);
       k:=pos(#13#10,t);
      end;
      if length(t)>0 then begin
       result:=result+'>'+t+#13#10;
      end;
     end else begin
      result:=result+'>'+t+#13#10;
     end;
     result:=result+#13#10;
    end else if (Node.TagName='CENTER') then begin
     result:=result+#13#10;
     t:='';
     for i:=0 to length(Node.Children)-1 do begin
      t:=t+Build(Node.Children[i]^);
     end;
     t:=TrimLeft(t);
     k:=pos(#13#10,t);
     while k>0 do begin
      s1:=copy(t,1,k-1);
      i:=(80-length(s1)) div 2;
      if i>0 then begin
       SetLength(s2,i);
       fillchar(s2[1],i,' ');
       s1:=s2+s1;
      end;
      result:=result+s1+#13#10;
      delete(t,1,k+1);
      k:=pos(#13#10,t);
     end;
     if length(t)>0 then begin
      s1:=t;
      i:=(80-length(s1)) div 2;
      if i>0 then begin
       SetLength(s2,i);
       fillchar(s2[1],i,' ');
       s1:=s2+s1;
      end;
      result:=result+s1+#13#10;
     end;
     result:=result+#13#10;
    end else if Node.TagName='PRE' then begin
     result:=result+#13#10;
     t:='';
     for i:=0 to length(Node.Children)-1 do begin
      t:=t+Build(Node.Children[i]^);
     end;
     t:=TrimLeft(t);
     k:=pos(#13#10,t);
     if k>0 then begin
      while k>0 do begin
       result:=result+'    '+copy(t,1,k-1)+#13#10;
       delete(t,1,k+1);
       k:=pos(#13#10,t);
      end;
      if length(t)>0 then begin
       result:=result+'    '+t+#13#10;
      end;
     end else begin
      result:=result+'    '+t+#13#10;
     end;
     result:=result+#13#10;
    end else if (Node.TagName='ABBR') or (Node.TagName='ACRONYM') then begin
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     for i:=0 to length(Node.TagParameters)-1 do begin
      if Node.TagParameters[i].Name='TITLE' then begin
       result:=result+'('+ConvertEntities(Node.TagParameters[i].Value,Charset)+')';
       break;
      end;
     end;
    end else if Node.TagName='TABLE' then begin
     SetLength(Cells,0,0);
     t:='';
     for i:=0 to length(Node.Children)-1 do begin
      if Node.Children[i]^.NodeType=ntTAG then begin
       if Node.Children[i]^.TagName='TR' then begin
        SetLength(Cells,length(Cells)+1);
        SetLength(Cells[length(Cells)-1],0);
        for j:=0 to length(Node.Children[i]^.Children)-1 do begin
         if Node.Children[i]^.Children[j]^.NodeType=ntTAG then begin
          if (Node.Children[i]^.Children[j]^.TagName='TD') or
             (Node.Children[i]^.Children[j]^.TagName='TH') then begin
           SetLength(Cells[length(Cells)-1],length(Cells[length(Cells)-1])+1);
           Cells[length(Cells)-1,length(Cells[length(Cells)-1])-1].Text:=Build(Node.Children[i]^.Children[j]^);
           Cells[length(Cells)-1,length(Cells[length(Cells)-1])-1].Align:=caLEFT;
           for k:=0 to length(Node.Children[i]^.Children[j]^.TagParameters)-1 do begin
            if Node.Children[i]^.Children[j]^.TagParameters[k].Name='ALIGN' then begin
             s1:=trim(uppercase(Node.Children[i]^.Children[j]^.TagParameters[k].Value));
             if s1='LEFT' then begin
              Cells[length(Cells)-1,length(Cells[length(Cells)-1])-1].Align:=caLEFT;
             end else if s1='CENTER' then begin
              Cells[length(Cells)-1,length(Cells[length(Cells)-1])-1].Align:=caCENTER;
             end else if s1='RIGHT' then begin
              Cells[length(Cells)-1,length(Cells[length(Cells)-1])-1].Align:=caRIGHT;
             end;
            end;
           end;
          end else begin
           t:=t+Build(Node.Children[i]^.Children[j]^);
          end;
         end else begin
          t:=t+Build(Node.Children[i]^.Children[j]^);
         end;
        end;
       end else begin
        t:=t+Build(Node.Children[i]^);
       end;
      end else begin
       t:=t+Build(Node.Children[i]^);
      end;
     end;
     if length(Cells)>0 then begin
      SetLength(ch,length(Cells));
      j:=0;
      for i:=0 to length(Cells)-1 do begin
       if j<length(Cells[i]) then begin
        j:=length(Cells[i]);
       end;
      end;
      if j>0 then begin
       SetLength(cw,j);
       for i:=0 to length(cw)-1 do begin
        cw[i]:=0;
       end;
       for i:=0 to length(ch)-1 do begin
        ch[i]:=0;
       end;
       for i:=0 to length(Cells)-1 do begin
        for j:=0 to length(Cells[i])-1 do begin
         s1:=Cells[i,j].Text;
         w:=0;
         h:=0;
         while length(s1)>0 do begin
          k:=pos(#13#10,s1);
          if k>0 then begin
           s2:=copy(s1,1,k-1);
           delete(s1,1,k+1);
          end else begin
           s2:=s1;
           s1:='';
          end;
          if w<length(s2) then begin
           w:=length(s2);
          end;
          inc(h);
         end;
         if (j+1)<length(cw) then begin
          inc(w);
         end;
         if cw[j]<w then begin
          cw[j]:=w;
         end;
         if ch[i]<h then begin
          ch[i]:=h;
         end;
        end;
       end;
       w:=2;
       for i:=0 to length(cw)-1 do begin
        inc(w,cw[i]);
       end;
       result:=result+'+';
       for i:=2 to w-1 do begin
        result:=result+'-';
       end;
       result:=result+'+'+#13#10;
       for i:=0 to length(Cells)-1 do begin
        SetLength(Rows,ch[i]);
        for j:=0 to length(Rows)-1 do begin
         SetLength(Rows[j],w);
         fillchar(Rows[j,1],w,#32);
         Rows[j,1]:='|';
         Rows[j,w]:='|';
        end;
        x:=2;
        for j:=0 to length(Cells[i])-1 do begin
         y:=0;
         s1:=Cells[i,j].Text;
         while length(s1)>0 do begin
          k:=pos(#13#10,s1);
          if k>0 then begin
           s2:=copy(s1,1,k-1);
           delete(s1,1,k+1);
          end else begin
           s2:=s1;
           s1:='';
          end;
          if y<length(Rows) then begin
           k:=length(s2);
           xx:=x;
           case Cells[i,j].Align of
            caRIGHT:begin
             if length(s2)=cw[j] then begin
              s2:=copy(s2,1,cw[j]);
              if (x+k)>=length(Rows[y]) then begin
               k:=length(Rows[y])-x;
              end;
              move(s2[1],Rows[y,xx],k);
             end else begin
              xx:=length(s2)-cw[j];
              if xx<1 then begin
               xx:=1;
              end;
              s2:=copy(s2,xx,cw[j]);
              xx:=(x+cw[j])-length(s2);
              if (xx+k)>=length(Rows[y]) then begin
               k:=length(Rows[y])-xx;
              end;
              move(s2[1],Rows[y,xx],k);
             end;
            end;
            caCENTER:begin
             if length(s2)=cw[j] then begin
              s2:=copy(s2,1,cw[j]);
              if (x+k)>=length(Rows[y]) then begin
               k:=length(Rows[y])-x;
              end;
              move(s2[1],Rows[y,xx],k);
             end else begin
              xx:=length(s2)-cw[j];
              if xx<1 then begin
               xx:=1;
              end;
              s2:=copy(s2,xx,cw[j]);
              xx:=x+(((cw[j]-length(s2))+1) div 2);
              while (xx+k)>=length(Rows[y]) do begin
               dec(xx);
               if xx<x then begin
                xx:=x;
                break;
               end;
              end;
              if (xx+k)>=length(Rows[y]) then begin
               k:=length(Rows[y])-xx;
              end;
              move(s2[1],Rows[y,xx],k);
             end;
            end;
            else begin
             s2:=copy(s2,1,cw[j]);
             if (x+k)>=length(Rows[y]) then begin
              k:=length(Rows[y])-x;
             end;
             move(s2[1],Rows[y,xx],k);
            end;
           end;
          end;
          inc(y);
         end;
         if (j+1)<length(cw) then begin
          for y:=0 to length(Rows)-1 do begin
           Rows[y,x+cw[j]-1]:='|';
          end;
         end;
         inc(x,cw[j]);
        end;
        for j:=0 to length(Rows)-1 do begin
         result:=result+Rows[j]+#13#10;
        end;
        SetLength(Rows,0);
        if (i+1)<length(ch) then begin
         SetLength(s1,w);
         fillchar(s1[2],w-2,'-');
         s1[1]:='|';
         s1[w]:='|';
         result:=result+s1+#13#10;
        end;
       end;
       result:=result+'+';
       for i:=2 to w-1 do begin
        result:=result+'-';
       end;
       result:=result+'+'+#13#10;
      end;
      SetLength(Cells,0,0);
     end;
     result:=result+t;
    end else if Node.TagName='CODE' then begin
     result:=result+#13#10;
     t:='';
     for i:=0 to length(Node.Children)-1 do begin
      t:=t+Build(Node.Children[i]^);
     end;
     t:=TrimLeft(t);
     k:=pos(#13#10,t);
     if k>0 then begin
      while k>0 do begin
       result:=result+'  '+copy(t,1,k-1)+#13#10;
       delete(t,1,k+1);
       k:=pos(#13#10,t);
      end;
      if length(t)>0 then begin
       result:=result+'  '+t+#13#10;
      end;
     end else begin
      result:=result+'  '+t+#13#10;
     end;
     result:=result+#13#10;
    end else if Node.TagName='P' then begin
     result:=#13#10;
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+#13#10;
    end else if Node.TagName='DIV' then begin
     result:=#13#10;
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+#13#10;
    end else if Node.TagName='A' then begin
     result:='{Link< ';
     for i:=0 to length(Node.TagParameters)-1 do begin
      if Node.TagParameters[i].Name='HREF' then begin
       result:=result+ConvertEntities(Node.TagParameters[i].Value,Charset);
       break;
      end;
     end;
     result:=result+' >[';
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+']}';
    end else if Node.TagName='SCRIPT' then begin
    end else if Node.TagName='STYLE' then begin
    end else if Node.TagName='LINK' then begin
    end else begin
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
    end;
   end;
   ntTEXT:begin
    result:=Node.Text;
   end;
   else begin
    result:='';
   end;
  end;
 end;
begin
 CharSet:=UTF_8;
 result:=Build(RootNode);
end;

function THTML.GetMarkDown:ansistring;
var Charset:THTMLCharset;
 function Build(var Node:THTMLNode):ansistring;
 type TCellAlign=(caLEFT,caCENTER,caRIGHT);
      TCell=record
       Text:ansistring;
       Align:TCellAlign;
      end;
 var i,j,k,h,w,x,y,xx:longint;
     t,s1,s2:ansistring;
     Cells:array of array of TCell;
     cw:array of longint;
     ch:array of longint;
     Rows:array of ansistring;
 begin
  case Node.NodeType of
   ntROOT:begin
    result:='';
    for i:=0 to length(Node.Children)-1 do begin
     result:=result+Build(Node.Children[i]^);
    end;
   end;
   ntTAG:begin
    result:='';
    if Node.TagName='META' then begin
     for i:=0 to length(Node.TagParameters)-1 do begin
      if Node.TagParameters[i].Name='HTTP-EQUIV' then begin
       if trim(uppercase(Node.TagParameters[i].Value))='CONTENT-TYPE' then begin
        for j:=0 to length(Node.TagParameters)-1 do begin
         if Node.TagParameters[j].Name='CONTENT' then begin
          Charset:=GetCodepage(Node.TagParameters[j].Value);
          break;
         end;
        end;
       end;
       break;
      end;
     end;
    end else if Node.TagName='HEAD' then begin
     for i:=0 to length(Node.Children)-1 do begin
      Build(Node.Children[i]^);
     end;
    end else if Node.TagName='BR' then begin
     result:=result+#13#10;
    end else if Node.TagName='HR' then begin
     result:=result+#13#10+#13#10+'*****'+#13#10+#13#10;
    end else if (Node.TagName='B') or (Node.TagName='STRONG') then begin
     result:=result+'**';
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+'**';
    end else if Node.TagName='SAMP' then begin
     result:=result+'"';
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+'"';
    end else if Node.TagName='U' then begin
     result:=result+'_';
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+'_';
    end else if (Node.TagName='I') or (Node.TagName='EM') then begin
     result:=result+'*';
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+'*';
    end else if Node.TagName='H1' then begin
     result:=result+#13#10#13#10+'# ';
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+' #'+#13#10#13#10;
    end else if Node.TagName='H2' then begin
     result:=result+#13#10#13#10+'## ';
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+' ##'+#13#10#13#10;
    end else if Node.TagName='H3' then begin
     result:=result+#13#10#13#10+'### ';
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+' ###'+#13#10#13#10;
    end else if Node.TagName='H4' then begin
     result:=result+#13#10#13#10+'#### ';
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+' ####'+#13#10#13#10;
    end else if Node.TagName='H5' then begin
     result:=result+#13#10#13#10+'##### ';
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+' #####'+#13#10#13#10;
    end else if Node.TagName='H5' then begin
     result:=result+#13#10#13#10+'##### ';
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+' #####'+#13#10#13#10;
    end else if Node.TagName='H6' then begin
     result:=result+#13#10#13#10+'###### ';
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+' ######'+#13#10#13#10;
    end else if Node.TagName='H7' then begin
     result:=result+#13#10#13#10+'####### ';
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+' #######'+#13#10#13#10;
    end else if Node.TagName='H8' then begin
     result:=result+#13#10#13#10+'######## ';
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+' ########'+#13#10#13#10;
    end else if Node.TagName='H9' then begin
     result:=result+#13#10#13#10+'######### ';
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+' #########'+#13#10#13#10;
    end else if Node.TagName='OL' then begin
     result:=result+#13#10#13#10;
     h:=1;
     for i:=0 to length(Node.Children)-1 do begin
      if Node.Children[i]^.NodeType=ntTAG then begin
       if Node.Children[i]^.TagName='LI' then begin
        str(h,s1);
        s1:=s1+'.';
        SetLength(s2,length(s1));
        fillchar(s2[1],length(s2),#32);
        j:=0;
        t:=TrimLeft(Build(Node.Children[i]^));
        k:=pos(#13#10,t);
        if k>0 then begin
         while k>0 do begin
          if j=0 then begin
           result:=result+s1+' '+copy(t,1,k-1)+#13#10;
          end else begin
           result:=result+s2+' '+copy(t,1,k-1)+#13#10;
          end;
          delete(t,1,k+1);
          inc(j);
          k:=pos(#13#10,t);
         end;
         if length(t)>0 then begin
          result:=result+s2+' '+t+#13#10;
         end;
        end else begin
         result:=result+s1+' '+t+#13#10;
        end;
        inc(h);
       end else begin
        result:=result+Build(Node.Children[i]^);
       end;
      end else begin
       result:=result+Build(Node.Children[i]^);
      end;
     end;
     result:=result+#13#10#13#10;
    end else if Node.TagName='UL' then begin
     result:=result+#13#10#13#10;
     for i:=0 to length(Node.Children)-1 do begin
      if Node.Children[i]^.NodeType=ntTAG then begin
       if Node.Children[i]^.TagName='LI' then begin
        j:=0;
        t:=TrimLeft(Build(Node.Children[i]^));
        k:=pos(#13#10,t);
        if k>0 then begin
         while k>0 do begin
          if j=0 then begin
           result:=result+'- '+copy(t,1,k-1)+#13#10;
          end else begin
           result:=result+'  '+copy(t,1,k-1)+#13#10;
          end;
          delete(t,1,k+1);
          inc(j);
          k:=pos(#13#10,t);
         end;
         if length(t)>0 then begin
          result:=result+'  '+t+#13#10;
         end;
        end else begin
         result:=result+'- '+t+#13#10;
        end;
       end else begin
        result:=result+Build(Node.Children[i]^);
       end;
      end else begin
       result:=result+Build(Node.Children[i]^);
      end;
     end;
     result:=result+#13#10#13#10;
    end else if (Node.TagName='DIR') or (Node.TagName='MENU') then begin
     result:=result+#13#10#13#10;
     for i:=0 to length(Node.Children)-1 do begin
      if Node.Children[i]^.NodeType=ntTAG then begin
       if Node.Children[i]^.TagName='LI' then begin
        j:=0;
        t:=TrimLeft(Build(Node.Children[i]^));
        k:=pos(#13#10,t);
        if k>0 then begin
         while k>0 do begin
          if j=0 then begin
           result:=result+'* '+copy(t,1,k-1)+#13#10;
          end else begin
           result:=result+'  '+copy(t,1,k-1)+#13#10;
          end;
          delete(t,1,k+1);
          inc(j);
          k:=pos(#13#10,t);
         end;
         if length(t)>0 then begin
          result:=result+'  '+t+#13#10;
         end;
        end else begin
         result:=result+'* '+t+#13#10;
        end;
       end else begin
        result:=result+Build(Node.Children[i]^);
       end;
      end else begin
       result:=result+Build(Node.Children[i]^);
      end;
     end;
     result:=result+#13#10#13#10;
    end else if Node.TagName='DL' then begin
     result:=result+#13#10#13#10;
     for i:=0 to length(Node.Children)-1 do begin
      if Node.Children[i]^.NodeType=ntTAG then begin
       if Node.Children[i]^.TagName='DT' then begin
        j:=0;
        t:=TrimLeft(Build(Node.Children[i]^));
        k:=pos(#13#10,t);
        if k>0 then begin
         while k>0 do begin
          if j=0 then begin
           result:=result+'+ '+copy(t,1,k-1)+#13#10;
          end else begin
           result:=result+'  '+copy(t,1,k-1)+#13#10;
          end;
          delete(t,1,k+1);
          inc(j);
          k:=pos(#13#10,t);
         end;
         if length(t)>0 then begin
          result:=result+'  '+t+#13#10;
         end;
        end else begin
         result:=result+'+ '+t+#13#10;
        end;
       end else if Node.Children[i]^.TagName='DD' then begin
        t:=TrimLeft(Build(Node.Children[i]^));
        k:=pos(#13#10,t);
        if k>0 then begin
         while k>0 do begin
          result:=result+'    '+copy(t,1,k-1)+#13#10;
          delete(t,1,k+1);
          k:=pos(#13#10,t);
         end;
         if length(t)>0 then begin
          result:=result+'    '+t+#13#10;
         end;
        end else begin
         result:=result+'    '+t+#13#10;
        end;
       end else begin
        result:=result+Build(Node.Children[i]^);
       end;
      end else begin
       result:=result+Build(Node.Children[i]^);
      end;
     end;
     result:=result+#13#10#13#10;
    end else if (Node.TagName='BLOCKQUOTE') or (Node.TagName='Q') then begin
     result:=result+#13#10#13#10;
     for i:=0 to length(Node.TagParameters)-1 do begin
      if Node.TagParameters[i].Name='CITE' then begin
       result:=result+Node.TagParameters[i].Value+' wrote:'+#13#10#13#10;
       break;
      end;
     end;
     t:='';
     for i:=0 to length(Node.Children)-1 do begin
      t:=t+Build(Node.Children[i]^);
     end;
     t:=TrimLeft(t);
     k:=pos(#13#10,t);
     if k>0 then begin
      while k>0 do begin
       result:=result+'>'+copy(t,1,k-1)+#13#10;
       delete(t,1,k+1);
       k:=pos(#13#10,t);
      end;
      if length(t)>0 then begin
       result:=result+'>'+t+#13#10;
      end;
     end else begin
      result:=result+'>'+t+#13#10;
     end;
     result:=result+#13#10#13#10;
    end else if (Node.TagName='CENTER') then begin
     result:=result+#13#10#13#10;
     t:='';
     for i:=0 to length(Node.Children)-1 do begin
      t:=t+Build(Node.Children[i]^);
     end;
     t:=TrimLeft(t);
     k:=pos(#13#10,t);
     while k>0 do begin
      s1:=copy(t,1,k-1);
      i:=(80-length(s1)) div 2;
      if i>0 then begin
       SetLength(s2,i);
       fillchar(s2[1],i,' ');
       s1:=s2+s1;
      end;
      result:=result+s1+#13#10;
      delete(t,1,k+1);
      k:=pos(#13#10,t);
     end;
     if length(t)>0 then begin
      s1:=t;
      i:=(80-length(s1)) div 2;
      if i>0 then begin
       SetLength(s2,i);
       fillchar(s2[1],i,' ');
       s1:=s2+s1;
      end;
      result:=result+s1+#13#10;
     end;
     result:=result+#13#10#13#10;
    end else if Node.TagName='PRE' then begin
     result:=result+#13#10#13#10;
     t:='';
     for i:=0 to length(Node.Children)-1 do begin
      t:=t+Build(Node.Children[i]^);
     end;
     t:=TrimLeft(t);
     k:=pos(#13#10,t);
     if k>0 then begin
      while k>0 do begin
       result:=result+'    '+copy(t,1,k-1)+#13#10;
       delete(t,1,k+1);
       k:=pos(#13#10,t);
      end;
      if length(t)>0 then begin
       result:=result+'    '+t+#13#10;
      end;
     end else begin
      result:=result+'    '+t+#13#10;
     end;
     result:=result+#13#10#13#10;
    end else if (Node.TagName='ABBR') or (Node.TagName='ACRONYM') then begin
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     for i:=0 to length(Node.TagParameters)-1 do begin
      if Node.TagParameters[i].Name='TITLE' then begin
       result:=result+'('+ConvertEntities(Node.TagParameters[i].Value,Charset)+')';
       break;
      end;
     end;
    end else if Node.TagName='TABLE' then begin
     SetLength(Cells,0,0);
     t:='';
     for i:=0 to length(Node.Children)-1 do begin
      if Node.Children[i]^.NodeType=ntTAG then begin
       if Node.Children[i]^.TagName='TR' then begin
        SetLength(Cells,length(Cells)+1);
        SetLength(Cells[length(Cells)-1],0);
        for j:=0 to length(Node.Children[i]^.Children)-1 do begin
         if Node.Children[i]^.Children[j]^.NodeType=ntTAG then begin
          if (Node.Children[i]^.Children[j]^.TagName='TD') or
             (Node.Children[i]^.Children[j]^.TagName='TH') then begin
           SetLength(Cells[length(Cells)-1],length(Cells[length(Cells)-1])+1);
           Cells[length(Cells)-1,length(Cells[length(Cells)-1])-1].Text:=Build(Node.Children[i]^.Children[j]^);
           Cells[length(Cells)-1,length(Cells[length(Cells)-1])-1].Align:=caLEFT;
           for k:=0 to length(Node.Children[i]^.Children[j]^.TagParameters)-1 do begin
            if Node.Children[i]^.Children[j]^.TagParameters[k].Name='ALIGN' then begin
             s1:=trim(uppercase(Node.Children[i]^.Children[j]^.TagParameters[k].Value));
             if s1='LEFT' then begin
              Cells[length(Cells)-1,length(Cells[length(Cells)-1])-1].Align:=caLEFT;
             end else if s1='CENTER' then begin
              Cells[length(Cells)-1,length(Cells[length(Cells)-1])-1].Align:=caCENTER;
             end else if s1='RIGHT' then begin
              Cells[length(Cells)-1,length(Cells[length(Cells)-1])-1].Align:=caRIGHT;
             end;
            end;
           end;
          end else begin
           t:=t+Build(Node.Children[i]^.Children[j]^);
          end;
         end else begin
          t:=t+Build(Node.Children[i]^.Children[j]^);
         end;
        end;
       end else begin
        t:=t+Build(Node.Children[i]^);
       end;
      end else begin
       t:=t+Build(Node.Children[i]^);
      end;
     end;
     if length(Cells)>0 then begin
      SetLength(ch,length(Cells));
      j:=0;
      for i:=0 to length(Cells)-1 do begin
       if j<length(Cells[i]) then begin
        j:=length(Cells[i]);
       end;
      end;
      if j>0 then begin
       SetLength(cw,j);
       for i:=0 to length(cw)-1 do begin
        cw[i]:=4;
       end;
       for i:=0 to length(ch)-1 do begin
        ch[i]:=0;
       end;
       for i:=0 to length(Cells)-1 do begin
        for j:=0 to length(Cells[i])-1 do begin
         s1:=Cells[i,j].Text;
         w:=0;
         h:=0;
         while length(s1)>0 do begin
          k:=pos(#13#10,s1);
          if k>0 then begin
           s2:=copy(s1,1,k-1);
           delete(s1,1,k+1);
          end else begin
           s2:=s1;
           s1:='';
          end;
          if w<length(s2) then begin
           w:=length(s2);
          end;
          inc(h);
         end;
         if (j+1)<length(cw) then begin
          inc(w);
         end;
         if cw[j]<w then begin
          cw[j]:=w;
         end;
         if ch[i]<h then begin
          ch[i]:=h;
         end;
        end;
       end;
       w:=2;
       for i:=0 to length(cw)-1 do begin
        inc(w,cw[i]);
       end;
       for i:=0 to length(Cells)-1 do begin
        SetLength(Rows,ch[i]);
        for j:=0 to length(Rows)-1 do begin
         SetLength(Rows[j],w);
         fillchar(Rows[j,1],w,#32);
         Rows[j,1]:='|';
         Rows[j,w]:='|';
        end;
        x:=2;
        for j:=0 to length(Cells[i])-1 do begin
         y:=0;
         s1:=Cells[i,j].Text;
         while length(s1)>0 do begin
          k:=pos(#13#10,s1);
          if k>0 then begin
           s2:=copy(s1,1,k-1);
           delete(s1,1,k+1);
          end else begin
           s2:=s1;
           s1:='';
          end;
          if y<length(Rows) then begin
           k:=length(s2);
           xx:=x;
           case Cells[i,j].Align of
            caRIGHT:begin
             if length(s2)=cw[j] then begin
              s2:=copy(s2,1,cw[j]);
              if (x+k)>=length(Rows[y]) then begin
               k:=length(Rows[y])-x;
              end;
              move(s2[1],Rows[y,xx],k);
             end else begin
              xx:=length(s2)-cw[j];
              if xx<1 then begin
               xx:=1;
              end;
              s2:=copy(s2,xx,cw[j]);
              xx:=(x+cw[j])-length(s2);
              if (xx+k)>=length(Rows[y]) then begin
               k:=length(Rows[y])-xx;
              end;
              move(s2[1],Rows[y,xx],k);
             end;
            end;
            caCENTER:begin
             if length(s2)=cw[j] then begin
              s2:=copy(s2,1,cw[j]);
              if (x+k)>=length(Rows[y]) then begin
               k:=length(Rows[y])-x;
              end;
              move(s2[1],Rows[y,xx],k);
             end else begin
              xx:=length(s2)-cw[j];
              if xx<1 then begin
               xx:=1;
              end;
              s2:=copy(s2,xx,cw[j]);
              xx:=x+(((cw[j]-length(s2))+1) div 2);
              while (xx+k)>=length(Rows[y]) do begin
               dec(xx);
               if xx<x then begin
                xx:=x;
                break;
               end;
              end;
              if (xx+k)>=length(Rows[y]) then begin
               k:=length(Rows[y])-xx;
              end;
              move(s2[1],Rows[y,xx],k);
             end;
            end;
            else begin
             s2:=copy(s2,1,cw[j]);
             if (x+k)>=length(Rows[y]) then begin
              k:=length(Rows[y])-x;
             end;
             move(s2[1],Rows[y,xx],k);
            end;
           end;
          end;
          inc(y);
         end;
         if (j+1)<length(cw) then begin
          for y:=0 to length(Rows)-1 do begin
           Rows[y,x+cw[j]-1]:='|';
          end;
         end;
         inc(x,cw[j]);
        end;
        for j:=0 to length(Rows)-1 do begin
         result:=result+Rows[j]+#13#10;
        end;
        SetLength(Rows,0);
        if (i+1)<length(ch) then begin
         SetLength(s1,w);
         fillchar(s1[2],w-2,'-');
         s1[1]:='|';
         s1[w]:='|';
         x:=2;
         for j:=0 to length(Cells[i])-1 do begin
          xx:=x;
          inc(x,cw[j]);
          if (x>=1) and (x<=w) then begin
           if (x>1) and ((j+1)<length(Cells[i])) then begin
            s1[x-1]:='|';
           end;
           case Cells[i,j].Align of
            caLEFT:begin
             if (xx>1) and (xx<=w) then begin
              s1[xx]:=':';
             end;
            end;
            caRIGHT:begin
             if (x>1) and (x<=w) then begin
              s1[x-1]:=':';
             end;
            end;
            caCENTER:begin
             if (xx>1) and (xx<=w) then begin
              s1[xx]:=':';
             end;
             if (x>1) and (x<=w) then begin
              s1[x-1]:=':';
             end;
            end;
           end;
          end;
         end;
         result:=result+s1+#13#10;
        end;
       end;
      end;
      SetLength(Cells,0,0);
     end;
     result:=result+t+#13#10#13#10;
    end else if Node.TagName='CODE' then begin
     result:='`';
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+'`';
    end else if Node.TagName='P' then begin
     result:=#13#10#13#10;
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+#13#10#13#10;
    end else if Node.TagName='DIV' then begin
     result:=#13#10#13#10;
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+#13#10#13#10;
    end else if Node.TagName='A' then begin
     result:=' [';
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
     result:=result+'](';
     for i:=0 to length(Node.TagParameters)-1 do begin
      if Node.TagParameters[i].Name='HREF' then begin
       result:=result+ConvertEntities(Node.TagParameters[i].Value,Charset);
       break;
      end;
     end;
     result:=result+') ';
    end else if Node.TagName='IMG' then begin
     result:=' ![';
     for i:=0 to length(Node.TagParameters)-1 do begin
      if Node.TagParameters[i].Name='ALT' then begin
       result:=result+ConvertEntities(Node.TagParameters[i].Value,Charset);
       break;
      end;
     end;
     result:=result+'](';
     for i:=0 to length(Node.TagParameters)-1 do begin
      if Node.TagParameters[i].Name='SRC' then begin
       result:=result+ConvertEntities(Node.TagParameters[i].Value,Charset);
       break;
      end;
     end;
     result:=result+') ';
    end else if Node.TagName='SCRIPT' then begin
    end else if Node.TagName='STYLE' then begin
    end else if Node.TagName='LINK' then begin
    end else begin
     for i:=0 to length(Node.Children)-1 do begin
      result:=result+Build(Node.Children[i]^);
     end;
    end;
   end;
   ntTEXT:begin
    result:=Node.Text;
   end;
   else begin
    result:='';
   end;
  end;
 end;
begin
 CharSet:=UTF_8;
 result:=Build(RootNode);
end;

function THTML.GetHTML(AllowedTags:TStringList=nil):ansistring;
 function Build(var Node:THTMLNode):ansistring;
 var i:longint;
 begin
  case Node.NodeType of
   ntROOT:begin
    result:='';
    for i:=0 to length(Node.Children)-1 do begin
     result:=result+Build(Node.Children[i]^);
    end;
   end;
   ntTAG:begin
    if (AllowedTags=nil) or (AllowedTags.IndexOf(LowerCase(Node.TagName))>=0) then begin
     result:='<'+LowerCase(Node.TagName);
     for i:=0 to length(Node.TagParameters)-1 do begin
      result:=result+' '+LowerCase(Node.TagParameters[i].Name)+'="'+ConvertToEntities(Node.TagParameters[i].Value)+'"';
     end;
     if (length(Node.Children)>0) or not ((Node.TagName='BR') or (Node.TagName='HR')) then begin
      result:=result+'>';
      for i:=0 to length(Node.Children)-1 do begin
       result:=result+Build(Node.Children[i]^);
      end;
      result:=result+'</'+LowerCase(Node.TagName)+'>';
     end else begin
      result:=result+'/>';
     end;
    end;
   end;
   ntTEXT:begin
    result:=ConvertToEntities(Node.Text);
   end;
   else begin
    result:='';
   end;
  end;
 end;
begin
 result:=Build(RootNode);
end;

function MarkDownToHTML(const pInputText:ansistring):ansistring;
const tcaNone=0;
      tcaLeft=1;
      tcaRight=2;
      tcaCenter=3;
      tfHeader=4;
type PMarkDownBlockType=^TMarkDownBlockType;
     TMarkDownBlockType=
      (
       mdbtRoot,
       mdbtBlankLine,
       mdbtText,
       mdbtEntity,
       mdbtHTMLTag,
       mdbtWebLink,
       mdbtSubscript,
       mdbtLink,
       mdbtImage,
       mdbtReferenceLink,
       mdbtReferenceImage,
       mdbtLineBreak,
       mdbtEmphasis,
       mdbtStrikethrough,
       mdbtATXHeader,
       mdbtHorizontalRule,
       mdbtHighlight,
       mdbtParagraph,
       mdbtSETextHeader,
       mdbtCodeBlock,
       mdbtFencedCodeBlock,
       mdbtCodeSpan,
       mdbtBlockQuote,
       mdbtUnorderedList,
       mdbtOrderedList,
       mdbtListItem,
       mdbtTable,
       mdbtTableRow,
       mdbtTableCell,
       mdbtHTMLBlock,
       mdbtHTMLComment,
       mdbtLinkReferenceDefinition
      );
     PMarkDownBlock=^TMarkDownBlock;
     TMarkDownBlock=record
      Previous:PMarkDownBlock;
      Next:PMarkDownBlock;
      Head:PMarkDownBlock;
      Tail:PMarkDownBlock;
      Parent:PMarkDownBlock;
      BlockType:TMarkDownBlockType;
      StringData:ansistring;
      Tag:longint;
     end;
var RootMarkDownBlock:PMarkDownBlock;
    LinkStringList:TStringList;
 function NewMarkDownBlock(const pParent:PMarkDownBlock;const pBlockType:TMarkDownBlockType;const pStringData:ansistring;const pTag:longint):PMarkDownBlock;
 begin
  GetMem(result,SizeOf(TMarkDownBlock));
  FillChar(result^,SizeOf(TMarkDownBlock),#0);
  Initialize(result^);
  if assigned(pParent^.Tail) then begin
   result^.Previous:=pParent^.Tail;
   pParent^.Tail^.Next:=result;
  end else begin
   result^.Previous:=nil;
   pParent^.Head:=result;
  end;
  result^.Next:=nil;
  pParent^.Tail:=result;
  result^.Parent:=pParent;
  result^.BlockType:=pBlockType;
  result^.StringData:=pStringData;
  result^.Tag:=pTag;
 end;
 procedure FreeMarkDownBlock(const pCurrentMarkDownBlock:PMarkDownBlock);
 var CurrentMarkDownBlock,NextMarkDownBlock:PMarkDownBlock;
 begin
  CurrentMarkDownBlock:=pCurrentMarkDownBlock^.Head;
  while assigned(CurrentMarkDownBlock) do begin
   NextMarkDownBlock:=CurrentMarkDownBlock^.Next;
   FreeMarkDownBlock(CurrentMarkDownBlock);
   CurrentMarkDownBlock:=NextMarkDownBlock;
  end;
  Finalize(pCurrentMarkDownBlock^);
  FreeMem(pCurrentMarkDownBlock);
 end;
 function CleanText(const pInputText:ansistring):ansistring;
 var InputPosition,InputLength,OutputPosition,LineBegin,TabCounter,LineLength:longint;
     LineHasContent:boolean;
 begin
  result:='';
  InputPosition:=1;
  InputLength:=length(pInputText);
  SetLength(result,InputLength);
  OutputPosition:=0;
  try
   TabCounter:=0;
   LineHasContent:=false;
   LineBegin:=OutputPosition;
   LineLength:=0;
   if ((InputPosition+2)<=InputLength) and
      (pInputText[InputPosition+0]=#$ef) and
      (pInputText[InputPosition+1]=#$bb) and
      (pInputText[InputPosition+2]=#$bf) then begin
    inc(InputPosition,3);
   end;
   while InputPosition<=InputLength do begin
    case pInputText[InputPosition] of
     #9:begin
      inc(InputPosition);
      TabCounter:=4-TabCounter;
      while TabCounter>0 do begin
       inc(OutputPosition);
       if length(result)<OutputPosition then begin
        SetLength(result,OutputPosition*2);
       end;
       result[OutputPosition]:=#32;
       dec(TabCounter);
      end;
      inc(LineLength);
     end;
     #10,#13:begin
      if (LineLength>0) and not LineHasContent then begin
       OutputPosition:=LineBegin;
      end;
      inc(OutputPosition);
      if length(result)<OutputPosition then begin
       SetLength(result,OutputPosition*2);
      end;
      result[OutputPosition]:=#10;
      case pInputText[InputPosition] of
       #10:begin
        inc(InputPosition);
        if (InputPosition<=InputLength) and (pInputText[InputPosition]=#13) then begin
         inc(InputPosition);
        end;
       end;
       #13:begin
        inc(InputPosition);
        if (InputPosition<=InputLength) and (pInputText[InputPosition]=#10) then begin
         inc(InputPosition);
        end;
       end;
      end;
      LineBegin:=OutputPosition;
      LineLength:=0;
      TabCounter:=0;
      LineHasContent:=false;
     end;
     else begin
      inc(OutputPosition);
      if length(result)<OutputPosition then begin
       SetLength(result,OutputPosition*2);
      end;
      case pInputText[InputPosition] of
       #0..#32:begin
        result[OutputPosition]:=#32;
       end;
       else begin
        result[OutputPosition]:=pInputText[InputPosition];
        LineHasContent:=true;
       end;
      end;
      inc(InputPosition);
      inc(LineLength);
      TabCounter:=(TabCounter+1) and 3;
     end;
    end;
   end;
  finally
   SetLength(result,OutputPosition);
  end;
 end;
 function CleanNewLines(pInputText:ansistring):ansistring;
 var InputPosition,InputLength,OutputPosition:longint;
 begin
  result:='';
  InputPosition:=1;
  InputLength:=length(pInputText);
  SetLength(result,InputLength);
  OutputPosition:=0;
  try
   while (InputPosition<=InputLength) and (pInputText[InputPosition]=#10) do begin
    inc(InputPosition);
   end;
   while InputPosition<=InputLength do begin
    case pInputText[InputPosition] of
     #10:begin
      inc(OutputPosition);
      if length(result)<OutputPosition then begin
       SetLength(result,OutputPosition*2);
      end;
      result[OutputPosition]:=#10;
      inc(InputPosition);
      while (InputPosition<=InputLength) and (pInputText[InputPosition]=#10) do begin
       inc(InputPosition);
      end;
     end;
     else begin
      inc(OutputPosition);
      if length(result)<OutputPosition then begin
       SetLength(result,OutputPosition*2);
      end;
      result[OutputPosition]:=pInputText[InputPosition];
      inc(InputPosition);
     end;
    end;
   end;
  finally
   SetLength(result,OutputPosition);
  end;
 end;
 function IsHeaderLine(const pInputText:ansistring;const pInputFromPosition,pInputToPosition:longint):longint;
 var InputPosition:longint;
 begin
  result:=0;
  InputPosition:=pInputFromPosition;
  if InputPosition<=pInputToPosition then begin
   case pInputText[InputPosition] of
    '=':begin
     while (InputPosition<=pInputToPosition) and (pInputText[InputPosition]='=') do begin
      inc(InputPosition);
     end;
     if InputPosition<=pInputToPosition then begin
      while (InputPosition<=pInputToPosition) and (pInputText[InputPosition]=#32) do begin
       inc(InputPosition);
      end;
      if (InputPosition>pInputToPosition) or (pInputText[InputPosition]=#10) then begin
       result:=1;
       exit;
      end;
     end;
    end;
    '-':begin
     while (InputPosition<=pInputToPosition) and (pInputText[InputPosition]='-') do begin
      inc(InputPosition);
     end;
     if InputPosition<=pInputToPosition then begin
      while (InputPosition<=pInputToPosition) and (pInputText[InputPosition]=#32) do begin
       inc(InputPosition);
      end;
      if (InputPosition>pInputToPosition) or (pInputText[InputPosition]=#10) then begin
       result:=2;
       exit;
      end;
     end;
    end;
   end;
  end;
 end;
 function IsNextHeaderLine(const pInputText:ansistring;const pInputFromPosition,pInputToPosition:longint):longint;
 var InputPosition:longint;
 begin
  result:=0;
  InputPosition:=pInputFromPosition;
  if InputPosition<=pInputToPosition then begin
   while (InputPosition<=pInputToPosition) and (pInputText[InputPosition]<>#10) do begin
    inc(InputPosition);
   end;
   if InputPosition<=pInputToPosition then begin
    result:=IsHeaderLine(pInputText,pInputFromPosition,pInputToPosition);
   end;
  end;
 end;
 function FindEmphasisChar(const pInputText:ansistring;const pInputFromPosition,pInputToPosition:longint;const pEmphasisChar:ansichar):longint;
 var InputPosition,OpenCount,CloseCount,TempResult:longint;
     CloseChar:ansichar;
 begin
  result:=0;
  InputPosition:=pInputFromPosition+1;
  while InputPosition<=pInputToPosition do begin
   while (InputPosition<=pInputToPosition) and not ((pInputText[InputPosition] in ['`','[']) or (pInputText[InputPosition]=pEmphasisChar)) do begin
    inc(InputPosition);
   end;
   if InputPosition<=pInputToPosition then begin
    if pInputText[InputPosition]=pEmphasisChar then begin
     result:=InputPosition;
     exit;
    end;
    if ((InputPosition-1)>=pInputFromPosition) and (pInputText[InputPosition-1]='\') then begin
     inc(InputPosition);
    end else begin
     case pInputText[InputPosition] of
      '`':begin
       OpenCount:=0;
       while (InputPosition<=pInputToPosition) and (pInputText[InputPosition]='`') do begin
        inc(InputPosition);
        inc(OpenCount);
       end;
       if InputPosition<=pInputToPosition then begin
        TempResult:=0;
        CloseCount:=0;
        while (InputPosition<=pInputToPosition) and (CloseCount<OpenCount) do begin
         case pInputText[InputPosition] of
          '`':begin
           inc(CloseCount);
          end;
          else begin
           CloseCount:=0;
           if (pInputText[InputPosition]=pEmphasisChar) and (TempResult=0) then begin
            TempResult:=InputPosition;
           end;
          end;
         end;
         inc(InputPosition);
        end;
        if InputPosition>pInputToPosition then begin
         result:=TempResult;
         exit;
        end;
       end;
      end;
      '[':begin
       TempResult:=0;
       inc(InputPosition);
       while InputPosition<=pInputToPosition do begin
        case pInputText[InputPosition] of
         ']':begin
          break;
         end;
         else begin
          if (pInputText[InputPosition]=pEmphasisChar) and (TempResult=0) then begin
           TempResult:=InputPosition;
          end;
         end;
        end;
        inc(InputPosition);
       end;
       inc(InputPosition);
       while (InputPosition<=pInputToPosition) and (pInputText[InputPosition] in [#10,#32]) do begin
        inc(InputPosition);
       end;
       if InputPosition<=pInputToPosition then begin
        CloseChar:=#0;
        if CloseChar=#0 then begin
        end;
        case pInputText[InputPosition] of
         '[':begin
          CloseChar:=']';
         end;
         '(':begin
          CloseChar:=')';
         end;
         else begin
          if TempResult<>0 then begin
           result:=TempResult;
           exit;
          end else begin
           continue;
          end;
         end;
        end;
        while (InputPosition<=pInputToPosition) and (pInputText[InputPosition]<>CloseChar) do begin
         if (pInputText[InputPosition]=pEmphasisChar) and (TempResult=0) then begin
          TempResult:=InputPosition;
         end;
         inc(InputPosition);
        end;
        if InputPosition<=pInputToPosition then begin
         inc(InputPosition);
        end else begin
         result:=TempResult;
         exit;
        end;
       end else begin
        result:=TempResult;
        exit;
       end;
      end;
     end;
    end;
   end;
  end;
 end;
 function ParseInline(const pParentMarkDownBlock:PMarkDownBlock;const pInputText:ansistring;const pInputFromPosition,pInputToPosition:longint):longint; forward;
 function ParseEmphasis1(const pParentMarkDownBlock:PMarkDownBlock;const pInputText:ansistring;const pInputFromPosition,pInputToPosition:longint;const pEmphasisChar:ansichar):longint;
 var InputPosition,TempPosition:longint;
     MarkDownBlock:PMarkDownBlock;
 begin
  result:=0;
  InputPosition:=pInputFromPosition;
  if ((InputPosition+1)<=pInputToPosition) and
     (pInputText[InputPosition+0]=pEmphasisChar) and
     (pInputText[InputPosition+1]=pEmphasisChar) then begin
   InputPosition:=1;
  end;
  while InputPosition<=pInputToPosition do begin
   TempPosition:=FindEmphasisChar(pInputText,InputPosition,pInputToPosition,pEmphasisChar);
   if TempPosition>=InputPosition then begin
    InputPosition:=TempPosition;
    if InputPosition<=pInputToPosition then begin
     if (pInputText[InputPosition]=pEmphasisChar) and not (((InputPosition-1)>=pInputFromPosition) and (pInputText[InputPosition-1]=#32)) then begin
      if ((InputPosition+1)<=pInputToPosition) and (pInputText[InputPosition+1] in ['A'..'Z','a'..'z','0'..'9']) then begin
       continue;
      end;
      MarkDownBlock:=NewMarkDownBlock(pParentMarkDownBlock,mdbtEmphasis,'',1);
      ParseInline(MarkDownBlock,pInputText,pInputFromPosition,InputPosition-1);
      result:=InputPosition+1;
      exit;
     end;
    end;
   end;
   break;
  end;
 end;
 function ParseEmphasis2(const pParentMarkDownBlock:PMarkDownBlock;const pInputText:ansistring;const pInputFromPosition,pInputToPosition:longint;const pEmphasisChar:ansichar):longint;
 var InputPosition,TempPosition:longint;
     MarkDownBlock:PMarkDownBlock;
 begin
  result:=0;
  InputPosition:=pInputFromPosition;
  while InputPosition<=pInputToPosition do begin
   TempPosition:=FindEmphasisChar(pInputText,InputPosition,pInputToPosition,pEmphasisChar);
   if TempPosition>=InputPosition then begin
    InputPosition:=TempPosition;
    if (((InputPosition+1)<=pInputToPosition) and
        (pInputText[InputPosition+0]=pEmphasisChar) and
        (pInputText[InputPosition+1]=pEmphasisChar)) and not
       (((InputPosition-1)>=pInputFromPosition) and (pInputText[InputPosition-1]=#32)) then begin
     case pEmphasisChar of
      '~':begin
       MarkDownBlock:=NewMarkDownBlock(pParentMarkDownBlock,mdbtStrikethrough,'',0);
      end;
      '=':begin
       MarkDownBlock:=NewMarkDownBlock(pParentMarkDownBlock,mdbtHighlight,'',0);
      end;
      else begin
       MarkDownBlock:=NewMarkDownBlock(pParentMarkDownBlock,mdbtEmphasis,'',2);
      end;
     end;
     ParseInline(MarkDownBlock,pInputText,pInputFromPosition,InputPosition-1);
     result:=InputPosition+2;
     exit;
    end;
    inc(InputPosition);
   end else begin
    break;
   end;
  end;
 end;
 function ParseEmphasis3(const pParentMarkDownBlock:PMarkDownBlock;const pInputText:ansistring;const pInputFromPosition,pInputToPosition:longint;const pEmphasisChar:ansichar):longint;
 var InputPosition,EndPosition:longint;
     MarkDownBlock:PMarkDownBlock;
 begin
  result:=0;
  InputPosition:=pInputFromPosition;
  while InputPosition<=pInputToPosition do begin
   EndPosition:=FindEmphasisChar(pInputText,InputPosition,pInputToPosition,pEmphasisChar);
   if EndPosition>=InputPosition then begin
    if ((EndPosition<=pInputToPosition) and
        (pInputText[EndPosition]=pEmphasisChar)) and not
       (((EndPosition-1)>=pInputFromPosition) and (pInputText[EndPosition-1]=#32)) then begin
     if ((EndPosition+2)<=pInputToPosition) and
        (pInputText[EndPosition+1]=pEmphasisChar) and
        (pInputText[EndPosition+2]=pEmphasisChar) then begin
      MarkDownBlock:=NewMarkDownBlock(pParentMarkDownBlock,mdbtEmphasis,'',3);
      ParseInline(MarkDownBlock,pInputText,pInputFromPosition,EndPosition-1);
      result:=EndPosition+3;
     end else if ((EndPosition+1)<=pInputToPosition) and
                 (pInputText[EndPosition+1]=pEmphasisChar) then begin
      result:=ParseEmphasis1(pParentMarkDownBlock,pInputText,EndPosition-2,pInputToPosition,pEmphasisChar);
     end else begin
      result:=ParseEmphasis2(pParentMarkDownBlock,pInputText,EndPosition-1,pInputToPosition,pEmphasisChar);
     end;
     exit;
    end;
    InputPosition:=EndPosition;
   end else begin
    break;
   end;
  end;
 end;
 function ParseInline(const pParentMarkDownBlock:PMarkDownBlock;const pInputText:ansistring;const pInputFromPosition,pInputToPosition:longint):longint;
 var InputPosition,EndPosition,TempPosition,NewPosition,Count,StartPosition,StopPosition,TempIndex:longint;
     MarkDownBlock:PMarkDownBlock;
     BlockText,LinkText,Link:ansistring;
     EmphasisChar:ansichar;
     IsImage,OK:boolean;
 begin
  result:=0;
  InputPosition:=pInputFromPosition;
  EndPosition:=InputPosition;
  while InputPosition<=pInputToPosition do begin
   while (EndPosition<=pInputToPosition) and not (pInputText[EndPosition] in ['*','_','~','=','`',#10,'!','[','<','\','&','^']) do begin
    inc(EndPosition);
   end;
   if InputPosition<=EndPosition then begin
    if InputPosition<EndPosition then begin
     NewPosition:=EndPosition;
     while ((EndPosition+2)<=pInputToPosition) and ((pInputText[EndPosition-1]=#32) and (pInputText[EndPosition-2]=#32)) do begin
      dec(EndPosition);
     end;
     BlockText:=copy(pInputText,InputPosition,EndPosition-InputPosition);
     if assigned(pParentMarkDownBlock^.Tail) and (pParentMarkDownBlock^.Tail^.BlockType=mdbtText) then begin
      pParentMarkDownBlock^.Tail^.StringData:=pParentMarkDownBlock^.Tail^.StringData+BlockText;
     end else begin
      NewMarkDownBlock(pParentMarkDownBlock,mdbtText,BlockText,0);
     end;
     InputPosition:=NewPosition;
    end;
    if InputPosition<=pInputToPosition then begin
     case pInputText[InputPosition] of
      '*','_','~','=':begin
       if (((EndPosition-1)<pInputFromPosition) or (pInputText[EndPosition-1] in [#32,'>'])) then begin
        EmphasisChar:=pInputText[EndPosition];
        if ((EndPosition+1)<=pInputToPosition) and
           (pInputText[EndPosition+1]<>EmphasisChar) then begin
         if (not (EmphasisChar in ['~','='])) and (pInputText[EndPosition+1]<>#32) then begin
          NewPosition:=ParseEmphasis1(pParentMarkDownBlock,pInputText,EndPosition+1,pInputToPosition,EmphasisChar);
          if NewPosition>EndPosition then begin
           InputPosition:=NewPosition;
           EndPosition:=NewPosition;
           continue;
          end;
         end;
        end else if ((EndPosition+2)<=pInputToPosition) and
                    (pInputText[EndPosition+1]=EmphasisChar) and
                    (pInputText[EndPosition+2]<>EmphasisChar) then begin
         if pInputText[EndPosition+2]<>#32 then begin
          NewPosition:=ParseEmphasis2(pParentMarkDownBlock,pInputText,EndPosition+2,pInputToPosition,EmphasisChar);
          if NewPosition>EndPosition then begin
           InputPosition:=NewPosition;
           EndPosition:=NewPosition;
           continue;
          end;
         end;
        end else if ((EndPosition+3)<=pInputToPosition) and
                    (pInputText[EndPosition+1]=EmphasisChar) and
                    (pInputText[EndPosition+2]=EmphasisChar) and
                    (pInputText[EndPosition+3]<>EmphasisChar) then begin
         if (not (EmphasisChar in ['~','='])) and (pInputText[EndPosition+3]<>#32) then begin
          NewPosition:=ParseEmphasis3(pParentMarkDownBlock,pInputText,EndPosition+3,pInputToPosition,EmphasisChar);
          if NewPosition>EndPosition then begin
           InputPosition:=NewPosition;
           EndPosition:=NewPosition;
           continue;
          end;
         end;
        end;
       end;
      end;
      '`':begin
       TempPosition:=EndPosition;
       Count:=0;
       while (TempPosition<=pInputToPosition) and (pInputText[TempPosition]='`') do begin
        inc(TempPosition);
        inc(Count);
       end;
       if Count>0 then begin
        StartPosition:=TempPosition;
        NewPosition:=0;
        while TempPosition<=pInputToPosition do begin
         if pInputText[TempPosition]='`' then begin
          NewPosition:=TempPosition;
          break;
         end;
         inc(TempPosition);
        end;
        if (NewPosition>=StartPosition) and (NewPosition<=pInputToPosition) then begin
         StopPosition:=NewPosition;
         TempPosition:=NewPosition;
         while (TempPosition<=pInputToPosition) and (pInputText[TempPosition]='`') do begin
          inc(TempPosition);
          dec(Count);
         end;
         if Count=0 then begin
          EndPosition:=TempPosition;
          InputPosition:=EndPosition;
          while (StartPosition<StopPosition) and (pInputText[StartPosition]=#32) do begin
           inc(StartPosition);
          end;
          while (StartPosition<StopPosition) and (pInputText[StopPosition-1]=#32) do begin
           dec(StopPosition);
          end;
          if StartPosition<StopPosition then begin
           NewMarkDownBlock(pParentMarkDownBlock,mdbtCodeSpan,copy(pInputText,StartPosition,StopPosition-StartPosition),0);
          end else begin
           NewMarkDownBlock(pParentMarkDownBlock,mdbtCodeSpan,'',0);
          end;
          continue;
         end;
        end;
       end;
      end;
      #10:begin
       if ((EndPosition-2)>=pInputFromPosition) and (pInputText[EndPosition-1]=#32) and (pInputText[EndPosition-2]=#32) then begin
        if assigned(pParentMarkDownBlock^.Tail) and (pParentMarkDownBlock^.Tail^.BlockType=mdbtText) then begin
         for TempIndex:=length(pParentMarkDownBlock^.Tail^.StringData) downto 1 do begin
          if pParentMarkDownBlock^.Tail^.StringData[TempIndex]<>#32 then begin
           SetLength(pParentMarkDownBlock^.Tail^.StringData,TempIndex);
           break;
          end;
         end;
        end;
        NewMarkDownBlock(pParentMarkDownBlock,mdbtLineBreak,'',0);
        inc(EndPosition);
        InputPosition:=EndPosition;
        continue;
       end;
      end;
      '!','[':begin
       TempPosition:=EndPosition;
       if (((TempPosition+1)<=pInputToPosition) and (pInputText[TempPosition]='!') and (pInputText[TempPosition+1]='[')) or
          ((TempPosition<=pInputToPosition) and (pInputText[TempPosition]='[')) then begin
        if (TempPosition<=pInputToPosition) and (pInputText[TempPosition]='!')then begin
         inc(TempPosition);
         IsImage:=true;
        end else begin
         IsImage:=false;
        end;
        if (TempPosition<=pInputToPosition) and (pInputText[TempPosition]='[') then begin
         inc(TempPosition);
         StartPosition:=TempPosition;
         StopPosition:=TempPosition;
         Count:=1;
         while TempPosition<=pInputToPosition do begin
          case pInputText[TempPosition] of
           '\':begin
            inc(TempPosition);
            if (TempPosition<=pInputToPosition) and (pInputText[TempPosition] in ['[',']']) then begin
             inc(TempPosition);
            end;
           end;
           '[':begin
            inc(Count);
            inc(TempPosition);
           end;
           ']':begin
            dec(Count);
            StopPosition:=TempPosition;
            inc(TempPosition);
            if Count=0 then begin
             break;
            end;
           end;
           else begin
            inc(TempPosition);
           end;
          end;
         end;
         if (StartPosition<StopPosition) and
            ((StopPosition<=pInputToPosition) and (pInputText[StopPosition]=']')) then begin
          LinkText:=copy(pInputText,StartPosition,StopPosition-StartPosition);
          if TempPosition<=pInputToPosition then begin
           if ((TempPosition+2)<=pInputToPosition) and
              (pInputText[TempPosition+0]=' ') and
              (pInputText[TempPosition+1] in ['(','[',':']) then begin
            inc(TempPosition);
           end;
           case pInputText[TempPosition] of
            '(':begin
             inc(TempPosition);
             StartPosition:=TempPosition;
             StopPosition:=TempPosition;
             Count:=1;
             while TempPosition<=pInputToPosition do begin
              case pInputText[TempPosition] of
               '\':begin
                inc(TempPosition);
                if (TempPosition<=pInputToPosition) and (pInputText[TempPosition] in ['(',')']) then begin
                 inc(TempPosition);
                end;
               end;
               '(':begin
                inc(Count);
                inc(TempPosition);
               end;
               ')':begin
                dec(Count);
                StopPosition:=TempPosition;
                inc(TempPosition);
                if Count=0 then begin
                 break;
                end;
               end;
               else begin
                inc(TempPosition);
               end;
              end;
             end;
             if (StartPosition<StopPosition) and
                ((StopPosition<=pInputToPosition) and (pInputText[StopPosition]=')')) then begin
              Link:=copy(pInputText,StartPosition,StopPosition-StartPosition);
              if IsImage then begin
               MarkDownBlock:=NewMarkDownBlock(pParentMarkDownBlock,mdbtImage,Link,0);
              end else begin
               MarkDownBlock:=NewMarkDownBlock(pParentMarkDownBlock,mdbtLink,Link,0);
              end;
              ParseInline(MarkDownBlock,LinkText,1,length(LinkText));
              InputPosition:=TempPosition;
              EndPosition:=TempPosition;
              continue;
             end;
            end;
            '[':begin
             inc(TempPosition);
             StartPosition:=TempPosition;
             StopPosition:=TempPosition;
             Count:=1;
             while TempPosition<=pInputToPosition do begin
              case pInputText[TempPosition] of
               '\':begin
                inc(TempPosition);
                if (TempPosition<=pInputToPosition) and (pInputText[TempPosition] in ['[',']']) then begin
                 inc(TempPosition);
                end;
               end;
               '[':begin
                inc(Count);
                inc(TempPosition);
               end;
               ']':begin
                dec(Count);
                StopPosition:=TempPosition;
                inc(TempPosition);
                if Count=0 then begin
                 break;
                end;
               end;
               else begin
                inc(TempPosition);
               end;
              end;
             end;
             if (StartPosition<StopPosition) and
                ((StopPosition<=pInputToPosition) and (pInputText[StopPosition]=']')) then begin
              Link:=copy(pInputText,StartPosition,StopPosition-StartPosition);
              if IsImage then begin
               MarkDownBlock:=NewMarkDownBlock(pParentMarkDownBlock,mdbtReferenceImage,Link,0);
              end else begin
               MarkDownBlock:=NewMarkDownBlock(pParentMarkDownBlock,mdbtReferenceLink,Link,0);
              end;
              ParseInline(MarkDownBlock,LinkText,1,length(LinkText));
              InputPosition:=TempPosition;
              EndPosition:=TempPosition;
              continue;
             end;
            end;
            ':':begin
             inc(TempPosition);
             while (TempPosition<=pInputToPosition) and (pInputText[TempPosition]=#32) do begin
              inc(TempPosition);
             end;
             StartPosition:=TempPosition;
             while (TempPosition<=pInputToPosition) and (pInputText[TempPosition]<>#10) do begin
              inc(TempPosition);
             end;
             StopPosition:=TempPosition;
             if StartPosition<StopPosition then begin
              while (StartPosition<StopPosition) and (pInputText[StopPosition-1]=#32) do begin
               dec(StopPosition);
              end;
              if StartPosition<StopPosition then begin
               Link:=copy(pInputText,StartPosition,StopPosition-StartPosition);
               LinkStringList.Values[LowerCase(Trim(LinkText))]:=Link;
               InputPosition:=TempPosition;
               EndPosition:=TempPosition;
               continue;
              end;
             end;
            end;
            else begin
             if IsImage then begin
              MarkDownBlock:=NewMarkDownBlock(pParentMarkDownBlock,mdbtReferenceImage,LinkText,0);
             end else begin
              MarkDownBlock:=NewMarkDownBlock(pParentMarkDownBlock,mdbtReferenceLink,LinkText,0);
             end;
             ParseInline(MarkDownBlock,LinkText,1,length(LinkText));
             InputPosition:=TempPosition;
             EndPosition:=TempPosition;
             continue;
            end;
           end;
          end;
         end;
        end;
       end;
      end;
      '<':begin
       TempPosition:=EndPosition;
       if TempPosition<=pInputToPosition then begin
        if ((TempPosition+3)<=pInputToPosition) and
           (pInputText[TempPosition+1]='!') and
           (pInputText[TempPosition+2]='-') and
           (pInputText[TempPosition+3]='-') then begin
         inc(TempPosition,4);
         OK:=false;
         while TempPosition<=pInputToPosition do begin
          if ((TempPosition+2)<=pInputToPosition) and
             (pInputText[TempPosition+0]='-') and
             (pInputText[TempPosition+1]='-') and
             (pInputText[TempPosition+2]='>') then begin
           inc(TempPosition,3);
           NewMarkDownBlock(pParentMarkDownBlock,mdbtHTMLTag,copy(pInputText,EndPosition,TempPosition-EndPosition),1);
           EndPosition:=TempPosition;
           InputPosition:=EndPosition;
           OK:=true;
           break;
          end else begin
           inc(TempPosition);
          end;
         end;
         if OK then begin
          continue;
         end;
        end else begin
         inc(TempPosition);
         if (TempPosition<=pInputToPosition) and (pInputText[TempPosition]='/') then begin
          inc(TempPosition);
          Count:=0;
          while (TempPosition<=pInputToPosition) and (pInputText[TempPosition] in ['A'..'Z','a'..'z','0'..'9']) do begin
           inc(TempPosition);
           inc(Count);
          end;
          if Count>0 then begin
           while (TempPosition<=pInputToPosition) and (pInputText[TempPosition]=#32) do begin
            inc(TempPosition);
           end;
           if (TempPosition<=pInputToPosition) and (pInputText[TempPosition]='>') then begin
            inc(TempPosition);
            NewMarkDownBlock(pParentMarkDownBlock,mdbtHTMLTag,copy(pInputText,EndPosition,TempPosition-EndPosition),2);
            EndPosition:=TempPosition;
            InputPosition:=EndPosition;
            continue;
           end;
          end;
         end else begin
          Count:=0;
          while (TempPosition<=pInputToPosition) and (pInputText[TempPosition] in ['A'..'Z','a'..'z','0'..'9']) do begin
           inc(TempPosition);
           inc(Count);
          end;
          if Count>0 then begin
           while (TempPosition<=pInputToPosition) and (pInputText[TempPosition]<>'>') do begin
            while (TempPosition<=pInputToPosition) and (pInputText[TempPosition]=#32) do begin
             inc(TempPosition);
            end;
            if (TempPosition<=pInputToPosition) and (pInputText[TempPosition] in ['A'..'Z','a'..'z','0'..'9']) then begin
             Count:=0;
             while (TempPosition<=pInputToPosition) and (pInputText[TempPosition] in ['A'..'Z','a'..'z','0'..'9']) do begin
              inc(TempPosition);
              inc(Count);
             end;
             if Count>0 then begin
              while (TempPosition<=pInputToPosition) and (pInputText[TempPosition]=#32) do begin
               inc(TempPosition);
              end;
              if (TempPosition<=pInputToPosition) and (pInputText[TempPosition]='=') then begin
               inc(TempPosition);
               if TempPosition<=pInputToPosition then begin
                case pInputText[TempPosition] of
                 '''':begin
                  inc(TempPosition);
                  while (TempPosition<=pInputToPosition) and (pInputText[TempPosition]<>'''') do begin
                   inc(TempPosition);
                  end;
                  if (TempPosition<=pInputToPosition) and (pInputText[TempPosition]='''') then begin
                   inc(TempPosition);
                  end else begin
                   break;
                  end;
                 end;
                 '"':begin
                  inc(TempPosition);
                  while (TempPosition<=pInputToPosition) and (pInputText[TempPosition]<>'"') do begin
                   inc(TempPosition);
                  end;
                  if (TempPosition<=pInputToPosition) and (pInputText[TempPosition]='"') then begin
                   inc(TempPosition);
                  end else begin
                   break;
                  end;
                 end;
                 else begin
                  Count:=0;
                  while (TempPosition<=pInputToPosition) and not (pInputText[TempPosition] in [#32,'>','/']) do begin
                   inc(Count);
                   inc(TempPosition);
                  end;
                  if Count=0 then begin
                   break;
                  end;
                 end;
                end;
               end else begin
                break;
               end;
              end else begin
               break;
              end;
             end else begin
              break;
             end;
            end else begin
             break;
            end;
           end;
           if (TempPosition<=pInputToPosition) and (pInputText[TempPosition]='/') then begin
            inc(TempPosition);
            if (TempPosition<=pInputToPosition) and (pInputText[TempPosition]='>') then begin
             inc(TempPosition);
             NewMarkDownBlock(pParentMarkDownBlock,mdbtHTMLTag,copy(pInputText,EndPosition,TempPosition-EndPosition),1);
             EndPosition:=TempPosition;
             InputPosition:=EndPosition;
             continue;
            end;
           end else begin
            if (TempPosition<=pInputToPosition) and (pInputText[TempPosition]='>') then begin
             inc(TempPosition);
             NewMarkDownBlock(pParentMarkDownBlock,mdbtHTMLTag,copy(pInputText,EndPosition,TempPosition-EndPosition),0);
             EndPosition:=TempPosition;
             InputPosition:=EndPosition;
             continue;
            end;
           end;
          end;
         end;
        end;
       end;
      end;
      '\':begin
       if ((EndPosition+1)<=pInputToPosition) and (pInputText[EndPosition+1] in ['*','_','~','=','`',#10,'[',']','<','>','#','+','-','.','!','|','&','(',')','{','}','\','&',':','^']) then begin
        BlockText:=pInputText[EndPosition+1];
        if assigned(pParentMarkDownBlock^.Tail) and (pParentMarkDownBlock^.Tail^.BlockType=mdbtText) then begin
         pParentMarkDownBlock^.Tail^.StringData:=pParentMarkDownBlock^.Tail^.StringData+BlockText;
        end else begin
         NewMarkDownBlock(pParentMarkDownBlock,mdbtText,BlockText,0);
        end;
        inc(EndPosition,2);
        InputPosition:=EndPosition;
        continue;
       end else if EndPosition<=pInputToPosition then begin
        BlockText:=pInputText[EndPosition];
        if assigned(pParentMarkDownBlock^.Tail) and (pParentMarkDownBlock^.Tail^.BlockType=mdbtText) then begin
         pParentMarkDownBlock^.Tail^.StringData:=pParentMarkDownBlock^.Tail^.StringData+BlockText;
        end else begin
         NewMarkDownBlock(pParentMarkDownBlock,mdbtText,BlockText,0);
        end;
        inc(EndPosition);
        InputPosition:=EndPosition;
        continue;
       end;
      end;
      '&':begin
       TempPosition:=EndPosition;
       if TempPosition<=pInputToPosition then begin
        if pInputText[TempPosition]='#' then begin
         inc(TempPosition);
        end;
        if TempPosition<=pInputToPosition then begin
         Count:=0;
         while (TempPosition<=pInputToPosition) and (pInputText[TempPosition] in ['A'..'Z','a'..'z','0'..'9']) do begin
          inc(Count);
          inc(TempPosition);
         end;
         if (Count>0) and (TempPosition<=pInputToPosition) and (pInputText[TempPosition]=';') then begin
          inc(TempPosition);
          NewMarkDownBlock(pParentMarkDownBlock,mdbtEntity,copy(pInputText,EndPosition,TempPosition-EndPosition),0);
          EndPosition:=TempPosition;
          InputPosition:=EndPosition;
          continue;
         end;
        end;
       end;
      end;
      '^':begin
       TempPosition:=EndPosition;
       if TempPosition<=pInputToPosition then begin
        if pInputText[TempPosition]='(' then begin
         inc(TempPosition);
         while TempPosition<=pInputToPosition do begin
          case pInputText[TempPosition] of
           '\':begin
            inc(TempPosition,2);
           end;
           ')':begin
            break;
           end;
           else begin
            inc(TempPosition);
           end;
          end;
         end;
         if (TempPosition<=pInputToPosition) and (pInputText[TempPosition]=')') then begin
          inc(TempPosition);
          MarkDownBlock:=NewMarkDownBlock(pParentMarkDownBlock,mdbtSubscript,'',0);
          ParseInline(MarkDownBlock,pInputText,EndPosition+2,TempPosition-1);
          EndPosition:=TempPosition;
          InputPosition:=EndPosition;
          continue;
         end;
        end else begin
         Count:=0;
         while (TempPosition<=pInputToPosition) and (pInputText[TempPosition]<>#32) do begin
          inc(TempPosition);
          inc(Count);
         end;
         if Count>0 then begin
          MarkDownBlock:=NewMarkDownBlock(pParentMarkDownBlock,mdbtSubscript,'',0);
          ParseInline(MarkDownBlock,pInputText,EndPosition+1,TempPosition-1);
          EndPosition:=TempPosition;
          InputPosition:=EndPosition;
          continue;
         end;
        end;
       end;
      end;
     end;
     inc(EndPosition);
     continue;
    end;
   end;
   break;
  end;
 end;
 function ParseBlock(const pParentMarkDownBlock:PMarkDownBlock;const pInputText:ansistring;const pInputFromPosition,pInputToPosition:longint):longint; forward;
 function IsFencedCode(const pInputText:ansistring;const pInputFromPosition,pInputToPosition:longint):boolean;
 var InputPosition,TempPosition,TempCount:longint;
     FencedCodeChar:ansichar;
 begin
  result:=false;
  InputPosition:=pInputFromPosition;
  if (InputPosition+2)<=pInputToPosition then begin
   if ((InputPosition+2)<=pInputToPosition) and (pInputText[InputPosition]=#32) then begin
    inc(InputPosition);
    if ((InputPosition+1)<=pInputToPosition) and (pInputText[InputPosition]=#32) then begin
     inc(InputPosition);
     if (InputPosition<=pInputToPosition) and (pInputText[InputPosition]=#32) then begin
      inc(InputPosition);
     end;
    end;
   end;
   if (InputPosition<=pInputToPosition) and (pInputText[InputPosition] in ['~','`']) then begin
    FencedCodeChar:=pInputText[InputPosition];
    TempPosition:=InputPosition;
    TempCount:=0;
    while TempPosition<=pInputToPosition do begin
     case pInputText[TempPosition] of
      #32:begin
       inc(TempPosition);
      end;
      else begin
       if pInputText[TempPosition]=FencedCodeChar then begin
        inc(TempPosition);
        inc(TempCount);
       end else begin
        break;
       end;
      end;
     end;
    end;
    if TempCount>2 then begin
     result:=true;
    end;
   end;
  end;
 end;
 function ParseFencedCode(const pParentMarkDownBlock:PMarkDownBlock;const pInputText:ansistring;const pInputFromPosition,pInputToPosition:longint):longint;
 var InputPosition,TempPosition,TempCount,StartPosition,StopPosition,OtherTempCount,BlockStart,BlockEnd:longint;
     FencedCodeChar:ansichar;
 begin
  result:=0;
  InputPosition:=pInputFromPosition;
  if (InputPosition+2)<=pInputToPosition then begin
   if ((InputPosition+2)<=pInputToPosition) and (pInputText[InputPosition]=#32) then begin
    inc(InputPosition);
    if ((InputPosition+1)<=pInputToPosition) and (pInputText[InputPosition]=#32) then begin
     inc(InputPosition);
     if (InputPosition<=pInputToPosition) and (pInputText[InputPosition]=#32) then begin
      inc(InputPosition);
     end;
    end;
   end;
   if (InputPosition<=pInputToPosition) and (pInputText[InputPosition] in ['~','`']) then begin
    FencedCodeChar:=pInputText[InputPosition];
    TempPosition:=InputPosition;
    TempCount:=0;
    while TempPosition<=pInputToPosition do begin
     case pInputText[TempPosition] of
      #32:begin
       inc(TempPosition);
      end;
      else begin
       if pInputText[TempPosition]=FencedCodeChar then begin
        inc(TempPosition);
        inc(TempCount);
       end else begin
        break;
       end;
      end;
     end;
    end;
    if TempCount>2 then begin
     while (InputPosition<=pInputToPosition) and (pInputText[InputPosition]=#32) do begin
      inc(InputPosition);
     end;
     if InputPosition<=pInputToPosition then begin
      case pInputText[InputPosition] of
       '{':begin
        inc(InputPosition);
        StartPosition:=InputPosition;
        while (InputPosition<=pInputToPosition) and not (pInputText[InputPosition] in [#10,'}']) do begin
         inc(InputPosition);
        end;
        StopPosition:=InputPosition;
        if (InputPosition<=pInputToPosition) and (pInputText[InputPosition]='}') then begin
         inc(InputPosition);
         while (StartPosition<StopPosition) and (pInputText[StartPosition]=#32) do begin
          inc(StartPosition);
         end;
         while ((StartPosition+1)<StopPosition) and (pInputText[StopPosition-1]=#32) do begin
          dec(StopPosition);
         end;
         if StartPosition<StopPosition then begin
          NewMarkDownBlock(pParentMarkDownBlock,mdbtFencedCodeBlock,copy(pInputText,StartPosition,StopPosition-StartPosition),0);
         end;
         result:=InputPosition;
        end;
       end;
       else begin
        BlockStart:=InputPosition;
        while (InputPosition<=pInputToPosition) and (pInputText[InputPosition]<>#10) do begin
         inc(InputPosition);
        end;
        while (InputPosition<=pInputToPosition) and (pInputText[InputPosition]=#10) do begin
         inc(InputPosition);
         StartPosition:=InputPosition;
         while (InputPosition<=pInputToPosition) and (pInputText[InputPosition]<>#10) do begin
          inc(InputPosition);
         end;
         StopPosition:=InputPosition;
         TempPosition:=StartPosition;
         if TempPosition<StopPosition then begin
          while (TempPosition<=StopPosition) and (pInputText[StopPosition]=#32) do begin
           inc(TempPosition);
          end;
          OtherTempCount:=0;
          while (TempPosition<=StopPosition) and (pInputText[StopPosition]=FencedCodeChar) and (OtherTempCount<TempCount) do begin
           inc(TempPosition);
           inc(OtherTempCount);
          end;
          if OtherTempCount=TempCount then begin
           BlockEnd:=StartPosition;
           if BlockStart<BlockEnd then begin
            result:=TempPosition;
            NewMarkDownBlock(pParentMarkDownBlock,mdbtFencedCodeBlock,copy(pInputText,BlockStart,BlockEnd-BlockStart),0);
           end;
           exit;
          end;
         end;
        end;
       end;
      end;
     end;
    end;
   end;
  end;
 end;
 function ParseHTMLBlock(const pParentMarkDownBlock:PMarkDownBlock;const pInputText:ansistring;const pInputFromPosition,pInputToPosition:longint):longint;
 var InputPosition,Indentation,Count,StartPosition,TagDepth:longint;
     Tag:ansistring;
 begin
  result:=0;
  InputPosition:=pInputFromPosition;
  if InputPosition<=pInputToPosition then begin
   Indentation:=0;
   while (InputPosition<=pInputToPosition) and (pInputText[InputPosition]=#32) and (Indentation<3) do begin
    inc(InputPosition);
    inc(Indentation);
   end;
   if (InputPosition<=pInputToPosition) and (pInputText[InputPosition]='<') then begin
    StartPosition:=InputPosition;
    if ((InputPosition+3)<=pInputToPosition) and
       (pInputText[InputPosition+1]='!') and
       (pInputText[InputPosition+2]='-') and
       (pInputText[InputPosition+3]='-') then begin
     inc(InputPosition,4);
     while InputPosition<=pInputToPosition do begin
      if ((InputPosition+2)<=pInputToPosition) and
         (pInputText[InputPosition+0]='-') and
         (pInputText[InputPosition+1]='-') and
         (pInputText[InputPosition+2]='>') then begin
       inc(InputPosition,3);
       if assigned(pParentMarkDownBlock) then begin
        NewMarkDownBlock(pParentMarkDownBlock,mdbtHTMLTag,copy(pInputText,StartPosition,InputPosition-StartPosition),1);
       end;
       result:=InputPosition;
       exit;
      end else begin
       inc(InputPosition);
      end;
     end;
    end else begin
     inc(InputPosition);
     Count:=0;
     while (InputPosition<=pInputToPosition) and (pInputText[InputPosition] in ['A'..'Z','a'..'z','0'..'9']) do begin
      inc(InputPosition);
      inc(Count);
     end;
     if Count>0 then begin
      Tag:=LowerCase(copy(pInputText,StartPosition+1,Count));
      while (InputPosition<=pInputToPosition) and (pInputText[InputPosition]<>'>') do begin
       while (InputPosition<=pInputToPosition) and (pInputText[InputPosition]=#32) do begin
        inc(InputPosition);
       end;
       if (InputPosition<=pInputToPosition) and (pInputText[InputPosition] in ['A'..'Z','a'..'z','0'..'9']) then begin
        Count:=0;
        while (InputPosition<=pInputToPosition) and (pInputText[InputPosition] in ['A'..'Z','a'..'z','0'..'9']) do begin
         inc(InputPosition);
         inc(Count);
        end;
        if Count>0 then begin
         while (InputPosition<=pInputToPosition) and (pInputText[InputPosition]=#32) do begin
          inc(InputPosition);
         end;
         if (InputPosition<=pInputToPosition) and (pInputText[InputPosition]='=') then begin
          inc(InputPosition);
          if InputPosition<=pInputToPosition then begin
           case pInputText[InputPosition] of
            '''':begin
             inc(InputPosition);
             while (InputPosition<=pInputToPosition) and (pInputText[InputPosition]<>'''') do begin
              inc(InputPosition);
             end;
             if (InputPosition<=pInputToPosition) and (pInputText[InputPosition]='''') then begin
              inc(InputPosition);
             end else begin
              break;
             end;
            end;
            '"':begin
             inc(InputPosition);
             while (InputPosition<=pInputToPosition) and (pInputText[InputPosition]<>'"') do begin
              inc(InputPosition);
             end;
             if (InputPosition<=pInputToPosition) and (pInputText[InputPosition]='"') then begin
              inc(InputPosition);
             end else begin
              break;
             end;
            end;
            else begin
             Count:=0;
             while (InputPosition<=pInputToPosition) and not (pInputText[InputPosition] in [#32,'>','/']) do begin
              inc(Count);
              inc(InputPosition);
             end;
             if Count=0 then begin
              break;
             end;
            end;
           end;
          end else begin
           break;
          end;
         end else begin
          break;
         end;
        end else begin
         break;
        end;
       end else begin
        break;
       end;
      end;
      if (InputPosition<=pInputToPosition) and (pInputText[InputPosition]='/') then begin
       inc(InputPosition);
       if (InputPosition<=pInputToPosition) and (pInputText[InputPosition]='>') then begin
        inc(InputPosition);
        if assigned(pParentMarkDownBlock) then begin
         NewMarkDownBlock(pParentMarkDownBlock,mdbtHTMLTag,copy(pInputText,StartPosition,InputPosition-StartPosition),1);
        end;
        result:=InputPosition;
        exit;
       end;
      end else begin
       if (InputPosition<=pInputToPosition) and (pInputText[InputPosition]='>') then begin
        inc(InputPosition);
        TagDepth:=1;
        while InputPosition<=pInputToPosition do begin
         if pInputText[InputPosition]='<' then begin
          inc(InputPosition);
          if (InputPosition<=pInputToPosition) and (pInputText[InputPosition]='/') then begin
           inc(InputPosition);
           if lowercase(copy(pInputText,InputPosition,length(Tag)))=Tag then begin
            inc(InputPosition,length(Tag));
            if (InputPosition<=pInputToPosition) and (pInputText[InputPosition] in [#32,'>']) then begin
             while (InputPosition<=pInputToPosition) and (pInputText[InputPosition]=#32) do begin
              inc(InputPosition);
             end;
             if (InputPosition<=pInputToPosition) and (pInputText[InputPosition]='>') then begin
              inc(InputPosition);
              dec(TagDepth);
              if TagDepth=0 then begin
               if assigned(pParentMarkDownBlock) then begin
                NewMarkDownBlock(pParentMarkDownBlock,mdbtHTMLBlock,copy(pInputText,StartPosition,InputPosition-StartPosition),1);
               end;
               result:=InputPosition;
               exit;
              end;
             end;
            end;
           end;
          end else begin
           if lowercase(copy(pInputText,InputPosition,length(Tag)))=Tag then begin
            inc(InputPosition,length(Tag));
            if (InputPosition<=pInputToPosition) and (pInputText[InputPosition] in [#32,'>']) then begin
             inc(TagDepth);
            end;
           end;
          end;
          while (InputPosition<=pInputToPosition) and (pInputText[InputPosition]<>'>') do begin
           inc(InputPosition);
          end;
         end else begin
          inc(InputPosition);
         end;
        end;
       end;
      end;
     end;
    end;
   end;
  end;
 end;
 function ParseTable(const pParentMarkDownBlock:PMarkDownBlock;const pInputText:ansistring;const pInputFromPosition,pInputToPosition:longint):longint;
 type PColumnAlignment=^TColumnAlignment;
      TColumnAlignment=longint;
      PColumn=^TColumn;
      TColumn=record
       Alignment:TColumnAlignment;
      end;
      TColumns=array of TColumn;
 var InputPosition,Indentation,Count,StartPosition,StopPosition,CountColumns,ColumnIndex,TempPosition,
     CountDashes,RowStartPosition,RootStartPosition,RootStopPosition:longint;
     Columns:TColumns;
     Column:PColumn;
     TableMarkDownBlock:PMarkDownBlock;
     WithHeader:boolean;
  function ParseTableRow(const pTableMarkDownBlock:PMarkDownBlock;const pInputText:ansistring;const pInputFromPosition,pInputToPosition:longint;const pIsHeader:boolean):boolean;
  type PColumnDataItem=^TColumnDataItem;
       TColumnDataItem=record
        StartPosition,StopPosition:longint;
       end;
       TColumnData=array of TColumnDataItem;
  var InputPosition,ColumnIndex,CellStartPosition,CellStopPosition,IsHeader:longint;
      Column:PColumn;
      ColumnData:TColumnData;
      ColumnDataItem:PColumnDataItem;
      TableRowMarkDownBlock:PMarkDownBlock;
      TableCellMarkDownBlock:PMarkDownBlock;
  begin
   ColumnData:=nil;
   try
    if pIsHeader then begin
     IsHeader:=tfHeader;
    end else begin
     IsHeader:=0;
    end;
    SetLength(ColumnData,CountColumns);
    for ColumnIndex:=0 to CountColumns-1 do begin
     ColumnDataItem:=@ColumnData[ColumnIndex];
     ColumnDataItem^.StartPosition:=0;
     ColumnDataItem^.StopPosition:=0;
    end;
    ColumnIndex:=0;
    InputPosition:=pInputFromPosition;
    while (ColumnIndex<CountColumns) and (InputPosition<=pInputToPosition) do begin
     ColumnDataItem:=@ColumnData[ColumnIndex];
     while (InputPosition<=pInputToPosition) and (pInputText[InputPosition]=#32) do begin
      inc(InputPosition);
     end;
     CellStartPosition:=InputPosition;
     while (InputPosition<=pInputToPosition) and (pInputText[InputPosition]<>'|') do begin
      inc(InputPosition);
     end;
     CellStopPosition:=InputPosition;
     while ((CellStartPosition+1)<=pInputToPosition) and (pInputText[CellStopPosition-1]=#32) do begin
      dec(CellStopPosition);
     end;
     ColumnDataItem^.StartPosition:=CellStartPosition;
     ColumnDataItem^.StopPosition:=CellStopPosition;
     if (InputPosition<=pInputToPosition) and (pInputText[InputPosition]='|') then begin
      inc(InputPosition);
      inc(ColumnIndex);
     end else begin
      break;
     end;
    end;
    TableRowMarkDownBlock:=NewMarkDownBlock(pTableMarkDownBlock,mdbtTableRow,'',IsHeader);
    for ColumnIndex:=0 to CountColumns-1 do begin
     Column:=@Columns[ColumnIndex];
     ColumnDataItem:=@ColumnData[ColumnIndex];
     TableCellMarkDownBlock:=NewMarkDownBlock(TableRowMarkDownBlock,mdbtTableCell,'',IsHeader or Column.Alignment);
     if ColumnDataItem^.StartPosition<ColumnDataItem^.StopPosition then begin
      ParseInline(TableCellMarkDownBlock,pInputText,ColumnDataItem^.StartPosition,ColumnDataItem^.StopPosition-1);
     end;
    end;
    result:=true;
   finally
    SetLength(ColumnData,0);
   end;
  end;
 begin
  result:=0;
  InputPosition:=pInputFromPosition;
  if InputPosition<=pInputToPosition then begin
   Indentation:=0;
   while (InputPosition<=pInputToPosition) and (pInputText[InputPosition]=#32) and (Indentation<3) do begin
    inc(InputPosition);
    inc(Indentation);
   end;
   if InputPosition<=pInputToPosition then begin
    CountColumns:=0;
    RootStartPosition:=InputPosition;
    while InputPosition<=pInputToPosition do begin
     case pInputText[InputPosition] of
      #10:begin
       break;
      end;
      '|':begin
       inc(InputPosition);
       inc(CountColumns);
      end;
      else begin
       inc(InputPosition);
      end;
     end;
    end;
    RootStopPosition:=InputPosition;
    if pInputText[RootStartPosition]='|' then begin
     inc(RootStartPosition);
     dec(CountColumns);
    end;
    if ((RootStartPosition+1)<RootStopPosition) and (pInputText[RootStopPosition-1]='|') then begin
     dec(CountColumns);
    end;
    if (CountColumns>0) and (InputPosition<=pInputToPosition) and (pInputText[InputPosition]=#10) then begin
     inc(CountColumns);
     inc(InputPosition);
     WithHeader:=false;
     Count:=Indentation;
     while (InputPosition<=pInputToPosition) and (pInputText[InputPosition]=#32) and (Count>0) do begin
      inc(InputPosition);
      dec(Count);
     end;
     if (InputPosition<=pInputToPosition) and (pInputText[InputPosition]='|') then begin
      inc(InputPosition);
     end;
     StartPosition:=InputPosition;
     while (InputPosition<=pInputToPosition) and (pInputText[InputPosition]<>#10) do begin
      inc(InputPosition);
     end;
     StopPosition:=InputPosition;
     TempPosition:=StartPosition;
     Columns:=nil;
     try
      SetLength(Columns,CountColumns);
      for ColumnIndex:=0 to CountColumns-1 do begin
       Column:=@Columns[ColumnIndex];
       Column^.Alignment:=tcaNone;
      end;
      ColumnIndex:=0;
      while (ColumnIndex<CountColumns) and (TempPosition<StopPosition) do begin
       Column:=@Columns[ColumnIndex];
       CountDashes:=0;
       while (TempPosition<StopPosition) and (pInputText[TempPosition]=#32) do begin
        inc(TempPosition);
       end;
       if (TempPosition<StopPosition) and (pInputText[TempPosition]=':') then begin
        Column^.Alignment:=Column^.Alignment or tcaLeft;
        inc(CountDashes);
        inc(TempPosition);
       end;
       while (TempPosition<StopPosition) and (pInputText[TempPosition]='-') do begin
        inc(TempPosition);
        inc(CountDashes);
       end;
       if (TempPosition<StopPosition) and (pInputText[TempPosition]=':') then begin
        Column^.Alignment:=Column^.Alignment or tcaRight;
        inc(CountDashes);
        inc(TempPosition);
       end;
       while (TempPosition<StopPosition) and (pInputText[TempPosition]=#32) do begin
        inc(TempPosition);
       end;
       WithHeader:=true;
       if (TempPosition<StopPosition) and (pInputText[TempPosition]='|') then begin
        inc(TempPosition);
        if CountDashes<3 then begin
         break;
        end else begin
         inc(ColumnIndex);
        end;
       end else begin
        break;
       end;
      end;
      if (ColumnIndex=CountColumns) and (InputPosition<=pInputToPosition) and (pInputText[InputPosition]=#10) then begin
       TableMarkDownBlock:=NewMarkDownBlock(pParentMarkDownBlock,mdbtTable,'',0);
       if ParseTableRow(TableMarkDownBlock,pInputText,RootStartPosition,RootStopPosition-1,WithHeader) then begin
        while (InputPosition<=pInputToPosition) and (pInputText[InputPosition]=#10) do begin
         inc(InputPosition);
         RowStartPosition:=InputPosition;
         Count:=Indentation;
         while (InputPosition<=pInputToPosition) and (pInputText[InputPosition]=#32) and (Count>0) do begin
          inc(InputPosition);
          dec(Count);
         end;
         Count:=0;
         if (InputPosition<=pInputToPosition) and (pInputText[InputPosition]='|') then begin
          inc(InputPosition);
          inc(Count);
         end;
         StartPosition:=InputPosition;
         while InputPosition<=pInputToPosition do begin
          case pInputText[InputPosition] of
           #10:begin
            break;
           end;
           '|':begin
            inc(Count);
           end;
          end;
          inc(InputPosition);
         end;
         StopPosition:=InputPosition;
         if (Count>0) and ParseTableRow(TableMarkDownBlock,pInputText,StartPosition,StopPosition-1,false) then begin
          continue;
         end else begin
          InputPosition:=RowStartPosition;
          break;
         end;
        end;
        result:=InputPosition;
       end;
      end;
     finally
      SetLength(Columns,0);
     end;
    end;
   end;
  end;
 end;
 function CheckListItem(const pInputText:ansistring;const pInputFromPosition,pInputToPosition:longint):longint;
 var InputPosition,Indentation:longint;
     IsOrdered:boolean;
 begin
  result:=0;
  InputPosition:=pInputFromPosition;
  if InputPosition<=pInputToPosition then begin
   Indentation:=0;
   while (InputPosition<=pInputToPosition) and (pInputText[InputPosition]=#32) and (Indentation<3) do begin
    inc(InputPosition);
    inc(Indentation);
   end;
   if InputPosition<=pInputToPosition then begin
    IsOrdered:=false;
    case pInputText[InputPosition] of
     '+','-','*':begin
      inc(InputPosition);
     end;
     '0'..'9':begin
      inc(InputPosition);
      while (InputPosition<=pInputToPosition) and (pInputText[InputPosition] in ['0'..'9']) do begin
       inc(InputPosition);
      end;
      if (InputPosition<=pInputToPosition) and (pInputText[InputPosition]='.') then begin
       inc(InputPosition);
      end else begin
       exit;
      end;
      IsOrdered:=true;
     end;
     else begin
      exit;
     end;
    end;
    if (InputPosition<=pInputToPosition) and (pInputText[InputPosition]=' ') then begin
     inc(InputPosition);
     if IsNextHeaderLine(pInputText,InputPosition,pInputToPosition)=0 then begin
      if isOrdered then begin
       result:=1;
      end else begin
       result:=2;
      end;
     end;
    end;
   end;
  end;
 end;
 function IsHorizontalRule(const pInputText:ansistring;const pInputFromPosition,pInputToPosition:longint):boolean; forward;
 function ParseListItem(const pParentMarkDownBlock:PMarkDownBlock;var pListParentMarkDownBlock:PMarkDownBlock;const pInputText:ansistring;const pInputFromPosition,pInputToPosition:longint;out pLineEnd:boolean):longint;
 var InputPosition,Indentation,NewIndentation,StartPosition,StopPosition,NextPosition,ListKind,NewListKind,TempIndex:longint;
     InlineText,BlockText:ansistring;
     InEmpty,EmptyLine,HasInsideEmpty,ToBlockText,InFencedCodeBlock:boolean;
     ListItemMarkDownBlock:PMarkDownBlock;
 begin
  result:=0;
  pLineEnd:=false;
  InputPosition:=pInputFromPosition;
  if InputPosition<=pInputToPosition then begin

   InEmpty:=false;
   HasInsideEmpty:=false;
   InFencedCodeBlock:=false;

   Indentation:=0;
   while (InputPosition<=pInputToPosition) and (pInputText[InputPosition]=#32) and (Indentation<3) do begin
    inc(InputPosition);
    inc(Indentation);
   end;

   if InputPosition<=pInputToPosition then begin

    ListKind:=2;

    case pInputText[InputPosition] of
     '+','-','*':begin
      inc(InputPosition);
     end;
     '0'..'9':begin
      inc(InputPosition);
      while (InputPosition<=pInputToPosition) and (pInputText[InputPosition] in ['0'..'9']) do begin
       inc(InputPosition);
      end;
      if (InputPosition<=pInputToPosition) and (pInputText[InputPosition]='.') then begin
       inc(InputPosition);
      end else begin
       exit;
      end;
      ListKind:=1;
     end;
     else begin
      exit;
     end;
    end;

    if (InputPosition<=pInputToPosition) and (pInputText[InputPosition]=' ') then begin
     inc(InputPosition);

     if IsNextHeaderLine(pInputText,InputPosition,pInputToPosition)=0 then begin

      StartPosition:=InputPosition;
      while (InputPosition<=pInputToPosition) and (pInputText[InputPosition]<>#10) do begin
       inc(InputPosition);
      end;
      StopPosition:=InputPosition;
      if StartPosition<StopPosition then begin
       InlineText:=copy(pInputText,StartPosition,StopPosition-StartPosition);
       for TempIndex:=length(InlineText) downto 1 do begin
        if InlineText[TempIndex]<>#32 then begin
         SetLength(InlineText,TempIndex);
         break;
        end;
       end;
      end else begin
       InlineText:='';
      end;

      BlockText:='';

      ToBlockText:=false;

      if pInputText[InputPosition]=#10 then begin
       NextPosition:=InputPosition+1;
      end else begin
       NextPosition:=InputPosition;
      end;

      while (InputPosition<=pInputToPosition) and (pInputText[InputPosition]=#10) do begin

       inc(InputPosition);

       StartPosition:=InputPosition;
       EmptyLine:=true;
       while InputPosition<=pInputToPosition do begin
        case pInputText[InputPosition] of
         #10:begin
          break;
         end;
         #32:begin
          inc(InputPosition);
         end;
         else begin
          EmptyLine:=false;
          inc(InputPosition);
         end;
        end;
       end;
       StopPosition:=InputPosition;
       if EmptyLine then begin
        InEmpty:=true;
        continue;
       end;

       NewIndentation:=0;
       while (NewIndentation<4) and ((StartPosition+NewIndentation)<StopPosition) and (pInputText[(StartPosition+NewIndentation)]=#32) do begin
        inc(NewIndentation);
       end;

       if IsFencedCode(pInputText,StartPosition,StopPosition-1) then begin
        InFencedCodeBlock:=not InFencedCodeBlock;
       end;

       if InFencedCodeBlock then begin
        NewListKind:=0;
       end else begin
        NewListKind:=CheckListItem(pInputText,StartPosition,StopPosition-1);
       end;
       
       if InEmpty or
          ((NewListKind<>0) and
          (ListKind<>NewListKind)) then begin
        pLineEnd:=true;
        break;
       end;

       if (NewListKind=1) or
          ((NewListKind=2) and not IsHorizontalRule(pInputText,StartPosition,StopPosition-1)) then begin
        if InEmpty then begin
         HasInsideEmpty:=true;
        end;
        if Indentation=NewIndentation then begin
         break;
        end;
        ToBlockText:=true;
       end else if InEmpty then begin
        if NewIndentation=0 then begin
         pLineEnd:=true;
         break;
        end else begin
         HasInsideEmpty:=true;
        end;
       end;

       InEmpty:=false;

       if ToBlockText then begin
        BlockText:=BlockText+#10;
        if (StartPosition+Indentation)<StopPosition then begin
         BlockText:=BlockText+copy(pInputText,(StartPosition+Indentation),StopPosition-(StartPosition+Indentation));
         for TempIndex:=length(BlockText) downto 1 do begin
          if BlockText[TempIndex]<>#32 then begin
           SetLength(BlockText,TempIndex);
           break;
          end;
         end;
        end;
       end else begin
        InlineText:=InlineText+#10;
        if (StartPosition+NewIndentation)<StopPosition then begin
         InlineText:=InlineText+copy(pInputText,StartPosition+NewIndentation,StopPosition-(StartPosition+NewIndentation));
         for TempIndex:=length(InlineText) downto 1 do begin
          if InlineText[TempIndex]<>#32 then begin
           SetLength(InlineText,TempIndex);
           break;
          end;
         end;
        end;
       end;

       if pInputText[InputPosition]=#10 then begin
        NextPosition:=InputPosition+1;
       end else begin
        NextPosition:=InputPosition;
       end;

      end;

      if not assigned(pListParentMarkDownBlock) then begin
       if ListKind=2 then begin
        pListParentMarkDownBlock:=NewMarkDownBlock(pParentMarkDownBlock,mdbtUnorderedList,'',0);
       end else begin
        pListParentMarkDownBlock:=NewMarkDownBlock(pParentMarkDownBlock,mdbtOrderedList,'',0);
       end;
      end;
      ListItemMarkDownBlock:=NewMarkDownBlock(pListParentMarkDownBlock,mdbtListItem,'',0);
      if length(InlineText)>0 then begin
       if HasInsideEmpty then begin
        ParseBlock(ListItemMarkDownBlock,InlineText,1,length(InlineText));
       end else begin
        ParseInline(ListItemMarkDownBlock,InlineText,1,length(InlineText));
       end;
      end;
      if length(BlockText)>0 then begin
       ParseBlock(ListItemMarkDownBlock,BlockText,1,length(BlockText));
      end;

      result:=NextPosition;

     end;

    end;

   end;
  end;
 end;
 function ParseList(const pParentMarkDownBlock:PMarkDownBlock;const pInputText:ansistring;const pInputFromPosition,pInputToPosition:longint):longint;
 var InputPosition,NewPosition:longint;
     ListMarkDownBlock:PMarkDownBlock;
     LineEnd:boolean;
 begin
  result:=0;
  InputPosition:=pInputFromPosition;
  if InputPosition<=pInputToPosition then begin
   ListMarkDownBlock:=nil;
   while InputPosition<=pInputToPosition do begin
    NewPosition:=ParseListItem(pParentMarkDownBlock,ListMarkDownBlock,pInputText,InputPosition,pInputToPosition,LineEnd);
    if NewPosition>0 then begin
     InputPosition:=NewPosition;
     if LineEnd then begin
      break;
     end;
    end else begin
     break;
    end;
   end;
   if assigned(ListMarkDownBlock) then begin
    result:=InputPosition;
   end;
  end;
 end;
 function ParseCodeBlock(const pParentMarkDownBlock:PMarkDownBlock;const pInputText:ansistring;const pInputFromPosition,pInputToPosition,pIndentation:longint):longint;
 const Indentation=4;
 var InputPosition,TempPosition,BlockTextSize,TempIndentation:longint;
     MarkDownBlock:PMarkDownBlock;
     BlockText:ansistring;
 begin
  result:=0;
  InputPosition:=pInputFromPosition;
  if (InputPosition<=pInputToPosition) and (pIndentation>=Indentation) then begin
   inc(InputPosition,Indentation);
   SetLength(BlockText,(pInputToPosition-InputPosition)+1);
   BlockTextSize:=0;
   try
    while InputPosition<=pInputToPosition do begin
     while InputPosition<=pInputToPosition do begin
      case pInputText[InputPosition] of
       #10:begin
        inc(BlockTextSize);
        if length(BlockText)<BlockTextSize then begin
         SetLength(BlockText,BlockTextSize*2);
        end;
        BlockText[BlockTextSize]:=#10;
        inc(InputPosition);
        break;
       end;
       else begin
        inc(BlockTextSize);
        if length(BlockText)<BlockTextSize then begin
         SetLength(BlockText,BlockTextSize*2);
        end;
        BlockText[BlockTextSize]:=pInputText[InputPosition];
        inc(InputPosition);
       end;
      end;
     end;
     TempIndentation:=0;
     TempPosition:=InputPosition;
     while (TempPosition<=pInputToPosition) and (pInputText[TempPosition]=#32) do begin
      inc(TempIndentation);
      inc(TempPosition);
     end;
     if TempIndentation>=Indentation then begin
      inc(InputPosition,Indentation);
     end else begin
      if (TempPosition<=pInputToPosition) and (pInputText[TempPosition]=#10) then begin
       inc(InputPosition);
       TempPosition:=InputPosition;
       while (TempPosition<=pInputToPosition) and (pInputText[TempPosition]=#32) do begin
        inc(TempPosition);
       end;
       if (TempPosition<=pInputToPosition) and (pInputText[TempPosition]<>#10) then begin
        continue;
       end;
      end;
      break;
     end;
    end;
   finally
    SetLength(BlockText,BlockTextSize);
   end;
   MarkDownBlock:=NewMarkDownBlock(pParentMarkDownBlock,mdbtCodeBlock,BlockText,0);
   result:=InputPosition;
  end;
 end;
 function IsBlockQuote(const pInputText:ansistring;const pInputFromPosition,pInputToPosition:longint):boolean;
 var InputPosition:longint;
 begin
  result:=false;
  InputPosition:=pInputFromPosition;
  if InputPosition<=pInputToPosition then begin
   if ((InputPosition+2)<=pInputToPosition) and (pInputText[InputPosition]=#32) then begin
    inc(InputPosition);
    if ((InputPosition+1)<=pInputToPosition) and (pInputText[InputPosition]=#32) then begin
     inc(InputPosition);
     if (InputPosition<=pInputToPosition) and (pInputText[InputPosition]=#32) then begin
      inc(InputPosition);
     end;
    end;
   end;
   if (InputPosition<=pInputToPosition) and (pInputText[InputPosition]='>') then begin
    result:=true;
   end;
  end;
 end;
 function ParseBlockQuote(const pParentMarkDownBlock:PMarkDownBlock;const pInputText:ansistring;const pInputFromPosition,pInputToPosition:longint):longint;
 var InputPosition,TempPosition,BlockTextSize:longint;
     MarkDownBlock:PMarkDownBlock;
     BlockText:ansistring;
 begin
  result:=0;
  InputPosition:=pInputFromPosition;
  if (InputPosition<=pInputToPosition) and (pInputText[InputPosition]='>') then begin
   TempPosition:=InputPosition+1;
   while (TempPosition<=pInputToPosition) and (pInputText[TempPosition]=#32) do begin
    inc(TempPosition);
   end;
   if (TempPosition<=pInputToPosition) and (pInputText[TempPosition]<>#10) then begin
    InputPosition:=TempPosition;
    SetLength(BlockText,(pInputToPosition-InputPosition)+1);
    BlockTextSize:=0;
    try
     while InputPosition<=pInputToPosition do begin
      while InputPosition<=pInputToPosition do begin
       case pInputText[InputPosition] of
        #10:begin
         inc(BlockTextSize);
         if length(BlockText)<BlockTextSize then begin
          SetLength(BlockText,BlockTextSize*2);
         end;
         BlockText[BlockTextSize]:=#10;
         inc(InputPosition);
         break;
        end;
        else begin
         inc(BlockTextSize);
         if length(BlockText)<BlockTextSize then begin
          SetLength(BlockText,BlockTextSize*2);
         end;
         BlockText[BlockTextSize]:=pInputText[InputPosition];
         inc(InputPosition);
        end;
       end;
      end;
      TempPosition:=InputPosition;
      while (TempPosition<=pInputToPosition) and (pInputText[TempPosition]=#32) do begin
       inc(TempPosition);
      end;
      if TempPosition<=pInputToPosition then begin
       case pInputText[TempPosition] of
        '>':begin
         InputPosition:=TempPosition+1;
         continue;
        end;
        #10:begin
         break;
        end;
        else begin
         continue;
        end;
       end;
      end else begin
       break;
      end;
     end;
    finally
     SetLength(BlockText,BlockTextSize);
    end;
    MarkDownBlock:=NewMarkDownBlock(pParentMarkDownBlock,mdbtBlockQuote,'',0);
    ParseBlock(MarkDownBlock,BlockText,1,length(BlockText));
    result:=InputPosition;
   end;
  end;
 end;
 function IsATXHeader(const pInputText:ansistring;const pInputFromPosition,pInputToPosition:longint):boolean;
 var InputPosition,TempPosition,TempCount,StartPosition:longint;
 begin
  result:=false;
  InputPosition:=pInputFromPosition;
  if (InputPosition<=pInputToPosition) and (pInputText[InputPosition]='#') then begin
   TempPosition:=InputPosition+1;
   TempCount:=1;
   while (TempPosition<=pInputToPosition) and (pInputText[TempPosition]='#') do begin
    inc(TempCount);
    inc(TempPosition);
   end;
   if (TempCount in [1..6]) and (TempPosition<=pInputToPosition) and (pInputText[TempPosition]=#32) then begin
    while (TempPosition<=pInputToPosition) and (pInputText[TempPosition]=#32) do begin
     inc(TempPosition);
    end;
    StartPosition:=TempPosition;
    while (TempPosition<=pInputToPosition) and (pInputText[TempPosition]<>#10) do begin
     inc(TempPosition);
    end;
    if StartPosition<TempPosition then begin
     result:=true;
    end;
   end;
  end;
 end;
 function ParseATXHeader(const pParentMarkDownBlock:PMarkDownBlock;const pInputText:ansistring;const pInputFromPosition,pInputToPosition:longint):longint;
 var InputPosition,TempPosition,TempCount,StartPosition,TempIndex,OtherTempIndex:longint;
     MarkDownBlock:PMarkDownBlock;
     BlockText:ansistring;
 begin
  result:=0;
  InputPosition:=pInputFromPosition;
  if (InputPosition<=pInputToPosition) and (pInputText[InputPosition]='#') then begin
   TempPosition:=InputPosition+1;
   TempCount:=1;
   while (TempPosition<=pInputToPosition) and (pInputText[TempPosition]='#') do begin
    inc(TempCount);
    inc(TempPosition);
   end;
   if (TempCount in [1..6]) and (TempPosition<=pInputToPosition) and (pInputText[TempPosition]=#32) then begin
    while (TempPosition<=pInputToPosition) and (pInputText[TempPosition]=#32) do begin
     inc(TempPosition);
    end;
    StartPosition:=TempPosition;
    while (TempPosition<=pInputToPosition) and (pInputText[TempPosition]<>#10) do begin
     inc(TempPosition);
    end;
    if StartPosition<TempPosition then begin
     InputPosition:=TempPosition;
     BlockText:=copy(pInputText,StartPosition,TempPosition-StartPosition);
     if (length(BlockText)>0) and (BlockText[length(BlockText)]=#32) then begin
      for TempIndex:=length(BlockText)-1 downto 1 do begin
       if BlockText[TempIndex]<>#32 then begin
        SetLength(BlockText,TempIndex);
        break;
       end;
      end;
     end;
     if (length(BlockText)>0) and (BlockText[length(BlockText)]='#') then begin
      for TempIndex:=length(BlockText)-1 downto 1 do begin
       if BlockText[TempIndex]<>'#' then begin
        if BlockText[TempIndex]=#32 then begin
         SetLength(BlockText,TempIndex);
         if (length(BlockText)>0) and (BlockText[length(BlockText)]=#32) then begin
          for OtherTempIndex:=length(BlockText)-1 downto 1 do begin
           if BlockText[OtherTempIndex]<>#32 then begin
            SetLength(BlockText,OtherTempIndex);
            break;
           end;
          end;
         end;
        end;
        break;
       end;
      end;
     end;
     MarkDownBlock:=NewMarkDownBlock(pParentMarkDownBlock,mdbtATXHeader,'',TempCount);
     ParseInline(MarkDownBlock,BlockText,1,length(BlockText));
     result:=InputPosition;
    end;
   end;
  end;
 end;
 function IsHorizontalRule(const pInputText:ansistring;const pInputFromPosition,pInputToPosition:longint):boolean;
 var InputPosition,TempPosition,TempCount:longint;
     HorizontalRuleChar:ansichar;
 begin
  result:=false;
  InputPosition:=pInputFromPosition;
  if (InputPosition+2)<=pInputToPosition then begin
   if ((InputPosition+2)<=pInputToPosition) and (pInputText[InputPosition]=#32) then begin
    inc(InputPosition);
    if ((InputPosition+1)<=pInputToPosition) and (pInputText[InputPosition]=#32) then begin
     inc(InputPosition);
     if (InputPosition<=pInputToPosition) and (pInputText[InputPosition]=#32) then begin
      inc(InputPosition);
     end;
    end;
   end;
   if (InputPosition<=pInputToPosition) and (pInputText[InputPosition] in ['-','_','*']) then begin
    HorizontalRuleChar:=pInputText[InputPosition];
    TempPosition:=InputPosition;
    TempCount:=0;
    while TempPosition<=pInputToPosition do begin
     case pInputText[TempPosition] of
      #32:begin
       inc(TempPosition);
      end;
      else begin
       if pInputText[TempPosition]=HorizontalRuleChar then begin
        inc(TempPosition);
        inc(TempCount);
       end else begin
        break;
       end;
      end;
     end;
    end;
    if (TempCount>2) and ((TempPosition>pInputToPosition) or (pInputText[TempPosition]=#10)) then begin
     result:=true;
    end;
   end;
  end;
 end;
 function ParseHorizontalRule(const pParentMarkDownBlock:PMarkDownBlock;const pInputText:ansistring;const pInputFromPosition,pInputToPosition:longint):longint;
 var InputPosition,TempPosition,TempCount:longint;
     MarkDownBlock:PMarkDownBlock;
     HorizontalRuleChar:ansichar;
 begin
  result:=0;
  InputPosition:=pInputFromPosition;
  if (InputPosition<=pInputToPosition) and (pInputText[InputPosition] in ['-','_','*']) then begin
   HorizontalRuleChar:=pInputText[InputPosition];
   TempPosition:=InputPosition;
   if (HorizontalRuleChar<>'-') or not (assigned(pParentMarkDownBlock^.Tail) and (pParentMarkDownBlock^.Tail^.BlockType=mdbtParagraph)) then begin
    TempCount:=0;
    while TempPosition<=pInputToPosition do begin
     case pInputText[TempPosition] of
      #32:begin
       inc(TempPosition);
      end;
      else begin
       if pInputText[TempPosition]=HorizontalRuleChar then begin
        inc(TempPosition);
        inc(TempCount);
       end else begin
        break;
       end;
      end;
     end;
    end;
    if (TempCount>2) and ((TempPosition>pInputToPosition) or (pInputText[TempPosition]=#10)) then begin
     if (TempPosition<=pInputToPosition) and (pInputText[TempPosition]=#10) then begin
      InputPosition:=TempPosition+1;
     end else begin
      InputPosition:=TempPosition;
     end;
     MarkDownBlock:=NewMarkDownBlock(pParentMarkDownBlock,mdbtHorizontalRule,'',0);
     result:=InputPosition;
    end;
   end;
  end;
 end;
 function ParseParagraph(const pParentMarkDownBlock:PMarkDownBlock;const pInputText:ansistring;const pInputFromPosition,pInputToPosition:longint):longint;
 var InputPosition,EndPosition,TempPosition,Level:longint;
     EmptyLine:boolean;
     ParagraphMarkDownBlock:PMarkDownBlock;
 begin

  Level:=0;

  InputPosition:=pInputFromPosition;
  EndPosition:=InputPosition+1;

  while InputPosition<=pInputToPosition do begin

   EndPosition:=InputPosition+1;
   while (EndPosition<=pInputToPosition) and (pInputText[EndPosition-1]<>#10) do begin
    inc(EndPosition);
   end;

   // Check for empty line
   EmptyLine:=true;
   TempPosition:=InputPosition;
   while TempPosition<=pInputToPosition do begin
    case pInputText[TempPosition] of
     #10:begin
      break;
     end;
     #32:begin
      inc(TempPosition);
     end;
     else begin
      EmptyLine:=false;
      break;
     end;
    end;
   end;
   if EmptyLine then begin
    break;
   end;

   // Check for possible header line
   Level:=IsHeaderLine(pInputText,InputPosition,pInputToPosition);
   if Level<>0 then begin
    break;
   end;

   if IsATXHeader(pInputText,InputPosition,pInputToPosition) or
      IsHorizontalRule(pInputText,InputPosition,pInputToPosition) or
      IsBlockQuote(pInputText,InputPosition,pInputToPosition) or
      (CheckListItem(pInputText,InputPosition,pInputToPosition)<>0) or
      IsFencedCode(pInputText,InputPosition,pInputToPosition) and
      ((InputPosition<pInputToPosition) and
       (pInputText[InputPosition]='<') and
       (ParseHTMLBlock(nil,pInputText,InputPosition,pInputToPosition)>0)) then begin
    EndPosition:=InputPosition;
    break;
   end;

   InputPosition:=EndPosition;
  end;

  while ((InputPosition+1)>=pInputFromPosition) and (InputPosition<=pInputToPosition) and (pInputText[InputPosition-1]=#10) do begin
   dec(InputPosition);
  end;

  case Level of
   1..2:begin
    if pInputFromPosition<InputPosition then begin
     ParagraphMarkDownBlock:=NewMarkDownBlock(pParentMarkDownBlock,mdbtSETextHeader,'',Level);
     ParseInline(ParagraphMarkDownBlock,pInputText,pInputFromPosition,InputPosition);
    end;
   end;
   else begin
    if pInputFromPosition<InputPosition then begin
     ParagraphMarkDownBlock:=NewMarkDownBlock(pParentMarkDownBlock,mdbtParagraph,'',0);
     ParseInline(ParagraphMarkDownBlock,pInputText,pInputFromPosition,InputPosition);
    end;
   end;
  end;

  result:=EndPosition;

 end;
 function ParseBlock(const pParentMarkDownBlock:PMarkDownBlock;const pInputText:ansistring;const pInputFromPosition,pInputToPosition:longint):longint;
 var InputPosition,TempPosition,NewPosition,StartPosition,Indentation:longint;
 begin
  result:=0;
  InputPosition:=pInputFromPosition;
  while InputPosition<=pInputToPosition do begin

   // Indentation scanner
   Indentation:=0;
   TempPosition:=InputPosition;
   while (TempPosition<=pInputToPosition) and (pInputText[TempPosition]=#32) do begin
    inc(Indentation);
    inc(TempPosition);
   end;

   // Blank line
   if (TempPosition>pInputToPosition) or (pInputText[TempPosition]=#10) then begin
    InputPosition:=TempPosition+1;
    NewMarkDownBlock(pParentMarkDownBlock,mdbtBlankLine,'',0);
    continue;
   end;

   // Code block
   if Indentation>=4 then begin
    NewPosition:=ParseCodeBlock(pParentMarkDownBlock,pInputText,InputPosition,pInputToPosition,Indentation);
    if NewPosition>0 then begin
     InputPosition:=NewPosition;
     continue;
    end;
   end;

   StartPosition:=TempPosition;

   case pInputText[StartPosition] of

    '>':begin

     // Block quote
     NewPosition:=ParseBlockQuote(pParentMarkDownBlock,pInputText,StartPosition,pInputToPosition);
     if NewPosition>0 then begin
      InputPosition:=NewPosition;
      continue;
     end;

    end;

    '#':begin

     // ATX header
     NewPosition:=ParseATXHeader(pParentMarkDownBlock,pInputText,StartPosition,pInputToPosition);
     if NewPosition>0 then begin
      InputPosition:=NewPosition;
      continue;
     end;

    end;

    '-','_','*':begin

     // Horizontal rule
     NewPosition:=ParseHorizontalRule(pParentMarkDownBlock,pInputText,StartPosition,pInputToPosition);
     if NewPosition>0 then begin
      InputPosition:=NewPosition;
      continue;
     end;

     // List
     NewPosition:=ParseList(pParentMarkDownBlock,pInputText,InputPosition,pInputToPosition);
     if NewPosition>0 then begin
      InputPosition:=NewPosition;
      continue;
     end;

    end;

    '+','0'..'9':begin

     // List
     NewPosition:=ParseList(pParentMarkDownBlock,pInputText,InputPosition,pInputToPosition);
     if NewPosition>0 then begin
      InputPosition:=NewPosition;
      continue;
     end;

    end;

    '~','`':begin

     // Fenced code
     NewPosition:=ParseFencedCode(pParentMarkDownBlock,pInputText,InputPosition,pInputToPosition);
     if NewPosition>0 then begin
      InputPosition:=NewPosition;
      continue;
     end;

    end;

    '<':begin

     // HTML-Block
     NewPosition:=ParseHTMLBlock(pParentMarkDownBlock,pInputText,InputPosition,pInputToPosition);
     if NewPosition>0 then begin
      InputPosition:=NewPosition;
      continue;
     end;

    end;

    '|':begin

     // Table
     NewPosition:=ParseTable(pParentMarkDownBlock,pInputText,InputPosition,pInputToPosition);
     if NewPosition>0 then begin
      InputPosition:=NewPosition;
      continue;
     end;

    end;

   end;

   // Paragraph
   NewPosition:=ParseParagraph(pParentMarkDownBlock,pInputText,InputPosition,pInputToPosition);
   if NewPosition>0 then begin
    InputPosition:=NewPosition;
   end else begin
    inc(InputPosition);
   end;

  end;
 end;
 function EscapeHTML(const pInputText:ansistring):ansistring;
 var InputPosition,InputLength,OutputPosition:longint;
 begin
  result:='';
  InputPosition:=1;
  InputLength:=length(pInputText);
  SetLength(result,InputLength);
  OutputPosition:=0;
  try
   while InputPosition<=InputLength do begin
    case pInputText[InputPosition] of
     '<':begin
      if length(result)<(OutputPosition+4) then begin
       SetLength(result,(OutputPosition+4)*2);
      end;
      result[OutputPosition+1]:='&';
      result[OutputPosition+2]:='l';
      result[OutputPosition+3]:='t';
      result[OutputPosition+4]:=';';
      inc(OutputPosition,4);
      inc(InputPosition);
     end;
     '>':begin
      if length(result)<(OutputPosition+4) then begin
       SetLength(result,(OutputPosition+4)*2);
      end;
      result[OutputPosition+1]:='&';
      result[OutputPosition+2]:='g';
      result[OutputPosition+3]:='t';
      result[OutputPosition+4]:=';';
      inc(OutputPosition,4);
      inc(InputPosition);
     end;
     '&':begin
      if length(result)<(OutputPosition+5) then begin
       SetLength(result,(OutputPosition+5)*2);
      end;
      result[OutputPosition+1]:='&';
      result[OutputPosition+2]:='a';
      result[OutputPosition+3]:='m';
      result[OutputPosition+4]:='p';
      result[OutputPosition+5]:=';';
      inc(OutputPosition,5);
      inc(InputPosition);
     end;
     '''':begin
      if length(result)<(OutputPosition+6) then begin
       SetLength(result,(OutputPosition+6)*2);
      end;
      result[OutputPosition+1]:='&';
      result[OutputPosition+2]:='#';
      result[OutputPosition+3]:='x';
      result[OutputPosition+4]:='2';
      result[OutputPosition+5]:='7';
      result[OutputPosition+6]:=';';
      inc(OutputPosition,6);
      inc(InputPosition);
     end;
     '"':begin
      if length(result)<(OutputPosition+6) then begin
       SetLength(result,(OutputPosition+6)*2);
      end;
      result[OutputPosition+1]:='&';
      result[OutputPosition+2]:='q';
      result[OutputPosition+3]:='u';
      result[OutputPosition+4]:='o';
      result[OutputPosition+5]:='t';
      result[OutputPosition+6]:=';';
      inc(OutputPosition,6);
      inc(InputPosition);
     end;
     '/':begin
      if length(result)<(OutputPosition+6) then begin
       SetLength(result,(OutputPosition+6)*2);
      end;
      result[OutputPosition+1]:='&';
      result[OutputPosition+2]:='#';
      result[OutputPosition+3]:='x';
      result[OutputPosition+4]:='2';
      result[OutputPosition+5]:='f';
      result[OutputPosition+6]:=';';
      inc(OutputPosition,6);
      inc(InputPosition);
     end;
     else begin
      inc(OutputPosition);
      if length(result)<OutputPosition then begin
       SetLength(result,OutputPosition*2);
      end;
      result[OutputPosition]:=pInputText[InputPosition];
      inc(InputPosition);
     end;
    end;
   end;
  finally
   SetLength(result,OutputPosition);
  end;
 end;
 function ProcessMarkDownBlock(const pCurrentMarkDownBlock:PMarkDownBlock):ansistring;
 var CurrentMarkDownBlock,NextMarkDownBlock:PMarkDownBlock;
 begin
  case pCurrentMarkDownBlock^.BlockType of
   mdbtRoot:begin
    result:='';
   end;
   mdbtBlankLine:begin
    result:='';
    exit;
   end;
   mdbtText:begin
    result:=EscapeHTML(pCurrentMarkDownBlock^.StringData);
    exit;
   end;
   mdbtEntity:begin
    result:=pCurrentMarkDownBlock^.StringData;
    exit;
   end;
   mdbtHTMLTag:begin
    result:=pCurrentMarkDownBlock^.StringData;
    exit;
   end;
   mdbtWebLink:begin
    result:='<a href="'+EscapeHTML(pCurrentMarkDownBlock^.StringData)+'">'+EscapeHTML(pCurrentMarkDownBlock^.StringData)+'</a>';
    exit;
   end;
   mdbtSubscript:begin
    result:='<sup>';
   end;
   mdbtLink:begin
    result:='<a href="'+EscapeHTML(pCurrentMarkDownBlock^.StringData)+'">';
   end;
   mdbtImage:begin
    result:='<img src="'+EscapeHTML(pCurrentMarkDownBlock^.StringData)+'" alt="';
   end;
   mdbtReferenceLink:begin
    result:='<a href="'+EscapeHTML(LinkStringList.Values[LowerCase(Trim(pCurrentMarkDownBlock^.StringData))])+'">';
   end;
   mdbtReferenceImage:begin
    result:='<img src="'+EscapeHTML(LinkStringList.Values[LowerCase(Trim(pCurrentMarkDownBlock^.StringData))])+'" alt="';
   end;
   mdbtLineBreak:begin
    result:='<br/>';
    exit;
   end;
   mdbtEmphasis:begin
    case pCurrentMarkDownBlock^.Tag of
     1:begin
      result:='<em>';
     end;
     2:begin
      result:='<strong>';
     end;
     3:begin
      result:='<strong>';
     end;
     else begin
      result:='';
     end;
    end;
   end;
   mdbtStrikethrough:begin
    result:='<del>';
   end;
   mdbtATXHeader:begin
    result:='<h'+IntToStr(pCurrentMarkDownBlock^.Tag)+'>';
   end;
   mdbtHorizontalRule:begin
    result:='<hr/>';
   end;
   mdbtHighlight:begin
    result:='<mark>';
   end;
   mdbtParagraph:begin
    result:='<p>';
   end;
   mdbtSETextHeader:begin
    result:='<h'+IntToStr(pCurrentMarkDownBlock^.Tag)+'>';
   end;
   mdbtCodeBlock:begin
    result:='<pre>'+EscapeHTML(pCurrentMarkDownBlock^.StringData)+'</pre>';
    exit;
   end;
   mdbtFencedCodeBlock:begin
    result:='<pre>'+EscapeHTML(pCurrentMarkDownBlock^.StringData)+'</pre>';
    exit;
   end;
   mdbtCodeSpan:begin
    result:='<code>'+EscapeHTML(pCurrentMarkDownBlock^.StringData)+'</code>';
    exit;
   end;
   mdbtBlockQuote:begin
    result:='<blockquote>';
   end;
   mdbtUnorderedList:begin
    result:='<ul>';
   end;
   mdbtOrderedList:begin
    result:='<ol>';
   end;
   mdbtListItem:begin
    result:='<li>';
   end;
   mdbtTable:begin
    result:='<table>';
   end;
   mdbtTableRow:begin
    result:='<tr>';
   end;
   mdbtTableCell:begin
    if (pCurrentMarkDownBlock^.Tag and tfHeader)<>0 then begin
     if (pCurrentMarkDownBlock^.Tag and (tcaLeft or tcaRight))=(tcaLeft or tcaRight) then begin
      result:='<th align="center">';
     end else if (pCurrentMarkDownBlock^.Tag and tcaLeft)<>0 then begin
      result:='<th align="left">';
     end else if (pCurrentMarkDownBlock^.Tag and tcaRight)<>0 then begin
      result:='<th align="right">';
     end;
    end else begin
     if (pCurrentMarkDownBlock^.Tag and (tcaLeft or tcaRight))=(tcaLeft or tcaRight) then begin
      result:='<td align="center">';
     end else if (pCurrentMarkDownBlock^.Tag and tcaLeft)<>0 then begin
      result:='<td align="left">';
     end else if (pCurrentMarkDownBlock^.Tag and tcaRight)<>0 then begin
      result:='<td align="right">';
     end;
    end;
   end;
   mdbtHTMLBlock:begin
    result:=pCurrentMarkDownBlock^.StringData;
    exit;
   end;
   mdbtHTMLComment:begin
    result:=pCurrentMarkDownBlock^.StringData;
    exit;
   end;
   mdbtLinkReferenceDefinition:begin
    result:=' ';
    exit;
   end;
  end;
  CurrentMarkDownBlock:=pCurrentMarkDownBlock^.Head;
  while assigned(CurrentMarkDownBlock) do begin
   NextMarkDownBlock:=CurrentMarkDownBlock^.Next;
   result:=result+ProcessMarkDownBlock(CurrentMarkDownBlock);
   CurrentMarkDownBlock:=NextMarkDownBlock;
  end;
  case pCurrentMarkDownBlock^.BlockType of
   mdbtRoot:begin
   end;
   mdbtBlankLine:begin
   end;
   mdbtText:begin
   end;
   mdbtEntity:begin
   end;
   mdbtHTMLTag:begin
   end;
   mdbtWebLink:begin
   end;
   mdbtSubscript:begin
    result:=result+'</sup>';
   end;
   mdbtLink:begin
    result:=result+'</a>';
   end;
   mdbtImage:begin
    result:=result+'" />';
   end;
   mdbtReferenceLink:begin
    result:=result+'</a>';
   end;
   mdbtReferenceImage:begin
    result:=result+'" />';
   end;
   mdbtLineBreak:begin
   end;
   mdbtEmphasis:begin
    case pCurrentMarkDownBlock^.Tag of
     1:begin
      result:=result+'</em>';
     end;
     2:begin
      result:=result+'</strong>';
     end;
     3:begin
      result:=result+'</strong>';
     end;
     else begin
      result:='';
     end;
    end;
   end;
   mdbtStrikethrough:begin
    result:=result+'</del>';
   end;
   mdbtATXHeader:begin
    result:=result+'</h'+IntToStr(pCurrentMarkDownBlock^.Tag)+'>';
   end;
   mdbtHorizontalRule:begin
   end;
   mdbtParagraph:begin
    result:=result+'</p>';
   end;
   mdbtHighlight:begin
    result:=result+'</mark>';
   end;
   mdbtSETextHeader:begin
    result:=result+'</h'+IntToStr(pCurrentMarkDownBlock^.Tag)+'>';
   end;
   mdbtCodeBlock:begin
   end;
   mdbtFencedCodeBlock:begin
   end;
   mdbtCodeSpan:begin
   end;
   mdbtBlockQuote:begin
    result:=result+'</blockquote>';
   end;
   mdbtUnorderedList:begin
    result:=result+'</ul>';
   end;
   mdbtOrderedList:begin
    result:=result+'</ol>';
   end;
   mdbtListItem:begin
    result:=result+'</li>';
   end;
   mdbtTable:begin
    result:=result+'</table>';
   end;
   mdbtTableRow:begin
    result:=result+'</tr>';
   end;
   mdbtTableCell:begin
    if (pCurrentMarkDownBlock^.Tag and tfHeader)<>0 then begin
     result:=result+'</th>';
    end else begin
     result:=result+'</td>';
    end;
   end;
   mdbtHTMLBlock:begin
   end;
   mdbtHTMLComment:begin
   end;
   mdbtLinkReferenceDefinition:begin
   end;
  end;
 end;
var InputText:ansistring;
begin
 GetMem(RootMarkDownBlock,SizeOf(TMarkDownBlock));
 try
  FillChar(RootMarkDownBlock^,SizeOf(TMarkDownBlock),#0);
  Initialize(RootMarkDownBlock^);
  RootMarkDownBlock^.BlockType:=mdbtRoot;
  LinkStringList:=TStringList.Create;
  try
   InputText:=CleanText(pInputText);
   ParseBlock(RootMarkDownBlock,InputText,1,length(InputText));
   result:=ProcessMarkDownBlock(RootMarkDownBlock);
  finally
   LinkStringList.Free;
  end;
 finally
  FreeMarkDownBlock(RootMarkDownBlock);
 end;
end;

procedure InitializeUTF8DFA;
type TAnsiCharSet=set of ansichar;
{$ifdef StrictUTF8}
{ c0  8 11000000   | d0  2 11(010000) | e0 10 11100000   | f0 11 11110000
  c1  8 11000001   | d1  2 11(010001) | e1  3 111(00001) | f1  6 111100(01)
  c2  2 11(000010) | d2  2 11(010010) | e2  3 111(00010) | f2  6 111100(10)
  c3  2 11(000011) | d3  2 11(010011) | e3  3 111(00011) | f3  6 111100(11)
  c4  2 11(000100) | d4  2 11(010100) | e4  3 111(00100) | f4  5 11110(100)
  c5  2 11(000101) | d5  2 11(010101) | e5  3 111(00101) | f5  8 11110101
  c6  2 11(000110) | d6  2 11(010110) | e6  3 111(00110) | f6  8 11110110
  c7  2 11(000111) | d7  2 11(010111) | e7  3 111(00111) | f7  8 11110111
  c8  2 11(001000) | d8  2 11(011000) | e8  3 111(01000) | f8  8 11111000
  c9  2 11(001001) | d9  2 11(011001) | e9  3 111(01001) | f9  8 11111001
  ca  2 11(001010) | da  2 11(011010) | ea  3 111(01010) | fa  8 11111010
  cb  2 11(001011) | db  2 11(011011) | eb  3 111(01011) | fb  8 11111011
  cc  2 11(001100) | dc  2 11(011100) | ec  3 111(01100) | fc  8 11111100
  cd  2 11(001101) | dd  2 11(011101) | ed  4 1110(1101) | fd  8 11111101
  ce  2 11(001110) | de  2 11(011110) | ee  3 111(01110) | fe  8 11111110
  cf  2 11(001111) | df  2 11(011111) | ef  3 111(01111) | ff  8 11111111  }
const cc007F=$0;
      cc808F=$1;
      ccC2DF=$2;
      ccE1ECEEEF=$3;
      ccED=$4;
      ccF4=$5;
      ccF1F3=$6;
      ccA0BF=$7;
      ccC0C1F5FF=$8;
      cc909F=$9;
      ccE0=$a;
      ccF0=$b;
      tsBEGIN=0;
      tsERROR=1;
      tsSINGLETAIL=2;
      tsDOUBLETAIL=3;
      tsDOUBLETAILwithA0BFonly=4;
      tsDOUBLETAILwith809FFonly=5;
      tsTRIPLETAILwith90BFonly=6;
      tsTRIPLETAIL=7;
      tsTRIPLETAILwith808Fonly=8;
{$else}
const cc007F=$0;
      cc80BF=$1; // Tail
      ccC0DF=$3; // ($ff shr $03)=$1f
      ccE0EF=$4; // ($ff shr $04)=$0f
      ccF0F7=$5; // ($ff shr $05)=$07
      ccF8FB=$6; // ($ff shr $06)=$03
      ccFCFD=$7; // ($ff shr $07)=$01
      ccFEFF=$8; // ($ff shr $08)=$00
      tsBEGIN=0;
      tsERROR=1;
      tsSINGLETAIL=2;
      tsDOUBLETAIL=3;
      tsTRIPLETAIL=4;
      tsQUADTAIL=5;
      tsQUINTAIL=6;
{$endif}
      tsMUL=16;
 procedure AssignCharsetToCharClass(const Charset:TAnsiCharSet;CharClass:byte);
 var c:ansichar;
 begin
  for c:=low(ansichar) to high(ansichar) do begin
   if c in Charset then begin
    UTF8DFACharClasses[c]:=CharClass;
   end;
  end;
 end;
 procedure AddTranslation(FromState,AtCharClass,ToState:byte);
 begin
  UTF8DFATransitions[(FromState*tsMUL)+AtCharClass]:=ToState*tsMUL;
 end;
var i:longint;
begin
 FillChar(UTF8DFACharClasses,sizeof(TUTF8Chars),#0);
 FillChar(UTF8DFATransitions,sizeof(TUTF8Bytes),#0);
 begin
{$ifdef StrictUTF8}
  AssignCharsetToCharClass([#$00..#$7f],cc007F);
  AssignCharsetToCharClass([#$80..#$8f],cc808F);
  AssignCharsetToCharClass([#$90..#$9f],cc909F);
  AssignCharsetToCharClass([#$a0..#$bf],ccA0BF);
  AssignCharsetToCharClass([#$c0..#$c1],ccC0C1F5FF);
  AssignCharsetToCharClass([#$c2..#$df],ccC2DF);
  AssignCharsetToCharClass([#$e0],ccE0);
  AssignCharsetToCharClass([#$e1..#$ec,#$ee..#$ef],ccE1ECEEEF);
  AssignCharsetToCharClass([#$ed],ccED);
  AssignCharsetToCharClass([#$f0],ccF0);
  AssignCharsetToCharClass([#$f1..#$f3],ccF1F3);
  AssignCharsetToCharClass([#$f4],ccF4);
  AssignCharsetToCharClass([#$f5..#$ff],ccC0C1F5FF);
{$else}
  AssignCharsetToCharClass([#$00..#$7f],cc007F);
  AssignCharsetToCharClass([#$80..#$bf],cc80BF);
  AssignCharsetToCharClass([#$c0..#$df],ccC0DF);
  AssignCharsetToCharClass([#$e0..#$ef],ccE0EF);
  AssignCharsetToCharClass([#$f0..#$f7],ccF0F7);
  AssignCharsetToCharClass([#$f8..#$fb],ccF8FB);
  AssignCharsetToCharClass([#$fc..#$fd],ccFCFD);
  AssignCharsetToCharClass([#$fe..#$ff],ccFEFF);
{$endif}
 end;
 begin
  for i:=low(TUTF8Bytes) to high(TUTF8Bytes) do begin
   UTF8DFATransitions[i]:=tsERROR*tsMUL;
  end;
{$ifdef StrictUTF8}
  begin
   AddTranslation(tsBEGIN,cc007F,tsBEGIN);
   AddTranslation(tsBEGIN,cc808F,tsERROR);
   AddTranslation(tsBEGIN,cc909F,tsERROR);
   AddTranslation(tsBEGIN,ccA0BF,tsERROR);
   AddTranslation(tsBEGIN,ccC2DF,tsSINGLETAIL);
   AddTranslation(tsBEGIN,ccE0,tsDOUBLETAILwithA0BFonly);
   AddTranslation(tsBEGIN,ccE1ECEEEF,tsDOUBLETAIL);
   AddTranslation(tsBEGIN,ccED,tsDOUBLETAILwith809FFonly);
   AddTranslation(tsBEGIN,ccF0,tsTRIPLETAILwith90BFonly);
   AddTranslation(tsBEGIN,ccF1F3,tsTRIPLETAIL);
   AddTranslation(tsBEGIN,ccF4,tsTRIPLETAILwith808Fonly);
   AddTranslation(tsBEGIN,ccC0C1F5FF,tsERROR);
  end;
  begin
   AddTranslation(tsERROR,cc007F,tsERROR);
   AddTranslation(tsERROR,cc808F,tsERROR);
   AddTranslation(tsERROR,cc909F,tsERROR);
   AddTranslation(tsERROR,ccA0BF,tsERROR);
   AddTranslation(tsERROR,ccC2DF,tsERROR);
   AddTranslation(tsERROR,ccE0,tsERROR);
   AddTranslation(tsERROR,ccE1ECEEEF,tsERROR);
   AddTranslation(tsERROR,ccED,tsERROR);
   AddTranslation(tsERROR,ccF0,tsERROR);
   AddTranslation(tsERROR,ccF1F3,tsERROR);
   AddTranslation(tsERROR,ccF4,tsERROR);
   AddTranslation(tsERROR,ccC0C1F5FF,tsERROR);
  end;
  begin
   AddTranslation(tsSINGLETAIL,cc007F,tsERROR);
   AddTranslation(tsSINGLETAIL,cc808F,tsBEGIN);
   AddTranslation(tsSINGLETAIL,cc909F,tsBEGIN);
   AddTranslation(tsSINGLETAIL,ccA0BF,tsBEGIN);
   AddTranslation(tsSINGLETAIL,ccC2DF,tsERROR);
   AddTranslation(tsSINGLETAIL,ccE0,tsERROR);
   AddTranslation(tsSINGLETAIL,ccE1ECEEEF,tsERROR);
   AddTranslation(tsSINGLETAIL,ccED,tsERROR);
   AddTranslation(tsSINGLETAIL,ccF0,tsERROR);
   AddTranslation(tsSINGLETAIL,ccF1F3,tsERROR);
   AddTranslation(tsSINGLETAIL,ccF4,tsERROR);
   AddTranslation(tsSINGLETAIL,ccC0C1F5FF,tsERROR);
  end;
  begin
   AddTranslation(tsDOUBLETAIL,cc007F,tsERROR);
   AddTranslation(tsDOUBLETAIL,cc808F,tsSINGLETAIL);
   AddTranslation(tsDOUBLETAIL,cc909F,tsSINGLETAIL);
   AddTranslation(tsDOUBLETAIL,ccA0BF,tsSINGLETAIL);
   AddTranslation(tsDOUBLETAIL,ccC2DF,tsERROR);
   AddTranslation(tsDOUBLETAIL,ccE0,tsERROR);
   AddTranslation(tsDOUBLETAIL,ccE1ECEEEF,tsERROR);
   AddTranslation(tsDOUBLETAIL,ccED,tsERROR);
   AddTranslation(tsDOUBLETAIL,ccF0,tsERROR);
   AddTranslation(tsDOUBLETAIL,ccF1F3,tsERROR);
   AddTranslation(tsDOUBLETAIL,ccF4,tsERROR);
   AddTranslation(tsDOUBLETAIL,ccC0C1F5FF,tsERROR);
  end;
  begin
   AddTranslation(tsDOUBLETAILwithA0BFonly,cc007F,tsERROR);
   AddTranslation(tsDOUBLETAILwithA0BFonly,cc808F,tsERROR);
   AddTranslation(tsDOUBLETAILwithA0BFonly,cc909F,tsERROR);
   AddTranslation(tsDOUBLETAILwithA0BFonly,ccA0BF,tsSINGLETAIL);
   AddTranslation(tsDOUBLETAILwithA0BFonly,ccC2DF,tsERROR);
   AddTranslation(tsDOUBLETAILwithA0BFonly,ccE0,tsERROR);
   AddTranslation(tsDOUBLETAILwithA0BFonly,ccE1ECEEEF,tsERROR);
   AddTranslation(tsDOUBLETAILwithA0BFonly,ccED,tsERROR);
   AddTranslation(tsDOUBLETAILwithA0BFonly,ccF0,tsERROR);
   AddTranslation(tsDOUBLETAILwithA0BFonly,ccF1F3,tsERROR);
   AddTranslation(tsDOUBLETAILwithA0BFonly,ccF4,tsERROR);
   AddTranslation(tsDOUBLETAILwithA0BFonly,ccC0C1F5FF,tsERROR);
  end;
  begin
   AddTranslation(tsDOUBLETAILwith809FFonly,cc007F,tsERROR);
   AddTranslation(tsDOUBLETAILwith809FFonly,cc808F,tsSINGLETAIL);
   AddTranslation(tsDOUBLETAILwith809FFonly,cc909F,tsSINGLETAIL);
   AddTranslation(tsDOUBLETAILwith809FFonly,ccA0BF,tsERROR);
   AddTranslation(tsDOUBLETAILwith809FFonly,ccC2DF,tsERROR);
   AddTranslation(tsDOUBLETAILwith809FFonly,ccE0,tsERROR);
   AddTranslation(tsDOUBLETAILwith809FFonly,ccE1ECEEEF,tsERROR);
   AddTranslation(tsDOUBLETAILwith809FFonly,ccED,tsERROR);
   AddTranslation(tsDOUBLETAILwith809FFonly,ccF0,tsERROR);
   AddTranslation(tsDOUBLETAILwith809FFonly,ccF1F3,tsERROR);
   AddTranslation(tsDOUBLETAILwith809FFonly,ccF4,tsERROR);
   AddTranslation(tsDOUBLETAILwith809FFonly,ccC0C1F5FF,tsERROR);
  end;
  begin
   AddTranslation(tsTRIPLETAILwith90BFonly,cc007F,tsERROR);
   AddTranslation(tsTRIPLETAILwith90BFonly,cc808F,tsERROR);
   AddTranslation(tsTRIPLETAILwith90BFonly,cc909F,tsDOUBLETAIL);
   AddTranslation(tsTRIPLETAILwith90BFonly,ccA0BF,tsDOUBLETAIL);
   AddTranslation(tsTRIPLETAILwith90BFonly,ccC2DF,tsERROR);
   AddTranslation(tsTRIPLETAILwith90BFonly,ccE0,tsERROR);
   AddTranslation(tsTRIPLETAILwith90BFonly,ccE1ECEEEF,tsERROR);
   AddTranslation(tsTRIPLETAILwith90BFonly,ccED,tsERROR);
   AddTranslation(tsTRIPLETAILwith90BFonly,ccF0,tsERROR);
   AddTranslation(tsTRIPLETAILwith90BFonly,ccF1F3,tsERROR);
   AddTranslation(tsTRIPLETAILwith90BFonly,ccF4,tsERROR);
   AddTranslation(tsTRIPLETAILwith90BFonly,ccC0C1F5FF,tsERROR);
  end;
  begin
   AddTranslation(tsTRIPLETAIL,cc007F,tsERROR);
   AddTranslation(tsTRIPLETAIL,cc808F,tsDOUBLETAIL);
   AddTranslation(tsTRIPLETAIL,cc909F,tsDOUBLETAIL);
   AddTranslation(tsTRIPLETAIL,ccA0BF,tsDOUBLETAIL);
   AddTranslation(tsTRIPLETAIL,ccC2DF,tsERROR);
   AddTranslation(tsTRIPLETAIL,ccE0,tsERROR);
   AddTranslation(tsTRIPLETAIL,ccE1ECEEEF,tsERROR);
   AddTranslation(tsTRIPLETAIL,ccED,tsERROR);
   AddTranslation(tsTRIPLETAIL,ccF0,tsERROR);
   AddTranslation(tsTRIPLETAIL,ccF1F3,tsERROR);
   AddTranslation(tsTRIPLETAIL,ccF4,tsERROR);
   AddTranslation(tsTRIPLETAIL,ccC0C1F5FF,tsERROR);
  end;
  begin
   AddTranslation(tsTRIPLETAILwith808Fonly,cc007F,tsERROR);
   AddTranslation(tsTRIPLETAILwith808Fonly,cc808F,tsDOUBLETAIL);
   AddTranslation(tsTRIPLETAILwith808Fonly,cc909F,tsERROR);
   AddTranslation(tsTRIPLETAILwith808Fonly,ccA0BF,tsERROR);
   AddTranslation(tsTRIPLETAILwith808Fonly,ccC2DF,tsERROR);
   AddTranslation(tsTRIPLETAILwith808Fonly,ccE0,tsERROR);
   AddTranslation(tsTRIPLETAILwith808Fonly,ccE1ECEEEF,tsERROR);
   AddTranslation(tsTRIPLETAILwith808Fonly,ccED,tsERROR);
   AddTranslation(tsTRIPLETAILwith808Fonly,ccF0,tsERROR);
   AddTranslation(tsTRIPLETAILwith808Fonly,ccF1F3,tsERROR);
   AddTranslation(tsTRIPLETAILwith808Fonly,ccF4,tsERROR);
   AddTranslation(tsTRIPLETAILwith808Fonly,ccC0C1F5FF,tsERROR);
  end;
 end;
{$else}
  begin
   AddTranslation(tsBEGIN,cc007F,tsBEGIN);
   AddTranslation(tsBEGIN,cc80BF,tsERROR);
   AddTranslation(tsBEGIN,ccC0DF,tsSINGLETAIL);
   AddTranslation(tsBEGIN,ccE0EF,tsDOUBLETAIL);
   AddTranslation(tsBEGIN,ccF0F7,tsTRIPLETAIL);
   AddTranslation(tsBEGIN,ccF8FB,tsQUADTAIL);
   AddTranslation(tsBEGIN,ccFCFD,tsQUINTAIL);
   AddTranslation(tsBEGIN,ccFEFF,tsERROR);
  end;
  begin
   AddTranslation(tsERROR,cc007F,tsERROR);
   AddTranslation(tsERROR,cc80BF,tsERROR);
   AddTranslation(tsERROR,ccC0DF,tsERROR);
   AddTranslation(tsERROR,ccE0EF,tsERROR);
   AddTranslation(tsERROR,ccF0F7,tsERROR);
   AddTranslation(tsERROR,ccF8FB,tsERROR);
   AddTranslation(tsERROR,ccFCFD,tsERROR);
   AddTranslation(tsERROR,ccFEFF,tsERROR);
  end;
  begin
   AddTranslation(tsSINGLETAIL,cc007F,tsERROR);
   AddTranslation(tsSINGLETAIL,cc80BF,tsBEGIN);
   AddTranslation(tsSINGLETAIL,ccC0DF,tsERROR);
   AddTranslation(tsSINGLETAIL,ccE0EF,tsERROR);
   AddTranslation(tsSINGLETAIL,ccF0F7,tsERROR);
   AddTranslation(tsSINGLETAIL,ccF8FB,tsERROR);
   AddTranslation(tsSINGLETAIL,ccFCFD,tsERROR);
   AddTranslation(tsSINGLETAIL,ccFEFF,tsERROR);
  end;
  begin
   AddTranslation(tsDOUBLETAIL,cc007F,tsERROR);
   AddTranslation(tsDOUBLETAIL,cc80BF,tsSINGLETAIL);
   AddTranslation(tsDOUBLETAIL,ccC0DF,tsERROR);
   AddTranslation(tsDOUBLETAIL,ccE0EF,tsERROR);
   AddTranslation(tsDOUBLETAIL,ccF0F7,tsERROR);
   AddTranslation(tsDOUBLETAIL,ccF8FB,tsERROR);
   AddTranslation(tsDOUBLETAIL,ccFCFD,tsERROR);
   AddTranslation(tsDOUBLETAIL,ccFEFF,tsERROR);
  end;
  begin
   AddTranslation(tsTRIPLETAIL,cc007F,tsERROR);
   AddTranslation(tsTRIPLETAIL,cc80BF,tsDOUBLETAIL);
   AddTranslation(tsTRIPLETAIL,ccC0DF,tsERROR);
   AddTranslation(tsTRIPLETAIL,ccE0EF,tsERROR);
   AddTranslation(tsTRIPLETAIL,ccF0F7,tsERROR);
   AddTranslation(tsTRIPLETAIL,ccF8FB,tsERROR);
   AddTranslation(tsTRIPLETAIL,ccFCFD,tsERROR);
   AddTranslation(tsTRIPLETAIL,ccFEFF,tsERROR);
  end;
  begin
   AddTranslation(tsQUADTAIL,cc007F,tsERROR);
   AddTranslation(tsQUADTAIL,cc80BF,tsTRIPLETAIL);
   AddTranslation(tsQUADTAIL,ccC0DF,tsERROR);
   AddTranslation(tsQUADTAIL,ccE0EF,tsERROR);
   AddTranslation(tsQUADTAIL,ccF0F7,tsERROR);
   AddTranslation(tsQUADTAIL,ccF8FB,tsERROR);
   AddTranslation(tsQUADTAIL,ccFCFD,tsERROR);
   AddTranslation(tsQUADTAIL,ccFEFF,tsERROR);
  end;
  begin
   AddTranslation(tsQUINTAIL,cc007F,tsERROR);
   AddTranslation(tsQUINTAIL,cc80BF,tsQUADTAIL);
   AddTranslation(tsQUINTAIL,ccC0DF,tsERROR);
   AddTranslation(tsQUINTAIL,ccE0EF,tsERROR);
   AddTranslation(tsQUINTAIL,ccF0F7,tsERROR);
   AddTranslation(tsQUINTAIL,ccF8FB,tsERROR);
   AddTranslation(tsQUINTAIL,ccFCFD,tsERROR);
   AddTranslation(tsQUINTAIL,ccFEFF,tsERROR);
  end;
 end;
{$endif}
end;

initialization
 InitBase64;
 InitializeUTF8DFA;
 InitializeEntites;
finalization
 FinalizeEntites;
end.


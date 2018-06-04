(********************************************************)
(*                                                      *)
(*  Codebot standard include file                       *)
(*                                                      *)
(* Unit naming suggestions:                             *)
(*                                                      *)
(* Either a descriptive name such as Buttons or Grids   *)
(* or a descriptive name followed by one of these       *)
(* suffixes:                                            *)
(*                                                      *)
(*   XXXCtrls                                           *)
(*   XXXIntf                                            *)
(*   XXXKit                                             *)
(*   XXXMgr                                             *)
(*   XXXObj                                             *)
(*   XXXReg                                             *)
(*   XXXSvr                                             *)
(*   XXXTools                                           *)
(*   XXXTypes                                           *)
(*                                                      *)
(********************************************************)

{$IFDEF WIN32}

  { Delphi 2009 }
  {$IFDEF VER200}
    {$DEFINE D12}
    {$DEFINE D12_UP}
    {$DEFINE D11_UP}
    {$DEFINE D10_UP}
    {$DEFINE D9_UP}
    {$DEFINE D8_UP}
    {$DEFINE D7_UP}
    {$DEFINE D6_UP}
    {$DEFINE D5_UP}
    {$DEFINE D4_UP}
  {$ENDIF}

  { Delphi 2007 }

  {$IFDEF VER185}
    {$DEFINE D11}
    {$DEFINE D11_UP}
    {$DEFINE D10_UP}
    {$DEFINE D9_UP}
    {$DEFINE D8_UP}
    {$DEFINE D7_UP}
    {$DEFINE D6_UP}
    {$DEFINE D5_UP}
    {$DEFINE D4_UP}
  {$ENDIF}

  { Delphi 2006 }
  {$IFDEF VER180}
    {$DEFINE D10}
    {$DEFINE D10_UP}
    {$DEFINE D9_UP}
    {$DEFINE D8_UP}
    {$DEFINE D7_UP}
    {$DEFINE D6_UP}
    {$DEFINE D5_UP}
    {$DEFINE D4_UP}
  {$ENDIF}

  { Delphi 2005 }
  {$IFDEF VER170}
    {$DEFINE D9}
    {$DEFINE D9_UP}
    {$DEFINE D8_UP}
    {$DEFINE D7_UP}
    {$DEFINE D6_UP}
    {$DEFINE D5_UP}
    {$DEFINE D4_UP}
  {$ENDIF}

  {$IFDEF VER160}
    {$DEFINE D8}
    {$DEFINE D8_UP}
    {$DEFINE D7_UP}
    {$DEFINE D6_UP}
    {$DEFINE D5_UP}
    {$DEFINE D4_UP}
  {$ENDIF}

  {$IFDEF VER150}
    {$DEFINE D7}
    {$DEFINE D7_UP}
    {$DEFINE D6_UP}
    {$DEFINE D5_UP}
    {$DEFINE D4_UP}
  {$ENDIF}

  {$IFDEF VER140}
    {$DEFINE D6}
    {$DEFINE D6_UP}
    {$DEFINE D5_UP}
    {$DEFINE D4_UP}
  {$ENDIF}

  {$IFDEF VER130}
    {$DEFINE D5}
    {$DEFINE D5_UP}
    {$DEFINE D4_UP}
  {$ENDIF}

  {$IFDEF VER120}
    {$DEFINE D4}
    {$DEFINE D4_UP}
  {$ENDIF}

{$IFDEF D12_UP}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$ENDIF}

{$IFDEF D6_UP}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN SYMBOL_LIBRARY OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN UNIT_DEPRECATED OFF}
  {$WARN UNIT_LIBRARY OFF}
  {$WARN UNIT_PLATFORM OFF}
{$ENDIF}

{$B-} { Complete Boolean Evaluation }
{$R-} { Range-Checking }
{$V-} { Var-String Checking }
{$T-} { Typed @ operator }
{$X+} { Extended syntax }
{$P+} { Open string params }

{$IFDEF WIN32}
  {$J+} { Writeable structured consts }
  {$H+} { Use long strings by default }
{$ENDIF}

 { Remove the . to activate compile modes }

{.$DEFINE LITE}
{.$DEFINE COMPONENT}
{.$DEFINE DEBUGLOG}
{.$DEFINE DEBUGMODE}
{.$DEFINE GDIPLUS}
{.$DEFINE FASTSTRING}

{.$IFNDEF DEBUGMODE}
  {.$L-}
  {.$D-}
  {.$Y-}
  {.$C-}
{.$ELSE}
  {.$C+}
{.$ENDIF}

{$ENDIF}

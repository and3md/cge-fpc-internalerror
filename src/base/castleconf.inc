{
  Copyright 2002-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Common compiler defines and symbols.
  This is a central include file for Castle Game Engine.

  This file is *not* included in 100% of our units,
  it's just included where it's needed.
  We don't try to set compilation options here (like $H+ or FPC objfpc mode)
  because:
  - Not all interesting compiler settings can be set here.
    For example optimization options can only be specified on the command-line
    (and they are important for a game engine).
  - It's error prone, it's too easy to forget to include this file in some unit.
    Using a consistent compiler options in scripts is easier.

  The proper compiler options to compile the engine are
  - in the ../../castle-fpc.cfg configuration file (used also by fpmake
    compilation method),
  - and they are replicated in Lazarus packages compilation settings,
  - and are replicated once more in the castle-engine build tool code.
}

{$ifndef CASTLE_CONF_INCLUDED}
{$define CASTLE_CONF_INCLUDED}

{ Configure syntax ----------------------------------------------------------- }

{ Although these options are also specified by the CGE build tool
  on the command-line,
  and are in Lazarus packages too,
  we still need to repeat them here.

  That is because user can specify "<option>-Mdelphi</option>"
  in CastleEngineManifest.xml, to compile the application in Delphi mode.
}
{$ifdef FPC}
  {$ifndef CASTLE_ENGINE_TEST_DELPHI_MODE}

    // CASTLE_CONF_DO_NOT_OVERRIDE_MODE is a hack for CastleUtils unit and FPC 3.0.x
    {$ifndef CASTLE_CONF_DO_NOT_OVERRIDE_MODE}
      {$mode objfpc}
      // Using CASTLE_OBJFPC instead of FPC_OBJFPC
      // allows to workaround FPC bug in some units, FPC seems to "lose"
      // the fact that FPC_OBJFPC should be defined.
      // The cleanest solution seems to just ignore FPC_OBJFPC,
      // and define equivalent symbol (CASTLE_OBJFPC) on our own.
      {$define CASTLE_OBJFPC}
    {$endif}

    {$H+}
    {$macro on}
  {$endif}
{$endif}

{ For FPC, these can only be specified here, not on the FPC command-line. }
{$writeableconst off}
{$ifdef FPC} {$modeswitch advancedrecords} {$endif}

{ Define various necessary things -------------------------------------------- }

(*EXTENDED_EQUALS_DOUBLE should be defined when Extended type is
  the same thing as Double type on this platform.

  One typical case when this is important is when you overload
  one procedure like
    p(single)
    p(double)
    p(extended)
  In such cases you must do it like this:
    p(single)
    p(double)
    {$ifndef EXTENDED_EQUALS_DOUBLE} p(extended) {$endif}

  According to FPC docs (Programmers Manual, 8.2.0: floating point types),
  there's no Extended (i.e. Extended = Double) for most of non-i386 architectures.
  Exception to the above is Linux on x86-64, that allows to use normal Extended.
  Maybe Darwin on x86-64 also?
*)
{$ifdef FPC}
  {$ifndef FPC_HAS_TYPE_EXTENDED}
    {$define EXTENDED_EQUALS_DOUBLE}
  {$endif}
{$endif}

{ We used to disable inline functions/methods when compiling from
  Lazarus package (when CASTLE_ENGINE_LAZARUS_PACKAGE),
  to workaround FPC bug http://bugs.freepascal.org/view.php?id=12223 .
  That bug is closed, but with an answer basically "it will never be fixed",
  which means that you cannot have units in Lazarus package having
  "inline". (Unless you add -Ur, which would be uncomfortable for
  engine development from Lazarus.)

  But with FPC 3.0.2 everything seems to work cool even with "inline",
  so let's try defining it always now.
  We only support FPC >= 3.0.0 now.
}
{$define SUPPORTS_INLINE}

{$define TOBJECT_HAS_EQUALS}

{$endif not CASTLE_CONF_INCLUDED}

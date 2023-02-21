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

{ VRML/X3D classic lexer (TX3DLexer). }
unit CastleInternalX3DLexer;

{$mode objfpc}
{$H+}
{$macro on}
{$writeableconst off}
{$modeswitch advancedrecords}


interface

uses SysUtils, Classes;

type
  TX3DEncoding = (xeClassic, xeXML); // remove this to get rid of exeption or internal error in trunk fpc

  TX3DVersion = object
    Major, Minor: Integer;
  end;

implementation

uses X3DFields;// remove this X3DFields (X3DLoadInternalUtils was here before) include to get rid of internal error

end.

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

{ X3D fields (TX3DField and many descendants). }
unit X3DFields;

{$mode objfpc}
{$H+}
{$macro on}
{$writeableconst off}
{$modeswitch advancedrecords}


interface

uses Classes, SysUtils, Generics.Collections,
  CastleInternalX3DLexer;

type
  { Base class for any item within VRML/X3D file: a node, a field, a route,
    a prototype etc. }
  TX3DFileItem = class
  end;

  TX3DField = class;
  TX3DEvent = class;

  TX3DField = class(TX3DFileItem)
  end;

  { X3D event. }
  TX3DEvent = class(TX3DFileItem)
  strict private
    AVariable: Integer; // just for keep strict private
    //ATest: Integer; // uncomment to get internal error
  end;

  TSFNodeEvent = class(TX3DEvent)
  end;

implementation

uses X3DNodes;

end.

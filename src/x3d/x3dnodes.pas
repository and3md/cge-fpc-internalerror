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

unit X3DNodes;

{$mode objfpc}
{$H+}
{$macro on}
{$writeableconst off}
{$modeswitch advancedrecords}

interface

uses SysUtils, Generics.Collections, Classes,
  CastleInternalX3DLexer, X3DFields,
  CastleMaterialProperties;

{$define read_interface}

type
  TX3DNode = class;
  TX3DRootNode = class;
  TSFNode = class;

  TX3DNodeClass = class of TX3DNode;

  {$I x3dnodes_x3dnode.inc}
  {$I x3dnodes_sfnode.inc}

  { Nodes from standard X3D components }
  {$I x3dnodes_standard_grouping.inc}

{$undef read_interface}

implementation

{$define read_implementation}

{$I x3dnodes_x3dnode.inc}
{$I x3dnodes_sfnode.inc}

{ Nodes from standard X3D components }
{$I x3dnodes_standard_grouping.inc}

{ unit init/fini ------------------------------------------------------------ }

initialization

finalization

end.

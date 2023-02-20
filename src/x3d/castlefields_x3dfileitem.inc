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

{$ifdef read_interface}

  { Base class for any item within VRML/X3D file: a node, a field, a route,
    a prototype etc. We need a common base class for all such things
    to store PositionInParent.

    About ancestry: TX3DFieldOrEvent make use of Assign mechanism
    and so need to descend from TPersistent. TX3DNode make use
    of interfaces and so must descend from something like
    TNonRefCountedInterfacedXxx. These are the only reasons, for now,
    why this descends from TNonRefCountedInterfacedPersistent. }
  TX3DFileItem = class
  end;

  TX3DFileItemList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TX3DFileItem>)
  public
    procedure Add(Item: TX3DFileItem); reintroduce;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TX3DFileItemList --------------------------------------------------------- }

procedure TX3DFileItemList.Add(Item: TX3DFileItem);
begin
  inherited Add(Item);
end;

{$endif read_implementation}

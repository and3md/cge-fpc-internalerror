{
  Copyright 2006-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Powerful automatic interpolation for any X3D node hierarchies,
  able to animate a sequence of any X3D scenes. }
unit CastleInternalNodeInterpolator;

{$I ../base/castleconf.inc}

interface

uses Classes, X3DNodes;

implementation

uses SysUtils;

{ Linear interpolation between Model1 and Model2.
  A = 0 means Model1, A = 1 means Model2, A between 0 and 1 is lerp
  between Model1 and Model2.

  If Model1 and Model2 are the same object (the same references),
  then this will return just Model1. This way it keeps memory optimization
  described by NodesMerge. This is also true if both Model1 and Model2
  are nil: then you can safely call this and it will return also nil. }
function NodesLerp(const A: Single; Model1, Model2: TX3DNode): TX3DNode;

  procedure SFNodeLerp(Target, Field1, Field2: TSFNode);
  begin
    Target.Value := NodesLerp(A, Field1.Value, Field2.Value)
  end;
begin
  if Model1 = Model2 then
    Exit(Model1);

  Result := TX3DNodeClass(Model1.ClassType).Create;
  try
    if Result is TX3DRootNode then
    begin
      { copy TX3DRootNode special fields, like TX3DRootNode.DeepCopyCore.
        This is necessary for WrapRootNode working Ok lower in this file. }
      TX3DRootNode(Result).HasForceVersion := (Model1 as TX3DRootNode).HasForceVersion;
      TX3DRootNode(Result).ForceVersion := (Model1 as TX3DRootNode).ForceVersion;
      TX3DRootNode(Result).Scale := (Model1 as TX3DRootNode).Scale;
      TX3DRootNode(Result).Profile := (Model1 as TX3DRootNode).Profile;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

end.

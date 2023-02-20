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

  { Forward declarations } { }
  TX3DNodeList = class;
  TX3DNode = class;
  TX3DRootNode = class;
  TSFNode = class;

  TX3DNodeClass = class of TX3DNode;

  TX3DNodeProc = procedure (Node: TX3DNode) of object;
  TX3DNodeSearchProc = function (Node: TX3DNode): Pointer of object;

  PTraversingInfo = ^TTraversingInfo;
  TTraversingInfo = record
    Node: TX3DNode;
    ParentInfo: PTraversingInfo;
  end;

  TEnumerateChildrenFunction =
    function (Node, Child: TX3DNode): Pointer of object;

  TEnumerateReplaceNodesFunction =
    procedure (ParentNode: TX3DNode; var Node: TX3DNode) of object;

  TNodeTransformationChange = (
    ntcNone,
    ntcSwitch, //< TSwitchNode
    ntcLOD, //< TAbstractLODNode
    ntcTransform, //< ITransformNode
    ntcGeometry, //< TAbstractGeometryNode
    ntcBackground, //< TAbstractBackgroundNode
    ntcFog, //< TFogNode
    ntcViewpoint, //< TAbstractViewpointNode
    ntcLight, //< TAbstractLightNode
    ntcProximitySensor, //< TProximitySensorNode
    ntcVisibilitySensor //< TVisibilitySensorNode
  );

{$endif read_interface}

{$ifdef read_implementation}

{$endif read_implementation}

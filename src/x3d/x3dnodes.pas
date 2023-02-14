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

(*
  @abstract(Nodes and other important bulding blocks
  of VRML/X3D (prototypes, routes and so on).)

  This is the central unit for VRML/X3D processing, as VRML/X3D file
  is basically just a graph of nodes. We represent whole VRML/X3D file
  by it's root node. This is what we load, save and process in this unit.

  The chapter "Reading, writing, processing VRML scene graph"
  in the documentation on
  [https://castle-engine.io/vrml_engine_doc/output/xsl/html/chapter.scene_graph.html]
  is almost completely devoted to documenting the design of this single unit.

  @bold(Various uses of this unit:)

  @unorderedList(
    @item(Nodes can be loaded or saved from the stream in a classic or
      XML encoding.
      For classic encoding we use a lexer in CastleInternalX3DLexer unit.
      For XML encoding, we use standard FPC DOM unit.
      Loading and saving of fields (in both encodings) is inside X3DFields unit.

      When reading VRML/X3D files, we generally do not change the VRML/X3D graph.
      So we're able to save exactly the same VRML/X3D graph
      back to another file. See also
      [https://castle-engine.io/vrml_engine_doc/output/xsl/html/section.writing_vrml.html#section.vrml_preserving].
      This allows writing various VRML/X3D
      processing tools, that can simply read the file, change whatever
      they want, and write the file back --- knowing that the "untouched"
      parts of graph are preserved perfectly.)

    @item(TX3DNode class offers a lot of methods to process VRML/X3D graph.
      See TX3DNode.Traverse, TX3DNode.EnumerateNodes and
      TX3DNode.FindNode. TX3DNode.Traverse is especially important, as it
      walks through VRML/X3D graph just as the specification says
      (accumulating transformation, visiting only active children of
      nodes like Switch or LOD),
      gathering some state (useful especially for VRML 1.0, but also
      used for various things in later VRML/X3D versions).

      When you want to render VRML/X3D graph, you can just traverse
      the graph and render each geometry node (TAbstractGeometryNode instance)
      knowing it's state (that will contain transformation and such).
      Alternatively, simple renderer can also use TAbstractGeometryNode.Triangulate.)

    @item(TAbstractGeometryNode is an important descendant of TX3DNode,
      as it defines stuff actually visible in the 3D world.
      It has useful routines for calculating bounding volumes,
      triangulating and such.

      But note that usually it's more comfortable
      to load your scene to TCastleScene or TCastleSceneCore and then
      query the shapes list in TCastleSceneCore.Shapes --- this is usually
      more comfortable, also TCastleSceneCore and TShape cache some results
      for speed.)

    @item(This unit doesn't depend on OpenGL, or any other particular rendering
      method. So it's suitable also for CastleRayTracer, and every other possible
      renderer that will ever get implemented.)

    @item(Your own units can define new VRML/X3D nodes, by declaring
      new classes descending from TX3DNode (or other, more specialized,
      descendant). You should register your new classes by calling
      @link(TNodesManager.RegisterNodeClasses NodesManager.RegisterNodeClasses).

      Examples of defining your own VRML/X3D node types (without modifying
      sources of this unit, or any other unit) are for example in "malfunction" game
      on https://github.com/castle-engine/malfunction (see LevelUnit).)
  )

  @bold(Node class names, and inheritance:)

  @unorderedList(
    @item(Normal VRML/X3D nodes are defined by classses
      named like @code(TXxxNode). These nodes can be specified inside the VRML/X3D
      files. See VRML/X3D specifications, and also our extensions specification,
      on [https://castle-engine.io/vrml_x3d.php].

      There are also abstract node classes. Their definitions are helpful
      for handling some functionality common to many descendants,
      and to declare allowed children in SFNode/MFNode fields.
      Abstract node classes are named like @code(TAbstractXxxNode).
      Some of the abstract nodes are also defined by X3D specification,
      and some of them are just our own inventions.

      Finally, there are some special-purpose node classes that play
      important role in our VRML/X3D organization.
      They are not abstract, but also their exact instances
      are not created under normal circumstances.
      These are named like @code(TX3DXxxNode), currently
      these are only: TX3DNode, TX3DRootNode, TX3DUnknownNode, TX3DPrototypeNode.

      All node classes descend from the base TX3DNode class.

      Some abstract nodes have also Pascal interfaces, like IAbstractXxxNode.
      Some ideas of X3D specification (although not many)
      need multiple inheritance, so interfaces have to be used.
      They all descend from IX3DNode.)

    @item(
      Optional suffix _1 or _2 at the node class name indicates that
      this is only for a specific VRML/X3D standard version.
      Suffix _1 indicates nodes specific to VRML 1.0.
      Suffix _2 indicates nodes specific to VRML 2.0 (aka 97),
      that are not available in X3D.
      Latest X3D nodes do not have any suffix
      (to not clutter the source code that simply wants to use the latest
      and best version of the standard).

      For example, we have TIndexedFaceSetNode_1 for VRML 1.0 and
      TIndexedFaceSetNode for VRML 2.0 and X3D.)
  )

  @bold(VRML/X3D versions handling:)

  @unorderedList(
    @item(
      We handle VRML 1.0, VRML 2.0 (aka VRML 97) and X3D (aka VRML 3.x).

      Every correct VRML / X3D file in classic and XML encoding should be parsed
      by this unit.
      See [https://castle-engine.io/x3d_implementation_status.php]
      for much more detailed information about supported features.)

    @item(
      Also many Inventor 1.0 files are correctly parsed.
      We handle Inventor 1.0 mostly like VRML 1.0, also some small
      things and nodes specific for Inventor 1.0 are implemented here, see
      [https://castle-engine.io/x3d_extensions.php#ext_iv_in_vrml].)

    @item(
      Note that structures in this unit are @italic(not) focused
      on either VRML 1.0 or VRML >= 2.0. On the contrary: we try to handle
      the @italic(sum of all VRML and X3D). When reading VRML 1.0,
      many VRML 2.0 constructs (that do not conflict with anything in VRML 1.0)
      are allowed, and the other way around too.

      Internally, we do not convert VRML 1.0-specific constructs
      to VRML 2.0/X3D constructs (or the other way around).
      See [https://castle-engine.io/vrml_engine_doc/output/xsl/html/section.vrml_1_2_sum.html]
      for more in-depth explanation of how, and why, we handle both
      old-style (Inventor, VRML 1.0) and new-style (VRML 2.0, X3D)
      syntax.)
  )
*)

unit X3DNodes;

{$I  ../base/castleconf.inc}

interface

uses SysUtils, Generics.Collections, Classes, DOM,
  CastleRectangles,
  CastleInternalX3DLexer, CastleClassUtils,
  X3DFields,
  CastleStringUtils, CastleMaterialProperties;

{$define read_interface}

type
  {$I x3dnodes_initial_types.inc}
  {$I x3dnodes_x3dnode.inc}
  {$I x3dnodes_sfnode.inc}
  {$I x3dnodes_mfnode.inc}

  { Nodes from standard X3D components }
  {$I x3dnodes_standard_core.inc}
  {$I x3dnodes_standard_grouping.inc}

  { More X3D nodes, not from X3D standard }
  {$I x3dnodes_x3dinterfacedeclaration.inc}

{$undef read_interface}

implementation

// Silence warnings about using CastleNURBS (that will soon be renamed CastleInternalNurbs)
{$warnings off}

uses
  Math,
  StrUtils;

{$warnings on}

{$define read_implementation}

{$I x3dnodes_initial_types.inc}

{$I x3dnodes_x3dinterfacedeclaration.inc}

// These must be includes after x3dnodes_encoding_{classic,xml}.inc
{$I x3dnodes_x3dnode.inc}
{$I x3dnodes_sfnode.inc}
{$I x3dnodes_mfnode.inc}

{ Nodes from standard X3D components }
{$I x3dnodes_standard_core.inc}
{$I x3dnodes_standard_grouping.inc}

{ unit init/fini ------------------------------------------------------------ }

initialization

  X3DFieldsManager.RegisterClasses([TSFNode, TMFNode]);
finalization

end.

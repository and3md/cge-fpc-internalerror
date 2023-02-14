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

{ TX3DNode class. Huge class, the basic building block of VRML nodes. }

{$ifdef read_interface}

  { Private stuff for TX3DNode.DeepCopy and friends implementation. }
  TX3DNodeDeepCopyState = class
  private
    { These two lists must always have exactly the same length. }
    Original, New: TX3DNodeList;
  public
    constructor Create;
    destructor Destroy; override;
    { Return a copy or OriginalNode.

      To keep sharing of nodes (DEF/USE mechanism) within the newly
      created copy, we need a list of already duplicated children.
      This method uses and updates such list. When called for the
      first time with OriginalNode, it actually creates a duplicate
      (by OriginalNode.DeepCopy). Next time, it will just return
      this copy. }
    function DeepCopy(OriginalNode: TX3DNode): TX3DNode;
  end;

  { X3D node interface.
    All other interfaces for X3D nodes descend from this. }
  IX3DNode = interface
  ['{5BD33327-430F-47EC-9241-AD899F072BF8}']
  end;

  { X3D node. Every VRML/X3D node class descends from this. }
  TX3DNode = class(TX3DFileItem, IX3DNode)
  private
    FX3DName: string;
    FBaseUrl: string;
    FVRML1Children, FVRML1Parents: TX3DNodeList;
    FKeepExisting: Cardinal;
    FParentFields: TX3DFieldList;
    FImports, FExports: TX3DFileItemList;
    FFields, FFieldsSFNode, FFieldsMFNode: TX3DFieldList;
    FEvents: TX3DEventList;
    FPrototypeInstance: boolean;
    FPrototypeInstanceHelpers: TX3DNode;
    FDefaultContainerField: string;
    FHasInterfaceDeclarations: TX3DAccessTypes;
    FInterfaceDeclarations: TX3DInterfaceDeclarationList;
    ///FScene: TX3DEventsEngine;
    FInternalSceneShape: TObject;

    { Internally used by PROTO instantiating mechanism,
      in TX3DPrototypeNode.InstantiateIsClauses
      and TX3DPrototypeNode.Instantiate.

      This is always @false outside of TX3DPrototypeNode.Instantiate
      implementation (as TX3DPrototypeNode.Instantiate sets it to @true
      and changes back to @false for appropriate nodes). }
    NeedsInstantiateIsClause: boolean;

    function GetVRML1Child(i: integer): TX3DNode;
    function GetVRML1Parent(i: integer): TX3DNode;
    procedure SetVRML1Child(I: Integer; Value: TX3DNode);
    function GetParentFieldsItem(Index: Integer): TX3DField;
    function GetParentFieldsNodeItem(Index: Integer): TX3DNode;
    procedure RemoveParentField(Field: TX3DField);
    procedure AddParentField(Field: TX3DField);
    procedure SetHasInterfaceDeclarations(const Value: TX3DAccessTypes);
    function GetFields(const Index: Integer): TX3DField;
    function GetEvents(const Index: Integer): TX3DEvent;
    procedure UnregisterSceneCallback(Node: TX3DNode);
  protected

    { Does actual DeepCopy work. You can override this to copy some
      more properties for descendants. }
    function DeepCopyCore(CopyState: TX3DNodeDeepCopyState): TX3DNode; virtual;

    { This should be a mere call to constructor of your own class.

      In TX3DNode, this simply calls default virtual constructor,
      which is Ok for all normal nodes. But we have some special nodes,
      like TX3DPrototypeNode or TX3DUnknownNode, that simply cannot
      be created by default constructor. They need to override this. }
    function DeepCopyCreate(CopyState: TX3DNodeDeepCopyState): TX3DNode; virtual;
  protected
    { Are VRML 1.0 children allowed.
      This condition is checked in VRML1ChildAdd, so it's strictly
      impossible to add a node that is not allowed.

      Note that in some special cases VRML1ChildrenAllowed and
      VRML1ChildrenParsingAllowed values may be changed during object lifetime.
      Currently, this may concern TX3DUnknownNode.

      Default @false. }
    VRML1ChildrenAllowed: boolean;

    { VRML 1.0 children allowed to be added during parsing.
      This is used only by *Inline nodes for now, that do not allow
      reading children during parsing but may get new children
      in memory. So their VRML1ChildrenParsingAllowed must be empty,
      but VRML1ChildrenAllowed must allow all.

      When VRML1ChildrenAllowed is @false then VRML1ChildrenParsingAllowed should
      also be @false.

      Default @false. }
    VRML1ChildrenParsingAllowed: boolean;

    { Should SaveToStream save our VRML1Children.
      In this class default implementation returns @true,
      this is what you will want in 99% of cases.
      It's useful to set this to false if you use
      VRML1Children internally, e.g. *Inline nodes. }
    class function VRML1ChildrenSaveToStream: boolean; virtual;

    { Enumerate all active child nodes of given node.

      "Active nodes" are the ones affecting current look or collisions,
      e.g. from Switch node only one child will be enumerated.
      See @link(Traverse) for more precise definition.

      "Direct" means that this enumerates only direct descendants,
      i.e. this is not recursive. See methods like Traverse or EnumerateNodes
      if you want recursive behavior.

      This can enumerate both @link(VRML1Children) nodes
      and nodes within TSFNode and TMFNode fields.

      Default implementation in this class enumerates all Children
      nodes of VRML 1.0. If you need to remove some children
      for VRML 1.0 (e.g. for Switch or LOD nodes)
      or add some children for VRML 2.0 you
      have to override this. @bold(You do not need to call inherited
      when overriding this --- in fact, you should not, if you
      want to omit some nodes.)

      Stops and returns immediately if Func returns non-nil for some child. }
    function DirectEnumerateActive(
      Func: TEnumerateChildrenFunction): Pointer; virtual;

    { Simply enumerate all direct descendant nodes.
      That is, all children in VRML 1.0 style and
      all nodes in SFNode and MFNode fields.
      This includes prototype stuff, if this node is expanded
      from prototype: PrototypeInstanceSourceNode and PrototypeInstanceHelpers. }
    function DirectEnumerateAll(
      Func: TEnumerateChildrenFunction): Pointer;

    { This enumerates direct descendant nodes of this node.
      This is equivalent to DirectEnumerateActive or
      DirectEnumerateAll, depending on value of OnlyActive param. }
    function DirectEnumerate(
      Func: TEnumerateChildrenFunction;
      OnlyActive: boolean): Pointer;

    procedure CreateNode; virtual;
  public
    { Node fields.

      For normal nodes, all fields are created and added
      using AddField from the constructor. Fields default values are set,
      and current field values are set to these defaults.
      Later, we only modify these fields current values (e.g. when parsing).

      However, there are special node classes that set their fields differently.
      TX3DPrototypeNode has their fields set according to it's VRML 2.0 prototype.
      TX3DUnknownNode may have it's fields set by VRML 1.0 "fields" feature
      (so it's Fields are initialized by parsing it).

      Nodes with HasInterfaceDeclarations have some Fields and Events
      added when reading node.

      All fields on this list are owned by this instance.
      @groupBegin }
    property Fields [Index: Integer]: TX3DField read GetFields;
    function FieldsCount: Integer;
    procedure AddField(const Value: TX3DField);
    function IndexOfField(const AName: string): Integer;
    { @groupEnd }

    { Explicit events (that is, not exposed by some field) of this node.
      For exposed events, see each field's property ExposedEvents.
      @groupBegin }
    property Events [Index: Integer]: TX3DEvent read GetEvents;
    function EventsCount: Integer;
    procedure AddEvent(const Value: TX3DEvent);
    function IndexOfEvent(const AName: string): Integer;
    { @groupEnd }

    { Search by name for given field.
      @nil if not found. }
    function Field(const AName: string): TX3DField;

    { Search by name for given field or event (exposed by some field or not).
      @nil if not found. }
    function FieldOrEvent(const AName: string): TX3DFieldOrEvent;

    { Search by name for given event (exposed by some field or not).
      @nil if not found. }
    function AnyEvent(const AName: string): TX3DEvent;

    { VRML 1.0 children nodes. These are nodes directly specified inside
      a VRML 1.0 node, they don't belong to any node field. (In VRML 1.0,
      there was no SFNode / MFNode fields.)

      In VRML 2.0, this is always empty.

      VRML 1.0 nodes may have any number of children.
      The children nodes refer back to it's parent nodes in VRML1Parents list.
      When travelling over VRML/X3D graph, remember that cycles
      are possible, because of DEF/USE.
      Obviously, they possible when travelling along the VRML1Parents list.
      We currently assume that there are no cycles when we treat the graph
      as directed, but it may change one day (some VRML/X3D models
      create real cycles).

      Adding/removing stuff from the VRML1Children list keeps track
      of how many times a node is used. If the child node has no parents,
      it will be freed.
      Actually, nodes can be children of both nodes (VRML 1.0 style,
      then VRML1Children and VRML1Parents is used) or fields (TMFNode or TSFNode,
      in VRML 2.0 style; then ParentFields is used). So the node is freed
      only when it's not referenced by any node and not referenced by any
      field. Generally, it's the parent that takes care of reference-counting
      and freeing the children, not the other way around.

      Note that given node instance may be a children of a single node
      multiple times, through DEF/USE mechanism. The order of children
      is important and preserved. (On the other hand, the order of
      VRML1Parents is not important, as that list is mostly for reference-counting.)

      You can replace one children with another like
      @code(VRML1Children[I] := NewChildren;).
      This works like a shortcut for
      @code(VRML1ChildRemove(I); VRML1ChildAdd(I, NewChildren);).
      But 1. it's more efficient; 2. it's safer --- if Children[I]
      is already equal to NewChildren, it does nothing.

      @groupBegin }
    property VRML1Children [i: integer]: TX3DNode read GetVRML1Child write SetVRML1Child;
    function VRML1ChildrenCount: integer;
    { @groupEnd }

    { Add a VRML 1.0 child node at given position.
      Index (position) must be in [0..VRML1ChildrenCount].
      Items at and above Index position are moved to the right,
      to insert new child at Index position. }
    procedure VRML1ChildAdd(Index: Integer; child: TX3DNode); overload;

    { Add a VRML 1.0 child node at the end of VRML1Children list. }
    procedure VRML1ChildAdd(child: TX3DNode); overload;

    procedure VRML1ChildRemove(i: integer);
    procedure VRML1ChildrenClear;

    { Remove a VRML 1.0 children at index I and return it (do not free it).
      Compare this with VRML1Remove, that removes children I and
      frees it if it's no longer used by anything.

      This removes children I, appropriately adjusting
      all parent / children links, but even if a node is unused after removing,
      it is not freed. It's always returned. }
    function VRML1ChildExtract(I: Integer): TX3DNode;

    { All VRML 1.0 parent nodes. VRML1Parents is a reverse of
      VRML1Children --- it lists all the nodes where we are on VRML1Children
      list.

      @groupBegin }
    property VRML1Parents [i: integer] :TX3DNode read GetVRML1Parent;
    function VRML1ParentsCount: integer;
    { @groupEnd }

    { All SFNode and MFNode fields where this node is referenced.
      This is somewhat analogous for VRML1Parents, but for VRML 2.0.

      ParentFieldsNode is just for your comfort, it returns always
      appropriate field's ParentNode property value
      (i.e. @code((ParentField[Index] as TSFNode).ParentNode)
      or @code((ParentField[Index] as TMFNode).ParentNode)).

      @groupBegin }
    property ParentFields[Index: Integer]: TX3DField read GetParentFieldsItem;
    property ParentFieldsNode[Index: Integer]: TX3DNode
      read GetParentFieldsNodeItem;
    function ParentFieldsCount: Integer;
    { @groupEnd }

    { Free this object (if it's not @nil) @italic(also removing
      it from @bold(all) parent nodes and fields).

      By design, normal destructor (Destroy called by Free)
      doesn't care about removing references to this object from
      it's parents. That's because it's the parents that usually
      initialize freeing of their children, and they free child
      when it's reference count is 0. So this freeing method
      is special in this regard.

      Use this if you really want to remove all node occurrences from the middle
      of VRML hierarchy. }
    procedure FreeRemovingFromAllParents;

    { Free this node if it is not referenced by any parent fields or nodes.
      Takes into account that node may have VRML 1.0 parent nodes
      and VRML 2.0 / X3D parent fields (SFNode or MFNode).
      This is a safe way of removing
      a node that may, but doesn't have to, be part of some VRML/X3D graph.
      The idea is that if a node is a part of some graph,
      we don't need to do anything (since you should have
      a reference to the entine graph somewhere anyway), otherwise node is
      considered unused and freed.

      Analogous to standard TObject.Free, this also works when called on
      a @nil value (does nothing in this case).

      For safety, it's advised to set reference to @nil after calling FreeIfUnused.
      You can use FreeIfUnusedAndNil utility for this (that employs a trick
      to @nil the visible reference even before freeing, which is even safer).
      This is analogous to standard FreeAndNil. }
    procedure FreeIfUnused;

    { Increase KeepExisting property value to disable automatic freeing
      of the node when unused.
      This may be useful in cases when you remove the node from VRML graph
      hierarchy, but you want to keep a valid reference to it anyway
      (for example, maybe you'll free it later by hand or add it somewhere
      else).

      Simple methods KeepExistingBegin and KeepExistingEnd simply increase /
      decrease KeepExisting value by 1. This is the most usual usage.

      @groupBegin }
    property KeepExisting: Cardinal read FKeepExisting write FKeepExisting;
    procedure KeepExistingBegin;
    procedure KeepExistingEnd;
    { @groupEnd }

    { Name of this node. When saving/loading, this comes from VRML/X3D
      "DEF" construct. Empty value means that the name is not defined.
      Do not change this during loading / saving of the X3D graph,
      or searching for nodes e.g. by @link(EnumerateNodes).

      Note that this property is deliberately not called @code(Name).
      In the future, this class may descend from the standard TComponent
      class, that defines a @code(Name) field with a special restrictions
      (it must be a valid Pascal identifier), which cannot apply to X3D node names
      (that can have quite free names, see
      http://www.web3d.org/documents/specifications/19776-2/V3.3/Part02/grammar.html ).
      We don't want to confuse these two properties. }
    property X3DName: string read FX3DName write FX3DName;
    property NodeName: string read FX3DName write FX3DName; deprecated 'use X3DName';

    { Base URL for all URLs inside our fields.
      Used everywhere as a base for relative URLs, to handle fields that refer
      to external files like textures, other 3D models (ImageTexture,
      Inline nodes, many others).

      It must be an absolute URL. Currently, it doesn't have to contain
      a protocol, and is then interpreted as a path on local filesystem
      (just like it had file:// prefix). Still, it must be an absolute path.

      This is set in constructor, and eventually adjusted by
      various parsing routines.
      This way we could, if only we would like to, resolve nodes
      like Inline or ImageTexture immediately after parsing them. }
    property BaseUrl: string read FBaseUrl write FBaseUrl;

    { Old deprecated name for BaseUrl. @deprecated }
    property WWWBasePath: string read FBaseUrl write FBaseUrl; deprecated;

    { Returns an absolute path, assuming that RelativePath is relative
      path from BaseUrl or that RelativePath is already absolute. }
    function PathFromBaseUrl(const RelativePath: string): string;

    { Constructor. Initializes various properties:

      @unorderedList(
        @item(Name, BaseUrl are initialized from given parameters.)
        @item(The @link(Fields), @link(Events) lists are filled
          in every descendant, to have all the fields/events defined
          by the specification.)
        @item(DefaultContainerField, and other node-specific
          stuff, is filled in descendants. This is actually implemented
          in CreateNode, that is called at the end of this constructor.)
      )
    }
    constructor Create(const AX3DName: string = '';
      const ABaseUrl: string = ''); virtual;

    destructor Destroy; override;

    { Type of the node in X3D, like @code('Group') or @code('Shape'). Never empty.
      Constant for the class lifetime (even for special
      TX3DUnknownNode and TX3DPrototypeNode, where this is calculated
      at runtime).

      Note that VRML/X3D is generally case-sensitive, so this property is too.

      In TX3DNode, this returns ClassX3DType, which is suitable
      for most nodes. See ClassX3DType. }
    function X3DType: string; virtual;
    function NodeTypeName: string; deprecated 'use X3DType';

    { Node type name in VRML/X3D, for this class.
      Normal VRML/X3D node classes should override this to return something
      non-empty, and then X3DType automatically will return the same value.

      Empty for classes that don't have a hardcoded VRML/X3D node name,
      like a special TX3DUnknownNode. Such special classes should override
      then X3DType to return actual non-empty name there.

      You usually should call X3DType. The only use of this method
      is that it works on classes (it's "class function"), without needing
      at actual instance. }
    class function ClassX3DType: string; virtual;

    { Enumerate all our children of some class. Recursively.
      Enumerates also this instance (Self), if it satisfies the
      NodeClass and SeekNodeName conditions.

      This enumerates both VRML 1.0 children (@link(VRML1Children)) as well as
      VRML >= 2.0 (including X3D) children (nodes in TSFNode and TMFNode fields).

      If OnlyActive then it will enumerate only active parts
      of the graph ("active" as defined by @link(Traverse)),
      so it will work as a simpler version of Traverse
      (simpler, because it doesn't track any state).

      If not OnlyActive then it will simply enumerate all nodes.
      This will include then also prototype helpers, if this node
      was expanded from prototype: see PrototypeInstanceSourceNode
      and PrototypeInstanceHelpers.

      If you give SeekNodeName parameter, we'll only look for nodes
      with X3DName = SeekNodeName. When SeekNodeName = '' it,
      consistently, looks for unnamed nodes (where X3DName = '').

      Enumerates children only after enumerating Self.
      So you can modify children before enumerating them in the Proc callback.
      You can only modify your children in the Proc callback.

      @groupBegin }
    procedure EnumerateNodes(
      proc: TX3DNodeProc; OnlyActive: boolean); overload;
    procedure EnumerateNodes(nodeClass: TX3DNodeClass;
      proc: TX3DNodeProc; OnlyActive: boolean); overload;
    procedure EnumerateNodes(nodeClass: TX3DNodeClass;
      const SeekNodeName: string;
      proc: TX3DNodeProc; OnlyActive: boolean); overload;
    { @groupEnd }

    { Search nodes, like enumerating nodes but allows early exit during
      the enumeration.
      We search for a node for which Proc(Node) returns something non-nil.

      For every node, the given Proc callback is run.
      If the Proc callback returns something non-nil,
      the enumeration of the following nodes stops,
      and we return the callback result. }
    function SearchNodes(const Proc: TX3DNodeSearchProc;
      const OnlyActive: boolean): Pointer;

    { TryFindNodeByName and TryFindNode seek for a node with
      given class (and node name, in case of TryFindNodeByName).
      If OnlyActive then they seek among only active nodes
      ("active" as defined by @link(Traverse)), otherwise all nodes.

      These functions are quite like EnumerateNodes, except
      they stop at the first occurrence and return it.

      TryFindNodeByName and TryFindNode return @nil if such node
      is not found. FindNodeByName and FindNode raise exception EX3DNotFound
      in this case.
      @raises(EX3DNotFound FindNodeByName and FindNode raise
        this exception when node is not found.)
      @groupBegin }
    function TryFindNodeByName(FindClass: TX3DNodeClass;
      const FindName: string;
      OnlyActive: boolean): TX3DNode;
    function FindNodeByName(FindClass: TX3DNodeClass;
      const FindName: string;
      OnlyActive: boolean): TX3DNode;
    function TryFindNode(FindClass: TX3DNodeClass;
      OnlyActive: boolean): TX3DNode;
    function FindNode(FindClass: TX3DNodeClass;
      OnlyActive: boolean): TX3DNode;
    { @groupEnd }

    { @groupEnd }

    { Seeks Self and parent nodes (from VRML1Parents and ParentFields,
      recursively) for given node name.

      In other words, this is similar to TryNodeByName or NodeByName,
      but it goes "upward" in graph hierarchy. Note that this
      never restricts itself only to "active" graph part
      ("active" as defined by @link(Traverse))
      because you really can't detect what is the "active"
      part of the graph when going upward.

      @raises(EX3DNotFound
        FindParentByName raises this exception when node is not found.)

      @groupBegin }
    function TryFindParentByName(const FindName: string): TX3DNode;
    function FindParentByName(const FindName: string): TX3DNode;
    { @groupEnd }

    { Looks for a given Node in parents (and self), recursively.
      Similar to TryFindParentByName. Returns @true only if found. }
    function HasParentNode(Node: TX3DNode): boolean;

    { Search immediate parents of this node for a node with given FindName.
      Returns @nil if not found. }
    function TryFindDirectParentByName(const FindName: string): TX3DNode;

    { Looks for a given node in our children (direct and not, including self).
      If OnlyActive, then only active parts are searched
      ("active" as defined by @link(Traverse)). }
    function IsNodePresent(Node: TX3DNode; OnlyActive: boolean): boolean;

    { Count the occurrences of given node class in our children.
      For example, you can pass NodeClass = TAbstractLightNode to count
      the light sources in the scene.

      If CountOnlyActiveNodes, then only active parts are searched
      ("active" as defined by @link(Traverse)).

      This traverses both VRML 1.0 children nodes and VRML 2.0 nodes
      inside SFNode and MFNode fields. }
    function NodesCount(NodeClass: TX3DNodeClass;
      CountOnlyActiveNodes: boolean): integer;

    { Save node to a stream. Saves everything, including node name,
      node type, and node contents. }
    procedure SaveToStream(Writer: TX3DWriter); override;

    { Some nodes are present only in specific VRML/X3D version.
      This functions decides it.

      For example some nodes can only work in VRML < 2.0,
      some others only in VRML >= 2.0. There are even some pairs
      of nodes: for example TConeNode_1 works with VRML < 2.0,
      TConeNode works with VRML >= 2.0.

      NodesManager will use this.

      Default implementation of this function returns always @true.
      Generally, I don't try to set this too aggresively ---
      in other words, for all cases when it's sensible, I allow
      nodes to be used in every VRML/X3D version, even when official
      specification doesn't. This means that when reading VRML 1.0
      files actually a large part of VRML 2.0 is allowed too,
      and also while reading VRML 2.0 many constructs from VRML 1.0
      (officially no longer present in VRML 2.0) are allowed too.
      I'm trying to support what I call a "sum of VRML 1.0 and 2.0".

      In practice I only use this function when various VRML/X3D versions
      specify the same node name but

      @unorderedList(
        @item(With different fields.

          For example Cone and Cylinder have slightly different fields,
          due to the fact that VRML 2.0 resigned from using TSFBitMask fields.)

        @item(With different behavior.

          For example definitions of Sphere for VRML 1.0
          and 2.0 are practically equal. However, the behavior from where
          to take texture and material info is different --- in VRML 1.0
          we take last Texture2, Material etc. nodes, while in VRML 2.0
          we look in parent Shape's "appearance" field. So once again
          two different Sphere classes are needed.)
      ) }
    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      virtual;

    { Enumerates all children nodes (recursively),
      allowing you to decide for each node to replace or remove it.

      So this is something like EnumerateNodes,
      except that it allows you to remove the nodes. It always
      enumerates all nodes, not only active (e.g. it enumerates all
      Switch node children, not only the chosen one).

      Note that (unlike regular EnumerateNodes) @bold(this doesn't
      report Self to Func !). Which is natural, since this may remove
      nodes by normal VRML1ChildRemove calls, so it needs to know ParentNode
      of the removed node.

      For each node Func will be called, with ParentNode and Node set.
      If you change the Node to something else, then the old node will
      be removed and new Node inserted in the same place.
      If new Node is @nil, then only the old node will be removed.

      Nodes are traversed in depth-first search. Node is first reported
      to Func, and then (if it's not replaced) we descend into this Node.

      @returns The number of removed (and possibly replaced) nodes. }
    function EnumerateReplaceChildren(
      Func: TEnumerateReplaceNodesFunction): Cardinal;

    { Removes all children (and their children, recursively) with
      node names matchig Wildcard. You can use * and ? special chars
      in the Wildcard.
      @returns The number of removed nodes. }
    function RemoveChildrenWithMatchingName(
      const Wildcard: string; IgnoreCase: Boolean): Cardinal;

    { Create a deep copy of this node and all it's children.

      New copy is completely independent from original,
      having all children nodes (in both VRML 1.0 sense (Children)
      and VRML >= 2.0 (inside SFNode and MFNode fields)) also
      duplicated. New copy has protypes, routes, interface declarations
      and generally everything established like in the original, using copied
      nodes.

      Doesn't copy things which are dependent on container hierarchy.
      (So copying them would be more dangerous than useful.)
      This means: DestructionNotifications, Scene, VRML1Parents,
      ParentFields. VRML1Parents and ParentFields will be set for children
      anyway (to appropriate copies).

      Caller owns this newly created copy --- as returned by this method,
      it's not linked anywhere. }
    function DeepCopy: TX3DNode;

    { Internal notes:

      TODO: memory leaks are known to be possible in some difficult cases
      with PrototypeInstanceHelpers. See e.g.
      ../../../demo-models/prototypes/warnings/errors/proto_leak.wrl and
      ../../../demo-models/prototypes/warnings/errors/proto_leak_2.wrl
      for simple testcases. Reason: PrototypeInstanceHelpers may contain,
      by DEF statements, links to Self.
      This causes circular dependency (Self is child of some node on
      PrototypeInstanceHelpers, but PrototypeInstanceHelpers will
      be freed only if Self is freed) causing some memory to be left
      always allocated.

      PrototypeInstanceHelpers are actually always TX3DRootNode,
      may be declared as such in the future if needed.
    }

    { PrototypeInstance = @true indicates that this node was created
      from a non-external prototype instantiation.

      Then PrototypeInstanceSourceNode is non-nil and indicates
      parsed prototype node (and PrototypeInstanceSourceNode.Prototype
      gives you even a link to the actual prototype specification).

      PrototypeInstanceSourceNode is used for events: any ROUTEs
      specified @italic(outside) of prototype and
      leading to/from instantiated prototype should actually lead
      to PrototypeInstanceSourceNode events (not to events of Self).
      Reason: prototype events may be different than actual expanded
      node events, and ROUTEs want to lead to prototype events.
      This is implemented when expanding prototype
      (@link(TX3DPrototypeNode.Instantiate))
      and when linking ROUTE (TX3DRoute.SetSource, TX3DRoute.SetDestination).

      PrototypeInstanceHelpers may be @nil if empty, or may contain
      a list of other nodes duplicated along with the main prototype node.
      From VRML spec:

      @italic(Any other nodes and accompanying scene graphs
      are not part of the transformation hierarchy, but may be referenced
      by ROUTE statements or Script nodes in the prototype definition.)

      Note that for TX3DPrototypeNode (within PrototypeInstanceSourceNode)
      these have a little different meaning: they describe the
      @italic(nested prototype), if any, that was used to create this node.
      This may happen if the node was expanded from one prototype within
      another. (Usually, you shouldn't be concerned about this;
      see TX3DPrototypeNode.Instantiate implementation comments for
      gory details about this.)

      @groupBegin }
    property PrototypeInstance: boolean read FPrototypeInstance;
    property PrototypeInstanceHelpers: TX3DNode read FPrototypeInstanceHelpers;
    { @groupEnd }

    { Does this node matches the given URN. This is used when searching for an built-in
      implementation of a node declared in X3D using the EXTERNPROTO.

      Implementors note: in this class, this returns @false.
      You can use constants like @link(URNVRML97Nodes) and functions like
      @link(URNMatchingCastle) to implement this easily. }
    class function URNMatching(const URN: string): boolean; virtual;

    { Default value of "containerField" attribute for this node in X3D XML
      encoding. }
    property DefaultContainerField: string
      read FDefaultContainerField write FDefaultContainerField;

    { For some special VRML / X3D nodes (like Script, ComposedShader)
      that allow the definition of additional fields/events within.

      In X3D specification this is marked like

      @preformatted(
        # And any number of:
        fieldType [in]     fieldName
        fieldType [in,out] fieldName    initialValue
        fieldType [out]    fieldName
        fieldType []       fieldName    initialValue
      )

      If HasInterfaceDeclarations is not [], then InterfaceDeclarations
      will be non-nil and parser (classic VRML parser in this unit,
      X3D XML reader too) will read this from VRML files.
      Moreover, for each interface declaration, also appropriate field/event
      will be added to the list of @link(Fields) or @link(Events),
      so fields/events created by interface declarations will function
      just like other standard fields everywhere.

      @groupBegin }
    property HasInterfaceDeclarations: TX3DAccessTypes
      read FHasInterfaceDeclarations
      write SetHasInterfaceDeclarations default [];

    property InterfaceDeclarations: TX3DInterfaceDeclarationList
      read FInterfaceDeclarations;
    { @groupEnd }

    { Scene that will be notified about changes to this node.
      This is necessary to allow the scene to properly process VRML/X3D
      events, and also to react properly to any other changes to the nodes
      (like when you change some field directly by ObjectPascal code,
      e.g. by TX3DField.Send).

      May be @nil if none.

      A given TX3DNode may be renderable only by a single renderer.
      This means it can be placed only within one TCastleScene. }
    ///property Scene: TX3DEventsEngine read FScene write FScene;

    { Internally used by @link(Scene) to store some information at node.
      @exclude }
    property InternalSceneShape: TObject
      read FInternalSceneShape write FInternalSceneShape;

    { This will be always called by VRML parsers after adding new item
      to our InterfaceDeclarations.

      In this class, this simply adds
      IDecl.FieldOrEvent to our normal fields/events by IDecl.AddFieldOrEvent.
      You may override this in subclasses to react in some special way
      to new fields/events, for example Script node may register here
      to receive notification when input event is received. }
    procedure PostAddInterfaceDeclaration(IDecl: TX3DInterfaceDeclaration); virtual;

    { Add to node InterfaceDeclarations given field.
      Sets the field's exposed to given Exposed parameter, for comfort.
      This should only be used with nodes having
      HasInterfaceDeclarations = @true, like Script or ComposedShader. }
    procedure AddCustomField(AField: TX3DField);

    { Add to node InterfaceDeclarations given field or event.
      This should only be used with nodes having
      HasInterfaceDeclarations = @true, like Script or ComposedShader. }
    procedure AddCustomFieldOrEvent(AFieldOrEvent: TX3DFieldOrEvent);

    { What happens when transformation of the node changes.
      This is internally used by TCastleSceneCore when Transform (or similar)
      node affecting this node changes. It's an important optimization,
      to avoid large if clause with many "is" and "Supports" checks
      that would be quite slow.
      @exclude }
    function TransformationChange: TNodeTransformationChange; virtual;

    { Nice and concise node description for user.
      Shows node type and name. }
    function NiceName: string;

    { Detach this node from parent scene, recursively.
      Useful if you take a node and want to put it in another TCastleSceneCore.

      @italic(You almost never need to call this method from your application).
      This is done automatically for you when TCastleSceneCore is destroyed.
      And a node can only be part of one TCastleSceneCore instance
      at one time.

      Notable exceptions when you need to call this:

      @orderedList(
        @item(
          If you process RootNode graph and extract some node from it
          (that is, delete node from one RootNode graph,
          but instead of freeing it you insert it into some other VRML/X3D graph)
          you must call it to manually
          detach this node (and all it's children) from a previous TCastleSceneCore
          instance.
        )

        @item(
          Right now the TCastleSceneCore doesn't call RootNode.UnregisterScene
          when TCastleSceneCore.OwnsRootNode.
          This is done for speed, because it's not needed in 99% cases.

          However, if your scene contains some node that isn't freed
          because of @link(KeepExistingBegin),
          then such node will remain attached to non-existing scene.
          You should call UnregisterScene on such node manually, for now.
        )
      )
    }
    procedure UnregisterScene;
  end;

  TX3DNodeList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TX3DNode>)
    { Find node by name, @nil if not found.

      For empty node name, always returns @nil. This follows the definition
      of @link(TX3DNode.X3DName):
      empty means that node has no name, so it should not be found
      by searches by name, ever.

      @seealso(IndexOfName Returns the node index. This is sometimes more
        flexible (you can always quickly get actual node knowing the index,
        by Items[Index], but not the other way around). It is also sometimes
        less comfortable (often you're not interested in node index).) }
    function FindName(const Name: string): TX3DNode;

    { Find index of a node with given name, -1 if not found. }
    function IndexOfName(const Name: string): Integer;

    procedure AddIfNotExists(const Node: TX3DNode);
    function Equals(SecondValue: TObject): boolean; {$ifdef TOBJECT_HAS_EQUALS} override; {$endif}
    procedure Assign(const Source: TX3DNodeList);
    procedure Assign(const Source: array of TX3DNode);
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TX3DNodeDeepCopyState ----------------------------------------------------- }

constructor TX3DNodeDeepCopyState.Create;
begin
  inherited;
  Original := TX3DNodeList.Create(false);
  New := TX3DNodeList.Create(false);
end;

destructor TX3DNodeDeepCopyState.Destroy;
begin
  FreeAndNil(Original);
  FreeAndNil(New);
  inherited;
end;

function TX3DNodeDeepCopyState.DeepCopy(OriginalNode: TX3DNode): TX3DNode;
var
  I: Integer;
begin
  Assert(New.Count = Original.Count);

  I := Original.IndexOf(OriginalNode);
  if I = -1 then
  begin
    { DeepCopyCore will expand (Original, New) lists by (OriginalNode, Result).
      This is needed, see DeepCopyCore comments. }
    Result := OriginalNode.DeepCopyCore(Self);
  end else
    Result := New[I];
end;

{ TX3DNode ------------------------------------------------------------------ }

constructor TX3DNode.Create(const AX3DName, ABaseUrl: string);
begin
  inherited Create;
  VRML1ChildrenAllowed := false;
  VRML1ChildrenParsingAllowed := false;

  FX3DName := AX3DName;
  FBaseUrl := ABaseUrl;

  FVRML1Children := TX3DNodeList.Create(false);
  FVRML1Parents := TX3DNodeList.Create(false);
  FParentFields := TX3DFieldList.Create(false);
  FFields := TX3DFieldList.Create(false);
  FEvents := TX3DEventList.Create(false);

  FHasInterfaceDeclarations := [];
  FInterfaceDeclarations := nil;

  CreateNode;
end;

destructor TX3DNode.Destroy;
var
  I: Integer;
begin
  if FVRML1Children <> nil then VRML1ChildrenClear;

  if PrototypeInstance then
  begin
    FreeAndNil(FPrototypeInstanceHelpers);
    FPrototypeInstance := false;
  end;

  FreeAndNil(FImports);
  FreeAndNil(FExports);
  FreeAndNil(FFieldsSFNode);
  FreeAndNil(FFieldsMFNode);

  { First free Fields and Events, before freeing InterfaceDeclarations.
    Reason: Fields and Events may contains references to InterfaceDeclarations
    items (since parsing added them there by PostAddInterfaceDeclaration(IDecl)).
    So these references have to be valid, and omitted by checking
    ParentInterfaceDeclaration <> nil. }

  if FEvents <> nil then
  begin
    for I := 0 to FEvents.Count - 1 do
      if FEvents[I].ParentInterfaceDeclaration = nil then
      begin
        FEvents[I].Free;
        FEvents[I] := nil;
      end;
    FreeAndNil(FEvents);
  end;

  if FFields <> nil then
  begin
    for I := 0 to FFields.Count - 1 do
      if FFields[I].ParentInterfaceDeclaration = nil then
      begin
        FFields[I].Free;
        FFields[I] := nil;
      end;
    FreeAndNil(FFields);
  end;

  FreeAndNil(FInterfaceDeclarations);

  FreeAndNil(FVRML1Children);
  FreeAndNil(FVRML1Parents);
  FreeAndNil(FParentFields);

  inherited;
end;

procedure TX3DNode.FreeIfUnused;
begin
  if (Self <> nil) and
     (FVRML1Parents.Count = 0) and
     (FParentFields.Count = 0) and
     (KeepExisting = 0) then
  begin
    { For FPC <= 2.2.2:
      This is written as "Self.Destroy" to actually do the desctruction,
      freeing memory etc. If I would just call it "Destroy", it would
      perform what destructor does but leaving object instance unfreed.

      In FPC 2.2.4, 2.4.0, 2.4.2, 2.4.4, 2.6.0, calling Destroy always
      does the actual destruction, there seems to be no difference
      between "Self.Destroy" and "Destroy" below. }
    Self.Destroy;
  end;
end;

procedure TX3DNode.KeepExistingBegin;
begin
  Inc(FKeepExisting);
end;

procedure TX3DNode.KeepExistingEnd;
begin
  Dec(FKeepExisting);
end;

procedure TX3DNode.VRML1ChildAdd(Index: Integer; child: TX3DNode);
begin
 child.FVRML1Parents.Add(Self);
 FVRML1Children.Insert(Index, child);
end;

procedure TX3DNode.VRML1ChildAdd(child: TX3DNode);
begin
  VRML1ChildAdd(FVRML1Children.Count, child);
end;

procedure TX3DNode.VRML1ChildRemove(i: integer);
var
  OldChild: TX3DNode;
begin
  OldChild := FVRML1Children[i];
  FVRML1Children.Delete(i);
  OldChild.FVRML1Parents.Remove(Self);
  OldChild.FreeIfUnused;
end;

function TX3DNode.VRML1ChildExtract(I: Integer): TX3DNode;
begin
  Result := FVRML1Children[i];
  FVRML1Children.Delete(i);
  Result.FVRML1Parents.Remove(Self);

  { VRML1ChildRemove now does
      OldChild.FreeIfUnused;
    but ExtractChild doesn't do it. }
end;

procedure TX3DNode.SetVRML1Child(I: Integer; Value: TX3DNode);
var
  OldChild: TX3DNode;
begin
  { Inefficient implementation: VRML1ChildRemove(I); VRML1ChildAdd(I, Value); }

  if Value <> FVRML1Children[I] then
  begin
    OldChild := FVRML1Children[i];
    FVRML1Children[I] := Value;

    OldChild.FVRML1Parents.Remove(Self);
    OldChild.FreeIfUnused;

    Value.FVRML1Parents.Add(Self);
  end;
end;

procedure TX3DNode.VRML1ChildrenClear;
begin
 while FVRML1Children.Count > 0 do VRML1ChildRemove(0);
end;

function TX3DNode.GetVRML1Child(i: integer): TX3DNode;
begin
  result := FVRML1Children[i];
end;

function TX3DNode.GetVRML1Parent(i: integer): TX3DNode;
begin
  result := FVRML1Parents[i];
end;

function TX3DNode.VRML1ChildrenCount: integer;
begin
  result := FVRML1Children.Count
end;

function TX3DNode.VRML1ParentsCount: integer;
begin
  result := FVRML1Parents.Count
end;

procedure TX3DNode.FreeRemovingFromAllParents;
var
  i, j: integer;
  SF: TSFNode;
begin
  if Self = nil then exit;

  for i := 0 to FVRML1Parents.Count - 1 do
  begin
    j := FVRML1Parents[i].FVRML1Children.IndexOf(Self);
    FVRML1Parents[i].FVRML1Children.Delete(j);
    { nie musimy sie tu martwic usuwaniem naszego Parenta z listy
      FVRML1Parents ktora
     wlasnie przegladamy bo przeciez i tak zaraz zrobimy sobie Destroy; }
  end;

  for I := 0 to FParentFields.Count - 1 do
  begin
    if FParentFields[I] is TSFNode then
    begin
      SF := TSFNode(FParentFields[I]);
      { We remove accessing private SF.FValue,
        not SF.Value property setter,
        to avoid checking our reference count (and possibly calling
        our destructor) by this setter. }
      SF.FValue := nil;
    end else
  end;

  Self.Destroy;
end;

function TX3DNode.GetParentFieldsItem(Index: Integer): TX3DField;
begin
  Result := FParentFields[Index];
end;

function TX3DNode.GetParentFieldsNodeItem(Index: Integer): TX3DNode;
var
  F: TX3DField;
begin
  F := ParentFields[Index];
  if F is TSFNode then
    Result := TSFNode(F).ParentNode
  else
    Result := nil;

end;

function TX3DNode.ParentFieldsCount: Integer;
begin
  Result := FParentFields.Count;
end;

function TX3DNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to VRML1ChildrenCount - 1 do
  begin
    Result := Func(Self, VRML1Children[I]);
    if Result <> nil then Exit;
  end;
end;

function TX3DNode.DirectEnumerateAll(
  Func: TEnumerateChildrenFunction): Pointer;
var
  I, J: Integer;
  SF: TSFNode;
begin
  Result := nil;

  for I := 0 to VRML1ChildrenCount - 1 do
  begin
    Result := Func(Self, VRML1Children[I]);
    if Result <> nil then Exit;
  end;

  if FFieldsSFNode <> nil then
    for I := 0 to FFieldsSFNode.Count - 1 do
    begin
      SF := TSFNode(FFieldsSFNode[I]);
      if (SF.Value <> nil) and not SF.WeakLink then
      begin
        Result := Func(Self, SF.Value);
        if Result <> nil then Exit;
      end;
    end;

  { Use InternalItems for an extra epsilon of speed. }

  if PrototypeInstance then
  begin
    if PrototypeInstanceHelpers <> nil then
    begin
      Result := Func(Self, PrototypeInstanceHelpers);
      if Result <> nil then Exit;
    end;
  end;
end;

function TX3DNode.DirectEnumerate(
  Func: TEnumerateChildrenFunction;
  OnlyActive: boolean): Pointer;
begin
  if OnlyActive then
    Result := DirectEnumerateActive(Func) else
    Result := DirectEnumerateAll(Func);
end;

function TX3DNode.X3DType: string;
begin
 result := ClassX3DType;
end;

function TX3DNode.NodeTypeName: string;
begin
 result := X3DType;
end;

class function TX3DNode.ClassX3DType: string;
begin
 result := '';
end;

function BlenderRelativePath(const RelativePath: string): boolean;
begin
  Result := false;
end;

function TX3DNode.PathFromBaseUrl(const RelativePath: string): string;
begin
  { This is a workaround for Blender errorneous VRML 1.0 export.
    Blender exports relative paths by prefixing them by "//"
    (that's a general convention used internally by Blender, AFAIK).
    Here we simply remove this "//". }
end;


procedure TX3DNode.CreateNode;
begin
end;

type
  TEnumerateNodes0Enumerator = class
    Proc: TX3DNodeProc;
    OnlyActive: boolean;
    function EnumerateChildrenFunction(Node, Child: TX3DNode): Pointer;
  end;

  function TEnumerateNodes0Enumerator.EnumerateChildrenFunction(
    Node, Child: TX3DNode): Pointer;
  begin
    Result := nil;
    Proc(Child);
    Child.DirectEnumerate({$ifdef CASTLE_OBJFPC}@{$endif} Self.EnumerateChildrenFunction, OnlyActive);
  end;

procedure TX3DNode.EnumerateNodes(
  Proc: TX3DNodeProc; OnlyActive: boolean);
var
  Enumerator: TEnumerateNodes0Enumerator;
begin
  Enumerator := TEnumerateNodes0Enumerator.Create;
  try
    Enumerator.Proc := Proc;
    Enumerator.OnlyActive := OnlyActive;
    Enumerator.EnumerateChildrenFunction(nil, Self);
  finally FreeAndNil(Enumerator) end;
end;

type
  TEnumerateNodes1Enumerator = class
    NodeClass: TX3DNodeClass;
    Proc: TX3DNodeProc;
    OnlyActive: boolean;
    function EnumerateChildrenFunction(Node, Child: TX3DNode): Pointer;
  end;

  function TEnumerateNodes1Enumerator.EnumerateChildrenFunction(
    Node, Child: TX3DNode): Pointer;
  begin
    Result := nil;
    if Child is NodeClass then Proc(Child);
    Child.DirectEnumerate({$ifdef CASTLE_OBJFPC}@{$endif} Self.EnumerateChildrenFunction, OnlyActive);
  end;

procedure TX3DNode.EnumerateNodes(nodeClass: TX3DNodeClass;
  Proc: TX3DNodeProc; OnlyActive: boolean);
var
  Enumerator: TEnumerateNodes1Enumerator;
begin
  Enumerator := TEnumerateNodes1Enumerator.Create;
  try
    Enumerator.NodeClass := NodeClass;
    Enumerator.Proc := Proc;
    Enumerator.OnlyActive := OnlyActive;
    Enumerator.EnumerateChildrenFunction(nil, Self);
  finally FreeAndNil(Enumerator) end;
end;

type
  TEnumerateNodes2Enumerator = class
    NodeClass: TX3DNodeClass;
    SeekNodeName: string;
    Proc: TX3DNodeProc;
    OnlyActive: boolean;
    function EnumerateChildrenFunction(Node, Child: TX3DNode): Pointer;
  end;

  function TEnumerateNodes2Enumerator.EnumerateChildrenFunction(
    Node, Child: TX3DNode): Pointer;
  begin
    Result := nil;
    if (Child is NodeClass) and
       (Child.X3DName = SeekNodeName) then
      Proc(Child);
    Child.DirectEnumerate({$ifdef CASTLE_OBJFPC}@{$endif} Self.EnumerateChildrenFunction, OnlyActive);
  end;

procedure TX3DNode.EnumerateNodes(NodeClass: TX3DNodeClass;
  const SeekNodeName: string;
  Proc: TX3DNodeProc; OnlyActive: boolean);
var
  Enumerator: TEnumerateNodes2Enumerator;
begin
  Enumerator := TEnumerateNodes2Enumerator.Create;
  try
    Enumerator.NodeClass := NodeClass;
    Enumerator.SeekNodeName := SeekNodeName;
    Enumerator.Proc := Proc;
    Enumerator.OnlyActive := OnlyActive;
    Enumerator.EnumerateChildrenFunction(nil, Self);
  finally FreeAndNil(Enumerator) end;
end;

type
  TSearchNodes0Helper = class
    Proc: TX3DNodeSearchProc;
    OnlyActive: boolean;
    function Callback(Node, Child: TX3DNode): Pointer;
  end;

  function TSearchNodes0Helper.Callback(
    Node, Child: TX3DNode): Pointer;
  begin
    Result := Proc(Child);
    if Result <> nil then Exit;

    Result := Child.DirectEnumerate({$ifdef CASTLE_OBJFPC}@{$endif} Self.Callback, OnlyActive);
  end;

function TX3DNode.SearchNodes(
  const Proc: TX3DNodeSearchProc; const OnlyActive: boolean): Pointer;
var
  Helper: TSearchNodes0Helper;
begin
  Helper := TSearchNodes0Helper.Create;
  try
    Helper.Proc := Proc;
    Helper.OnlyActive := OnlyActive;
    Result := Helper.Callback(nil, Self);
  finally FreeAndNil(Helper) end;
end;

type
  TFindNodeHelper = class
    NodeClass: TX3DNodeClass;
    function Callback(Node: TX3DNode): Pointer;
  end;

  function TFindNodeHelper.Callback(Node: TX3DNode): Pointer;
  begin
    if Node is NodeClass then
      Result := Node else
      Result := nil;
  end;

function TX3DNode.TryFindNode(FindClass: TX3DNodeClass; OnlyActive: boolean): TX3DNode;
var
  Helper: TFindNodeHelper;
begin
  Helper := TFindNodeHelper.Create;
  try
    Helper.NodeClass := FindClass;
    Result := TX3DNode(SearchNodes({$ifdef CASTLE_OBJFPC}@{$endif} Helper.Callback, OnlyActive));
  finally FreeAndNil(Helper) end;
end;

function TX3DNode.FindNode(FindClass: TX3DNodeClass; OnlyActive: boolean): TX3DNode;
begin
  result := TryFindNode(FindClass, OnlyActive);
  if Result = nil then
    raise EX3DNotFound.CreateFmt('Node class "%s" not found', [FindClass.ClassName]);
end;

type
  TFindNodeByNameHelper = class
    NodeClass: TX3DNodeClass;
    NodeName: string;
    function Callback(Node: TX3DNode): Pointer;
  end;

  function TFindNodeByNameHelper.Callback(Node: TX3DNode): Pointer;
  begin
    if (Node is NodeClass) and (Node.X3DName = NodeName) then
      Result := Node else
      Result := nil;
  end;

function TX3DNode.TryFindNodeByName(
  FindClass: TX3DNodeClass; const FindName: string; OnlyActive: boolean): TX3DNode;
var
  Helper: TFindNodeByNameHelper;
begin
  Helper := TFindNodeByNameHelper.Create;
  try
    Helper.NodeClass := FindClass;
    Helper.NodeName := FindName;
    Result := TX3DNode(SearchNodes({$ifdef CASTLE_OBJFPC}@{$endif} Helper.Callback, OnlyActive));
  finally FreeAndNil(Helper) end;
end;

function TX3DNode.FindNodeByName(
  FindClass: TX3DNodeClass; const FindName: string;
  OnlyActive: boolean): TX3DNode;
begin
  result := TryFindNodeByName(FindClass, FindName, OnlyActive);
  if Result = nil then
    raise EX3DNotFound.CreateFmt('Node name "%s" not found', [FindName]);
end;

{ TX3DNode.TryFindNodeState/Transform ----------------------------------------- }


function TX3DNode.TryFindParentByName(const FindName: string): TX3DNode;
var
  I: integer;
begin
  if X3DName = FindName then
    result := Self else
  begin
    result := nil;

    for I := 0 to VRML1ParentsCount - 1 do
    begin
      result := VRML1Parents[I].TryFindParentByName(FindName);
      if result <> nil then exit;
    end;

    for I := 0 to ParentFieldsCount - 1 do
    begin
      result := ParentFieldsNode[I].TryFindParentByName(FindName);
      if result <> nil then exit;
    end;
  end;
end;

function TX3DNode.FindParentByName(const FindName: string): TX3DNode;
begin
  result := TryFindParentByName(FindName);
  if Result = nil then
    raise EX3DNotFound.CreateFmt('Node name "%s" not found in parents', [FindName]);
end;

function TX3DNode.TryFindDirectParentByName(const FindName: string): TX3DNode;
var
  I: Integer;
begin
  for I := 0 to VRML1ParentsCount - 1 do
  begin
    Result := VRML1Parents[I];
    if Result.X3DName = FindName then Exit;
  end;

  for I := 0 to ParentFieldsCount - 1 do
  begin
    Result := ParentFieldsNode[I];
    if Result.X3DName = FindName then Exit;
  end;

  Result := nil;
end;

function TX3DNode.HasParentNode(Node: TX3DNode): boolean;
var
  I: integer;
begin
  if Self = Node then
    result := true else
  begin
    for i := 0 to VRML1ParentsCount - 1 do
      if VRML1Parents[i].HasParentNode(Node) then Exit(true);

    for i := 0 to ParentFieldsCount - 1 do
      if ParentFieldsNode[i].HasParentNode(Node) then Exit(true);

    result := False;
  end;
end;

type
  TIsNodePresentHelper = class
    SeekNode: TX3DNode;
    function Callback(Node: TX3DNode): Pointer;
  end;

  function TIsNodePresentHelper.Callback(Node: TX3DNode): Pointer;
  begin
    if Node = SeekNode then
      Result := Node { doesn't matter what, anything non-nil } else
      Result := nil;
  end;

function TX3DNode.IsNodePresent(Node: TX3DNode; OnlyActive: boolean): boolean;
var
  Helper: TIsNodePresentHelper;
begin
  Helper := TIsNodePresentHelper.Create;
  try
    Helper.SeekNode := Node;
    Result := SearchNodes({$ifdef CASTLE_OBJFPC}@{$endif} Helper.Callback, OnlyActive) <> nil;
  finally FreeAndNil(Helper) end;
end;

type
  TNodesCountHelper = class
    Counter: integer;
    procedure CountNode(node: TX3DNode);
  end;

  procedure TNodesCountHelper.CountNode(node: TX3DNode);
  begin Inc(Counter) end;

function TX3DNode.NodesCount(NodeClass: TX3DNodeClass;
  CountOnlyActiveNodes: boolean): integer;
var
  C: TNodesCountHelper;
begin
  C := TNodesCountHelper.Create;
  try
    EnumerateNodes(NodeClass, {$ifdef CASTLE_OBJFPC}@{$endif} C.CountNode, CountOnlyActiveNodes);
    result := C.Counter;
  finally C.Free end;
end;

procedure TX3DNode.SaveToStream(Writer: TX3DWriter);
begin
end;

class function TX3DNode.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := true;
end;

procedure TX3DNode.RemoveParentField(Field: TX3DField);
begin
  FreeIfUnused;
end;

procedure TX3DNode.AddParentField(Field: TX3DField);
begin
  FParentFields.Add(Field);
end;

class function TX3DNode.VRML1ChildrenSaveToStream: boolean;
begin
  Result := true;
end;

function TX3DNode.EnumerateReplaceChildren(
  Func: TEnumerateReplaceNodesFunction): Cardinal;
var
  I, J: Integer;
  SF: TSFNode;
  NewNode: TX3DNode;
begin
  { I don't use EnumerateNodes since I have to enumerate them myself,
    since they may be removed during enumeration.
    The code below mimics TX3DNode.DirectEnumerateAll implementation,
    but it takes into account that nodes may be removed. }

  Result := 0;

  I := 0;
  while I < VRML1ChildrenCount do
  begin
    NewNode := VRML1Children[I];
    Func(Self, NewNode);
    if NewNode <> VRML1Children[I] then
    begin
      VRML1ChildRemove(I);
      Inc(Result);
      if NewNode <> nil then
      begin
        VRML1ChildAdd(I, NewNode);
        Inc(I);
      end;
    end else
    begin
      Result := Result + VRML1Children[I].EnumerateReplaceChildren(Func);
      Inc(I);
    end;
  end;

  if FFieldsSFNode <> nil then
    for I := 0 to FFieldsSFNode.Count - 1 do
    begin
      SF := TSFNode(FFieldsSFNode[I]);
      if (SF.Value <> nil) and not SF.WeakLink then
      begin
        NewNode := SF.Value;
        Func(Self, NewNode);
        if NewNode <> SF.Value then
        begin
          SF.Value := NewNode;
          Inc(Result);
        end else
        begin
          Result := Result + SF.Value.EnumerateReplaceChildren(Func);
        end;
      end;
    end;
end;

  type
    TRemoveChildrenWithMatchingNameHelper = class
      Wildcard: string;
      IgnoreCase: boolean;
      procedure DoIt(ParentNode: TX3DNode; var Node: TX3DNode);
    end;

  procedure TRemoveChildrenWithMatchingNameHelper.DoIt(
    ParentNode: TX3DNode; var Node: TX3DNode);
  begin
    if IsWild(Node.X3DName, Wildcard, IgnoreCase) then
      Node := nil;
  end;

function TX3DNode.RemoveChildrenWithMatchingName(
  const Wildcard: string; IgnoreCase: Boolean): Cardinal;
var
  Helper: TRemoveChildrenWithMatchingNameHelper;
begin
  Helper := TRemoveChildrenWithMatchingNameHelper.Create;
  try
    Helper.Wildcard := Wildcard;
    Helper.IgnoreCase := IgnoreCase;
    Result := EnumerateReplaceChildren({$ifdef CASTLE_OBJFPC}@{$endif} Helper.DoIt);
  finally FreeAndNil(Helper) end;
end;

class function TX3DNode.URNMatching(const URN: string): boolean;
begin
  Result := false;
end;

function TX3DNode.Field(const AName: string): TX3DField;
var
  I: Integer;
begin
  I := FFields.IndexOfName(AName);
  if I <> -1 then
    Result := FFields[I] else
    Result := nil; { not found }
end;

function TX3DNode.FieldOrEvent(const AName: string): TX3DFieldOrEvent;
var
  I: Integer;
  ResultEvent: TX3DEvent;
begin
  I := FFields.IndexOfName(AName);
  if I <> -1 then
    Exit(FFields[I]);

  { I use helper ResultEvent below, instead of passing
    "TX3DEvent(Result)" as last param: don't know why,
    but with FPC 2.2.0 this cast may fail (even though it shouldn't
    be checked at all?), testcase:
      view3dscene www.web3d.org/x3d/content/examples/Basic/CAD/CADGeometryPrototypes.x3d
  }

  I := FFields.IndexOfExposedEvent(AName, ResultEvent);
  if I <> -1 then
    Exit(ResultEvent);

  I := FEvents.IndexOfName(AName);
  if I <> -1 then
    Exit(Events[I]);

  Result := nil; { not found }
end;

function TX3DNode.AnyEvent(const AName: string): TX3DEvent;
var
  I: Integer;
begin
  I := FFields.IndexOfExposedEvent(AName, Result);
  if I <> -1 then
    Exit; { Result is already set }

  I := FEvents.IndexOfName(AName);
  if I <> -1 then
    Exit(Events[I]);

  Result := nil; { not found }
end;

procedure TX3DNode.SetHasInterfaceDeclarations(const Value: TX3DAccessTypes);
begin
  if Value <> HasInterfaceDeclarations then
  begin
    FHasInterfaceDeclarations := Value;
    if HasInterfaceDeclarations <> [] then
    begin
      { make sure InterfaceDeclarations is non-nil }
      if FInterfaceDeclarations = nil then
        FInterfaceDeclarations := TX3DInterfaceDeclarationList.Create(true);
    end else
    begin
      { make sure InterfaceDeclarations is nil }
      FreeAndNil(FInterfaceDeclarations);
    end;
  end;
end;

{ I couldn't implement this as TX3DField method, as X3DFields
  cannot declare TX3DNode in the interface, so also
  cannot declare TX3DNodeDeepCopyState. }
procedure FieldDeepCopyContents(Destination, Source: TX3DField;
  CopyState: TX3DNodeDeepCopyState);
var
  J: Integer;
  SF: TSFNode;
begin
  { First make "shallow copy" of the field by simple Assign }
  Destination.Assign(Source);

  if Destination is TSFNode then
  begin
    SF := TSFNode(Destination);
    if SF.Value <> nil then
      SF.Value := CopyState.DeepCopy(SF.Value);
  end;
end;

procedure EventDeepCopyContents(Destination, Source: TX3DEvent);
begin
  { For events, simple Assign does all copy we need --- at least for now. }
  Destination.Assign(Source);
end;

function TX3DNode.DeepCopyCreate(CopyState: TX3DNodeDeepCopyState): TX3DNode;
begin
  Result := TX3DNodeClass(ClassType).Create(X3DName, BaseUrl);
end;

function TX3DNode.DeepCopyCore(CopyState: TX3DNodeDeepCopyState): TX3DNode;
var
  I: Integer;
  IDecl: TX3DInterfaceDeclaration;
begin
  Result := DeepCopyCreate(CopyState);

  try

    { We expand CopyState arrays now, right after DeepCopyCreate.
      This is needed, as later during DeepCopyCore we may need this node
      in case of loops within hierarchy.

      For example, internal
      routes from TX3DPrototypeNode are established to handle "IS" clauses
      for events. There routes are to/from TX3DPrototypeNode, and are
      also placed within this TX3DPrototypeNode instance. So when copying
      node routes, this node must already be present in CopyState arrays.

      Also, in the future we will have to allow loops in Script nodes
      (USE within Script node may refer to the same node). So again loop
      will be created. }
    CopyState.Original.Add(Self);
    CopyState.New.Add(Result);

    for I := 0 to VRML1ChildrenCount - 1 do
      Result.VRML1ChildAdd(CopyState.DeepCopy(VRML1Children[I]));

    { Copy InterfaceDeclarations first, before copying Fields and Events
      (as some Fields and Events come from InterfaceDeclarations). }
    Result.HasInterfaceDeclarations := HasInterfaceDeclarations;

    if InterfaceDeclarations <> nil then
    begin
      for I := 0 to InterfaceDeclarations.Count - 1 do
      begin
        IDecl := InterfaceDeclarations[I].DeepCopy(Result, CopyState);
        Result.InterfaceDeclarations.Add(IDecl);
        Result.PostAddInterfaceDeclaration(IDecl);
      end;
    end;

    { TODO: No need to copy prototypes for now?

      This DeepCopy is used for now by protos expanding.
      It does not need prototype links (as protos are already expanded when copying,
      and they don't need anything more).

      for I := 0 to Prototypes.Count - 1 do
        ...(Prototypes[I]);
    }

    Assert(FieldsCount = Result.FieldsCount);
    Assert(EventsCount = Result.EventsCount);

    for I := 0 to FieldsCount - 1 do
      { Copying InterfaceDeclarations field/event already handled. }
      if Fields[I].ParentInterfaceDeclaration = nil then
      begin
        FieldDeepCopyContents(Result.Fields[I], Fields[I], CopyState);

        if Result.Fields[I].Exposed then
        begin
          EventDeepCopyContents(Result.Fields[I].EventIn , Fields[I].EventIn );
          EventDeepCopyContents(Result.Fields[I].EventOut, Fields[I].EventOut);
        end;
      end;

    for I := 0 to EventsCount - 1 do
      { Copying InterfaceDeclarations field/event already handled. }
      if Events[I].ParentInterfaceDeclaration = nil then
        EventDeepCopyContents(Result.Events[I], Events[I]);

    if PrototypeInstance then
    begin
      Result.FPrototypeInstance := PrototypeInstance;
      if PrototypeInstanceHelpers <> nil then
        Result.FPrototypeInstanceHelpers := CopyState.DeepCopy(PrototypeInstanceHelpers);
    end;

    Result.DefaultContainerField := DefaultContainerField;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TX3DNode.DeepCopy: TX3DNode;
var
  CopyState: TX3DNodeDeepCopyState;
begin
  CopyState := TX3DNodeDeepCopyState.Create;
  try
    Result := CopyState.DeepCopy(Self);
  finally FreeAndNil(CopyState); end;
end;

procedure TX3DNode.PostAddInterfaceDeclaration(IDecl: TX3DInterfaceDeclaration);
begin
  IDecl.AddFieldOrEvent(Self);
end;

procedure TX3DNode.AddCustomFieldOrEvent(AFieldOrEvent: TX3DFieldOrEvent);
var
  IDecl: TX3DInterfaceDeclaration;
begin
  IDecl := TX3DInterfaceDeclaration.Create(Self);
  IDecl.FieldOrEvent := AFieldOrEvent;
  AFieldOrEvent.ParentInterfaceDeclaration := IDecl;
  InterfaceDeclarations.Add(IDecl);
  PostAddInterfaceDeclaration(IDecl);
end;

procedure TX3DNode.AddCustomField(AField: TX3DField);
begin
  AddCustomFieldOrEvent(AField);
end;

function TX3DNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcNone;
end;

function TX3DNode.NiceName: string;
begin
  Result := X3DType;
  if X3DName <> '' then
    Result := Result + '(' + X3DName + ')';
end;

function TX3DNode.GetFields(const Index: Integer): TX3DField;
begin
  Result := FFields[Index];
end;

function TX3DNode.FieldsCount: Integer;
begin
  Result := FFields.Count;
end;

procedure TX3DNode.AddField(const Value: TX3DField);
begin
  FFields.Add(Value);

  { add the field also to FFieldsSFNode or FFieldsMFNode }
  if Value is TSFNode then
  begin
    if FFieldsSFNode = nil then
      FFieldsSFNode := TX3DFieldList.Create(false);
    FFieldsSFNode.Add(Value);
  end else
end;

function TX3DNode.IndexOfField(const AName: string): Integer;
begin
  Result := FFields.IndexOfName(AName);
end;

function TX3DNode.GetEvents(const Index: Integer): TX3DEvent;
begin
  Result := FEvents[Index];
end;

function TX3DNode.EventsCount: Integer;
begin
  Result := FEvents.Count;
end;

procedure TX3DNode.AddEvent(const Value: TX3DEvent);
begin
  FEvents.Add(Value);
end;

function TX3DNode.IndexOfEvent(const AName: string): Integer;
begin
  Result := FEvents.IndexOfName(AName);
end;

procedure TX3DNode.UnregisterSceneCallback(Node: TX3DNode);
begin
  //Node.Scene := nil;
end;

procedure TX3DNode.UnregisterScene;
begin
  EnumerateNodes(TX3DNode, {$ifdef CASTLE_OBJFPC}@{$endif} UnregisterSceneCallback, false);
end;

{ TX3DNodeList ------------------------------------------------------------- }

function TX3DNodeList.FindName(const Name: string): TX3DNode;
var
  I: Integer;
begin
  if Name = '' then
    Exit(nil);

  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.X3DName = Name then
      Exit;
  end;
  Result := nil;
end;

function TX3DNodeList.IndexOfName(const Name: string): Integer;
begin
  if Name = '' then
    Exit(-1);

  for Result := 0 to Count - 1 do
    if Items[Result].X3DName = Name then
      Exit;
  Result := -1;
end;

procedure TX3DNodeList.AddIfNotExists(const Node: TX3DNode);
begin
  if IndexOf(Node) = -1 then
    Add(Node);
end;

function TX3DNodeList.Equals(SecondValue: TObject): boolean;
var
  I: Integer;
begin
  Result :=
    (SecondValue <> nil) and
    (SecondValue is TX3DNodeList) and
    (TX3DNodeList(SecondValue).Count = Count);

  if Result then
    for I := 0 to Count - 1 do
      if Items[I] <> TX3DNodeList(SecondValue)[I] then
        Exit(false);
end;

procedure TX3DNodeList.Assign(const Source: TX3DNodeList);
begin
  Clear;
  AddRange(Source);
end;

procedure TX3DNodeList.Assign(const Source: array of TX3DNode);
begin
  Clear;
  AddRange(Source);
end;

{$endif read_implementation}

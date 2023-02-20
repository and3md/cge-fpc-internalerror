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

  { X3D node. Every VRML/X3D node class descends from this. }
  TX3DNode = class(TX3DFileItem)
  private
    FX3DName: string;
    FVRML1Children, FVRML1Parents: TX3DNodeList;
    FParentFields: TX3DFieldList;
    FImports, FExports: TX3DFileItemList;
    FFields, FFieldsSFNode, FFieldsMFNode: TX3DFieldList;
    FEvents: TX3DEventList;

    //ATest:Integer; //add to get internal error

    function GetVRML1Child(i: integer): TX3DNode;
    function GetVRML1Parent(i: integer): TX3DNode;
    procedure SetVRML1Child(I: Integer; Value: TX3DNode);
    function GetParentFieldsItem(Index: Integer): TX3DField;
    function GetParentFieldsNodeItem(Index: Integer): TX3DNode;
    procedure RemoveParentField(Field: TX3DField);
    procedure AddParentField(Field: TX3DField);
    function GetFields(const Index: Integer): TX3DField;
    function GetEvents(const Index: Integer): TX3DEvent;
  protected
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
    function FieldOrEvent(const AName: string): TX3DFileItem;

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
    constructor Create(const AX3DName: string = ''); virtual;

    destructor Destroy; override;

    { Type of the node in X3D, like @code('Group') or @code('Shape'). Never empty.
      Constant for the class lifetime (even for special
      TX3DUnknownNode and TX3DPrototypeNode, where this is calculated
      at runtime).

      Note that VRML/X3D is generally case-sensitive, so this property is too.

      In TX3DNode, this returns ClassX3DType, which is suitable
      for most nodes. See ClassX3DType. }
    function X3DType: string; virtual;

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

{ TX3DNode ------------------------------------------------------------------ }

constructor TX3DNode.Create(const AX3DName: String);
begin
  inherited Create;
  FX3DName := AX3DName;

  FVRML1Children := TX3DNodeList.Create(false);
  FVRML1Parents := TX3DNodeList.Create(false);
  FParentFields := TX3DFieldList.Create(false);
  FFields := TX3DFieldList.Create(false);
  FEvents := TX3DEventList.Create(false);

  CreateNode;
end;

destructor TX3DNode.Destroy;
var
  I: Integer;
begin
  if FVRML1Children <> nil then VRML1ChildrenClear;

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
    begin
        FEvents[I].Free;
        FEvents[I] := nil;
    end;
    FreeAndNil(FEvents);
  end;

  if FFields <> nil then
  begin
    for I := 0 to FFields.Count - 1 do
    begin
      FFields[I].Free;
      FFields[I] := nil;
    end;
    FreeAndNil(FFields);
  end;

  FreeAndNil(FVRML1Children);
  FreeAndNil(FVRML1Parents);
  FreeAndNil(FParentFields);

  inherited;
end;

procedure TX3DNode.FreeIfUnused;
begin
  if (Self <> nil) and
     (FVRML1Parents.Count = 0) and
     (FParentFields.Count = 0) then
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

function TX3DNode.X3DType: string;
begin
 result := ClassX3DType;
end;

class function TX3DNode.ClassX3DType: string;
begin
 result := '';
end;

function BlenderRelativePath(const RelativePath: string): boolean;
begin
  Result := false;
end;


procedure TX3DNode.CreateNode;
begin
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

procedure TX3DNode.RemoveParentField(Field: TX3DField);
begin
  FreeIfUnused;
end;

procedure TX3DNode.AddParentField(Field: TX3DField);
begin
  FParentFields.Add(Field);
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

function TX3DNode.Field(const AName: string): TX3DField;
var
  I: Integer;
begin
  I := FFields.IndexOfName(AName);
  if I <> -1 then
    Result := FFields[I] else
    Result := nil; { not found }
end;

function TX3DNode.FieldOrEvent(const AName: string): TX3DFileItem;
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

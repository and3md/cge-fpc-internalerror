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
    FFields, FFieldsSFNode, FFieldsMFNode: TX3DFieldList;
    FEvents: TX3DEventList;

    //ATest:Integer; //add to get internal error
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

    { Search by name for given event (exposed by some field or not).
      @nil if not found. }
    function AnyEvent(const AName: string): TX3DEvent;

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

  FFields := TX3DFieldList.Create(false);
  FEvents := TX3DEventList.Create(false);

  CreateNode;
end;

destructor TX3DNode.Destroy;
var
  I: Integer;
begin
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

  inherited;
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

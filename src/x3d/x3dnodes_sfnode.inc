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

  TAllowedChildren = (acAll, acClasses, acInterface);

  { VRML/X3D field holding a reference to a single node.
    It's defined in this unit, not in X3DFields, since it uses
    TX3DNode definition. NULL value of the field is indicated by
    Value field = nil.

    Note that we store AllowedChildren list, which is a list of
    classes allowed as a Value (also nil is always allowed).
    But this is used only to produce warnings for a user.
    You should never assert that Value actually is one the requested
    classes. We want to keep here even not allowed items,
    because we want operation "read from VRML file + write to VRML file"
    to be as non-destructible as possible. So if user wrote
    invalid class hierarchy, we will output this invalid class hierarchy. }
  TSFNode = class(TX3DField)
  private
    FValue: TX3DNode;
    FParentNode: TX3DNode;
    AllowedChildren: TAllowedChildren;
    AllowedChildrenInterface: TGUID;
    FDefaultValue: TX3DNode;
    FDefaultValueExists: boolean;
    FWeakLink: boolean;
    procedure SetValue(AValue: TX3DNode);
    procedure SetDefaultValue(ADefaultValue: TX3DNode);
    procedure SetDefaultValueExists(AValue: boolean);
    procedure SetWeakLink(const AValue: boolean);
    procedure WarningIfUnusedWeakLink;
    procedure DestructionNotification(Node: TX3DNode);
  public
    constructor Create(const AParentNode: TX3DNode;
      const AExposed: boolean; const AValue: TX3DNode = nil); overload;
    { Constructor that allows as children any implementor of given interface. }
    destructor Destroy; override;

    { Default value of SFNode field.

      While X3D specification says for all SFNode fields that their
      default value is NULL, this is not necessarily true for PROTO
      SFNode fiels. So we have to take into account that any DefaultValue
      is possible.

      Note that this doesn't have to be @nil, but will be irrelevant
      if not DefaultValueExists. (Once I had an idea to automatically
      set DefaultValue to @nil when DefaultValueExists is set to @false,
      but this was uncomfortable (like "what to do when DefaultValue
      is assigned non-nil when DefaultValueExists is false?").)

      Freeing of this is automatically managed, just like the normal
      @link(Value) property. This means that you can simply set
      DefaultValue to @nil or some existing node, and eventual memory
      deallocation of previous DefaultValue node (if unused) will happen
      automatically. }
    property DefaultValue: TX3DNode
      read FDefaultValue write SetDefaultValue;
    property DefaultValueExists: boolean
      read FDefaultValueExists write SetDefaultValueExists default false;

    property Value: TX3DNode read FValue write SetValue;

    { VRML node containing this field. May be @nil if unknown, in special
      cases.

      Note that this property is exactly the same as
      TX3DFieldOrEvent.ParentNode,
      contains always the same value. But this is declared as TX3DNode,
      so it's more comfortable. }
    property ParentNode: TX3DNode read FParentNode;

    class function CreateEvent: TX3DEvent; override;

    function ChildAllowed(Child: TX3DNode): boolean;
    function CurrentChildAllowed: boolean;

    procedure Send(const AValue: TX3DNode); overload;

    { Use weak links to deal with cycles in the X3D graph.

      Marking a field as a @italic(weak link) can only be done
      when the field value is empty, right when the field is created,
      in @link(TX3DNode.CreateNode) descendant.

      Being a @italic(weak link) means two things:

      @orderedList(
        @item(The nodes inside a weak link are not enumerated
          when traversing the X3D graph in @italic(any) way.
          This includes @link(TX3DNode.EnumerateNodes),
          @link(TX3DNode.Traverse) and all others.
          Nodes implementing @link(TX3DNode.DirectEnumerateActive)
          should also omit these fields.)

        @item(A weak link does not create a reference count
          preventing the node from being freed (or freeing
          it automatically when ref count drops to zero).
          Instead, weak links merely observe the nodes, and automatically
          set their value to @nil when the node gets freed.)
      )

      If effect, this avoids loops when enumerating (and avoids
      recursive loops in reference counts, which would cause memory leaks),
      but use this only when you know that the node
      must occur somewhere else in the X3D graph anyway (or it's OK to
      ignore it).
      For example, this is useful for
      @link(TGeneratedShadowMapNode.Light), as we know that the light
      must occur somewhere else in the graph anyway to be useful.
    }
    property WeakLink: boolean
      read FWeakLink write SetWeakLink default false;
  end;

  TSFNodeEventHelper = class helper for TSFNodeEvent
    procedure Send(const Value: TX3DNode); overload;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TSFNode --------------------------------------------------------------------- }
constructor TSFNode.Create(const AParentNode: TX3DNode;
  const AExposed: boolean; const AValue: TX3DNode);
begin
  inherited Create(AExposed);

  { FParentNode is just a copy of inherited (TX3DFieldOrEvent) FParentNode,
    but casted to TX3DNode }
  FParentNode := AParentNode;

  AllowedChildren := acClasses;

  Value := AValue;
end;

destructor TSFNode.Destroy;
begin
  { To delete Self from Value.FParentFields, and eventually free Value. }
  Value := nil;
  { To delete Self from DefaultValue.FParentFields, and eventually free DefaultValue. }
  DefaultValue := nil;
  inherited;
end;

function TSFNode.ChildAllowed(Child: TX3DNode): boolean;
begin
  case AllowedChildren of
    acAll      : Result := true;
    acClasses  : Result := (Child = nil);
    acInterface: Result := (Child = nil) or Supports(Child, AllowedChildrenInterface);
    else raise Exception.Create('AllowedChildren?');
  end;
end;

function TSFNode.CurrentChildAllowed: boolean;
begin
  Result := ChildAllowed(Value);
end;

procedure TSFNode.WarningIfUnusedWeakLink;
begin
  if WeakLink and
     (Value <> nil) then
  begin
    FValue := nil;
  end;
end;

procedure TSFNode.SetValue(AValue: TX3DNode);
begin
  if FValue <> AValue then
    FValue := AValue;
end;

procedure TSFNode.SetDefaultValue(ADefaultValue: TX3DNode);
begin
  if FDefaultValue <> ADefaultValue then
    FDefaultValue := ADefaultValue;
end;

procedure TSFNode.DestructionNotification(Node: TX3DNode);
begin
  if WeakLink then
  begin
    if FValue = Node then
      FValue := nil;
    if FDefaultValue = Node then
      FDefaultValue := nil;
  end;
end;

procedure TSFNode.SetDefaultValueExists(AValue: boolean);
begin
  FDefaultValueExists := AValue;
end;

class function TSFNode.CreateEvent: TX3DEvent;
begin
  Result := TSFNodeEvent.Create;
end;

procedure TSFNode.Send(const AValue: TX3DNode);
var
  FieldValue: TSFNode;
begin
  { We construct using CreateUndefined constructor,to have AllowedChildren = acAll }
  { AExposed = false below, because not needed otherwise. }
  FieldValue := TSFNode.Create(ParentNode, false);
  try
    FieldValue.Value := AValue;
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

procedure TSFNode.SetWeakLink(const AValue: boolean);
begin
  if FWeakLink <> AValue then
  begin
    if Value <> nil then
      raise Exception.Create('TSFNode.WeakLink cannot change when some node is already assigned');
    FWeakLink := AValue;
  end;
end;

{ TSFNodeEventHelper --------------------------------------------------------- }

procedure TSFNodeEventHelper.Send(const Value: TX3DNode);
begin
  {if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.NextEventTime);}
end;

{$endif read_implementation}

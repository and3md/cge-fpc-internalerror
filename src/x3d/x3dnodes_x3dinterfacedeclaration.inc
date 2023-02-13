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

  { Interface declaration, used in VRML/X3D (exposed) prototypes and
    for nodes with dynamic fields (Script, ComposedShader).
    See VRML 2.0 and X3D specs.

    Each interface specification is a field or an event, stored
    in FieldOrEvent. FieldOrEvent is @nil before parsing.

    Field value is not initialized if you passed FieldValue = @false
    to @link(Parse) (although IsClauseNames will
    always be initialized). FieldValue = @true is used for prototype
    (not external) declarations and nodes with interface declarations
    (Script, ComposedShader etc.).
    In the future maybe some property like
    FieldValueInitialized will be exposed here, if needed at some point.

    Interface declaration doesn't have much properties, since all
    the information is contained within FieldOrEvent
    instance, like Name, field class type, out or in (in case of event),
    exposed or not (in case of field), IsClauseNames. }
  TX3DInterfaceDeclaration = class(TX3DFileItem)
  strict private
    FFieldOrEvent: TX3DFieldOrEvent;

    { kept in synch with FFieldOrEvent by SetFieldOrEvent }
    FField: TX3DField;
    FEvent: TX3DEvent;

    procedure SetFieldOrEvent(const Value: TX3DFieldOrEvent);
  strict private
    FParentNode: TX3DNode;
  public
    constructor Create(AParentNode: TX3DNode);
    destructor Destroy; override;

    { Containing node, if any, for this VRML interface declaration.
      This must also be set to FieldOrEvent.ParentNode created for this
      interface declaration. }
    property ParentNode: TX3DNode read FParentNode;

    { Field or event of this interface declaration.
      Is non-nil after parsing.

      You can assign to this property, to constructs interface
      declarations (and so also prototypes) in your own code
      (e.g. this is used X3D XML reader). Just be careful, and remember
      that this object owns FieldOrEvent (that is, will free it
      at destruction). }
    property FieldOrEvent: TX3DFieldOrEvent
      read FFieldOrEvent write SetFieldOrEvent;

    { Create a copy of current FieldOrEvent.
      Sets NewParentNode as Result.ParentNode.
      Note the new copy will not have ParentIntefaceDeclaration set
      (as the idea is that you own created copy, not this TX3DInterfaceDeclaration
      instance). }
    function CopyFieldOrEvent(NewParentNode: TX3DNode): TX3DFieldOrEvent;

    { Create a copy of current FieldOrEvent, and add it to Node.Fields
      or Node.Events. }
    procedure CopyAndAddFieldOrEvent(Node: TX3DNode);

    { Copies only reference to FieldOrEvent, adding it to Node.Fields
      or Node.Events. }
    procedure AddFieldOrEvent(Node: TX3DNode);

    { Return FieldOrEvent casted as appropriate class.
      @nil if such cast is not possible, for example when
      FieldOrEvent is an event and you try to use Field method.
      @groupBegin }
    property Field: TX3DField read FField;
    property Event: TX3DEvent read FEvent;
    { @groupEnd }

    { Save this interface declaration to stream.
      @seealso IDeclSaveToStream }
    procedure SaveToStream(Writer: TX3DWriter); override;

    { Returns access type, corresponding to current @link(Event)
      and @link(Field) values.

      Result is undefined if both Event
      and Field are @nil (which may happen when it's not initialized
      (e.g. parsed) yet) or when both are non-nil (which should never
      happen). }
    function AccessType: TX3DAccessType;

    function DeepCopy(NewParentNode: TX3DNode;
      CopyState: TX3DNodeDeepCopyState): TX3DInterfaceDeclaration;
  end;

  TX3DInterfaceDeclarationList = class(specialize TObjectList<TX3DInterfaceDeclaration>)
  public
    { Find field or event with given Name.
      @nil if not found. }
    function TryFindName(const Name: string): TX3DFieldOrEvent;

    { Find field with given Name.
      @nil if not found. }
    function TryFindFieldName(const Name: string): TX3DField;

    { Find event with given Name.
      @nil if not found. }
    function TryFindEventName(const Name: string): TX3DEvent;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TX3DInterfaceDeclaration -------------------------------------------------- }

constructor TX3DInterfaceDeclaration.Create(AParentNode: TX3DNode);
begin
  inherited Create;
  FParentNode := AParentNode;
end;

destructor TX3DInterfaceDeclaration.Destroy;
begin
  FreeAndNil(FFieldOrEvent);
  FField := nil;
  FEvent := nil;

  inherited;
end;

procedure TX3DInterfaceDeclaration.SetFieldOrEvent(
  const Value: TX3DFieldOrEvent);
begin
  FFieldOrEvent := Value;

  { set FField and FEvent, for fast access to them }
  if FFieldOrEvent = nil then
  begin
    FField := nil;
    FEvent := nil;
  end else
  if FFieldOrEvent is TX3DField then
  begin
    FField := TX3DField(FFieldOrEvent);
    FEvent := nil;
  end else
  begin
    Assert(FFieldOrEvent is TX3DEvent);
    FField := nil;
    FEvent := TX3DEvent(FFieldOrEvent);
  end;
end;

function TX3DInterfaceDeclaration.CopyFieldOrEvent(
  NewParentNode: TX3DNode): TX3DFieldOrEvent;
var
  F: TX3DField absolute Result;
  E: TX3DEvent absolute Result;
begin
  if Field <> nil then
  begin
    { F := copy of Field }
    F := TX3DFieldClass(Field.ClassType).CreateUndefined(NewParentNode,
      Field.Exposed, Field.X3DName);
    F.Assign(Field);

    { CreateUndefined creates field without any default value,
      so it will always get saved later to file.

      But this is not nice: for non-node fields, it merely makes
      resulting file longer. For node fields (SFNode and MFNode)
      this means that node value will be written to file. But this
      is bad, since this means that node contents will have to duplicated,
      if node is not named or it's name is unbound now (e.g. overridden
      by other node name) (otherwise "USE Xxx" could be used, which
      is acceptable).

      See ../../../demo_models/x3d/proto_sfnode_default.x3dv
      and tricky_def_use.x3dv for
      examples (open and save it back e.g. in view3dscene).

      So to make it work right, we have to set DefaultValue for our
      fields, in particular for TSFNode and TMFNode fields.
      So that EqualsDefaultValue will work Ok when saving to file. }
    F.AssignDefaultValueFromValue;
  end else
  if Event <> nil then
  begin
    { E := copy of Event }
    E := Event.FieldClass.CreateEvent(NewParentNode, Event.X3DName, Event.InEvent);
    { Although above constructor already copied most event properties,
      some were omitted (like IsClauseNames --- important for Script with
      eventIn/out events with IS clauses inside prototypes).
      Assign call below takes care of them. }
    E.Assign(Event);
  end else
    raise Exception.Create('interface declaration but no Field or Event');

  Result.ParentInterfaceDeclaration := nil;
end;

procedure TX3DInterfaceDeclaration.AddFieldOrEvent(
  Node: TX3DNode);
begin
  if Field <> nil then
    Node.AddField(Field) else
  begin
    Assert(Event <> nil);
    Node.AddEvent(Event);
  end;
end;

procedure TX3DInterfaceDeclaration.CopyAndAddFieldOrEvent(
  Node: TX3DNode);
var
  Copy: TX3DFieldOrEvent;
begin
  Copy := CopyFieldOrEvent(Node);
  if Copy is TX3DField then
    Node.AddField(TX3DField(Copy)) else
  begin
    Assert(Copy is TX3DEvent);
    Node.AddEvent(TX3DEvent(Copy));
  end;
end;

procedure TX3DInterfaceDeclaration.SaveToStream(Writer: TX3DWriter);
begin
  //IDeclSaveToStream(Writer, true);
end;

function TX3DInterfaceDeclaration.AccessType: TX3DAccessType;
begin
  if Event <> nil then
  begin
    if Event.InEvent then
      Result := atInputOnly else
      Result := atOutputOnly;
  end else
  if Field <> nil then
  begin
    if Field.Exposed then
      Result := atInputOutput else
      Result := atInitializeOnly;
  end else
    { Result is undefined in this case, but we don't want to signal any error }
    Result := atInitializeOnly
end;

function TX3DInterfaceDeclaration.DeepCopy(
  NewParentNode: TX3DNode;
  CopyState: TX3DNodeDeepCopyState): TX3DInterfaceDeclaration;
begin
  Result := TX3DInterfaceDeclaration.Create(NewParentNode);
  Result.FieldOrEvent := CopyFieldOrEvent(NewParentNode);
  Result.FieldOrEvent.ParentInterfaceDeclaration := Result;
end;

{ TX3DInterfaceDeclarationList --------------------------------------------- }

function TX3DInterfaceDeclarationList.TryFindName(
  const Name: string): TX3DFieldOrEvent;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I].FieldOrEvent;
    if Result.X3DName = Name then
      Exit;
  end;
  Result := nil;
end;

function TX3DInterfaceDeclarationList.TryFindFieldName(const Name: string): TX3DField;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].FieldOrEvent is TX3DField then
    begin
      Result := Items[I].Field;
      if Result.X3DName = Name then
        Exit;
    end;
  Result := nil;
end;

function TX3DInterfaceDeclarationList.TryFindEventName(const Name: string): TX3DEvent;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].FieldOrEvent is TX3DEvent then
    begin
      Result := Items[I].Event;
      if Result.X3DName = Name then
        Exit;
    end;
  Result := nil;
end;

{$endif read_implementation}

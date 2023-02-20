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
    FFields, FFieldsSFNode, FFieldsMFNode: TX3DFieldList;

    //ATest:Integer; //add to get internal error
    function GetFields(const Index: Integer): TX3DField;
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

    { Search by name for given field.
      @nil if not found. }
    function Field(const AName: string): TX3DField;

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
    constructor Create; virtual;

    destructor Destroy; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TX3DNodeDeepCopyState ----------------------------------------------------- }

{ TX3DNode ------------------------------------------------------------------ }

constructor TX3DNode.Create;
begin
  inherited Create;
  FFields := TX3DFieldList.Create(false);
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

procedure TX3DNode.CreateNode;
begin
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

{ TX3DNodeList ------------------------------------------------------------- }

{$endif read_implementation}

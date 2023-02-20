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
  { VRML/X3D field holding a reference to a single node.
    It's defined in this unit, not in X3DFields, since it uses
    TX3DNode definition. NULL value of the field is indicated by
    Value field = nil. }
  TSFNode = class(TX3DField)
  private
    FValue: TX3DNode;
    FParentNode: TX3DNode;

    procedure SetValue(AValue: TX3DNode);
  public
    constructor Create(const AParentNode: TX3DNode;
      const AExposed: boolean; const AValue: TX3DNode = nil); overload;
    { Constructor that allows as children any implementor of given interface. }
    destructor Destroy; override;
    property Value: TX3DNode read FValue write SetValue;

    property ParentNode: TX3DNode read FParentNode;

    class function CreateEvent: TX3DEvent; override;

    procedure Send(const AValue: TX3DNode); overload;
  end;

  // removing this helper removes internal error on fixes branch fpc 3.2
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

  Value := AValue;
end;

destructor TSFNode.Destroy;
begin
  { To delete Self from Value.FParentFields, and eventually free Value. }
  Value := nil;
  inherited;
end;

procedure TSFNode.SetValue(AValue: TX3DNode);
begin
  if FValue <> AValue then
    FValue := AValue;
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

{ TSFNodeEventHelper --------------------------------------------------------- }

procedure TSFNodeEventHelper.Send(const Value: TX3DNode);
begin
  {if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.NextEventTime);}
end;

{$endif read_implementation}

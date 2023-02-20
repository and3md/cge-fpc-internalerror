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
    FFields: TX3DFieldList;

    //ATest:Integer; //add to get internal error
    function GetFields(const Index: Integer): TX3DField;
  protected
    procedure CreateNode; virtual;
  public
    property Fields [Index: Integer]: TX3DField read GetFields;
    function FieldsCount: Integer;
    procedure AddField(const Value: TX3DField);
    function IndexOfField(const AName: string): Integer;

    { Search by name for given field.
      @nil if not found. }
    function Field(const AName: string): TX3DField;

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
end;

function TX3DNode.IndexOfField(const AName: string): Integer;
begin
  Result := FFields.IndexOfName(AName);
end;

{ TX3DNodeList ------------------------------------------------------------- }

{$endif read_implementation}

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

unit X3DNodes;

{$mode objfpc}
{$H+}
{$macro on}
{$writeableconst off}
{$modeswitch advancedrecords}

interface

uses SysUtils, Classes,
  CastleInternalX3DLexer, X3DFields,
  CastleMaterialProperties;

type
  TX3DNode = class;
  TX3DRootNode = class;

  TX3DNodeClass = class of TX3DNode;

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

    constructor Create; virtual;
    destructor Destroy; override;
  end;


  // removing this helper removes internal error on fixes branch fpc 3.2
  TSFNodeEventHelper = class helper for TSFNodeEvent
    procedure Test;
  end;

  { A top-level VRML/X3D node }
  TX3DRootNode = class(TX3DNode)
  strict private
    FHasForceVersion: boolean;
  public
    ForceVersion: TX3DVersion;

    property HasForceVersion: boolean
      read FHasForceVersion write FHasForceVersion default false;
  end;

implementation

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

{ TSFNodeEventHelper --------------------------------------------------------- }

procedure TSFNodeEventHelper.Test;
begin
end;


{ Nodes from standard X3D components }

{ unit init/fini ------------------------------------------------------------ }

initialization

finalization

end.

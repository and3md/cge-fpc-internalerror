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

{ X3D fields (TX3DField and many descendants). }
unit X3DFields;

{$mode objfpc}
{$H+}
{$macro on}
{$writeableconst off}
{$modeswitch advancedrecords}


interface

uses Classes, SysUtils, Generics.Collections,
  CastleInternalX3DLexer;

type
  { Base class for any item within VRML/X3D file: a node, a field, a route,
    a prototype etc. We need a common base class for all such things
    to store PositionInParent.

    About ancestry: TX3DFieldOrEvent make use of Assign mechanism
    and so need to descend from TPersistent. TX3DNode make use
    of interfaces and so must descend from something like
    TNonRefCountedInterfacedXxx. These are the only reasons, for now,
    why this descends from TNonRefCountedInterfacedPersistent. }
  TX3DFileItem = class
  end;

  TX3DFileItemList = class(specialize TObjectList<TX3DFileItem>)
  public
    procedure Add(Item: TX3DFileItem); reintroduce;
  end;

  TX3DField = class;
  TX3DFieldList = class;
  TX3DEvent = class;

  TX3DField = class(TX3DFileItem)
  strict private
    FExposedEvents: array [boolean] of TX3DEvent;

    FExposed: boolean;

    procedure SetExposed(Value: boolean);
    function GetExposedEvents(InEvent: boolean): TX3DEvent;
  public
    constructor Create(const AExposed: boolean);

    destructor Destroy; override;

    property Exposed: boolean read FExposed write SetExposed default false;

    { These are the set_xxx and xxx_changed events exposed by this field.
      @nil if Exposed is @false. }
    property ExposedEvents [InEvent: boolean]: TX3DEvent
      read GetExposedEvents;

    { Exposed events of this field. @nil if this field is not exposed.
      EventIn is always equivalent to ExposedEvents[true],
      EventOut is always equivalent to ExposedEvents[false].
      @groupBegin }
    function EventIn: TX3DEvent;
    function EventOut: TX3DEvent;
    { @groupEnd }
  end;

  TX3DFieldList = class(specialize TObjectList<TX3DField>)
  public
  end;

  { X3D event. }
  TX3DEvent = class(TX3DFileItem)
  strict private
    AVariable: Integer; // just for keep strict private
    //ATest: Integer; // uncomment to get internal error
  end;

  TSFNodeEvent = class(TX3DEvent)
  end;

implementation

uses X3DNodes;

{ TX3DFileItemList --------------------------------------------------------- }

procedure TX3DFileItemList.Add(Item: TX3DFileItem);
begin
  inherited Add(Item);
end;

{ TX3DField ------------------------------------------------------------- }

constructor TX3DField.Create(const AExposed: boolean);
begin
  inherited Create;

  { Set Exposed by the property, to force FExposedEvents initialization }
  FExposed := false;
  Exposed := AExposed;
end;

destructor TX3DField.Destroy;
begin
  FreeAndNil(FExposedEvents[false]);
  FreeAndNil(FExposedEvents[true]);
  inherited;
end;

function TX3DField.GetExposedEvents(InEvent: boolean): TX3DEvent;
begin
  Result := FExposedEvents[InEvent];
end;

function TX3DField.EventIn: TX3DEvent;
begin
  Result := FExposedEvents[true];
end;

function TX3DField.EventOut: TX3DEvent;
begin
  Result := FExposedEvents[false];
end;

procedure TX3DField.SetExposed(Value: boolean);
begin
  if Value <> Exposed then
  begin
    FExposed := Value;
    if Exposed then
    begin
      FreeAndNil(FExposedEvents[false]);
      FreeAndNil(FExposedEvents[true]);
    end;
  end;
end;


end.

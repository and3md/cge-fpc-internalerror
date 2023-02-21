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

  TX3DFieldClass = class of TX3DField;

  TX3DField = class(TX3DFileItem)
  strict private
    FExposedEvents: array [boolean] of TX3DEvent;

    FExposed: boolean;

    procedure SetExposed(Value: boolean);
    function GetExposedEvents(InEvent: boolean): TX3DEvent;
  strict protected
    class function ExposedEventsFieldClass: TX3DFieldClass; virtual;
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

    { Create TX3DEvent descendant suitable as exposed event for this field. }
    class function CreateEvent: TX3DEvent; virtual;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); virtual;

    { @abstract(Is AssignLerp usable on this field type?)

      @italic(Descendants implementors notes):
      In this class, this always returns @false. }
    function CanAssignLerp: boolean; virtual;

    procedure Send(Value: TX3DField);
  end;

  TX3DFieldList = class(specialize TObjectList<TX3DField>)
  public
  end;

  { X3D event. }
  TX3DEvent = class(TX3DFileItem)
  strict private
    FFieldClass: TX3DFieldClass;
    //ATest: Integer; // uncomment to get internal error
  public
    constructor Create(const AFieldClass: TX3DFieldClass);

    property FieldClass: TX3DFieldClass read FFieldClass;

    procedure Send(Field: TX3DField); overload;
  end;

  TSFNodeEvent = class(TX3DEvent)
  public
    constructor Create;
  end;

  { X3D field containing a vector. }
  generic TSFGenericVector<
    TItem,
    TEvent> = class(TX3DField)
  public
    Value: TItem;

    DefaultValue: TItem;
    DefaultValueExists: boolean;

    constructor Create(const AExposed: boolean; const AValue: TItem);

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure Send(const AValue: TItem); overload;

    class function CreateEvent: TX3DEvent; override;
  end;

implementation

uses Generics.Defaults,
  X3DNodes;

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

procedure TX3DField.Send(Value: TX3DField);
begin
  if Exposed then
    EventIn.Send(Value);
end;

class function TX3DField.ExposedEventsFieldClass: TX3DFieldClass;
begin
  Result := TX3DFieldClass(ClassType);
end;

class function TX3DField.CreateEvent: TX3DEvent;
begin
  Result := TX3DEvent.Create(ExposedEventsFieldClass);
end;

procedure TX3DField.SetExposed(Value: boolean);
begin
  if Value <> Exposed then
  begin
    FExposed := Value;
    if Exposed then
    begin
      FExposedEvents[false] := CreateEvent;
      FExposedEvents[true] := CreateEvent;

      FreeAndNil(FExposedEvents[false]);
      FreeAndNil(FExposedEvents[true]);
    end;
  end;
end;

procedure TX3DField.AssignLerp(const A: Double; Value1, Value2: TX3DField);
begin
  { do nothing, CanAssignLerp is false }
end;

function TX3DField.CanAssignLerp: boolean;
begin
  Result := false;
end;

{ TX3DEvent ----------------------------------------------------------------- }

constructor TX3DEvent.Create(const AFieldClass: TX3DFieldClass);
begin
  inherited Create;
  FFieldClass := AFieldClass;
end;

procedure TX3DEvent.Send(Field: TX3DField);
begin
  Assert(Field <> nil);
  Assert(Field is FieldClass);
end;

constructor TSFNodeEvent.Create;
begin
  inherited Create(TSFNode);
end;

{ TSFGenericVector ----------------------------------------------------------- }

constructor TSFGenericVector.Create(const AExposed: boolean; const AValue: TItem);
begin
  inherited Create(AExposed);

  Value := AValue;
end;

procedure TSFGenericVector.AssignLerp(const A: Double; Value1, Value2: TX3DField);
begin
  Value := TItem.Lerp(A, (Value1 as TSFGenericVector).Value, (Value2 as TSFGenericVector).Value);
end;

function TSFGenericVector.CanAssignLerp: boolean;
begin
  Result := true;
end;

class function TSFGenericVector.CreateEvent: TX3DEvent;
begin
  Result := TEvent.Create;
end;

procedure TSFGenericVector.Send(const AValue: TItem);
var
  FieldValue: TSFGenericVector;
begin
  FieldValue := TSFGenericVector(
    TX3DFieldClass(ClassType).Create(false));
  try
    FieldValue.Value := AValue;
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;


finalization

end.

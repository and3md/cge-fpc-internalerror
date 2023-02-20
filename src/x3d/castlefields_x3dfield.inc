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

    procedure Parse(Lexer: TX3DLexer);

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

    { Assigns value to this node calculated from linear interpolation
      between two given nodes Value1, Value2. Just like other lerp
      functions in our units (like @link(CastleVectors.Lerp)).

      Like AssignValue, this copies only the current value.
      All other properties (like Name, IsClauseNames, ValueFromIsClause,
      default value) are untouched.

      There are some special precautions for this:

      @unorderedList(
        @item(First of all, AssignLerp is defined only for fields where
          CanAssignLerp returns @true, so always check CanAssignLerp first.
          All float-based fields should have this implemented.)

        @item(Use this only if Value1 and Value2
          are equal or descendant of target (Self) class.)

        @item(For multiple-value fields, counts of Value1 and Value2
          must be equal, or EListsDifferentCount will be raised.)
      )

      @raises(EListsDifferentCount When field is multiple-value
        field and Value1.Count <> Value2.Count.)
    }
    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); virtual;

    { @abstract(Is AssignLerp usable on this field type?)

      @italic(Descendants implementors notes):
      In this class, this always returns @false. }
    function CanAssignLerp: boolean; virtual;

    { Set the value of the field, notifying the scenes and events engine.
      This sets the value of this field in the nicest possible way for
      any possible TCastleSceneCore (with events on or off) containing the node
      with this field.

      Precise specification:

      @unorderedList(
        @item(If this is an exposed field and we have events engine working:

          We will send this value through
          it's input event. In this case, this is equivalent to doing
          @code(EventIn.Send(Value, Scene.Time)).
          The scenes (including events engine) will be notified correctly
          by exposed events handler already.)

        @item(Otherwise, we will just set the fields value.
          And then notify the scenes (including events engine).)
      ) }
    procedure Send(Value: TX3DField);
  end;

  TX3DFieldList = class(specialize TObjectList<TX3DField>)
  public
  end;

{$endif read_interface}

{$ifdef read_implementation}

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
var
  I: Integer;
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

procedure TX3DField.Parse(Lexer: TX3DLexer);
begin
end;

procedure TX3DField.AssignLerp(const A: Double; Value1, Value2: TX3DField);
begin
  { do nothing, CanAssignLerp is false }
end;

function TX3DField.CanAssignLerp: boolean;
begin
  Result := false;
end;

{$endif read_implementation}

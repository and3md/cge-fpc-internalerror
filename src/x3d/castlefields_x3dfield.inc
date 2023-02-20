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

  { Base class for all VRML/X3D fields.

    Common notes for all descendants: most of them expose a field or property
    "Value", which specifies the current value of the field.
    Many of them also expose DefaultValue and DefaultValueExists
    fields/properties, these should be the default X3D value for this field.
    You can even change DefaultValue after the object is created.

    Most of descendants include constructor that initializes
    both DefaultValue and Value to the same thing, as this is what
    you usually want.

    Some notes about @code(Assign) method (inherited from TPersistent and
    overridied appropriately in TX3DField descendants):

    @orderedList(
      @item(There are some exceptions, but usually
        assignment is possible only when source and destination field classes
        are equal.)

      @item(Assignment (by @code(Assign), inherited from TPersistent)
        tries to copy everything: name (with alternative names), default value,
        IsClauseNames, ValueFromIsClause, Exposed, and of course current value.

        Exceptions are things related to hierarchy of containers:
        ParentNode, ParentInterfaceDeclaration. Also ExposedEventsLinked.

        If you want to copy only the current value, use AssignValue
        (or AssignLerp, where available).))
  }
  TX3DField = class(TX3DFileItem)
  strict private
    FExposedEvents: array [boolean] of TX3DEvent;

    // keep booleans together, to pack them together in memory
    FExposed: boolean;
    FValueFromIsClause: boolean;
    FExposedEventsLinked: boolean;

    procedure SetExposed(Value: boolean);
    function GetExposedEvents(InEvent: boolean): TX3DEvent;
    procedure SetExposedEventsLinked(const Value: boolean);
  strict protected
    function SendsByEvent: boolean;

    { Class of the fields allowed in the exposed events of this field.
      This should usually be using ClassType of this object,
      and this is the default implementation of this method in TX3DField.

      You can override this to return some ancestor (from which, and to which,
      you can assign) if your TX3DField descendant
      doesn't change how the @code(Assign) method works.
      E.g. TSFTextureUpdate class, that wants to be fully compatible with normal
      TSFString. }
    class function ExposedEventsFieldClass: TX3DFieldClass; virtual;

    { Handle exposed input event. In TX3DField class, this does everything
      usually needed --- assigns value, sends an output event, notifies
      @link(Changed).

      You can override this for some special purposes. For special needs,
      you do not even need to call @code(inherited) in overriden versions.
      This is suitable e.g. for cases when TimeSensor.set_startTime or such
      must be ignored. }
    procedure ExposedEventReceive(Event: TX3DEvent; Value: TX3DField); virtual;
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

    { When @true (default) we will automatically handle exposed events
      behavior. This means that we will listen on EventIn,
      and when something will be received we will set current field's value
      and produce appropriate EventOut.

      You almost certainly want to leave this as @true in all typical
      situations, as it takes care of implementing required exposed events
      behavior.

      That said, in special cases you may decide to break this. }
    property ExposedEventsLinked: boolean
      read FExposedEventsLinked write SetExposedEventsLinked
      default true;

    { Field type in X3D, like @code('SFString') or @code('MFInt32').
      As for VRML/X3D interface declaration statements.
      In base TX3DField class, this returns @code(XFAny)
      (name indicating any type, used by instantreality and us). }
    class function X3DType: string; virtual;

    { Create TX3DEvent descendant suitable as exposed event for this field. }
    class function CreateEvent: TX3DEvent; virtual;

    { Copies the current field value. Contrary to TPersistent.Assign, this
      doesn't copy the rest of properties.

      After setting, our ValueFromIsClause is always changed to @false.
      You can manually change it to @true, if this copy indeed was done
      following "IS" clause.

      @raises(EX3DFieldAssignInvalidClass
        Usually it's required the Source class to be equal to our class,
        if Source classes cannot be assigned we raise EX3DFieldCannotAssignClass.)

      @raises(EX3DFieldAssign
        Raised in case of any field assignment problem. It's guaranteed that
        in case of such problem, our value will not be modified before
        raising the exception.

        EX3DFieldAssignInvalidClass inherits from EX3DFieldAssign,
        so actually EX3DFieldAssignInvalidClass is just a special case of this
        exceptiion.)

      @italic(Descendants implementors notes):

      In this class, implementation takes care of
      setting our ValueFromIsClause to @false. In descendants,
      you should do like

      @longCode(#
        if Source is <appropriate class> then
        begin
          inherited;
          Value := Source.value;
        end else
          AssignValueRaiseInvalidClass(Source);
      #)
    }
    procedure AssignValue(Source: TX3DField); virtual;

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

  TX3DFieldList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TX3DField>)
  public
    { Searches for a field with given Name, returns it's index or -1 if not found. }
    function IndexOfName(const AName: string): integer;

    { Returns if EventName is an event implicitly exposed by one of our
      exposed fields (i.e. set_xxx or xxx_changed). If yes, then
      returns index of event, and the event reference itself
      (so always @code(Fields[ReturnedIndex].ExposedEvent[ReturnedEvent.InEvent]
      = ReturnedEvent)). Otherwise, returns -1. }
    function IndexOfExposedEvent(const EventName: string;
      out Event: TX3DEvent): Integer;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TPartialSend --------------------------------------------------------------- }

{ TX3DField.TPartialReceived --------------------------------------------------------------- }

{ TX3DField ------------------------------------------------------------- }

constructor TX3DField.Create(const AExposed: boolean);
begin
  inherited Create;
  FExposedEventsLinked := true;

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

procedure TX3DField.ExposedEventReceive(Event: TX3DEvent; Value: TX3DField);
var
  ValuePossiblyChanged: boolean;
begin
  Assert(Exposed);
  Assert(Event = FExposedEvents[true]);
  Assert(Value is ExposedEventsFieldClass);
  { This is trivial handling of exposed events: just set our value,
    and call out event. }

  AssignValue(Value);

  FExposedEvents[false].Send(Value);
end;

function TX3DField.SendsByEvent: boolean;
begin
  Result := Exposed ;
end;

procedure TX3DField.Send(Value: TX3DField);
begin
  if SendsByEvent then
  begin
    EventIn.Send(Value);
  end else
  begin
    { Call AssignValue regardless of ValuePossiblyChanged.
      Reason: AssignValue also removes "IS" clause. }
    AssignValue(Value);
  end;
end;

const
  SetPrefix = 'set_';
  ChangedSuffix = '_changed';

procedure TX3DField.SetExposedEventsLinked(const Value: boolean);
begin
  if FExposedEventsLinked <> Value then
    FExposedEventsLinked := Value;
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

procedure TX3DField.AssignValue(Source: TX3DField);
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

{ Note that TX3DField.X3DType cannot be abstract:
  it may be used if source event is of XFAny type in warning message
  in TX3DRoute.SetEndingInternal }
class function TX3DField.X3DType: string;
begin
  Result := 'XFAny';
end;

{ TX3DFieldList ------------------------------------------------------------- }

function TX3DFieldList.IndexOfName(const AName: string): integer;
begin
  Result := -1;
end;

function TX3DFieldList.IndexOfExposedEvent(const EventName: string;
  out Event: TX3DEvent): Integer;
var
  InEvent: boolean;
begin
  { This implementation is quite optimized.
    Instead of browsing all fields and their ExposedEvents,
    looking for EventName event, instead we examine EventName
    to look whether this has any chance of being set_xxx or xxx_changed
    event. So we utilize the fact that exposed events have consistent
    naming. }

  Result := -1;
  InEvent := true;
  { check is field really exposed now }
  if (Result <> -1) and (not Items[Result].Exposed) then
  begin
    Result := -1;
  end;

  if Result <> -1 then
  begin
    Event := Items[Result].ExposedEvents[InEvent];
  end;
end;

{$endif read_implementation}

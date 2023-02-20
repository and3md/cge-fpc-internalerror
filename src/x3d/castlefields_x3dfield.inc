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
    FChangesAlways: TX3DChanges;

    // keep booleans together, to pack them together in memory
    FExposed: boolean;
    FValueFromIsClause: boolean;
    FExposedEventsLinked: boolean;

    procedure SetExposed(Value: boolean);
    function GetExposedEvents(InEvent: boolean): TX3DEvent;
    procedure SetExposedEventsLinked(const Value: boolean);
  private
    type
      { Tracking of fading-in/out animation state on this particular field. }
      TPartialReceived = class
        { Strength of the fading-in/out value accumulated inside PartialValue.
          Reset each frame.

          Sending value with TPartialSend.Partial = X always adds X to this
          TPartialReceived.Partial, when send in the same frame.
          So e.g. sending two values, one with TPartialSend.Partial = 0.3,
          one with TPartialSend.Partial = 0.7, sets TPartialReceived.Partial = 1.0.

          At the end, PartialValue will be mixed (lerp) with SettledValue,
          to produce final Value.
          This way animation blending works in various cases:
          - New animations fades-in, old animation fades-out
            (at the end, Partial = 1.0).
          - New animations fades-in, no old animation fades-out
            (maybe fading-out animation does not affect this field,
            or maybe there is no fading-out animation)
            (at the end, Partial <= 1.0).
        }
        Partial: Single;

        { Accumulated (this frame) partial values. }
        PartialValue: TX3DField;

        { Last known value when animation blending did not occur. }
        SettledValue: TX3DField;

        { Should the PartialValue, SettledValue be used to determine
          field value this frame.

          Why do we need this?

            Checking this in InternalAffectedPartial avoids processing the field
            twice, in case it was added twice to PartialSend.AffectedFields
            (easily possible if it was under the influence of both old and new
            animations).

          Why not using Partial <> 0 for this?

            Because sometimes Partial may be 0.

            When starting a 1st animation, and it has TransitionDuration <> 0,
            then the SetTime in HandleChangeTimeStopStart
            activates the sensor, and the next SetTime (from Update)
            may produce PartialReceived instance with PartialReceived.Partial = 0.

            We *want* to apply this PartialReceived.Partial with 0.
            Otherwise, the field value would be set immediately to the 1st pose
            of new animation, disregarding smooth animation blending.

            Testcase: examples/play_animation/ , switch to sample 3D,
            increase transition = max, play "idle".
            When "Partial <> 0" is used instead of ToBeApplied,
            the 1st frame of "idle" quickly blinks at the beginning.

            We could hack it by not doing SetTime in HandleChangeTimeStopStart,
            but it's better to behave reliably in this case (Partial = 0) alwatys.

          This is now just a shortcut for "PartialValue <> nil", as this satisfies
          our needs.
        }
        function ToBeApplied: boolean;

        destructor Destroy; override;
      end;

    var
      { Internally set to non-nil only on fields representing event value
        (Value parameter in TX3DEventReceive callback),
        in case they correspond to a fading-in or fading-out animation. }
      FPartialSendIgnore: Cardinal; static;

      { Used by fields that have their value affected by the fading-in/fading-out
        animation. }
      PartialReceived: TPartialReceived;
  strict protected
    function SendsByEvent: boolean;

    { Save field value to a stream. Must be overriden for each specific
      field.

      For classic encoding, FieldSaveToStream and SaveToStream write
      Indent, Name, ' ', then call SaveToStreamValue, then write @link(NL).

      IS clauses are not saved by FieldSaveToStream or SaveToStream.
      (They must be saved specially, by SaveToStreamClassicIsClauses
      or special XML output.)
      SaveToStream still checks ValueFromIsClause, if ValueFromIsClause
      we will not call SaveToStreamValue. So when overriding
      SaveToStreamValue, you can safely assume that ValueFromIsClause
      is @false. }
    procedure SaveToStreamValue; virtual; abstract;

    { Call this inside overriden Assign methods.
      I don't want to place this inside TX3DField.Assign, since I want
      "inherited" in Assign methods to cause exception. }
    procedure VRMLFieldAssignCommon(Source: TX3DField);

    procedure AssignValueRaiseInvalidClass(Source: TX3DField);

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
    { Normal constructor.

      @italic(Descendants implementors notes:)
      when implementing constructors in descendants,
      remember that Create in this class actually just calls CreateUndefined,
      and CreateUndefined is virtual. So when calling @code(inherited Create),
      be aware that actually you may be calling your own overriden
      CreateUndefined.

      In fact, in descendants you should focus on moving all the work to
      CreateUndefined constructor.
      The Create constructor should be just a comfortable extension of
      CreateUndefined, that does the same and addiionally gets parameters
      that specify default field value. }
    constructor Create(const AParentNode: TX3DFileItem;
      const AExposed: boolean; const AName: string);

    { Virtual constructor, that you can use to construct field instance when
      field class is known only at runtime.

      The idea is that in some cases, you need to create fields using
      variable like FieldClass: TX3DFieldClass. See e.g. TX3DInterfaceDeclaration,
      VRML 2.0 feature that simply requires this ability, also
      implementation of TX3DSimpleMultField.Parse and
      TX3DSimpleMultField.CreateItemBeforeParse.

      Later you can initialize such instance from string using it's Parse method.

      Note that some exceptional fields simply cannot work when initialized
      by this constructor: these are SFEnum and SFBitMask fields.
      They simply need to know their TSFEnum.EnumNames, or
      TSFBitMask.FlagNames + TSFBitMask.NoneString + TSFBitMask.AllString
      before they can be parsed. I guess that's one of the reasons why these
      field types were entirely removed from VRML 2.0. }
    constructor CreateUndefined(const AParentNode: TX3DFileItem;
      const AExposed: boolean; const AName: string); virtual;

    destructor Destroy; override;

    { Parse inits properties from Lexer.

      In this class, Parse only appends to IsClauseNames:
      if we stand on "IS" clause (see VRML 2.0 spec about "IS" clause)
      and IsClauseAllowed then we append specified identifier to
      IsClauseNames.

      If "IS" clause not found, we call ParseValue which should
      actually parse field's value.
      Descendants should override ParseValue. }
    procedure Parse(Lexer: TX3DLexer; IsClauseAllowed: boolean);

    procedure ParseValue(Lexer: TX3DLexer); virtual; abstract;

    { Parse field value from X3D XML encoded attribute using a Lexer.
      Attributes in X3D are generally encoded such that normal
      @code(ParseValue(Lexer, nil)) call is appropriate,
      so this is done in this class. }
    procedure ParseXMLAttributeLexer(Lexer: TX3DLexer); virtual;

    { Parse field value from X3D XML encoded attribute.

      Implementation in this class creates a Lexer to parse the string,
      and calls ParseXMLAttributeLexer. }
    procedure ParseXMLAttribute(const AttributeValue: string); virtual;

    { Parse field's value from XML Element children.
      This is used to read SFNode / MFNode field value inside <field>
      (for interface declaration default field value) and <fieldValue>
      inside <ProtoInstance>. }
    procedure ParseXMLElement(Element: TDOMElement); virtual;

    { Save the field to the stream.
      Field name (if set, omitted if empty) and value are saved.
      Unless the current field value equals default value and
      FieldSaveWhenDefault is @false (default), then nothing is saved.

      IS clauses are not saved here (because they often have to be treated
      specially anyway, for XML encoding, for prototype declarations etc.). }
    procedure FieldSaveToStream(
      FieldSaveWhenDefault: boolean = false;
      XmlAvoidSavingNameBeforeValue: boolean = false);

    { Save the field to the stream.

      This simply calls FieldSaveToStream(Writer).
      See FieldSaveToStream for more comments and when you need control over
      FieldSaveWhenDefault behavior.

      It doesn't actually save anything if field value is defined
      and equals default value. }
    procedure SaveToStream; override;

    { Does current field value came from expanding "IS" clause.
      If yes, then saving this field to stream will only save it's "IS" clauses,
      never saving actual value. }
    property ValueFromIsClause: boolean
      read FValueFromIsClause write FValueFromIsClause;

    { Zwraca zawsze false w tej klasie. Mozesz to przedefiniowac w podklasach
      aby SaveToStream nie zapisywalo do strumienia pol o wartosci domyslnej. }
    function EqualsDefaultValue: boolean; virtual;

    { @true if the SecondValue object has exactly the same type and properties.
      For this class, this returns just (SecondValue.Name = Name).

      All descendants (that add some property that should be compared)
      should override this like

      @longCode(#
        Result := (inherited Equals(SecondValue)) and
          (SecondValue is TMyType) and
          (TMyType(SecondValue).MyProperty = MyProperty);
      #)

      The floating-point fields may be compared with a small epsilon
      tolerance by this method.

      Note that this *doesn't* compare the default values of two fields
      instances. This compares only the current values of two fields
      instances, and eventually some other properties that affect
      parsing (like names for TSFEnum and TSFBitMask) or allowed
      future values (like TSFFloat.MustBeNonnegative).
    }
    function Equals(SecondValue: TX3DField): boolean; virtual; reintroduce;

    { Compare value of this field, with other field, fast.

      This compares only the values of the fields, not other properties
      (it doesn't care about names of the fields or such, or default values;
      only current values). In other words, it compares only the things
      copied by AssignValue.

      This tries to compare very fast, which means that for large
      (multi-valued) fields it may give up and answer @false even
      when they are in fact equal. So this is usable only for optimization
      purposes: when it answers @true, it is @true. When it answers @false,
      it actually doesn't know.

      Default implementation in this class (@classname) just returns @false. }
    function FastEqualsValue(SecondValue: TX3DField): boolean; virtual;

    { Does this field generate/accept events, that is
      an "exposedField" (in VRML 2.0) or "inputOutput" (in X3D). }
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
    class function TypeName: string; deprecated 'use X3DType';

    { Create TX3DEvent descendant suitable as exposed event for this field. }
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; virtual;

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

    { Set field's default value from the current value.

      Note that for now this doesn't guarantee that every possible field's value
      can be stored as default value. In case of trouble, it will silently
      record "no default is known" information, so e.g. EqualsDefaultValue
      will always return @false.
      Our default value mechanisms are sometimes
      limited, not every value can be a default value. For example,
      for multiple-valued nodes, we usually cannot save arrays longer than
      one as default value. This is not a problem, since X3D specification
      doesn't specify too long default values. But it may be a problem
      for prototypes, since then user can assign any value as default value.
      May be corrected in the future. }
    procedure AssignDefaultValueFromValue; virtual;

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

    { Notify ParentNode.Scene that the value of this field changed. }
    procedure Changed;

    { What always happens when the value of this field changes.

      This is included in the @link(ExecuteChanges) method result. So instead of
      using this property, you could always override @link(ExecuteChanges)
      method. But often it's easier to use the property.

      By default this is an empty set. This is suitable for
      things that aren't *directly* an actual content (but only an
      intermediate value to change other stuff). This includes
      all metadata fields and nodes, all fields in event utilities,
      Script node, interpolators...

      See TX3DChange for possible values. }
    property ChangesAlways: TX3DChanges read FChangesAlways write FChangesAlways;

    { What happens when the value of this field changes.
      This is called, exactly once, by TCastleSceneCore.InternalChangedField
      to determine what must be done when we know that value of this field changed.

      In overridden descendants, this can also do something immediately.
      Overriding this is similar to registering your callback by
      @link(TX3DEvent.AddNotification EventOut.AddNotification), with two additional benefits:

      @orderedList(
        @item(This method may be not called (although no guarantees)
          when the actual field value did not change.
          In contrast, the event notification is always fired,
          even when you send the same value to an exposed field,
          because VRML/X3D events and routes must be fired anyway.)

        @item(This is useful also for fields that are not exposed,
          and can be changed only by ObjectPascal code.)
      )

      So overridding this is closer to "do something when field value changes"
      than registering notification by @link(TX3DEvent.AddNotification EventOut.AddNotification). }
    function ExecuteChanges: TX3DChanges; virtual;

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
  strict private
    function GetByName(const AName: string): TX3DField;
  public
    { Access field by name.
      Raises EX3DNotFound if the given Name doesn't exist. }
    property ByName[const AName: string]: TX3DField read GetByName;

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

function TX3DField.TPartialReceived.ToBeApplied: boolean;
begin
  Result := PartialValue <> nil;
end;

destructor TX3DField.TPartialReceived.Destroy;
begin
  FreeAndNil(SettledValue);
  FreeAndNil(PartialValue);
  inherited;
end;

{ TX3DField ------------------------------------------------------------- }

constructor TX3DField.Create(const AParentNode: TX3DFileItem;
  const AExposed: boolean; const AName: string);
begin
  CreateUndefined(AParentNode, AExposed, AName);
end;

constructor TX3DField.CreateUndefined(const AParentNode: TX3DFileItem;
  const AExposed: boolean; const AName: string);
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
  // FreeAndNil(FPartialSend); // do not free this, TCastleSceneCore frees it
  FreeAndNil(PartialReceived);
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

  { When not ValuePossiblyChanged, we don't have to call InternalChangedField.
    (Although we still have to call FExposedEvents[false].Send,
    to push the change through the routes.)
    This may be an important optimization when simple field's change
    causes large time-consuming work in InternalChangedField. }
  ValuePossiblyChanged := not FastEqualsValue(Value);

  { This is trivial handling of exposed events: just set our value,
    and call out event. }

  AssignValue(Value);

  FExposedEvents[false].Send(Value);

  { Tests:
  if not ValuePossiblyChanged then
    writeln('ignored field ', Name, ' change, since values the same'); }
  if ValuePossiblyChanged and
    // do not send Changed if it will be send later by InternalAffectedPartial
    not ((PartialReceived <> nil) and PartialReceived.ToBeApplied) then
    Changed;
end;

procedure TX3DField.Changed;
begin
end;

function TX3DField.ExecuteChanges: TX3DChanges;
begin
  Result := ChangesAlways;
end;

function TX3DField.SendsByEvent: boolean;
begin
  Result := Exposed ;
end;

procedure TX3DField.Send(Value: TX3DField);
var
  ValuePossiblyChanged: boolean;
begin
  if SendsByEvent then
  begin
    EventIn.Send(Value);
  end else
  begin
    ValuePossiblyChanged := not FastEqualsValue(Value);
    { Call AssignValue regardless of ValuePossiblyChanged.
      Reason: AssignValue also removes "IS" clause. }
    AssignValue(Value);
    if ValuePossiblyChanged then Changed;
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

class function TX3DField.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TX3DEvent.Create(AParentNode, AName, ExposedEventsFieldClass, AInEvent);
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
      FExposedEvents[false] := CreateEvent(nil, 'X3DName' + ChangedSuffix, false);
      FExposedEvents[false].ParentExposedField := Self;
      FExposedEvents[true] := CreateEvent(nil, SetPrefix + 'X3DName', true);
      FExposedEvents[true].ParentExposedField := Self;

      FreeAndNil(FExposedEvents[false]);
      FreeAndNil(FExposedEvents[true]);
    end;
  end;
end;

procedure TX3DField.FieldSaveToStream(FieldSaveWhenDefault, XmlAvoidSavingNameBeforeValue: boolean);
begin
end;

procedure TX3DField.SaveToStream;
begin
  FieldSaveToStream;
end;

function TX3DField.EqualsDefaultValue: boolean;
begin
  Result := false;
end;

function TX3DField.Equals(SecondValue: TX3DField): boolean;
begin
  Result := true;
end;

function TX3DField.FastEqualsValue(SecondValue: TX3DField): boolean;
begin
  Result := false;
end;

procedure TX3DField.Parse(Lexer: TX3DLexer;IsClauseAllowed: boolean);
begin
end;

procedure TX3DField.ParseXMLAttributeLexer(Lexer: TX3DLexer);
begin
end;

procedure TX3DField.ParseXMLAttribute(const AttributeValue: string);
var
  Lexer: TX3DLexer;
  A: TX3dVersion;
begin
  A.Major := 1;
  A.Minor := 1;
  Lexer := TX3DLexer.CreateForPartialStream(AttributeValue, A);
  try
    try
      ParseXMLAttributeLexer(Lexer);
    except
      ;
    end;
  finally FreeAndNil(Lexer) end;
end;

procedure TX3DField.ParseXMLElement(Element: TDOMElement);
begin
end;

procedure TX3DField.VRMLFieldAssignCommon(Source: TX3DField);
begin
  ValueFromIsClause := Source.ValueFromIsClause;

  Exposed := Source.Exposed;
  Assert(Exposed = (ExposedEvents[false] <> nil));
  Assert(Exposed = (ExposedEvents[true] <> nil));

  { This is a little tricky: we copied Exposed value by SetExposed,
    to actually create or destroy exposed events.

    But note that events in
    ExposedEvents have names dependent on our name. So we have to eventually
    change their names too. This is not needed if exposed
    changes from true->false (then events will be destroyed),
    changes from false->true (then events will be created with already new names),
    stays as false->false (then events don't exist).
    So it's needed only when exposed was true, and stays true, but name changed.
  }

  { Once again an issue with dependency of ExposedEvents on our name:
    potentially alternative names changed,
    so we have to redo this in exposed events. }
end;

procedure TX3DField.AssignValueRaiseInvalidClass(Source: TX3DField);
begin
  raise Exception.CreateFmt('Cannot assign VRML/X3D field ' +
    '%s (%s) from %s (%s)',
    [        'X3DName',        X3DType,
      'Source.X3DName', Source.X3DType]);
end;

procedure TX3DField.AssignValue(Source: TX3DField);
begin
  ValueFromIsClause := false;
end;

procedure TX3DField.AssignDefaultValueFromValue;
begin
  { do nothing in this class }
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

class function TX3DField.TypeName: string;
begin
  Result := X3DType;
end;

{ TX3DFieldList ------------------------------------------------------------- }

function TX3DFieldList.IndexOfName(const AName: string): integer;
begin
  Result := -1;
end;

function TX3DFieldList.GetByName(const AName: string): TX3DField;
var
  I: integer;
begin
  I := IndexOfName(AName);
  if I <> -1 then
    Result := Items[I] else
    raise Exception.CreateFmt('Field name "%s" not found', [AName]);
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

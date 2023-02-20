{
  Copyright 2007-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { X3D event. }
  TX3DEvent = class(TX3DFileItem)
  strict private
    FFieldClass: TX3DFieldClass;
    FInEvent: boolean;
    { To avoid allocating memory for FNotifications list in the most-common case
      when 0 or 1 notification is enough, store 1st notification in special
      field instead. }
    //ATest: Integer; // uncomment to get internal error
    FParentExposedField: TX3DField;
    Temp: TX3DField;
    TempUsed: boolean;
  public
    constructor Create(AParentNode: TX3DFileItem;
      const AName: string;
      const AFieldClass: TX3DFieldClass; const AInEvent: boolean);
    destructor Destroy; override;

    { Internal helper utility for SendXxx implementations. @exclude }
    procedure FreeTemp(const Field: TX3DField);

    property FieldClass: TX3DFieldClass read FFieldClass;

    { @abstract(Is it "in" or "out" event ?) }
    property InEvent: boolean read FInEvent;

    { This only reads (optional) "IS" clause of the event, as may occur
      in VRML nodeBodyStatement. }
    procedure Parse(Lexer: TX3DLexer);

    procedure SaveToStream; override;

    { If this event is an exposed event belonging to some field,
      this references parent field. Otherwise it's @nil. }
    property ParentExposedField: TX3DField
      read FParentExposedField write FParentExposedField;

    { Send a value to event. For input fields, this is used by routes, scripts
      etc. to send an event to a field. For output fields, this is used by
      node itself to send event to routes, scripts etc.

      Field must be non-nil, of class FieldClass.

      The Field instance doesn't become owned in any way by the TX3DEvent.
      That is, it's the caller responsibility to free the Field instance
      at any comfortable time, possibly right after calling Send.

      Overloaded versions without explicit Time parameter just take time
      from ParentNode.Scene.GetTime. If ParentNode is @nil
      (which should not happen with normal fields within nodes)
      or if ParentNode.Scene is @nil (which may happen only if events
      processing is not turned on, that is TCastleSceneCore.ProcessEvents is @false)
      then event is not send. }
    procedure Send(Field: TX3DField); overload;

    { @abstract(Is anything actually listening on this events @link(Send)?)

      Sometimes, even preparing a value to Send is quite time-consuming
      (example: CoordinateInterpolator, where a long MFVec3f value
      has to be computer). Then checking SendNeeded is useful:
      if SendNeeded = @false, you know that there's no point in preparing
      Value to send, because actually Send will do nothing.

      For example, event out to which no ROUTE is connected, and no Script
      can listen to it.

      For now, this simply returns whether any callback is registered
      by AddNotification. }
    function SendNeeded: boolean;

  end;

  TX3DEventList = class(specialize TObjectList<TX3DEvent>)
  strict private
    function GetByName(const AName: string): TX3DEvent;
  public
    { Access event by name.
      Raises EX3DNotFound if the given Name doesn't exist. }
    property ByName[const AName: string]: TX3DEvent read GetByName;

    { Find event index on the list. -1 if not found. }
    function IndexOfName(const Name: string): Integer;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TX3DEvent ----------------------------------------------------------------- }

constructor TX3DEvent.Create(AParentNode: TX3DFileItem;
  const AName: string; const AFieldClass: TX3DFieldClass;
  const AInEvent: boolean);
begin
  inherited Create;
  FFieldClass := AFieldClass;
  FInEvent := AInEvent;
end;

destructor TX3DEvent.Destroy;
begin
  FreeAndNil(Temp);
  inherited;
end;

procedure TX3DEvent.Parse(Lexer: TX3DLexer);
begin
end;

procedure TX3DEvent.SaveToStream;
begin
  { saves nothing. IS clauses are saved separately by SaveToStreamIsClauses }
end;

procedure TX3DEvent.Send(Field: TX3DField);
begin
  Assert(Field <> nil);
  Assert(Field is FieldClass);
end;

function TX3DEvent.SendNeeded: boolean;
begin
  Result := false;
end;

procedure TX3DEvent.FreeTemp(const Field: TX3DField);
begin
  if Field = Temp then
    TempUsed := false else
    Field.Free;
end;

{ TX3DEventList ------------------------------------------------------------ }

function TX3DEventList.IndexOfName(const Name: string): Integer;
begin
  Result := -1;
end;

function TX3DEventList.GetByName(const AName: string): TX3DEvent;
var
  I: integer;
begin
  I := IndexOfName(AName);
  if I <> -1 then
    Result := Items[I] else
    raise Exception.CreateFmt('Event name "%s" not found', [AName]);
end;

{$endif read_implementation}

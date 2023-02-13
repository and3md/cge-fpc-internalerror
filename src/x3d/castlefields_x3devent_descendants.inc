{
  Copyright 2013-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TX3DEvent descendants with comfortable Send methods.
  Note that it is *not* necessary to use descendants below,
  instead you can create and use direct TX3DEvent instances.
  The important thing is to set FieldClass right.

  These descendants only add easy Send methods, that can be type-checked
  at compile-time. They are all just shortcuts for calling
  the @link(Send Send(TX3DField, TX3DTime)) method.
  They create a temporary instance of TX3DField for this purpose
  (with field name equal to event name, this is useful for debugging
  --- e.g. by Logger node). }

{$ifdef read_interface}
  { }
  TSFBoolEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
    procedure Send(const Value: boolean; const Time: TX3DTime; const PartialSend: TPartialSend = nil); overload;
    procedure Send(const Value: boolean); overload;
  end;

  TSFDoubleEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
    procedure Send(const Value: Double; const Time: TX3DTime; const PartialSend: TPartialSend = nil); overload;
    procedure Send(const Value: Double); overload;
  end;

  TSFFloatEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
    procedure Send(const Value: Single; const Time: TX3DTime; const PartialSend: TPartialSend = nil); overload;
    procedure Send(const Value: Single); overload;
  end;

  TSFImageEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TSFLongEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
    procedure Send(const Value: LongInt; const Time: TX3DTime; const PartialSend: TPartialSend = nil); overload;
    procedure Send(const Value: LongInt); overload;
  end;

  TSFInt32Event = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
    procedure Send(const Value: LongInt; const Time: TX3DTime; const PartialSend: TPartialSend = nil); overload;
    procedure Send(const Value: LongInt); overload;
  end;

  TSFNodeEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TSFStringEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
    procedure Send(const Value: string; const Time: TX3DTime; const PartialSend: TPartialSend = nil); overload;
    procedure Send(const Value: string); overload;
  end;

  { Universal input event that can receive value of any type.
    It is used by some InstantReality extensions, like Logger.write.
    InstantReality calls this event type XFAny. }
  TXFAnyEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFBoolEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFDoubleEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFTimeEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFFloatEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFLongEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFInt32Event = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFMatrix3dEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFMatrix3fEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFMatrix4dEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFMatrix4fEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFNodeEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFRotationEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFStringEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFVec2dEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFVec2fEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFVec3dEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFVec3fEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFVec4dEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFVec4fEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFColorEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFColorRGBAEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;
{$endif read_interface}

{$ifdef read_implementation}

{ TSFBoolEvent --------------------------------------------------------------- }

constructor TSFBoolEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TSFBool, AInEvent);
end;

procedure TSFBoolEvent.Send(const Value: boolean; const Time: TX3DTime; const PartialSend: TPartialSend);
var
  Field: TX3DField;
begin
  Field := CreateTemp(PartialSend);
  (Field as TSFBool).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TSFBoolEvent.Send(const Value: boolean);
var
  a:TX3dTime;
begin
  if (ParentNode <> nil)
      then
    Send(Value, a);
end;

{ TSFDoubleEvent ------------------------------------------------------------- }

constructor TSFDoubleEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TSFDouble, AInEvent);
end;

procedure TSFDoubleEvent.Send(const Value: Double; const Time: TX3DTime; const PartialSend: TPartialSend);
var
  Field: TX3DField;
begin
  Field := CreateTemp(PartialSend);
  (Field as TSFDouble).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TSFDoubleEvent.Send(const Value: Double);
var
  a: TX3dTime;
begin
  if (ParentNode <> nil)
     then
    Send(Value, a);
end;

{ TSFFloatEvent -------------------------------------------------------------- }

constructor TSFFloatEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TSFFloat, AInEvent);
end;

procedure TSFFloatEvent.Send(const Value: Single; const Time: TX3DTime; const PartialSend: TPartialSend);
var
  Field: TX3DField;
begin
  Field := CreateTemp(PartialSend);
  (Field as TSFFloat).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TSFFloatEvent.Send(const Value: Single);
var
  a: TX3dTime;
begin
  if (ParentNode <> nil)  then
    Send(Value, a);
end;

{ TSFImageEvent -------------------------------------------------------------- }

constructor TSFImageEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  /// inherited Create(AParentNode, AName, TSFImage, AInEvent);
end;

{ TSFLongEvent --------------------------------------------------------------- }

constructor TSFLongEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TSFLong, AInEvent);
end;

procedure TSFLongEvent.Send(const Value: LongInt; const Time: TX3DTime; const PartialSend: TPartialSend);
var
  Field: TX3DField;
begin
  Field := CreateTemp(PartialSend);
  (Field as TSFLong).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TSFLongEvent.Send(const Value: LongInt);
var
  a: TX3dTime;
begin
  if (ParentNode <> nil)
      then
    Send(Value, a);
end;

{ TSFInt32Event -------------------------------------------------------------- }

constructor TSFInt32Event.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TSFInt32, AInEvent);
end;

procedure TSFInt32Event.Send(const Value: LongInt; const Time: TX3DTime; const PartialSend: TPartialSend);
var
  Field: TX3DField;
begin
  Field := CreateTemp(PartialSend);
  (Field as TSFInt32).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TSFInt32Event.Send(const Value: LongInt);
var
  a: TX3dTime;
begin
  if (ParentNode <> nil) then
    Send(Value, a);
end;

{ TSFNodeEvent --------------------------------------------------------------- }

constructor TSFNodeEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TSFNode, AInEvent);
end;

{ TSFRotationEvent ----------------------------------------------------------- }

{ TSFStringEvent ------------------------------------------------------------- }

constructor TSFStringEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TSFString, AInEvent);
end;

procedure TSFStringEvent.Send(const Value: string; const Time: TX3DTime; const PartialSend: TPartialSend);
var
  Field: TX3DField;
begin
  Field := CreateTemp(PartialSend);
  (Field as TSFString).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TSFStringEvent.Send(const Value: string);
var
  a: TX3dTime;
begin
  if (ParentNode <> nil) then
    Send(Value, a);
end;

{ TSFVec2dEvent -------------------------------------------------------------- }

{ TSFColorEvent -------------------------------------------------------------- }

{ TSFColorRGBAEvent ---------------------------------------------------------- }

{ ---------------------------------------------------------------------------- }

constructor TXFAnyEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TX3DField, AInEvent);
end;

constructor TMFBoolEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFBool, AInEvent);
end;

constructor TMFDoubleEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFDouble, AInEvent);
end;

constructor TMFTimeEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFTime, AInEvent);
end;

constructor TMFFloatEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFFloat, AInEvent);
end;

constructor TMFLongEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFLong, AInEvent);
end;

constructor TMFInt32Event.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFInt32, AInEvent);
end;

constructor TMFMatrix3dEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFInt32, AInEvent);
end;

constructor TMFMatrix3fEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFInt32, AInEvent);
end;

constructor TMFMatrix4dEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFInt32, AInEvent);
end;

constructor TMFMatrix4fEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFInt32, AInEvent);
end;

constructor TMFNodeEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFNode, AInEvent);
end;

constructor TMFRotationEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFInt32, AInEvent);
end;

constructor TMFStringEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFString, AInEvent);
end;

constructor TMFVec2dEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFInt32, AInEvent);
end;

constructor TMFVec2fEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFInt32, AInEvent);
end;

constructor TMFVec3dEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFInt32, AInEvent);
end;

constructor TMFVec3fEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFInt32, AInEvent);
end;

constructor TMFVec4dEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFInt32, AInEvent);
end;

constructor TMFVec4fEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFInt32, AInEvent);
end;

constructor TMFColorEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFInt32, AInEvent);
end;

constructor TMFColorRGBAEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFInt32, AInEvent);
end;

{$endif read_implementation}

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
    procedure Send(const Value: boolean); overload;
  end;

  TSFDoubleEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
    procedure Send(const Value: Double); overload;
  end;

  TSFFloatEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
    procedure Send(const Value: Single); overload;
  end;

  TSFNodeEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TSFStringEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
    procedure Send(const Value: string); overload;
  end;

  { Universal input event that can receive value of any type.
    It is used by some InstantReality extensions, like Logger.write.
    InstantReality calls this event type XFAny. }
  TXFAnyEvent = class(TX3DEvent)
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

procedure TSFBoolEvent.Send(const Value: boolean);
begin
  if (ParentNode <> nil)
      then
    Send(Value);
end;

{ TSFDoubleEvent ------------------------------------------------------------- }

constructor TSFDoubleEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TSFDouble, AInEvent);
end;

procedure TSFDoubleEvent.Send(const Value: Double);
begin
  if (ParentNode <> nil)
     then
    Send(Value);
end;

{ TSFFloatEvent -------------------------------------------------------------- }

constructor TSFFloatEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TSFFloat, AInEvent);
end;

procedure TSFFloatEvent.Send(const Value: Single);
begin
  if (ParentNode <> nil)  then
    Send(Value);
end;

{ TSFLongEvent --------------------------------------------------------------- }


{ TSFInt32Event -------------------------------------------------------------- }


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

procedure TSFStringEvent.Send(const Value: string);
begin
end;

{ TSFVec2dEvent -------------------------------------------------------------- }

{ TSFColorEvent -------------------------------------------------------------- }

{ TSFColorRGBAEvent ---------------------------------------------------------- }

{ ---------------------------------------------------------------------------- }

constructor TXFAnyEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TX3DField, AInEvent);
end;

{$endif read_implementation}

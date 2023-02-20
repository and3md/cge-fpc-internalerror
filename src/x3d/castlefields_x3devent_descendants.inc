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
  TSFNodeEvent = class(TX3DEvent)
  public
    constructor Create;
  end;

{$endif read_interface}

{$ifdef read_implementation}

constructor TSFNodeEvent.Create;
begin
  inherited Create(TSFNode);
end;

{ ---------------------------------------------------------------------------- }

{$endif read_implementation}

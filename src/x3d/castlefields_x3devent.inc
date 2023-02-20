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
    //ATest: Integer; // uncomment to get internal error
  public
    constructor Create(const AFieldClass: TX3DFieldClass);

    property FieldClass: TX3DFieldClass read FFieldClass;

    procedure Parse(Lexer: TX3DLexer);

    procedure Send(Field: TX3DField); overload;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TX3DEvent ----------------------------------------------------------------- }

constructor TX3DEvent.Create(const AFieldClass: TX3DFieldClass);
begin
  inherited Create;
  FFieldClass := AFieldClass;
end;

procedure TX3DEvent.Parse(Lexer: TX3DLexer);
begin
end;

procedure TX3DEvent.Send(Field: TX3DField);
begin
  Assert(Field <> nil);
  Assert(Field is FieldClass);
end;

{ TX3DEventList ------------------------------------------------------------ }

{$endif read_implementation}

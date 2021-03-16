------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------
--  UTF-8 decoder. Implementation is conformant with W3C/Unicode requirements
--  to process ill-formed sequences with substitutions of U+FFFD REPLACEMENT
--  CHARACTER.

private package VSS.Strings.Converters.Decoders.UTF8 is

   type UTF8_Decoder is new Abstract_Decoder with private;

private

   type UTF8_Decoder is new Abstract_Decoder with record
      Flags    : Converter_Flags;
      Code     : VSS.Unicode.Code_Point;
      Needed   : VSS.Unicode.UTF8_Code_Unit_Count;
      Seen     : VSS.Unicode.UTF8_Code_Unit_Count;
      Lower    : Ada.Streams.Stream_Element;
      Upper    : Ada.Streams.Stream_Element;
      Error    : Boolean;
      Skip_BOM : Boolean;
   end record;

   overriding procedure Initialize
     (Self  : in out UTF8_Decoder;
      Flags : Converter_Flags);

   overriding procedure Decode
     (Self   : in out UTF8_Decoder;
      Source : Ada.Streams.Stream_Element_Array;
      Target : out VSS.Implementation.Strings.String_Data);

   overriding function Has_Error (Self : UTF8_Decoder) return Boolean;

   overriding function Error_Message
     (Self : UTF8_Decoder) return VSS.Strings.Virtual_String;

   overriding procedure Reset_State (Self : in out UTF8_Decoder);

end VSS.Strings.Converters.Decoders.UTF8;

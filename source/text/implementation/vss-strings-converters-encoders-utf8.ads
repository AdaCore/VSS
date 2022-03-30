------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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
--  UTF-8 encoder.

private package VSS.Strings.Converters.Encoders.UTF8 is

   type UTF8_Encoder is new Abstract_Encoder with private;

private

   type UTF8_Encoder is new Abstract_Encoder with record
      Flags       : Converter_Flags;
      BOM_Written : Boolean;
   end record;

   overriding procedure Initialize
     (Self  : in out UTF8_Encoder;
      Flags : Converter_Flags);

   overriding procedure Encode
     (Self   : in out UTF8_Encoder;
      Source : VSS.Unicode.Code_Point;
      Target : in out VSS.Stream_Element_Vectors.Stream_Element_Vector'Class);

   overriding function Has_Error (Self : UTF8_Encoder) return Boolean;

   overriding function Error_Message
     (Self : UTF8_Encoder) return VSS.Strings.Virtual_String;

   overriding procedure Reset_State (Self : in out UTF8_Encoder);

end VSS.Strings.Converters.Encoders.UTF8;

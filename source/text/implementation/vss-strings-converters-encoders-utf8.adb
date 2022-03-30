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

with VSS.Implementation.UTF8_Encoding;

package body VSS.Strings.Converters.Encoders.UTF8 is

   ------------
   -- Encode --
   ------------

   overriding procedure Encode
     (Self   : in out UTF8_Encoder;
      Source : VSS.Unicode.Code_Point;
      Target : in out VSS.Stream_Element_Vectors.Stream_Element_Vector'Class)
   is
      Size    : VSS.Implementation.UTF8_Encoding.UTF8_Sequence_Length;
      Encoded :
        VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
          (VSS.Implementation.UTF8_Encoding.UTF8_Sequence_Length'Range);
      Buffer  : Ada.Streams.Stream_Element_Array
                  (Ada.Streams.Stream_Element_Offset (Encoded'First)
                     .. Ada.Streams.Stream_Element_Offset (Encoded'Last))
        with Address => Encoded'Address;

   begin
      if not Self.BOM_Written then
         Self.BOM_Written := True;
         Self.Encode (Zero_Width_No_Break_Space_Character, Target);
      end if;

      VSS.Implementation.UTF8_Encoding.Encode
        (Source,
         Size,
         Encoded (1),
         Encoded (2),
         Encoded (3),
         Encoded (4));

      for J in Buffer'First .. Ada.Streams.Stream_Element_Offset (Size) loop
         Target.Append (Buffer (J));
      end loop;
   end Encode;

   -------------------
   -- Error_Message --
   -------------------

   overriding function Error_Message
     (Self : UTF8_Encoder) return VSS.Strings.Virtual_String is
   begin
      return VSS.Strings.Empty_Virtual_String;
   end Error_Message;

   ---------------
   -- Has_Error --
   ---------------

   overriding function Has_Error (Self : UTF8_Encoder) return Boolean is
   begin
      return False;
   end Has_Error;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Self  : in out UTF8_Encoder;
      Flags : Converter_Flags) is
   begin
      Self.Flags := Flags;
      Self.Reset_State;
   end Initialize;

   -----------------
   -- Reset_State --
   -----------------

   overriding procedure Reset_State (Self : in out UTF8_Encoder) is
   begin
      Self.BOM_Written := not Self.Flags (Process_BOM);
   end Reset_State;

end VSS.Strings.Converters.Encoders.UTF8;

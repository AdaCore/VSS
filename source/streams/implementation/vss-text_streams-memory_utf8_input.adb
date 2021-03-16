------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                    Copyright (C) 2020-2021, AdaCore                      --
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

with VSS.Strings;
with VSS.Unicode;

package body VSS.Text_Streams.Memory_UTF8_Input is

   use type Ada.Streams.Stream_Element_Offset;

   -------------------
   -- Error_Message --
   -------------------

   overriding function Error_Message
     (Self : Memory_UTF8_Input_Stream) return VSS.Strings.Virtual_String is
   begin
      case Self.Error is
         when VSS.Implementation.UTF8_Encoding.None =>
            return VSS.Strings.Empty_Virtual_String;

         when VSS.Implementation.UTF8_Encoding.Incomplete_2 =>
            return
              VSS.Strings.To_Virtual_String
                ("incomplete two code unit sequence");

         when VSS.Implementation.UTF8_Encoding.Incomplete_3 =>
            return
              VSS.Strings.To_Virtual_String
                ("incomplete three code unit sequence");

         when VSS.Implementation.UTF8_Encoding.Incomplete_4 =>
            return
              VSS.Strings.To_Virtual_String
                ("incomplete four code unit sequence");

         when VSS.Implementation.UTF8_Encoding.Invalid_1 =>
            return
              VSS.Strings.To_Virtual_String
                ("invalid UTF-8 sequence (wrong start code unit)");

         when VSS.Implementation.UTF8_Encoding.Invalid_2_Of_2 =>
            return
              VSS.Strings.To_Virtual_String
                ("invalid UTF-8 sequence (wrong second code unit of"
                 & " two code units sequene)");

         when VSS.Implementation.UTF8_Encoding.Invalid_2_Of_3 =>
            return
              VSS.Strings.To_Virtual_String
                ("invalid UTF-8 sequence (wrong second code unit of"
                 & " three code units sequene)");

         when VSS.Implementation.UTF8_Encoding.Invalid_3_Of_3 =>
            return
              VSS.Strings.To_Virtual_String
                ("invalid UTF-8 sequence (wrong third code unit of"
                 & " three code units sequene)");

         when VSS.Implementation.UTF8_Encoding.Invalid_2_Of_4 =>
            return
              VSS.Strings.To_Virtual_String
                ("invalid UTF-8 sequence (wrong second code unit of"
                 & " four code units sequene)");

         when VSS.Implementation.UTF8_Encoding.Invalid_3_Of_4 =>
            return
              VSS.Strings.To_Virtual_String
                ("invalid UTF-8 sequence (wrong third code unit of"
                 & " four code units sequene)");

         when VSS.Implementation.UTF8_Encoding.Invalid_4_Of_4 =>
            return
              VSS.Strings.To_Virtual_String
                ("invalid UTF-8 sequence (wrong forth code unit of"
                 & " four code units sequene)");
      end case;
   end Error_Message;

   ---------
   -- Get --
   ---------

   overriding procedure Get
     (Self    : in out Memory_UTF8_Input_Stream;
      Item    : out VSS.Characters.Virtual_Character;
      Success : in out Boolean)
   is
      Code : VSS.Unicode.Code_Point;

   begin
      VSS.Implementation.UTF8_Encoding.Decode
        (Self.Buffer, Self.Current, Code, Success, Self.Error);
      Item := VSS.Characters.Virtual_Character'Val (Code);
   end Get;

   ---------------
   -- Has_Error --
   ---------------

   overriding function Has_Error
     (Self : Memory_UTF8_Input_Stream) return Boolean
   is
      use type VSS.Implementation.UTF8_Encoding.UTF8_Decode_Error;

   begin
      return Self.Error /= VSS.Implementation.UTF8_Encoding.None;
   end Has_Error;

   --------------------
   -- Is_End_Of_Data --
   --------------------

   overriding function Is_End_Of_Data
     (Self : Memory_UTF8_Input_Stream) return Boolean is
   begin
      if Self.Current > Self.Buffer.Length then
         return False;
      end if;

      return True;
   end Is_End_Of_Data;

   ----------------------
   -- Is_End_Of_Stream --
   ----------------------

   overriding function Is_End_Of_Stream
     (Self : Memory_UTF8_Input_Stream) return Boolean is
   begin
      return Self.Current > Self.Buffer.Length;
   end Is_End_Of_Stream;

   ------------
   -- Rewind --
   ------------

   procedure Rewind (Self : in out Memory_UTF8_Input_Stream'Class) is
   begin
      Self.Current := 1;
      Self.Error   := VSS.Implementation.UTF8_Encoding.None;
   end Rewind;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Self : in out Memory_UTF8_Input_Stream'Class;
      Data : VSS.Stream_Element_Vectors.Stream_Element_Vector) is
   begin
      Self.Buffer  := Data;
      Self.Current := 1;
      Self.Error   := VSS.Implementation.UTF8_Encoding.None;
   end Set_Data;

end VSS.Text_Streams.Memory_UTF8_Input;

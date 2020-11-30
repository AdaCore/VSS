------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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

with VSS.Strings.Conversions;
with VSS.Implementation.UTF8_Encoding;
with VSS.Unicode;

package body VSS.Text_Streams.Memory is

   use type Ada.Streams.Stream_Element_Offset;

   -------------------
   -- Error_Message --
   -------------------

   overriding function Error_Message
     (Self : Memory_UTF8_Input_Stream) return VSS.Strings.Virtual_String is
   begin
      return Self.Diagnosis;
   end Error_Message;

   ---------
   -- Get --
   ---------

   overriding procedure Get
     (Self    : in out Memory_UTF8_Input_Stream;
      Item    : out VSS.Characters.Virtual_Character;
      Success : in out Boolean)
   is
      procedure Report_Error (Message : String);

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error (Message : String) is
      begin
         Success := False;
         Item    := VSS.Characters.Virtual_Character'Val (0);

         Self.Diagnosis := VSS.Strings.Conversions.To_Virtual_String (Message);
      end Report_Error;

      Code  : VSS.Unicode.Code_Point;
      Error : VSS.Implementation.UTF8_Encoding.UTF8_Decode_Error;

   begin
      VSS.Implementation.UTF8_Encoding.Decode
        (Self.Buffer, Self.Current, Code, Success, Error);

      Item := VSS.Characters.Virtual_Character'Val (Code);

      case Error is
         when VSS.Implementation.UTF8_Encoding.None =>
            null;

         when VSS.Implementation.UTF8_Encoding.Incomplete_2 =>
            Report_Error ("incomplete two code unit sequence");

         when VSS.Implementation.UTF8_Encoding.Incomplete_3 =>
            Report_Error ("incomplete three code unit sequence");

         when VSS.Implementation.UTF8_Encoding.Incomplete_4 =>
            Report_Error ("incomplete four code unit sequence");

         when VSS.Implementation.UTF8_Encoding.Invalid_1 =>
            Report_Error
              ("invalid UTF-8 sequence (wrong start code unit)");

         when VSS.Implementation.UTF8_Encoding.Invalid_2_Of_2 =>
            Report_Error
              ("invalid UTF-8 sequence (wrong second code unit of"
               & " two code units sequene)");

         when VSS.Implementation.UTF8_Encoding.Invalid_2_Of_3 =>
            Report_Error
              ("invalid UTF-8 sequence (wrong second code unit of"
               & " three code units sequene)");

         when VSS.Implementation.UTF8_Encoding.Invalid_3_Of_3 =>
            Report_Error
              ("invalid UTF-8 sequence (wrong third code unit of"
               & " three code units sequene)");

         when VSS.Implementation.UTF8_Encoding.Invalid_2_Of_4 =>
            Report_Error
              ("invalid UTF-8 sequence (wrong second code unit of"
               & " four code units sequene)");

         when VSS.Implementation.UTF8_Encoding.Invalid_3_Of_4 =>
            Report_Error
              ("invalid UTF-8 sequence (wrong third code unit of"
               & " four code units sequene)");

         when VSS.Implementation.UTF8_Encoding.Invalid_4_Of_4 =>
            Report_Error
              ("invalid UTF-8 sequence (wrong forth code unit of"
               & " four code units sequene)");
      end case;
   end Get;

   ---------------
   -- Has_Error --
   ---------------

   overriding function Has_Error
     (Self : Memory_UTF8_Input_Stream) return Boolean is
   begin
      return not Self.Diagnosis.Is_Null;
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

   ---------
   -- Put --
   ---------

   overriding procedure Put
     (Self    : in out Memory_UTF8_Output_Stream;
      Item    : VSS.Characters.Virtual_Character;
      Success : in out Boolean)
   is
      pragma Unreferenced (Success);

      use type VSS.Implementation.UTF8_Encoding.UTF8_Sequence_Length;

      Code : constant VSS.Unicode.Code_Point :=
        VSS.Characters.Virtual_Character'Pos (Item);
      L    : VSS.Implementation.UTF8_Encoding.UTF8_Sequence_Length;
      U1   : VSS.Unicode.UTF8_Code_Unit;
      U2   : VSS.Unicode.UTF8_Code_Unit;
      U3   : VSS.Unicode.UTF8_Code_Unit;
      U4   : VSS.Unicode.UTF8_Code_Unit;

   begin
      VSS.Implementation.UTF8_Encoding.Encode (Code, L, U1, U2, U3, U4);

      Self.Buffer.Append (Ada.Streams.Stream_Element (U1));

      if L >= 2 then
         Self.Buffer.Append (Ada.Streams.Stream_Element (U2));

         if L >= 3 then
            Self.Buffer.Append (Ada.Streams.Stream_Element (U3));

            if L = 4 then
               Self.Buffer.Append (Ada.Streams.Stream_Element (U4));
            end if;
         end if;
      end if;
   end Put;

end VSS.Text_Streams.Memory;

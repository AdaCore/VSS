--
--  Copyright (C) 2020-2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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
                 & " two code units sequence)");

         when VSS.Implementation.UTF8_Encoding.Invalid_2_Of_3 =>
            return
              VSS.Strings.To_Virtual_String
                ("invalid UTF-8 sequence (wrong second code unit of"
                 & " three code units sequence)");

         when VSS.Implementation.UTF8_Encoding.Invalid_3_Of_3 =>
            return
              VSS.Strings.To_Virtual_String
                ("invalid UTF-8 sequence (wrong third code unit of"
                 & " three code units sequence)");

         when VSS.Implementation.UTF8_Encoding.Invalid_2_Of_4 =>
            return
              VSS.Strings.To_Virtual_String
                ("invalid UTF-8 sequence (wrong second code unit of"
                 & " four code units sequence)");

         when VSS.Implementation.UTF8_Encoding.Invalid_3_Of_4 =>
            return
              VSS.Strings.To_Virtual_String
                ("invalid UTF-8 sequence (wrong third code unit of"
                 & " four code units sequence)");

         when VSS.Implementation.UTF8_Encoding.Invalid_4_Of_4 =>
            return
              VSS.Strings.To_Virtual_String
                ("invalid UTF-8 sequence (wrong forth code unit of"
                 & " four code units sequence)");
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

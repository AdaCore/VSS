--
--  Copyright (C) 2020, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Implementation.UTF8_Encoding;
with VSS.Strings.Conversions;
with VSS.Unicode;

package body Tests_Text_Streams is

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
      if Self.Incremental then
         if Self.Skip then
            Self.Skip := False;
            Success := False;
            Item := VSS.Characters.Virtual_Character'Val (0);

            return;

         else
            Self.Skip := True;
         end if;
      end if;

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
               & " two code units sequence)");

         when VSS.Implementation.UTF8_Encoding.Invalid_2_Of_3 =>
            Report_Error
              ("invalid UTF-8 sequence (wrong second code unit of"
               & " three code units sequence)");

         when VSS.Implementation.UTF8_Encoding.Invalid_3_Of_3 =>
            Report_Error
              ("invalid UTF-8 sequence (wrong third code unit of"
               & " three code units sequence)");

         when VSS.Implementation.UTF8_Encoding.Invalid_2_Of_4 =>
            Report_Error
              ("invalid UTF-8 sequence (wrong second code unit of"
               & " four code units sequence)");

         when VSS.Implementation.UTF8_Encoding.Invalid_3_Of_4 =>
            Report_Error
              ("invalid UTF-8 sequence (wrong third code unit of"
               & " four code units sequence)");

         when VSS.Implementation.UTF8_Encoding.Invalid_4_Of_4 =>
            Report_Error
              ("invalid UTF-8 sequence (wrong forth code unit of"
               & " four code units sequence)");
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
      use type VSS.Implementation.UTF8_Encoding.UTF8_Sequence_Length;
      use type VSS.Strings.Character_Count;

      Code : constant VSS.Unicode.Code_Point :=
        VSS.Characters.Virtual_Character'Pos (Item);
      L    : VSS.Implementation.UTF8_Encoding.UTF8_Sequence_Length;
      U1   : VSS.Unicode.UTF8_Code_Unit;
      U2   : VSS.Unicode.UTF8_Code_Unit;
      U3   : VSS.Unicode.UTF8_Code_Unit;
      U4   : VSS.Unicode.UTF8_Code_Unit;

   begin
      if Self.Count >= Self.Limit then
         Success := False;

         return;

      else
         Self.Count := Self.Count + 1;
      end if;

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

   ---------
   -- Put --
   ---------

   overriding procedure Put
     (Self    : in out String_Output_Stream;
      Item    : VSS.Characters.Virtual_Character;
      Success : in out Boolean)
   is
      use type VSS.Strings.Character_Count;

   begin
      if Self.Buffer.Character_Length >= Self.Limit then
         Success := False;

         return;

      else
         Self.Buffer.Append (Item);
      end if;
   end Put;

   ---------------------
   -- Set_Incremental --
   ---------------------

   procedure Set_Incremental
     (Self : in out Memory_UTF8_Input_Stream'Class;
      To   : Boolean) is
   begin
      Self.Incremental := To;
      Self.Skip        := True;
   end Set_Incremental;

   ---------------
   -- Set_Limit --
   ---------------

   procedure Set_Limit
     (Self : in out Memory_UTF8_Output_Stream'Class;
      To   : VSS.Strings.Character_Count) is
   begin
      Self.Limit := To;
   end Set_Limit;

   ---------------
   -- Set_Limit --
   ---------------

   procedure Set_Limit
     (Self : in out String_Output_Stream'Class;
      To   : VSS.Strings.Character_Count) is
   begin
      Self.Limit := To;
   end Set_Limit;

end Tests_Text_Streams;

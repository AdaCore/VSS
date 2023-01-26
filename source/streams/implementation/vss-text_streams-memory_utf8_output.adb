--
--  Copyright (C) 2020-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Streams;

with VSS.Characters.Latin;
with VSS.Implementation.UTF8_Encoding;
with VSS.Strings.Character_Iterators;
with VSS.Unicode;

package body VSS.Text_Streams.Memory_UTF8_Output is

   -------------------
   -- Error_Message --
   -------------------

   overriding function Error_Message
     (Self : Memory_UTF8_Output_Stream) return VSS.Strings.Virtual_String is
   begin
      return VSS.Strings.Empty_Virtual_String;
   end Error_Message;

   ---------------
   -- Has_Error --
   ---------------

   overriding function Has_Error
     (Self : Memory_UTF8_Output_Stream) return Boolean is
   begin
      return False;
   end Has_Error;

   --------------
   -- New_Line --
   --------------

   overriding procedure New_Line
     (Self    : in out Memory_UTF8_Output_Stream;
      Success : in out Boolean) is
   begin
      Self.Put (VSS.Characters.Latin.Line_Feed, Success);
   end New_Line;

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

   ---------
   -- Put --
   ---------

   overriding procedure Put
     (Self    : in out Memory_UTF8_Output_Stream;
      Item    : VSS.Strings.Virtual_String;
      Success : in out Boolean)
   is
      Iterator : VSS.Strings.Character_Iterators.Character_Iterator :=
        Item.Before_First_Character;

   begin
      while Iterator.Forward loop
         Self.Put (Iterator.Element, Success);
      end loop;
   end Put;

   --------------
   -- Put_Line --
   --------------

   overriding procedure Put_Line
     (Self    : in out Memory_UTF8_Output_Stream;
      Item    : VSS.Strings.Virtual_String;
      Success : in out Boolean) is
   begin
      Self.Put (Item, Success);
      Self.New_Line (Success);
   end Put_Line;

end VSS.Text_Streams.Memory_UTF8_Output;

------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Magic.Characters;
with Magic.Strings.Iterators.Characters;
with Magic.Unicode;

package body Magic.JSON.Streams.Writers is

   use type Magic.Text_Streams.Output_Text_Stream_Access;

   procedure Check_Effective_Stream
     (Self : in out JSON_Simple_Writer'Class; Success : in out Boolean);

   procedure Escaped_String_Value
     (Self    : in out JSON_Simple_Writer'Class;
      Item    : Magic.Strings.Magic_String'Class;
      Success : in out Boolean);
   --  Outputs escaped string value

   -------------------
   -- Boolean_Value --
   -------------------

   overriding procedure Boolean_Value
     (Self    : in out JSON_Simple_Writer;
      Value   : Boolean;
      Success : in out Boolean) is
   begin
      Self.Check_Effective_Stream (Success);

      if not Success then
         return;
      end if;

      if Self.Open_Parenthesis then
         Self.Effective_Stream.Put (',', Success);

         if not Success then
            return;
         end if;

         Self.Open_Parenthesis := False;
      end if;

      if Value then
         Self.Effective_Stream.Put ('t', Success);

         if not Success then
            return;
         end if;

         Self.Effective_Stream.Put ('r', Success);

         if not Success then
            return;
         end if;

         Self.Effective_Stream.Put ('u', Success);

         if not Success then
            return;
         end if;

         Self.Effective_Stream.Put ('e', Success);

         if not Success then
            return;
         end if;

      else
         Self.Effective_Stream.Put ('f', Success);

         if not Success then
            return;
         end if;

         Self.Effective_Stream.Put ('a', Success);

         if not Success then
            return;
         end if;

         Self.Effective_Stream.Put ('l', Success);

         if not Success then
            return;
         end if;

         Self.Effective_Stream.Put ('s', Success);

         if not Success then
            return;
         end if;

         Self.Effective_Stream.Put ('e', Success);

         if not Success then
            return;
         end if;
      end if;
   end Boolean_Value;

   ----------------------------
   -- Check_Effective_Stream --
   ----------------------------

   procedure Check_Effective_Stream
     (Self : in out JSON_Simple_Writer'Class; Success : in out Boolean) is
   begin
      if Self.Effective_Stream = null then
         Success := False;
      end if;
   end Check_Effective_Stream;

   ---------------
   -- End_Array --
   ---------------

   overriding procedure End_Array
     (Self : in out JSON_Simple_Writer; Success : in out Boolean) is
   begin
      Self.Check_Effective_Stream (Success);

      if not Success then
         return;
      end if;

      Self.Effective_Stream.Put (']', Success);

      if not Success then
         return;
      end if;

      Self.Open_Parenthesis := False;
   end End_Array;

   ------------------
   -- End_Document --
   ------------------

   overriding procedure End_Document
     (Self : in out JSON_Simple_Writer; Success : in out Boolean) is
   begin
      Self.Check_Effective_Stream (Success);

      if not Success then
         return;
      end if;

      Self.Effective_Stream := null;
   end End_Document;

   ----------------
   -- End_Object --
   ----------------

   overriding procedure End_Object
     (Self : in out JSON_Simple_Writer; Success : in out Boolean) is
   begin
      Self.Check_Effective_Stream (Success);

      if not Success then
         return;
      end if;

      Self.Effective_Stream.Put ('}', Success);

      if not Success then
         return;
      end if;

      Self.Open_Parenthesis := False;
   end End_Object;

   --------------------------
   -- Escaped_String_Value --
   --------------------------

   procedure Escaped_String_Value
     (Self    : in out JSON_Simple_Writer'Class;
      Item    : Magic.Strings.Magic_String'Class;
      Success : in out Boolean)
   is
      procedure Escaped_Control_Character
        (Item : Magic.Characters.Magic_Character);
      --  Outputs escape sequence for given control character using hex format

      function Hex_Digit
        (C : Magic.Unicode.Code_Point) return Magic.Characters.Magic_Character;
      --  Returns hexadecimal digit for given code point.

      -------------------------------
      -- Escaped_Control_Character --
      -------------------------------

      procedure Escaped_Control_Character
        (Item : Magic.Characters.Magic_Character)
      is
         use type Magic.Unicode.Code_Point;

         C  : constant Magic.Unicode.Code_Point :=
           Magic.Characters.Magic_Character'Pos (Item);
         D4 : constant Magic.Unicode.Code_Point := C and 16#00_000F#;
         D3 : constant Magic.Unicode.Code_Point :=
           C and 16#00_00F0# / 16#00_0010#;
         D2 : constant Magic.Unicode.Code_Point :=
           C and 16#00_0F00# / 16#00_0100#;
         D1 : constant Magic.Unicode.Code_Point :=
           C and 16#00_F000# / 16#00_1000#;

      begin
         Self.Effective_Stream.Put ('\', Success);

         if not Success then
            return;
         end if;

         Self.Effective_Stream.Put ('u', Success);

         if not Success then
            return;
         end if;

         Self.Effective_Stream.Put (Hex_Digit (D1), Success);

         if not Success then
            return;
         end if;
      end Escaped_Control_Character;

      ---------------
      -- Hex_Digit --
      ---------------

      function Hex_Digit
        (C : Magic.Unicode.Code_Point) return Magic.Characters.Magic_Character
      is
         use type Magic.Unicode.Code_Point;

      begin
         case C is
            when 16#0# .. 16#9# =>
               return Magic.Characters.Magic_Character'Val (16#30# + C);

            when 16#A# .. 16#F# =>
               return Magic.Characters.Magic_Character'Val (16#41# + C - 10);

            when others =>
               raise Program_Error;
         end case;
      end Hex_Digit;

   begin
      Self.Effective_Stream.Put ('"', Success);

      if not Success then
         return;
      end if;

      if not Item.Is_Empty then
         declare
            J : Magic.Strings.Iterators.Characters.Character_Iterator :=
              Item.First_Character;

         begin
            loop
               case J.Element is
                  when Magic.Characters.Magic_Character'Val (16#00_0000#)
                     .. Magic.Characters.Magic_Character'Val (16#00_0007#)
                     | Magic.Characters.Magic_Character'Val (16#00_000B#)
                     | Magic.Characters.Magic_Character'Val (16#00_000E#)
                     .. Magic.Characters.Magic_Character'Val (16#00_001F#)
                  =>
                     null;

                  when Magic.Characters.Magic_Character'Val (16#00_0008#) =>
                     --  Escape backspace

                     Self.Effective_Stream.Put ('\', Success);

                     if not Success then
                        return;
                     end if;

                     Self.Effective_Stream.Put ('b', Success);

                     if not Success then
                        return;
                     end if;

                  when Magic.Characters.Magic_Character'Val (16#00_0009#) =>
                     --  Escape character tabulation

                     Self.Effective_Stream.Put ('\', Success);

                     if not Success then
                        return;
                     end if;

                     Self.Effective_Stream.Put ('t', Success);

                     if not Success then
                        return;
                     end if;

                  when Magic.Characters.Magic_Character'Val (16#00_000A#) =>
                     --  Escape form feed

                     Self.Effective_Stream.Put ('\', Success);

                     if not Success then
                        return;
                     end if;

                     Self.Effective_Stream.Put ('l', Success);

                     if not Success then
                        return;
                     end if;

                  when Magic.Characters.Magic_Character'Val (16#00_000C#) =>
                     --  Escape form feed

                     Self.Effective_Stream.Put ('\', Success);

                     if not Success then
                        return;
                     end if;

                     Self.Effective_Stream.Put ('f', Success);

                     if not Success then
                        return;
                     end if;

                  when Magic.Characters.Magic_Character'Val (16#00_000D#) =>
                     --  Escape carriage return

                     Self.Effective_Stream.Put ('\', Success);

                     if not Success then
                        return;
                     end if;

                     Self.Effective_Stream.Put ('r', Success);

                     if not Success then
                        return;
                     end if;

                  when '"' =>
                     --  Escape double quotation mark

                     Self.Effective_Stream.Put ('\', Success);

                     if not Success then
                        return;
                     end if;

                     Self.Effective_Stream.Put ('"', Success);

                     if not Success then
                        return;
                     end if;

                  when '\' =>
                     --  Escape reverse solidus

                     Self.Effective_Stream.Put ('\', Success);

                     if not Success then
                        return;
                     end if;

                     Self.Effective_Stream.Put ('\', Success);

                     if not Success then
                        return;
                     end if;

                  when others =>
                     Self.Effective_Stream.Put (J.Element, Success);

                     if not Success then
                        return;
                     end if;
               end case;

               exit when not J.Forward;
            end loop;
         end;
      end if;

      Self.Effective_Stream.Put ('"', Success);

      if not Success then
         return;
      end if;
   end Escaped_String_Value;

   -----------------
   -- Float_Value --
   -----------------

   overriding procedure Float_Value
     (Self    : in out JSON_Simple_Writer;
      Value   : Interfaces.IEEE_Float_64;
      Success : in out Boolean)
   is
      Image : constant Wide_Wide_String :=
        Interfaces.IEEE_Float_64'Wide_Wide_Image (Value);

   begin
      Self.Check_Effective_Stream (Success);

      if not Success then
         return;
      end if;

      if Self.Open_Parenthesis then
         Self.Effective_Stream.Put (',', Success);

         if not Success then
            return;
         end if;

         Self.Open_Parenthesis := False;
      end if;

      for C of Image loop
         if C /= ' ' then
            Self.Effective_Stream.Put
              (Magic.Characters.Magic_Character (C), Success);

            if not Success then
               return;
            end if;
         end if;
      end loop;
   end Float_Value;

   -------------------
   -- Integer_Value --
   -------------------

   overriding procedure Integer_Value
     (Self    : in out JSON_Simple_Writer;
      Value   : Interfaces.Integer_64;
      Success : in out Boolean)
   is
      Image : constant Wide_Wide_String :=
        Interfaces.Integer_64'Wide_Wide_Image (Value);

   begin
      Self.Check_Effective_Stream (Success);

      if not Success then
         return;
      end if;

      if Self.Open_Parenthesis then
         Self.Effective_Stream.Put (',', Success);

         if not Success then
            return;
         end if;

         Self.Open_Parenthesis := False;
      end if;

      for C of Image loop
         if C /= ' ' then
            Self.Effective_Stream.Put
              (Magic.Characters.Magic_Character (C), Success);

            if not Success then
               return;
            end if;
         end if;
      end loop;
   end Integer_Value;

   --------------
   -- Key_Name --
   --------------

   overriding procedure Key_Name
     (Self    : in out JSON_Simple_Writer;
      Name    : Magic.Strings.Magic_String'Class;
      Success : in out Boolean) is
   begin
      Self.Check_Effective_Stream (Success);

      if not Success then
         return;
      end if;

      if Self.Open_Parenthesis then
         Self.Effective_Stream.Put (',', Success);

         if not Success then
            return;
         end if;

         Self.Open_Parenthesis := False;
      end if;

      if Name.Is_Empty then
         Success := False;

         return;
      end if;

      Self.Escaped_String_Value (Name, Success);

      if not Success then
         return;
      end if;

      Self.Effective_Stream.Put (':', Success);

      if not Success then
         return;
      end if;
   end Key_Name;

   ----------------
   -- Null_Value --
   ----------------

   overriding procedure Null_Value
     (Self : in out JSON_Simple_Writer; Success : in out Boolean) is
   begin
      Self.Check_Effective_Stream (Success);

      if not Success then
         return;
      end if;

      if Self.Open_Parenthesis then
         Self.Effective_Stream.Put (',', Success);

         if not Success then
            return;
         end if;

         Self.Open_Parenthesis := False;
      end if;

      Self.Effective_Stream.Put ('n', Success);

      if not Success then
         return;
      end if;

      Self.Effective_Stream.Put ('u', Success);

      if not Success then
         return;
      end if;

      Self.Effective_Stream.Put ('l', Success);

      if not Success then
         return;
      end if;

      Self.Effective_Stream.Put ('l', Success);

      if not Success then
         return;
      end if;
   end Null_Value;

   ----------------
   -- Set_Stream --
   ----------------

   procedure Set_Stream
     (Self   : in out JSON_Simple_Writer'Class;
      Stream : not null Magic.Text_Streams.Output_Text_Stream_Access) is
   begin
      Self.Configured_Stream := Stream;
   end Set_Stream;

   -----------------
   -- Start_Array --
   -----------------

   overriding procedure Start_Array
     (Self : in out JSON_Simple_Writer; Success : in out Boolean) is
   begin
      Self.Check_Effective_Stream (Success);

      if not Success then
         return;
      end if;

      Self.Effective_Stream.Put ('[', Success);

      if not Success then
         return;
      end if;

      Self.Open_Parenthesis := True;
   end Start_Array;

   --------------------
   -- Start_Document --
   --------------------

   overriding procedure Start_Document
     (Self : in out JSON_Simple_Writer; Success : in out Boolean) is
   begin
      if Self.Effective_Stream /= null then
         Success := False;

      elsif Self.Configured_Stream /= null then
         Success := False;

      else
         Self.Effective_Stream := Self.Configured_Stream;
         Self.Open_Parenthesis := True;
      end if;
   end Start_Document;

   ------------------
   -- Start_Object --
   ------------------

   overriding procedure Start_Object
     (Self : in out JSON_Simple_Writer; Success : in out Boolean) is
   begin
      Self.Check_Effective_Stream (Success);

      if not Success then
         return;
      end if;

      if Self.Open_Parenthesis then
         Self.Effective_Stream.Put (',', Success);

         if not Success then
            return;
         end if;
      end if;

      Self.Effective_Stream.Put ('{', Success);

      if not Success then
         return;
      end if;

      Self.Open_Parenthesis := True;
   end Start_Object;

   ------------------
   -- String_Value --
   ------------------

   overriding procedure String_Value
     (Self    : in out JSON_Simple_Writer;
      Value   : Magic.Strings.Magic_String'Class;
      Success : in out Boolean) is
   begin
      Self.Check_Effective_Stream (Success);

      if not Success then
         return;
      end if;

      if Self.Open_Parenthesis then
         Self.Effective_Stream.Put (',', Success);

         if not Success then
            return;
         end if;

         Self.Open_Parenthesis := False;
      end if;

      Self.Escaped_String_Value (Value, Success);

      if not Success then
         return;
      end if;
   end String_Value;

end Magic.JSON.Streams.Writers;

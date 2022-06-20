--
--  Copyright (C) 2020-2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Assertions;

with VSS.Characters;
with VSS.Implementation.Strings;
with VSS.Implementation.String_Handlers;
with VSS.Strings.Internals;
with VSS.Unicode;

package body VSS.JSON.Push_Writers is

   use type VSS.Text_Streams.Output_Text_Stream_Access;

   procedure Check_Effective_Stream
     (Self : in out JSON_Simple_Push_Writer'Class; Success : in out Boolean);

   procedure Escaped_String_Value
     (Self    : in out JSON_Simple_Push_Writer'Class;
      Item    : VSS.Strings.Virtual_String'Class;
      Success : in out Boolean);
   --  Outputs escaped string value

   -------------------
   -- Boolean_Value --
   -------------------

   overriding procedure Boolean_Value
     (Self    : in out JSON_Simple_Push_Writer;
      Value   : Boolean;
      Success : in out Boolean) is
   begin
      Self.Check_Effective_Stream (Success);

      if not Success then
         return;
      end if;

      if not Self.Open_Parenthesis then
         Self.Effective_Stream.Put (',', Success);

         if not Success then
            return;
         end if;

      else
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
     (Self : in out JSON_Simple_Push_Writer'Class; Success : in out Boolean) is
   begin
      if Self.Effective_Stream = null then
         Success := False;
      end if;
   end Check_Effective_Stream;

   ---------------
   -- End_Array --
   ---------------

   overriding procedure End_Array
     (Self : in out JSON_Simple_Push_Writer; Success : in out Boolean) is
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
     (Self : in out JSON_Simple_Push_Writer; Success : in out Boolean) is
   begin
      --  End_Document is special exception: it must shutdown processing even
      --  in case of intermediate failures.

      declare
         Aux : Boolean := True;

      begin
         Self.Check_Effective_Stream (Aux);

         if not Aux then
            Success := False;

            return;
         end if;
      end;

      Self.Effective_Stream := null;
   end End_Document;

   ----------------
   -- End_Object --
   ----------------

   overriding procedure End_Object
     (Self : in out JSON_Simple_Push_Writer; Success : in out Boolean) is
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

   -------------------
   -- Error_Message --
   -------------------

   overriding function Error_Message
     (Self : JSON_Simple_Push_Writer) return VSS.Strings.Virtual_String is
   begin
      return VSS.Strings.Empty_Virtual_String;
   end Error_Message;

   --------------------------
   -- Escaped_String_Value --
   --------------------------

   procedure Escaped_String_Value
     (Self    : in out JSON_Simple_Push_Writer'Class;
      Item    : VSS.Strings.Virtual_String'Class;
      Success : in out Boolean)
   is
      procedure Escaped_Control_Character (Item : VSS.Unicode.Code_Point);
      --  Outputs escape sequence for given control character using hex format

      function Hex_Digit
        (C : VSS.Unicode.Code_Point) return VSS.Characters.Virtual_Character;
      --  Returns hexadecimal digit for given code point.

      -------------------------------
      -- Escaped_Control_Character --
      -------------------------------

      procedure Escaped_Control_Character (Item : VSS.Unicode.Code_Point) is
         use type VSS.Unicode.Code_Point;

         D4 : constant VSS.Unicode.Code_Point := Item and 16#00_000F#;
         D3 : constant VSS.Unicode.Code_Point :=
           (Item / 16#00_0010#) and 16#00_000F#;
         D2 : constant VSS.Unicode.Code_Point :=
           (Item / 16#00_0100#) and 16#00_000F#;
         D1 : constant VSS.Unicode.Code_Point :=
           (Item / 16#00_1000#) and 16#00_000F#;

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

         Self.Effective_Stream.Put (Hex_Digit (D2), Success);

         if not Success then
            return;
         end if;

         Self.Effective_Stream.Put (Hex_Digit (D3), Success);

         if not Success then
            return;
         end if;

         Self.Effective_Stream.Put (Hex_Digit (D4), Success);

         if not Success then
            return;
         end if;
      end Escaped_Control_Character;

      ---------------
      -- Hex_Digit --
      ---------------

      function Hex_Digit
        (C : VSS.Unicode.Code_Point) return VSS.Characters.Virtual_Character
      is
         use type VSS.Unicode.Code_Point;

      begin
         case C is
            when 16#0# .. 16#9# =>
               return VSS.Characters.Virtual_Character'Val (16#30# + C);

            when 16#A# .. 16#F# =>
               return VSS.Characters.Virtual_Character'Val (16#41# + C - 10);

            when others =>
               raise Ada.Assertions.Assertion_Error;
         end case;
      end Hex_Digit;

   begin
      Self.Effective_Stream.Put ('"', Success);

      if not Success then
         return;
      end if;

      if not Item.Is_Empty then
         --  Code block below use direct access to the internal string API to
         --  improve performance, check for empty string case was done above,
         --  thus check for null handler is not necessary here.

         declare
            Data     : VSS.Implementation.Strings.String_Data
              renames VSS.Strings.Internals.Data_Access_Constant (Item).all;
            Handler  : constant not null
              VSS.Implementation.Strings.String_Handler_Access :=
                VSS.Implementation.Strings.Handler (Data);
            Position : VSS.Implementation.Strings.Cursor;
            Code     : VSS.Unicode.Code_Point;

         begin
            Handler.Before_First_Character (Data, Position);

            while Handler.Forward (Data, Position) loop
               Code := Handler.Element (Data, Position);

               case Code is
                  when 16#00_0000# .. 16#00_0007#
                     | 16#00_000B#
                     | 16#00_000E# .. 16#00_001F#
                  =>
                     Escaped_Control_Character (Code);

                     if not Success then
                        return;
                     end if;

                  when 16#00_0008# =>
                     --  Escape backspace

                     Self.Effective_Stream.Put ('\', Success);

                     if not Success then
                        return;
                     end if;

                     Self.Effective_Stream.Put ('b', Success);

                     if not Success then
                        return;
                     end if;

                  when 16#00_0009# =>
                     --  Escape character tabulation

                     Self.Effective_Stream.Put ('\', Success);

                     if not Success then
                        return;
                     end if;

                     Self.Effective_Stream.Put ('t', Success);

                     if not Success then
                        return;
                     end if;

                  when 16#00_000A# =>
                     --  Escape line feed

                     Self.Effective_Stream.Put ('\', Success);

                     if not Success then
                        return;
                     end if;

                     Self.Effective_Stream.Put ('n', Success);

                     if not Success then
                        return;
                     end if;

                  when 16#00_000C# =>
                     --  Escape form feed

                     Self.Effective_Stream.Put ('\', Success);

                     if not Success then
                        return;
                     end if;

                     Self.Effective_Stream.Put ('f', Success);

                     if not Success then
                        return;
                     end if;

                  when 16#00_000D# =>
                     --  Escape carriage return

                     Self.Effective_Stream.Put ('\', Success);

                     if not Success then
                        return;
                     end if;

                     Self.Effective_Stream.Put ('r', Success);

                     if not Success then
                        return;
                     end if;

                  when Wide_Wide_Character'Pos ('"') =>
                     --  Escape double quotation mark

                     Self.Effective_Stream.Put ('\', Success);

                     if not Success then
                        return;
                     end if;

                     Self.Effective_Stream.Put ('"', Success);

                     if not Success then
                        return;
                     end if;

                  when Wide_Wide_Character'Pos ('\') =>
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
                     Self.Effective_Stream.Put
                       (VSS.Characters.Virtual_Character'Val (Code), Success);

                     if not Success then
                        return;
                     end if;
               end case;
            end loop;
         end;
      end if;

      Self.Effective_Stream.Put ('"', Success);

      if not Success then
         return;
      end if;
   end Escaped_String_Value;

   --------------
   -- Key_Name --
   --------------

   overriding procedure Key_Name
     (Self    : in out JSON_Simple_Push_Writer;
      Name    : VSS.Strings.Virtual_String'Class;
      Success : in out Boolean) is
   begin
      Self.Check_Effective_Stream (Success);

      if not Success then
         return;
      end if;

      if not Self.Open_Parenthesis then
         Self.Effective_Stream.Put (',', Success);

         if not Success then
            return;
         end if;
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

      Self.Open_Parenthesis := True;
   end Key_Name;

   ----------------
   -- Null_Value --
   ----------------

   overriding procedure Null_Value
     (Self : in out JSON_Simple_Push_Writer; Success : in out Boolean) is
   begin
      Self.Check_Effective_Stream (Success);

      if not Success then
         return;
      end if;

      if not Self.Open_Parenthesis then
         Self.Effective_Stream.Put (',', Success);

         if not Success then
            return;
         end if;

      else
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

   ------------------
   -- Number_Value --
   ------------------

   overriding procedure Number_Value
     (Self    : in out JSON_Simple_Push_Writer;
      Value   : VSS.JSON.JSON_Number;
      Success : in out Boolean) is
   begin
      Self.Check_Effective_Stream (Success);

      if not Success then
         return;
      end if;

      if not Self.Open_Parenthesis then
         Self.Effective_Stream.Put (',', Success);

         if not Success then
            return;
         end if;

      else
         Self.Open_Parenthesis := False;
      end if;

      case Value.Kind is
         when VSS.JSON.None =>
            raise Program_Error;

         when VSS.JSON.JSON_Integer =>
            declare
               Image : constant Wide_Wide_String :=
                 Interfaces.Integer_64'Wide_Wide_Image (Value.Integer_Value);

            begin
               for C of Image loop
                  if C /= ' ' then
                     Self.Effective_Stream.Put
                       (VSS.Characters.Virtual_Character (C), Success);

                     if not Success then
                        return;
                     end if;
                  end if;
               end loop;
            end;

         when VSS.JSON.JSON_Float =>
            declare
               Image : constant Wide_Wide_String :=
                 Interfaces.IEEE_Float_64'Wide_Wide_Image (Value.Float_Value);

            begin
               for C of Image loop
                  if C /= ' ' then
                     Self.Effective_Stream.Put
                       (VSS.Characters.Virtual_Character (C), Success);

                     if not Success then
                        return;
                     end if;
                  end if;
               end loop;
            end;

         when VSS.JSON.Out_Of_Range =>
            --  ??? Not implemented. Note, image must be checked for validity
            --  first.

            raise Program_Error;
      end case;
   end Number_Value;

   ----------------
   -- Set_Stream --
   ----------------

   procedure Set_Stream
     (Self   : in out JSON_Simple_Push_Writer'Class;
      Stream : not null VSS.Text_Streams.Output_Text_Stream_Access) is
   begin
      Self.Configured_Stream := Stream;
   end Set_Stream;

   -----------------
   -- Start_Array --
   -----------------

   overriding procedure Start_Array
     (Self : in out JSON_Simple_Push_Writer; Success : in out Boolean) is
   begin
      Self.Check_Effective_Stream (Success);

      if not Success then
         return;
      end if;

      if not Self.Open_Parenthesis then
         Self.Effective_Stream.Put (',', Success);

         if not Success then
            return;
         end if;
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
     (Self : in out JSON_Simple_Push_Writer; Success : in out Boolean) is
   begin
      if Self.Effective_Stream /= null then
         Success := False;

      elsif Self.Configured_Stream = null then
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
     (Self : in out JSON_Simple_Push_Writer; Success : in out Boolean) is
   begin
      Self.Check_Effective_Stream (Success);

      if not Success then
         return;
      end if;

      if not Self.Open_Parenthesis then
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
     (Self    : in out JSON_Simple_Push_Writer;
      Value   : VSS.Strings.Virtual_String'Class;
      Success : in out Boolean) is
   begin
      Self.Check_Effective_Stream (Success);

      if not Success then
         return;
      end if;

      if not Self.Open_Parenthesis then
         Self.Effective_Stream.Put (',', Success);

         if not Success then
            return;
         end if;

      else
         Self.Open_Parenthesis := False;
      end if;

      Self.Escaped_String_Value (Value, Success);

      if not Success then
         return;
      end if;
   end String_Value;

end VSS.JSON.Push_Writers;

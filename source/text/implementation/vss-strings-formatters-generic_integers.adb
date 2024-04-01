--
--  Copyright (C) 2023-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with Interfaces;

with VSS.Characters.Latin;
with VSS.Implementation.Character_Codes;
with VSS.Implementation.Text_Handlers;
with VSS.Unicode;

package body VSS.Strings.Formatters.Generic_Integers is

   type Sign_Options is
     (Compact,
      --  Don't preserve space for sign, but start negative values with
      --  hyphen-minus.
      Space_Or_Minus,
      --  Preserve space for sign, fill it by whitespace for positive values.
      Plus_Or_Minus);
      --  Preserve space for sign, fill it by plus sign for positive values.

   type Formatter_Options is record
      Sign          : Sign_Options                       := Compact;
      Width         : VSS.Strings.Grapheme_Cluster_Count := 0;
      Leading_Zeros : Boolean                            := False;
      Base          : Natural                            := 10;
      Group         : VSS.Strings.Grapheme_Cluster_Count := 0;
      Separator     : VSS.Characters.Virtual_Character   := '_';
   end record;

   procedure Parse
     (Format  : VSS.Strings.Virtual_String;
      Options : in out Formatter_Options);

   ------------
   -- Format --
   ------------

   overriding function Format
     (Self   : Formatter;
      Format : VSS.Strings.Formatters.Format_Information)
      return VSS.Strings.Virtual_String
   is
      use VSS.Implementation.Character_Codes;
      use type Interfaces.Unsigned_128;
      use type VSS.Unicode.Code_Point_Unit;

      Buffer   : Wide_Wide_String (1 .. Integer_Type'Size);
      First    : Positive := Buffer'Last + 1;
      Options  : Formatter_Options;
      Negative : Boolean;
      Value    : Interfaces.Unsigned_128;
      Result   : VSS.Strings.Virtual_String;
      Digit    : VSS.Unicode.Code_Point_Unit;
      Length   : VSS.Strings.Grapheme_Cluster_Count;

      procedure Append_Sign;

      -----------------
      -- Append_Sign --
      -----------------

      procedure Append_Sign is
      begin
         if Negative then
            Result.Append (VSS.Characters.Latin.Hyphen_Minus);

         else
            case Options.Sign is
               when Compact =>
                  null;

               when Space_Or_Minus =>
                  Result.Append (VSS.Characters.Latin.Space);

               when Plus_Or_Minus =>
                  Result.Append (VSS.Characters.Latin.Plus_Sign);
            end case;
         end if;
      end Append_Sign;

   begin
      Parse (Format.Format, Options);

      --  Process sign

      if Self.Value < 0 then
         declare
            pragma Suppress (Overflow_Check);

         begin
            Negative := True;
            Value    := Interfaces.Unsigned_128 (-Self.Value);
         end;

      else
         Negative := False;
         Value    := Interfaces.Unsigned_128 (Self.Value);
      end if;

      --  Convert positive integer value into the text representation.

      if Value = 0 then
         First := @ - 1;
         Buffer (First) := Wide_Wide_Character'Val (Digit_Zero);
      end if;

      while Value /= 0 loop
         Digit :=
           VSS.Unicode.Code_Point_Unit
             (Value mod Interfaces.Unsigned_128 (Options.Base));

         if Digit in 0 .. 9 then
            First := @ - 1;
            Buffer (First) := Wide_Wide_Character'Val (Digit + Digit_Zero);

         elsif Digit in 10 .. 25 then
            First := @ - 1;
            Buffer (First) :=
              Wide_Wide_Character'Val (Digit - 10 + Latin_Capital_Letter_A);

         else
            raise Program_Error;
         end if;

         Value := Value / Interfaces.Unsigned_128 (Options.Base);
      end loop;

      --  Fill leading zeros/spaces and sign.

      Length := VSS.Strings.Grapheme_Cluster_Count (Buffer'Last - First + 1);

      if Options.Width = 0 then
         Append_Sign;

      elsif Options.Leading_Zeros then
         Append_Sign;

         for J in reverse Length + 1 .. Options.Width loop
            Result.Append (VSS.Characters.Latin.Digit_Zero);

            if Options.Group /= 0
              and then (J - 1) mod Options.Group = 0
            then
               Result.Append (Options.Separator);
            end if;
         end loop;

      else
         for J in reverse Length + 1 .. Options.Width loop
            Result.Append (VSS.Characters.Latin.Space);
         end loop;

         Append_Sign;
      end if;

      --  Append text representation.

      for J in First .. Buffer'Last loop
         Result.Append (VSS.Characters.Virtual_Character (Buffer (J)));

         if Options.Group /= 0
           and then J /= Buffer'Last
           and then VSS.Strings.Grapheme_Cluster_Count (Buffer'Last - J)
                      mod Options.Group = 0
         then
            Result.Append (Options.Separator);
         end if;
      end loop;

      return Result;
   end Format;

   -----------
   -- Image --
   -----------

   function Image (Item : Integer_Type) return Formatter is
   begin
      return (Name => <>, Value => Item);
   end Image;

   -----------
   -- Image --
   -----------

   function Image
     (Name : VSS.Strings.Virtual_String;
      Item : Integer_Type) return Formatter is
   begin
      return (Name => Name, Value => Item);
   end Image;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Self : Formatter) return VSS.Strings.Virtual_String is
   begin
      return Self.Name;
   end Name;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Format  : VSS.Strings.Virtual_String;
      Options : in out Formatter_Options)
   is
      use VSS.Implementation.Character_Codes;
      use type VSS.Unicode.Code_Point_Unit;

      type States is
        (Initial, Zero_Width_Base_Group, Width, Base, Group, Error);

      Handler  : constant
        VSS.Implementation.Strings.Constant_Text_Handler_Access :=
          VSS.Implementation.Strings.Constant_Handler (Format.Data);
      Position : VSS.Implementation.Strings.Cursor;
      Code     : VSS.Unicode.Code_Point'Base;
      State    : States := Initial;

   begin
      Handler.Before_First_Character (Format.Data, Position);

      while Handler.Forward_Element (Format.Data, Position, Code) loop
         case State is
            when Initial =>
               case Code is
                  when Plus_Sign =>
                     State        := Zero_Width_Base_Group;
                     Options.Sign := Plus_Or_Minus;

                  when Hyphen_Minus =>
                     State        := Zero_Width_Base_Group;
                     Options.Sign := Space_Or_Minus;

                  when Digit_Zero =>
                     State                 := Width;
                     Options.Leading_Zeros := True;
                     Options.Width         := 0;

                  when Digit_One .. Digit_Nine =>
                     State                 := Width;
                     Options.Leading_Zeros := False;
                     Options.Width         :=
                       VSS.Strings.Grapheme_Cluster_Count (Code - Digit_Zero);

                  when Number_Sign =>
                     State        := Base;
                     Options.Base := 0;

                  when Low_Line =>
                     State := Group;

                  when others =>
                     State := Error;
               end case;

            when Zero_Width_Base_Group =>
               case Code is
                  when Digit_Zero =>
                     State                 := Width;
                     Options.Leading_Zeros := True;
                     Options.Width         := 0;

                  when Digit_One .. Digit_Nine =>
                     State                 := Width;
                     Options.Leading_Zeros := False;
                     Options.Width         :=
                       VSS.Strings.Grapheme_Cluster_Count (Code - Digit_Zero);

                  when Number_Sign =>
                     State        := Base;
                     Options.Base := 0;

                  when Low_Line =>
                     State := Group;

                  when others =>
                     State := Error;
               end case;

            when Width =>
               case Code is
                  when Digit_Zero .. Digit_Nine =>
                     Options.Width :=
                       @ * 10
                         + VSS.Strings.Grapheme_Cluster_Count
                             (Code - Digit_Zero);

                  when Number_Sign =>
                     State        := Base;
                     Options.Base := 0;

                  when Low_Line =>
                     State := Group;

                  when others =>
                     State := Error;
               end case;

            when Base =>
               case Code is
                  when Digit_Zero .. Digit_Nine =>
                     Options.Base := @ * 10 + Natural (Code - Digit_Zero);

                  when Low_Line =>
                     State := Group;

                  when others =>
                     State := Error;
               end case;

            when Group =>
               case Code is
                  when Digit_Zero .. Digit_Nine =>
                     Options.Group :=
                       @ * 10
                         + VSS.Strings.Grapheme_Cluster_Count
                             (Code - Digit_Zero);

                  when others =>
                     State             := Error;
                     Options.Separator :=
                       VSS.Characters.Virtual_Character'Val (Code);
               end case;

            when Error =>
               exit;
         end case;
      end loop;
   end Parse;

end VSS.Strings.Formatters.Generic_Integers;

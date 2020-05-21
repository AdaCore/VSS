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
--  Generic implementation of the string which use UTF-8 encoding for data.

package body Magic.Strings.UTF8 is

   use type Magic.Unicode.UTF8_Code_Unit_Count;

   type UTF8_Segment_Access is access all UTF8_Segment;

   type Verification_State is
     (Initial,    --  ASCII or start of multibyte sequence
      U31,        --  A0 .. BF | UT1
      U33,        --  80 .. 9F | UT1
      U41,        --  90 .. BF | UT2
      U43,        --  80 .. 8F | UT2
      UT1,        --  1 (80 .. BF)
      UT2,        --  2 (80 .. BF)
      UT3,        --  3 (80 .. BF)
      Ill_Formed);
   --  Unicode defines well-formed UTF-8 as
   --
   --  00 .. 7F
   --  C2 .. DF | 80 .. BF
   --  E0       | A0 .. BF | 80 .. BF
   --  E1 .. EC | 80 .. BF | 80 .. BF
   --  ED       | 80 .. 9F | 80 .. BF
   --  EE .. EF | 80 .. BF | 80 .. BF
   --  F0       | 90 .. BF | 80 .. BF | 80 .. BF
   --  F1 .. F3 | 80 .. BF | 80 .. BF | 80 .. BF
   --  F4       | 80 .. 8F | 80 .. BF | 80 .. BF

   -------------
   -- Element --
   -------------

   overriding function Element
     (Self     : UTF8_Segment;
      Position : Cursor) return Magic.Unicode.Code_Point
   is
      use type Magic.Unicode.Code_Point;
      use type Magic.Unicode.UTF8_Code_Unit;

      U1 : Magic.Unicode.Code_Point;
      U2 : Magic.Unicode.Code_Point;
      U3 : Magic.Unicode.Code_Point;
      U4 : Magic.Unicode.Code_Point;

   begin
      if Position.Index < 1 or else Position.Index > Self.Length then
         return 16#00_0000#;
      end if;

      U1 := Magic.Unicode.Code_Point (Self.Data (Position.UTF8_Offset));

      case U1 is
         when 16#00# .. 16#7F# =>
            --  1x code units sequence

            return U1;

         when 16#C2# .. 16#DF# =>
            --  2x code units sequence

            U1 := (U1 and 2#0001_1111#) * 2#0100_0000#;
            U2 :=
              Magic.Unicode.Code_Point
                (Self.Data (Position.UTF8_Offset + 1) and 2#0011_1111#);

            return U1 or U2;

         when 16#E0# .. 16#EF# =>
            --  3x code units sequence

            U1 := (U1 and 2#0000_1111#) * 2#01_0000_0000_0000#;
            U2 := Magic.Unicode.Code_Point
              (Self.Data (Position.UTF8_Offset + 1) and 2#0011_1111#)
                * 2#0100_0000#;
            U3 :=
              Magic.Unicode.Code_Point
                (Self.Data (Position.UTF8_Offset + 2) and 2#0011_1111#);

            return U1 or U2 or U3;

         when 16#F0# .. 16#F4# =>
            --  4x code units sequence

            U1 := (U1 and 2#0000_0111#) * 2#0100_0000_0000_0000_0000#;
            U2 := Magic.Unicode.Code_Point
              (Self.Data (Position.UTF8_Offset + 1) and 2#0011_1111#)
                * 2#010_000_0000_0000#;
            U3 :=
              Magic.Unicode.Code_Point
                (Self.Data (Position.UTF8_Offset + 2) and 2#0011_1111#)
                * 2#0100_0000#;
            U4 :=
              Magic.Unicode.Code_Point
                (Self.Data (Position.UTF8_Offset + 3) and 2#0011_1111#);

            return U1 or U2 or U3 or U4;

         when others =>
            raise Program_Error;
      end case;
   end Element;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Self     : UTF8_Text;
      Position : Cursor) return Magic.Unicode.Code_Point is
   begin
      return 0;
   end Element;

   ---------------------
   -- First_Character --
   ---------------------

   overriding procedure First_Character
     (Self     : UTF8_Segment;
      Position : in out Cursor) is
   begin
      Position := (Index => 1, UTF8_Offset => 0, UTF16_Offset => 0);
   end First_Character;

   ---------------------
   -- First_Character --
   ---------------------

   overriding procedure First_Character
     (Self     : UTF8_Text;
      Position : in out Cursor) is
   begin
      Position := (Index => 1, UTF8_Offset => 0, UTF16_Offset => 0);
   end First_Character;

   -------------
   -- Forward --
   -------------

   overriding function Forward
     (Self     : UTF8_Segment;
      Position : in out Cursor) return Boolean
   is
      use type Magic.Unicode.UTF16_Code_Unit_Count;

   begin
      if Position.Index < 1 or else Position.Index > Self.Length then
         return False;
      end if;

      declare
         Code : constant Magic.Unicode.UTF8_Code_Unit :=
           Self.Data (Position.UTF8_Offset);

      begin
         Position.Index := Position.Index + 1;

         case Code is
            when 16#00# .. 16#7F# =>
               Position.UTF8_Offset  := Position.UTF8_Offset + 1;
               Position.UTF16_Offset := Position.UTF16_Offset + 1;

            when 16#C2# .. 16#DF# =>
               Position.UTF8_Offset  := Position.UTF8_Offset + 2;
               Position.UTF16_Offset := Position.UTF16_Offset + 1;

            when 16#E0# .. 16#EF# =>
               Position.UTF8_Offset  := Position.UTF8_Offset + 3;
               Position.UTF16_Offset := Position.UTF16_Offset + 1;

            when 16#F0# .. 16#F4# =>
               Position.UTF8_Offset  := Position.UTF8_Offset + 4;
               Position.UTF16_Offset := Position.UTF16_Offset + 2;

            when others =>
               raise Program_Error with "string data is corrupted";
         end case;

         --  XXX case statement above may be rewritten as below to avoid
         --  use of branch instructions.
         --
         --  Position.UTF8_Offset  :=
         --    Position.UTF8_Offset + 1
         --      + (if (Code and 2#1000_0000#) = 2#1000_0000# then 1 else 0)
         --      + (if (Code and 2#1110_0000#) = 2#1110_0000# then 1 else 0)
         --      + (if (Code and 2#1111_0000#) = 2#1111_0000# then 1 else 0);
         --
         --  Position.UTF16_Offset :=
         --    Position.UTF16_Offset + 1
         --      + (if (Code and 2#1111_0000#) = 2#1111_0000# then 1 else 0);

         return Position.Index <= Self.Length;
      end;
   end Forward;

   -------------
   -- Forward --
   -------------

   overriding function Forward
     (Self     : UTF8_Text;
      Position : in out Cursor) return Boolean is
   begin
      return False;
   end Forward;

   -----------------
   -- From_String --
   -----------------

   procedure From_UTF_8_String
     (Item    : Ada.Strings.UTF_Encoding.UTF_8_String;
      Segment : out String_Access;
      Success : out Boolean)
   is
      Aux    : UTF8_Segment_Access;
      Code   : Magic.Unicode.UTF8_Code_Unit;
      State  : Verification_State := Initial;
      Length : Character_Count := 0;

   begin
      if Item'Length = 0 then
         Segment := null;
         Success := True;

         return;
      end if;

      Aux :=
        new UTF8_Segment (Magic.Unicode.UTF8_Code_Unit_Count (Item'Length));

      for J in Item'Range loop
         Code := Standard.Character'Pos (Item (J));

         case State is
            when Initial =>
               Length := Length + 1;

               case Code is
                  when 16#00# .. 16#7F# =>
                     null;

                  when 16#C2# .. 16#DF# =>
                     State := UT1;

                  when 16#E0# =>
                     State := U31;

                  when 16#E1# .. 16#EC# =>
                     State := UT2;

                  when 16#ED# =>
                     State := U33;

                  when 16#EE# .. 16#EF# =>
                     State := UT2;

                  when 16#F0# =>
                     State := U41;

                  when 16#F1# .. 16#F3# =>
                     State := UT3;

                  when 16#F4# =>
                     State := U43;

                  when others =>
                     State := Ill_Formed;
               end case;

            when U31 =>
               case Code is
                  when 16#A0# .. 16#BF# =>
                     State := UT1;

                  when others =>
                     State := Ill_Formed;
               end case;

            when U33 =>
               case Code is
                  when 16#80# .. 16#9F# =>
                     State := UT1;

                  when others =>
                     State := Ill_Formed;
               end case;

            when U41 =>
               case Code is
                  when 16#90# .. 16#BF# =>
                     State := UT2;

                  when others =>
                     State := Ill_Formed;
               end case;

            when U43 =>
               case Code is
                  when 16#80# .. 16#8F# =>
                     State := UT2;

                  when others =>
                     State := Ill_Formed;
               end case;

            when UT1 =>
               case Code is
                  when 16#80# .. 16#BF# =>
                     State := Initial;

                  when others =>
                     State := Ill_Formed;
               end case;

            when UT2 =>
               case Code is
                  when 16#80# .. 16#BF# =>
                     State := UT1;

                  when others =>
                     State := Ill_Formed;
               end case;

            when UT3 =>
               case Code is
                  when 16#80# .. 16#BF# =>
                     State := UT2;

                  when others =>
                     State := Ill_Formed;
               end case;

            when Ill_Formed =>
               exit;
         end case;

         Aux.Data (Magic.Unicode.UTF8_Code_Unit_Count (J - Item'First)) := Code;
      end loop;

      if State = Initial then
         Aux.Data (Magic.Unicode.UTF8_Code_Unit_Count (Item'Length)) := 16#00#;
         Aux.Size := Item'Length;
         Aux.Length := Length;

         Success := True;
         Segment := String_Access (Aux);

      else
         Aux.Unreference;
         Success := False;
         Segment := null;
      end if;
   end From_UTF_8_String;

   --------------
   -- Is_Empty --
   --------------

   overriding function Is_Empty (Self : UTF8_Segment) return Boolean is
   begin
      return Self.Length = 0;
   end Is_Empty;

   --------------
   -- Is_Empty --
   --------------

   overriding function Is_Empty (Self : UTF8_Text) return Boolean is
   begin
      raise Program_Error;
      return False;
   end Is_Empty;

   -------------
   -- To_Text --
   -------------

   overriding function To_Text (Self : in out UTF8_Segment) return String_Access is
   begin
      return new UTF8_Text;
   end To_Text;

   -------------
   -- To_Text --
   -------------

   overriding function To_Text (Self : in out UTF8_Text) return String_Access is
   begin
      return Self.Reference;
   end To_Text;

   ---------------------
   -- To_UTF_8_String --
   ---------------------

   overriding function To_UTF_8_String (Self : UTF8_Text) return String is
   begin
      --  XXX Not implemented.

      return "";
   end To_UTF_8_String;

   ---------------------
   -- To_UTF_8_String --
   ---------------------

   overriding function To_UTF_8_String
     (Self : UTF8_Segment) return Ada.Strings.UTF_Encoding.UTF_8_String is
   begin
      return Result : Ada.Strings.UTF_Encoding.UTF_8_String
                        (1 .. Natural (Self.Size))
      do
         for J in Result'Range loop
            Result (J) :=
              Standard.Character'Val
                (Self.Data (Magic.Unicode.UTF8_Code_Unit_Count (J - 1)));
         end loop;
      end return;
   end To_UTF_8_String;

end Magic.Strings.UTF8;

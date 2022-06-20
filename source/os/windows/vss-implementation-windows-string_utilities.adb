--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Unchecked_Deallocation;

with VSS.Characters;
with VSS.Strings.Character_Iterators;
with VSS.Unicode;

package body VSS.Implementation.Windows.String_Utilities is

   use type Interfaces.C.size_t;

   function wcslen (Item : LPWSTR) return Interfaces.C.size_t
     with Import, Convention => C, External_Name => "wcslen";

   ----------
   -- Free --
   ----------

   procedure Free (Item : in out char16_array_access) is
      procedure Internal is
        new Ada.Unchecked_Deallocation
              (Interfaces.C.char16_array, char16_array_access);

   begin
      Internal (Item);
   end Free;

   ------------------------
   -- From_Native_String --
   ------------------------

   function From_Native_String
     (Item : Interfaces.C.char16_array) return VSS.Strings.Virtual_String
   is
      procedure Free is
        new Ada.Unchecked_Deallocation
             (Wide_Wide_String,
              Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_String_Access);

      High_Surrogate_First  : constant := 16#D800#;
      Low_Surrogate_First   : constant := 16#DC00#;
      Surrogate_Kind_Mask   : constant := 16#FC00#;
      Masked_High_Surrogate : constant := 16#D800#;

      UCS4_Fixup : constant
        := High_Surrogate_First * 16#400# + Low_Surrogate_First - 16#1_0000#;
      --  When code point is encoded as pair of surrogates its value computed
      --  as:
      --
      --    C := (S (J) - HB) << 10 + S (J + 1) - LB + 0x10000
      --
      --  to optimize number of computations this expression is transformed to
      --
      --    C := S (J) << 10 + S (J + 1) - (HB << 10 + LB - 0x10000)
      --                                   ^^^^^^^^^^^^^^^^^^^^^^^^^
      --  This constant represents constant part of the expression.

      Aux   : Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_String_Access :=
        new Wide_Wide_String (1 .. Item'Length);
      Last  : Natural := 0;
      Index : Interfaces.C.size_t := Item'First;

   begin
      while Index <= Item'Last loop
         declare
            use type VSS.Unicode.Code_Point;

            C : VSS.Unicode.Code_Point :=
              Interfaces.C.char16_t'Pos (Item (Index));

         begin
            Index := Index + 1;

            if (C and Surrogate_Kind_Mask) = Masked_High_Surrogate then
               C :=
                 C * 16#400#
                 + Interfaces.C.char16_t'Pos (Item (Index))
                 - UCS4_Fixup;
               Index := Index + 1;
            end if;

            exit when C = 16#0000#;

            Last := Last + 1;
            Aux (Last) := Wide_Wide_Character'Val (C);
         end;
      end loop;

      return Result : constant VSS.Strings.Virtual_String :=
        VSS.Strings.To_Virtual_String (Aux (1 .. Last))
      do
         Free (Aux);
      end return;

   exception
      when others =>
         Free (Aux);

         raise;
   end From_Native_String;

   ------------------------
   -- From_Native_String --
   ------------------------

   function From_Native_String
     (Item : LPWSTR) return VSS.Strings.Virtual_String
   is
      Aux : constant Interfaces.C.char16_array (0 .. wcslen (Item) - 1)
        with Import, Convention => C, Address => Item.all'Address;

   begin
      return From_Native_String (Aux);
   end From_Native_String;

   ------------------------------
   -- New_Native_String_Buffer --
   ------------------------------

   function New_Native_String_Buffer
     (Size : Interfaces.C.size_t) return char16_array_access is
   begin
      return new Interfaces.C.char16_array (0 .. Size);
   end New_Native_String_Buffer;

   --------------------------
   -- To_New_Native_String --
   --------------------------

   function To_New_Native_String
     (Item : VSS.Strings.Virtual_String) return char16_array_access
   is
      use type VSS.Unicode.Code_Point;

      --  This conversion function process data without use of primary stack,
      --  thus prevent crashes on conversion of big data.
      --  It takes into account that content of Virtual_String is always valid,
      --  there are no any checks for invalid ranges of code points here.

      High_Surrogate_First : constant := 16#D800#;
      Low_Surrogate_First  : constant := 16#DC00#;

      High_Surrogate_First_Store : constant
        := High_Surrogate_First - 16#1_0000# / 16#400#;
      --  Code point is converted to surrogate pair as:
      --
      --  S (J)     := HB + (C - 0x10000) >> 10
      --  S (J + 1) := LB + (C - 0x10000) & 0x3FF
      --
      --  to optimize implementation they are rewritten as:
      --
      --  S (J + 1) := LB + C & 0x3FF
      --  S (J)     := (HB - 0x10000 >> 10) + C >> 10
      --               ^^^^^^^^^^^^^^^^^^^^
      --  This constant represents constant part of the expression.

      Result   : char16_array_access;
      Last     : Interfaces.C.size_t := 0;
      Position : VSS.Strings.Character_Iterators.Character_Iterator :=
        Item.Before_First_Character;
      Code     : VSS.Unicode.Code_Point;

   begin
      if not Item.Is_Null then
         Result :=
           New_Native_String_Buffer
             (Interfaces.C.size_t (Item.Character_Length) * 2);

         while Position.Forward loop
            Code := VSS.Characters.Virtual_Character'Pos (Position.Element);

            if Code <= 16#FFFF# then
               Result (Last) := Interfaces.C.char16_t'Val (Code);
               Last := Last + 1;

            else
               Result (Last) :=
                 Interfaces.C.char16_t'Val
                   (High_Surrogate_First_Store + Code / 16#400#);
               Last := Last + 1;
               Result (Last) :=
                 Interfaces.C.char16_t'Val
                   (Low_Surrogate_First + Code mod 16#400#);
               Last := Last + 1;
            end if;
         end loop;

         Result (Last) := Interfaces.C.char16_t'Val (0);
      end if;

      return Result;

   exception
      when others =>
         Free (Result);

         raise;
   end To_New_Native_String;

end VSS.Implementation.Windows.String_Utilities;

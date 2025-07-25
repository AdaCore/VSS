--
--  Copyright (C) 2020-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Warnings (Off, ".* is an internal GNAT unit");
with Ada.Strings.Unbounded.Aux;
with Ada.Strings.Unbounded.VSS_Aux;
with Ada.Strings.Wide_Wide_Unbounded.Aux;
with Ada.Strings.Wide_Wide_Unbounded.VSS_Aux;
pragma Warnings (On, ".* is an internal GNAT unit");

with VSS.Implementation.Strings;
with VSS.Implementation.UTF8_Encoding;
with VSS.Implementation.UTF8_Strings.Mutable_Operations;
with VSS.Unicode;

package body VSS.Strings.Conversions is

   ----------------------
   -- Set_UTF_8_String --
   ----------------------

   procedure Set_UTF_8_String
     (Item : Virtual_String'Class;
      Into : out Ada.Strings.UTF_Encoding.UTF_8_String)
   is
      use type VSS.Unicode.UTF8_Code_Unit_Offset;

      Position : aliased VSS.Implementation.Strings.Cursor;
      U_Buffer :
        VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array (1 .. 4);
      S_Buffer : String (1 .. 4) with Address => U_Buffer'Address;
      Length   : VSS.Unicode.Scalar_Value_UTF8_Code_Unit_Length;
      Last     : Natural := Into'First - 1;

   begin
      if Into'Length /= Item.Data.Size then
         raise Constraint_Error;
      end if;

      VSS.Implementation.UTF8_Strings.Before_First_Character
        (Item.Data, Position);

      while VSS.Implementation.UTF8_Strings.Forward (Item.Data, Position) loop
         VSS.Implementation.UTF8_Encoding.Encode
           (VSS.Implementation.UTF8_Strings.Element (Item.Data, Position),
            Length,
            U_Buffer (1),
            U_Buffer (2),
            U_Buffer (3),
            U_Buffer (4));

         Into (Last + 1 .. Last + Natural (Length)) :=
           S_Buffer (1 .. Natural (Length));

         Last := Last + Natural (Length);
      end loop;

      pragma Assert (Last = Into'Last);
   end Set_UTF_8_String;

   ----------------------
   -- Set_UTF_8_String --
   ----------------------

   procedure Set_UTF_8_String
     (Item : Virtual_String'Class;
      Last : out Natural;
      Into : out Ada.Strings.UTF_Encoding.UTF_8_String) is
   begin
      Set_UTF_8_String (Item, Into'First, Last, Into);
   end Set_UTF_8_String;

   ----------------------
   -- Set_UTF_8_String --
   ----------------------

   procedure Set_UTF_8_String
     (Item : Virtual_String'Class;
      From : Positive;
      Last : out Natural;
      Into : out Ada.Strings.UTF_Encoding.UTF_8_String)
   is
      Position : aliased VSS.Implementation.Strings.Cursor;
      U_Buffer :
        VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array (1 .. 4);
      S_Buffer : String (1 .. 4) with Address => U_Buffer'Address;
      Length   : VSS.Unicode.Scalar_Value_UTF8_Code_Unit_Length;

   begin
      Last := From - 1;

      if Item.Is_Empty then
         return;
      end if;

      if Natural (Item.Data.Size) > Into'Last - From + 1 then
         raise Constraint_Error;
      end if;

      VSS.Implementation.UTF8_Strings.Before_First_Character
        (Item.Data, Position);

      while VSS.Implementation.UTF8_Strings.Forward (Item.Data, Position) loop
         VSS.Implementation.UTF8_Encoding.Encode
           (VSS.Implementation.UTF8_Strings.Element (Item.Data, Position),
            Length,
            U_Buffer (1),
            U_Buffer (2),
            U_Buffer (3),
            U_Buffer (4));

         Into (Last + 1 .. Last + Natural (Length)) :=
           S_Buffer (1 .. Natural (Length));

         Last := Last + Natural (Length);
      end loop;
   end Set_UTF_8_String;

   --------------------------
   -- Set_Wide_Wide_String --
   --------------------------

   procedure Set_Wide_Wide_String
     (Item : Virtual_String'Class;
      Into : out Wide_Wide_String)
   is
      Position : aliased VSS.Implementation.Strings.Cursor;

   begin
      if Item.Character_Length /= Into'Length then
         raise Constraint_Error;
      end if;

      if Item.Is_Empty then
         return;
      end if;

      VSS.Implementation.UTF8_Strings.Before_First_Character
        (Item.Data, Position);

      while VSS.Implementation.UTF8_Strings.Forward (Item.Data, Position) loop
         Into (Into'First + Integer (Position.Index) - 1) :=
           Wide_Wide_Character'Val
             (VSS.Implementation.UTF8_Strings.Element (Item.Data, Position));
      end loop;
   end Set_Wide_Wide_String;

   --------------------------
   -- Set_Wide_Wide_String --
   --------------------------

   procedure Set_Wide_Wide_String
     (Item : Virtual_String'Class;
      Last : out Natural;
      Into : out Wide_Wide_String) is
   begin
      Set_Wide_Wide_String (Item, Into'First, Last, Into);
   end Set_Wide_Wide_String;

   --------------------------
   -- Set_Wide_Wide_String --
   --------------------------

   procedure Set_Wide_Wide_String
     (Item : Virtual_String'Class;
      From : Positive;
      Last : out Natural;
      Into : out Wide_Wide_String)
   is
      Position : aliased VSS.Implementation.Strings.Cursor;

   begin
      Last := From - 1;

      if Item.Is_Empty then
         return;
      end if;

      if Natural (Item.Character_Length) > Into'Last - From + 1 then
         raise Constraint_Error;
      end if;

      VSS.Implementation.UTF8_Strings.Before_First_Character
        (Item.Data, Position);

      while VSS.Implementation.UTF8_Strings.Forward (Item.Data, Position) loop
         Last := Last + 1;
         Into (Last) :=
           Wide_Wide_Character'Val
             (VSS.Implementation.UTF8_Strings.Element (Item.Data, Position));
      end loop;
   end Set_Wide_Wide_String;

   -------------------------------
   -- To_Unbounded_UTF_8_String --
   -------------------------------

   function To_Unbounded_UTF_8_String
     (Item : Virtual_String'Class)
      return Ada.Strings.Unbounded.Unbounded_String
   is
      use type VSS.Unicode.UTF8_Code_Unit_Offset;

      Position : aliased VSS.Implementation.Strings.Cursor;

      procedure Set (S : out String);

      ---------
      -- Set --
      ---------

      procedure Set (S : out String) is
         U_Buffer :
           VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array (1 .. 4);
         S_Buffer : String (1 .. 4) with Address => U_Buffer'Address;
         Length   : VSS.Unicode.Scalar_Value_UTF8_Code_Unit_Length;
         Last     : Natural := 0;

      begin
         VSS.Implementation.UTF8_Strings.Before_First_Character
           (Item.Data, Position);

         while VSS.Implementation.UTF8_Strings.Forward (Item.Data, Position)
         loop
            VSS.Implementation.UTF8_Encoding.Encode
              (VSS.Implementation.UTF8_Strings.Element (Item.Data, Position),
               Length,
               U_Buffer (1),
               U_Buffer (2),
               U_Buffer (3),
               U_Buffer (4));

            S (Last + 1 .. Last + Natural (Length)) :=
              S_Buffer (1 .. Natural (Length));

            Last := Last + Natural (Length);
         end loop;

         pragma Assert (Last = S'Last);
      end Set;

   begin
      return Result : Ada.Strings.Unbounded.Unbounded_String do
         if Item.Data.Size /= 0 then
            Ada.Strings.Unbounded.VSS_Aux.Set_String
              (Result,
               Natural (Item.Data.Size),
               Set'Access);
         end if;
      end return;
   end To_Unbounded_UTF_8_String;

   -----------------------------------
   -- To_Unbounded_Wide_Wide_String --
   -----------------------------------

   function To_Unbounded_Wide_Wide_String
     (Item : Virtual_String'Class)
      return Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String
   is
      procedure Set (String : out Wide_Wide_String);

      ---------
      -- Set --
      ---------

      procedure Set (String : out Wide_Wide_String) is
      begin
         Set_Wide_Wide_String (Item, String);
      end Set;

   begin
      return Result :
        Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String
      do
         if not Item.Is_Empty then
            Ada.Strings.Wide_Wide_Unbounded.VSS_Aux.Set_String
              (Result, Integer (Item.Character_Length), Set'Access);
         end if;
      end return;
   end To_Unbounded_Wide_Wide_String;

   ---------------------
   -- To_UTF_8_String --
   ---------------------

   function To_UTF_8_String
     (Item : Virtual_String'Class)
      return Ada.Strings.UTF_Encoding.UTF_8_String is
   begin
      return VSS.Implementation.UTF8_Strings.To_UTF_8_String (Item.Data);
   end To_UTF_8_String;

   -----------------------
   -- To_Virtual_String --
   -----------------------

   function To_Virtual_String
     (Item : Ada.Strings.UTF_Encoding.UTF_8_String) return Virtual_String
   is
      Success : Boolean;

   begin
      return Result : Virtual_String do
         VSS.Implementation.UTF8_Strings.Mutable_Operations.From_UTF_8_String
           (Result.Data, Item, Success);

         if not Success then
            raise Constraint_Error with "Ill-formed UTF-8 data";
         end if;
      end return;
   end To_Virtual_String;

   -----------------------
   -- To_Virtual_String --
   -----------------------

   function To_Virtual_String
     (Item : Ada.Strings.Unbounded.Unbounded_String) return Virtual_String
   is
      Success : Boolean;
      Data    : Ada.Strings.Unbounded.Aux.Big_String_Access;
      Last    : Natural;

   begin
      return Result : Virtual_String do
         --  Retrieve data from unbounded string.

         Ada.Strings.Unbounded.Aux.Get_String (Item, Data, Last);

         VSS.Implementation.UTF8_Strings.Mutable_Operations.From_UTF_8_String
           (Result.Data, Data (1 .. Last), Success);

         if not Success then
            raise Constraint_Error with "Ill-formed UTF-8 data";
         end if;
      end return;
   end To_Virtual_String;

   -----------------------
   -- To_Virtual_String --
   -----------------------

   function To_Virtual_String
     (Item : Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String)
      return Virtual_String
   is
      Success : Boolean;
      Data    :
        Ada.Strings.Wide_Wide_Unbounded.Aux.Big_Wide_Wide_String_Access;
      Last    : Natural;

   begin
      return Result : Virtual_String do
         --  Retrieve data from unbounded string.

         Ada.Strings.Wide_Wide_Unbounded.Aux.Get_Wide_Wide_String
           (Item, Data, Last);

         VSS.Implementation.UTF8_Strings.Mutable_Operations
           .From_Wide_Wide_String (Result.Data, Data (1 .. Last), Success);

         if not Success then
            raise Constraint_Error with "Ill-formed UTF-32 data";
         end if;
      end return;
   end To_Virtual_String;

   -------------------------
   -- To_Wide_Wide_String --
   -------------------------

   function To_Wide_Wide_String
     (Item : Virtual_String'Class) return Wide_Wide_String is
   begin
      if Item.Is_Empty then
         return "";

      else
         return Result :
           Wide_Wide_String (1 .. Integer (Item.Character_Length))
         do
            Set_Wide_Wide_String (Item, Result);
         end return;
      end if;
   end To_Wide_Wide_String;

end VSS.Strings.Conversions;

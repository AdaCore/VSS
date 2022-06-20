--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Wide_Wide_Unbounded;   use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;
with Ada.Wide_Wide_Text_IO;             use Ada.Wide_Wide_Text_IO;

with UCD.Characters;

package body Gen_UCD.Enumeration_Types is

   function Type_Identifier
     (Property : not null UCD.Properties.Property_Access)
      return Wide_Wide_String;

   function Value_Identifier
     (Property : not null UCD.Properties.Property_Access;
      Value    : not null UCD.Properties.Property_Value_Access)
      return Wide_Wide_String;

   function Representation
     (Self  : Enumeration_Type'Class;
      Value : not null UCD.Properties.Property_Value_Access) return Natural;

   function Minimum_Bits (Value : Ada.Containers.Count_Type) return Integer;

   -------------------------------
   -- Generate_Type_Declaration --
   -------------------------------

   procedure Generate_Type_Declaration
     (Self : Enumeration_Type'Class;
      File : Ada.Wide_Wide_Text_IO.File_Type)
   is
      First : Boolean := True;

   begin
      Put_Line (File, "   type " & Type_Identifier (Self.Property) & " is");

      for Value of Self.Property.All_Values loop
         if Value.Is_Used then
            if First then
               Put (File, "     (");
               First := False;

            else
               Put_Line (File, ",");
               Put (File, "      ");
            end if;

            Put (File, Value_Identifier (Self.Property, Value));
         end if;
      end loop;

      Put_Line (File, ");");

      Put_Line
        (File,
         "   for " & Type_Identifier (Self.Property) & "'Size use"
         & Natural'Wide_Wide_Image (Minimum_Bits (Self.Map.Length))
         & ";");
      Put_Line (File, "   for " & Type_Identifier (Self.Property) & " use");
      First := True;

      for Value of Self.Property.All_Values loop
         if Value.Is_Used then
            if First then
               Put (File, "     (");
               First := False;

            else
               Put_Line (File, ",");
               Put (File, "      ");
            end if;

            Put (File, Value_Identifier (Self.Property, Value));
            Put (File, " =>");
            Put (File, Integer'Wide_Wide_Image (Self.Representation (Value)));
         end if;
      end loop;

      Put_Line (File, ");");
      New_Line (File);
   end Generate_Type_Declaration;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self     : in out Enumeration_Type'Class;
      Property : not null UCD.Properties.Property_Access) is
   begin
      Self.Property := Property;

      for Value of Self.Property.All_Values loop
         if Value.Is_Used then
            Self.Map.Insert (Value, Natural (Self.Map.Length));
         end if;
      end loop;
   end Initialize;

   ------------------
   -- Minimum_Bits --
   ------------------

   function Minimum_Bits (Value : Ada.Containers.Count_Type) return Integer is
      Aux : Unsigned_32 := Unsigned_32 (Value);

   begin
      return Result : Integer := 32 do
         loop
            exit when Aux / 16#8000_0000# = 1;

            Result := Result - 1;
            Aux    := Aux * 2;
         end loop;
      end return;
   end Minimum_Bits;

   --------------------
   -- Representation --
   --------------------

   function Representation
     (Self : Enumeration_Type'Class;
      Code : UCD.Code_Point) return Natural
   is
      Value : constant not null UCD.Properties.Property_Value_Access :=
        UCD.Characters.Get (Code, Self.Property);

   begin
      return Self.Representation (Value);
   end Representation;

   --------------------
   -- Representation --
   --------------------

   function Representation
     (Self  : Enumeration_Type'Class;
      Value : not null UCD.Properties.Property_Value_Access) return Natural is
   begin
      return Self.Map (Value);
   end Representation;

   ---------------------
   -- Type_Identifier --
   ---------------------

   function Type_Identifier
     (Property : not null UCD.Properties.Property_Access)
      return Wide_Wide_String
   is
      Property_Name : constant Wide_Wide_String :=
        To_Upper (To_Wide_Wide_String (Property.Names.First_Element));

   begin
      return Property_Name & "_Values";
   end Type_Identifier;

   ----------------------
   -- Value_Identifier --
   ----------------------

   function Value_Identifier
     (Property : not null UCD.Properties.Property_Access;
      Value    : not null UCD.Properties.Property_Value_Access)
      return Wide_Wide_String
   is
      Property_Name : constant Wide_Wide_String :=
        To_Upper (To_Wide_Wide_String (Property.Names.First_Element));
      Value_Name    : constant Wide_Wide_String :=
        To_Wide_Wide_String (Value.Names.First_Element);

   begin
      return Property_Name & '_' & Value_Name;
   end Value_Identifier;

end Gen_UCD.Enumeration_Types;

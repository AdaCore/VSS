--
--  Copyright (C) 2021, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;

with Interfaces;

with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

package body UCD.Characters is

   type Boolean_Array is array (1 .. 96) of Boolean with Pack;

   type Unsigned_16_Array is
     array (1 .. 24) of Interfaces.Unsigned_16 with Pack;

   type String_Array is
     array (1 .. 16) of Properties.Property_Value_Access;

   type Character_Record is record
      Boolean     : Boolean_Array;
      Enumeration : Unsigned_16_Array;
      String      : String_Array;
   end record;

   type Character_Array is array (UCD.Code_Point) of aliased Character_Record;

   type Character_Array_Access is access all Character_Array;

   Database : Character_Array_Access;

   function Hash
     (Item : Properties.Property_Access) return Ada.Containers.Hash_Type;

   package Property_Integer_Maps is
     new Ada.Containers.Hashed_Maps
       (Properties.Property_Access,
        Positive,
        Hash,
        Properties."=");

   Boolean_Properties            : Properties.Property_Vectors.Vector;
   Boolean_Property_To_Index     : Property_Integer_Maps.Map;
   Enumeration_Properties        : Properties.Property_Vectors.Vector;
   Enumeration_Property_To_Index : Property_Integer_Maps.Map;
   String_Properties             : Properties.Property_Vectors.Vector;
   String_Property_To_Index      : Property_Integer_Maps.Map;

   function Internal_Enumeration_Value
     (Property  : not null Properties.Property_Access;
      Value     : not null Properties.Property_Value_Access)
      return Interfaces.Unsigned_16;

   procedure Register_String_Property
     (Property : not null Properties.Property_Access);
   --  Register property of "string" type.

   ---------
   -- Get --
   ---------

   function Get
     (Character : Code_Point;
      Property  : not null UCD.Properties.Property_Access)
      return UCD.Properties.Property_Value_Access is
   begin
      if Property.Is_Binary then
         return
           Property.Name_To_Value.Element
             ((if Database (Character).Boolean
                    (Boolean_Property_To_Index.Element (Property))
               then To_Unbounded_Wide_Wide_String ("Y")
               else To_Unbounded_Wide_Wide_String ("N")));

      elsif Property.Is_Enumeration then
         return
           Property.All_Values
             (Positive
                (Database (Character).Enumeration
                   (Enumeration_Property_To_Index.Element (Property))));

      elsif Property.Is_String then
         return
           Database (Character).String
             (String_Property_To_Index.Element (Property));

      else
         raise Program_Error;
      end if;
   end Get;

   ----------
   -- Hash --
   ----------

   function Hash
     (Item : Properties.Property_Access) return Ada.Containers.Hash_Type is
   begin
      return Wide_Wide_Hash (Item.Names.First_Element);
   end Hash;

   -----------------------------------
   -- Initialize_Character_Database --
   -----------------------------------

   procedure Initialize_Character_Database is
      use type Ada.Containers.Count_Type;

   begin
      --  Detect binary properties

      for P of Properties.All_Properties loop

         if P.All_Values.Is_Empty then
            --  Ignore non-enumerated properties.

            null;

         elsif P.All_Values.Length = 2
           and then P.Name_To_Value.Contains
                      (To_Unbounded_Wide_Wide_String ("N"))
           and then P.Name_To_Value.Contains
                      (To_Unbounded_Wide_Wide_String ("Y"))
         then
            P.Is_Binary := True;

            Boolean_Properties.Append (P);
            Boolean_Property_To_Index.Insert
              (P, Boolean_Properties.Last_Index);

         else
            P.Is_Enumeration := True;

            Enumeration_Properties.Append (P);
            Enumeration_Property_To_Index.Insert
              (P, Enumeration_Properties.Last_Index);
         end if;
      end loop;

      Put_Line
        ("  - boolean properties     :"
         & Ada.Containers.Count_Type'Wide_Wide_Image
           (Boolean_Properties.Length)
         & " (of"
         & Integer'Wide_Wide_Image (Boolean_Array'Length)
         & ')');
      Put_Line
        ("  - enumeration properties :"
         & Ada.Containers.Count_Type'Wide_Wide_Image
           (Enumeration_Properties.Length)
         & " (of"
         & Integer'Wide_Wide_Image (Unsigned_16_Array'Length)
         & ')');
      Put_Line
        ("  - string properties      : dynamic (of"
         & Integer'Wide_Wide_Image (String_Array'Length)
         & ')');

      --  Allocate database and reset all information.

      Put_Line ("Initializing in-memory database");
      Database :=
        new Character_Array'
          (others =>
             (Boolean     => (others => False),
              Enumeration => (others => 0),
              String      => (others => null)));

      --  Initialize special cases.

      --  Unicode 13.0: exception: Extended_Pictographic property is Y by
      --  default for unassigned code points in few ranges of the code points.
      --
      --  To construct this property UnicodeData.txt must be loaded first,
      --  thus it is initialized when corresponding data is loaded from the
      --  emoji/emoji-data.txt file.

      --  Default value for General_Category is 'Cn' ("Unassigned")

      declare
         GC_Property    : constant not null Properties.Property_Access :=
           Properties.Resolve ("gc");
         GC_Value       : constant not null Properties.Property_Value_Access :=
           Properties.Resolve (GC_Property, "Cn");
         GC_Index       : constant Positive :=
           Enumeration_Property_To_Index (GC_Property);
         GC_Value_Index : constant Interfaces.Unsigned_16 :=
           Internal_Enumeration_Value (GC_Property, GC_Value);

      begin
         for C in Code_Point loop
            Database (C).Enumeration (GC_Index) := GC_Value_Index;
         end loop;

         GC_Value.Is_Used := True;
      end;

      --  Default value for Canonical_Combining_Class is 'NR' ("Not_Reordered")

      declare
         CCC_Property    : constant not null Properties.Property_Access :=
           Properties.Resolve ("ccc");
         CCC_Value       : constant not null
           Properties.Property_Value_Access :=
             Properties.Resolve (CCC_Property, "NR");
         CCC_Index       : constant Positive :=
           Enumeration_Property_To_Index (CCC_Property);
         CCC_Value_Index : constant Interfaces.Unsigned_16 :=
           Internal_Enumeration_Value (CCC_Property, CCC_Value);

      begin
         for C in Code_Point loop
            Database (C).Enumeration (CCC_Index) := CCC_Value_Index;
         end loop;

         CCC_Value.Is_Used := True;
      end;

      --  Default value for Decomposition_Type is 'None'

      declare
         DT_Property    : constant not null Properties.Property_Access :=
           Properties.Resolve ("dt");
         DT_Value       : constant not null Properties.Property_Value_Access :=
           Properties.Resolve (DT_Property, "None");
         DT_Index       : constant Positive :=
           Enumeration_Property_To_Index (DT_Property);
         DT_Value_Index : constant Interfaces.Unsigned_16 :=
           Internal_Enumeration_Value (DT_Property, DT_Value);

      begin
         for C in Code_Point loop
            Database (C).Enumeration (DT_Index) := DT_Value_Index;
         end loop;

         DT_Value.Is_Used := True;
      end;
   end Initialize_Character_Database;

   --------------------------------
   -- Internal_Enumeration_Value --
   --------------------------------

   function Internal_Enumeration_Value
     (Property  : not null Properties.Property_Access;
      Value     : not null Properties.Property_Value_Access)
      return Interfaces.Unsigned_16
   is
      use type UCD.Properties.Property_Value_Access;

   begin
      for J in Property.All_Values.First_Index
                 .. Property.All_Values.Last_Index
      loop
         if Property.All_Values.Element (J) = Value then
            return Interfaces.Unsigned_16 (J);
         end if;
      end loop;

      raise Program_Error;
   end Internal_Enumeration_Value;

   ------------------------------
   -- Register_String_Property --
   ------------------------------

   procedure Register_String_Property
     (Property : not null Properties.Property_Access) is
   begin
      if not String_Property_To_Index.Contains (Property) then
         String_Properties.Append (Property);
         String_Property_To_Index.Insert
           (Property, String_Properties.Last_Index);
      end if;
   end Register_String_Property;

   ---------
   -- Set --
   ---------

   procedure Set
     (Character : Code_Point;
      Property  : not null UCD.Properties.Property_Access;
      Value     : not null UCD.Properties.Property_Value_Access) is
   begin
      if Property.Is_Binary then
         Database (Character).Boolean
           (Boolean_Property_To_Index.Element (Property)) :=
             Value.Names.First_Element = "Y";

      elsif Property.Is_Enumeration then
         Database (Character).Enumeration
           (Enumeration_Property_To_Index.Element (Property)) :=
             Internal_Enumeration_Value (Property, Value);

         --  Set flag of use of value.

         Value.Is_Used := True;

      elsif Property.Is_String then
         Register_String_Property (Property);

         Database (Character).String
           (String_Property_To_Index.Element (Property)) := Value;

      else
         raise Program_Error;
      end if;
   end Set;

end UCD.Characters;

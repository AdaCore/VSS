------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;

with Interfaces;

with Gen_UCD.Properties;

with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

package body Gen_UCD.Characters is

   type Boolean_Array is array (1 .. 96) of Boolean with Pack;

   type Unsigned_16_Array is
     array (1 .. 24) of Interfaces.Unsigned_16 with Pack;

   type Character_Record is record
      Boolean     : Boolean_Array;
      Enumeration : Unsigned_16_Array;
   end record;

   type Character_Array is
     array (Gen_UCD.Code_Point) of aliased Character_Record;

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

      --  Allocate database and reset all information.

      Put_Line ("Initializing in-memory database");
      Database :=
        new Character_Array'
          (others =>
             (Boolean     => (others => False),
              Enumeration => (others => 0)));

      --  Initialize special cases.

      --  Unicode 13.0: exception: Extended_Pictographic property is Y by
      --  default.

      declare
         ExtPict_Property : constant not null Properties.Property_Access :=
           Properties.Name_To_Property.Element
             (To_Unbounded_Wide_Wide_String ("ExtPict"));
         ExtPict_Index    : constant Positive :=
           Boolean_Property_To_Index (ExtPict_Property);

      begin
         for R of Database.all loop
            R.Boolean (ExtPict_Index) := True;
         end loop;
      end;
   end Initialize_Character_Database;

end Gen_UCD.Characters;

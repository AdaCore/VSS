------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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
--
--  JSON Schema types for Draft 4 JSON Schema specification

with Ada.Containers.Vectors;

with VSS.Strings;
with VSS.String_Vectors;
with VSS.JSON;

package JSON_Schema is
   pragma Preelaborate;

   type Schema is tagged;
   type Schema_Access is access all Schema;

   package Definitions is
      package Schema_Vectors is new Ada.Containers.Vectors
        (Positive, Schema_Access);

      type Schema_Array is new Schema_Vectors.Vector with null record;
      --  Min_Items = 1 ???

      type Positive_Integer is new Integer range 0 .. Integer'Last;

      type Positive_Integer_Default_0 is new Positive_Integer
        with Default_Value => 0;

      type Simple_Types is
        (An_Array,
         A_Boolean,
         An_Integer,
         A_Null,
         A_Number,
         An_Object,
         A_String);

      type String_Array is new VSS.String_Vectors.Virtual_String_Vector
        with null record;
      --  Min_Items = 1, uniqueItems ???
   end Definitions;

   type Boolean_Or_Schema (Is_Boolean : Boolean := False) is record
      case Is_Boolean is
         when True =>
            Boolean : Standard.Boolean;
         when False =>
            Schema  : JSON_Schema.Schema_Access;
      end case;
   end record;

   type String_Or_Schema (Is_String : Boolean := False) is record
      case Is_String is
         when True =>
            String : VSS.Strings.Virtual_String;
         when False =>
            Schema  : JSON_Schema.Schema_Access;
      end case;
   end record;

   type Property is record
      Name   : VSS.Strings.Virtual_String;
      Schema : Schema_Access;
   end record;

   package Property_Vectors is new Ada.Containers.Vectors (Positive, Property);

   package Simple_Type_Vectors is new Ada.Containers.Vectors
     (Positive, Definitions.Simple_Types, Definitions."=");

   type Schema is tagged limited record
      Id                 : VSS.Strings.Virtual_String;
      URI                : VSS.Strings.Virtual_String;
      Title              : VSS.Strings.Virtual_String;
      Description        : VSS.Strings.Virtual_String;
      Multiple_Of        : VSS.JSON.JSON_Number;
      Maximum            : VSS.JSON.JSON_Number;
      Exclusive_Maximum  : Boolean := False;
      Minimum            : VSS.JSON.JSON_Number;
      Exclusive_Minimum  : Boolean := False;
      Max_Length         : Definitions.Positive_Integer;  --  Optional?
      Min_Length         : Definitions.Positive_Integer_Default_0;  --  Opt?
      Pattern            : VSS.Strings.Virtual_String;  --  regexp?
      Additional_Items   : Boolean_Or_Schema;
      Items              : Definitions.Schema_Array;  --  if a single Schema?
      Max_Items          : Definitions.Positive_Integer :=
                            Definitions.Positive_Integer'Last;
      Min_Items          : Definitions.Positive_Integer_Default_0 := 0;
      Unique_Items       : Boolean := False;
      Max_Properties     : Definitions.Positive_Integer :=
                            Definitions.Positive_Integer'Last;
      Min_Properties     : Definitions.Positive_Integer_Default_0 := 0;
      Required           : Definitions.String_Array;

      Additional_Properties : Boolean_Or_Schema;

      Properties         : Property_Vectors.Vector;
      Pattern_Properties : Property_Vectors.Vector;
      Dependencies       : String_Or_Schema;
      Enum               : Definitions.String_Array;
      Kind               : Simple_Type_Vectors.Vector;  -- "type"
      Format             : VSS.Strings.Virtual_String;
      All_Of             : Definitions.Schema_Array;
      Any_Of             : Definitions.Schema_Array;
      One_Of             : Definitions.Schema_Array;
      Negate             : Schema_Access;

      Ref                : VSS.Strings.Virtual_String;
      --  Value of the `$ref` attribute
   end record;

end JSON_Schema;

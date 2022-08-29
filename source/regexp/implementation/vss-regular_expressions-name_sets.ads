--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;

with VSS.Characters;

package VSS.Regular_Expressions.Name_Sets is
   pragma Preelaborate;

   type General_Category_Set is private
     with Aggregate => (Empty       => Empty,
                        Add_Unnamed => Include);

   function "or"
     (Left, Right : General_Category_Set) return General_Category_Set;

   function "not" (Left : General_Category_Set) return General_Category_Set;

   function Empty return General_Category_Set;
   --  Return an empty set

   procedure Include
     (Self  : in out General_Category_Set;
      Value : VSS.Characters.General_Category);
   --  Include a value into set

   function Contains
     (Self  : General_Category_Set;
      Value : VSS.Characters.General_Category) return Boolean
     with Inline;
   --  Check if set contains a value

   procedure To_General_Category_Set
     (Name  : VSS.Strings.Virtual_String;
      Value : out General_Category_Set;
      Ok    : out Boolean);
   --  Return set corresponding to well-known name

   procedure Initialize;
   --  Initialize internal data. The call of it is optional

private

   type Boolean_Array is array
     (VSS.Characters.General_Category) of Boolean
       with Pack;

   type General_Category_Set is new Boolean_Array
     with Default_Component_Value => False;

   function Empty return General_Category_Set is
     (General_Category_Set (Boolean_Array'[others => False]));

   function Contains
     (Self  : General_Category_Set;
      Value : VSS.Characters.General_Category) return Boolean is
        (Self (Value));

end VSS.Regular_Expressions.Name_Sets;

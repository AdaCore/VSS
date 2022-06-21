--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--
--  This parser accepts regular expression patterns described in the ECMAScript
--  2020 standard.

with VSS.Characters;
with VSS.Strings.Character_Iterators;
with VSS.Regular_Expressions.Name_Sets;

private generic
   type Node is private;

   with function Create_Character (Value : VSS.Characters.Virtual_Character)
     return Node is <>;

   with function Create_Character_Range
     (From, To : VSS.Characters.Virtual_Character) return Node is <>;

   with function Create_General_Category_Set
     (Value : Name_Sets.General_Category_Set) return Node is <>;

   with function Create_Simple_Assertion (Kind : Simple_Assertion_Kind)
     return Node is <>;

   with function Create_Sequence (Left, Right : Node) return Node is <>;
   with function Create_Alternative (Left, Right : Node) return Node is <>;
   with function Create_Star (Left : Node) return Node is <>;

   with function Create_Negated_Class (Left : Node) return Node is <>;

   with function Create_Group
     (Left : Node; Group : Positive) return Node is <>;

   with function Create_Empty return Node is <>;

package VSS.Regular_Expressions.ECMA_Parser is

   pragma Preelaborate;

   procedure Parse_Pattern
     (Cursor : in out VSS.Strings.Character_Iterators.Character_Iterator;
      Error  : out VSS.Strings.Virtual_String;
      Result : out Node);

end VSS.Regular_Expressions.ECMA_Parser;

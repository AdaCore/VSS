------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                    Copyright (C) 2020-2022, AdaCore                      --
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
--  This parser accepts regular expression patterns described in the ECMAScript
--  2020 standard.

with VSS.Characters;
with VSS.Strings.Character_Iterators;
with VSS.Regular_Expressions.Name_Sets;

generic
   type Node is private;

   with function Create_Character (Value : VSS.Characters.Virtual_Character)
     return Node is <>;

   with function Create_Character_Range
     (From, To : VSS.Characters.Virtual_Character) return Node is <>;

   with function Create_General_Category_Set
     (Value : Name_Sets.General_Category_Set) return Node is <>;

   with function Create_Sequence (Left, Right : Node) return Node is <>;
   with function Create_Alternative (Left, Right : Node) return Node is <>;
   with function Create_Star (Left : Node) return Node is <>;

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

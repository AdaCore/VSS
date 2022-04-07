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

pragma Warnings (Off);
pragma Ada_2020;
pragma Ada_2022;
pragma Warnings (On);

with VSS.Characters;

package VSS.Regular_Expressions.Name_Sets is
   pragma Preelaborate;

   type General_Category_Set is private
     with Aggregate => (Empty       => Empty,
                        Add_Unnamed => Include);

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

   type General_Category_Set is array
     (VSS.Characters.General_Category) of Boolean
       with Pack, Default_Component_Value => False;

   function Contains
     (Self  : General_Category_Set;
      Value : VSS.Characters.General_Category) return Boolean is
        (Self (Value));

end VSS.Regular_Expressions.Name_Sets;

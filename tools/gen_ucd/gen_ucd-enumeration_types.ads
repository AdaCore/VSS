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

private with Ada.Containers.Hashed_Maps;
with Ada.Wide_Wide_Text_IO;

with UCD.Properties;

package Gen_UCD.Enumeration_Types is

   type Enumeration_Type is tagged limited private;

   procedure Initialize
     (Self     : in out Enumeration_Type'Class;
      Property : not null UCD.Properties.Property_Access);
   --  Initialize mapping of used enumeration property values to continuous
   --  range of natural numbers used for internal representation.

   function Representation
     (Self : Enumeration_Type'Class;
      Code : UCD.Code_Point) return Natural;
   --  Return internal representation of the given value of the property.

   procedure Generate_Type_Declaration
     (Self : Enumeration_Type'Class;
      File : Ada.Wide_Wide_Text_IO.File_Type);
   --  Generate enumeration type declaration and necessary representation
   --  clauses.

private

   package Property_Value_Integer_Maps is
     new Ada.Containers.Hashed_Maps
       (UCD.Properties.Property_Value_Access,
        Natural,
        UCD.Properties.Hash,
        UCD.Properties."=");

   type Enumeration_Type is tagged limited record
      Property : UCD.Properties.Property_Access;
      Map      : Property_Value_Integer_Maps.Map;
   end record;

end Gen_UCD.Enumeration_Types;

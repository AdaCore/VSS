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

with Ada.Containers.Doubly_Linked_Lists;

with VSS.Strings;

package HTML_Writers is

   type HTML_Attribute is record
      Name  : VSS.Strings.Virtual_String;
      Value : VSS.Strings.Virtual_String;
   end record;

   package HTML_Attribute_Lists is new Ada.Containers.Doubly_Linked_Lists
     (HTML_Attribute);

   type HTML_Attributes is new HTML_Attribute_Lists.List with null record;

   No_Attributes : constant HTML_Attributes :=
     (HTML_Attribute_Lists.Empty_List with null record);

   type Writer is tagged limited private;

   procedure Characters
     (Self : in out Writer;
      Text : VSS.Strings.Virtual_String);

   procedure End_Element
     (Self       : in out Writer;
      Local_Name : VSS.Strings.Virtual_String);

   procedure Start_Element
     (Self       : in out Writer;
      Local_Name : VSS.Strings.Virtual_String;
      Attributes : HTML_Attributes'Class := No_Attributes);

private

   type Writer is tagged limited record
      Tag   : VSS.Strings.Virtual_String;
      CDATA : Boolean := False;
   end record;

end HTML_Writers;

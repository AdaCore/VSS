------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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
--  String buffers

with VSS.Characters;

private with VSS.Implementation.String_Buffer_Handlers;

package VSS.Strings.Buffers is

   pragma Preelaborate;

   type Virtual_String_Buffer is
     new VSS.Strings.Virtual_String with private;

   procedure Append
     (Self : in out Virtual_String_Buffer'Class;
      Item : VSS.Characters.Virtual_Character);

private

   type Virtual_String_Buffer is
     new VSS.Strings.Virtual_String with null record;

   function Handler
     (Self : Virtual_String_Buffer'Class)
      return access
        VSS.Implementation.String_Buffer_Handlers
          .Abstract_String_Buffer_Handler'Class;
   --  Returns string data handler should be used to process data of given
   --  object.

end VSS.Strings.Buffers;

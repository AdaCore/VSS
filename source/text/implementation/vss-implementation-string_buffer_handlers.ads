-----------------------------------------------------------------------------
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
--  Abstract_String_Buffer_Hanlder is abstract set of operations on string
--  buffer data. It provides default generic implementation of some operations
--  which derived handlers may override to provide better implementation.

with VSS.Implementation.Strings;
with VSS.Implementation.String_Handlers;
with VSS.Unicode;

package VSS.Implementation.String_Buffer_Handlers is

   pragma Preelaborate;

   type Abstract_String_Buffer_Handler is
     abstract new VSS.Implementation.String_Handlers.Abstract_String_Handler
       with null record;

   not overriding procedure Append
     (Self : Abstract_String_Buffer_Handler;
      Data : in out VSS.Implementation.Strings.String_Data;
      Code : VSS.Unicode.Code_Point) is abstract
     with Pre'Class => Code not in 16#D800# .. 16#DFFF#;
   --  Append single code point to the data.

end VSS.Implementation.String_Buffer_Handlers;

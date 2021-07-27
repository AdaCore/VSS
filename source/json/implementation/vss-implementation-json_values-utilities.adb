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

package body VSS.Implementation.JSON_Values.Utilities is

   -------------------
   -- To_JSON_Value --
   -------------------

   function To_JSON_Value
     (Node : VSS.Implementation.JSON_Values.Node_Access)
      return VSS.JSON.Documents.Values.JSON_Value is
   begin
      if Node = null then
         return (Kind => VSS.JSON.Documents.Values.No_JSON_Value);
      end if;

      case Node.Kind is
         when Array_Node =>
            raise Program_Error;

         when Object_Node =>
            raise Program_Error;

         when String_Node =>
            return
              (VSS.JSON.Documents.Values.JSON_String_Value,
               Node.String_Data);

         when Number_Node =>
            raise Program_Error;
         when Boolean_Node =>
            raise Program_Error;
         when Null_Node =>
            raise Program_Error;
      end case;
   end To_JSON_Value;

end VSS.Implementation.JSON_Values.Utilities;

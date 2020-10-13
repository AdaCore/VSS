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

with System.Atomic_Counters;

with VSS.Implementation.Strings;

package VSS.Implementation.String_Vectors is

   pragma Preelaborate;

   type String_Data_Array is
     array (Positive range <>) of VSS.Implementation.Strings.String_Data;

   type String_Vector_Data (Bulk : Natural) is record
      Counter : System.Atomic_Counters.Atomic_Counter;
      Last    : Natural := 0;
      Data    : String_Data_Array (1 .. Bulk);
   end record;

   type String_Vector_Data_Access is access all String_Vector_Data;

   procedure Reference (Self : String_Vector_Data_Access) with Inline;

   procedure Unreference (Self : in out String_Vector_Data_Access);

   procedure Append
     (Self : in out String_Vector_Data_Access;
      Item : VSS.Implementation.Strings.String_Data);
   --  Appends "copy" of the given string to the end of the string vector.

   procedure Append_And_Move_Ownership
     (Self : in out String_Vector_Data_Access;
      Item : VSS.Implementation.Strings.String_Data);
   --  Appends given string to the end of the string vector with moving of the
   --  ownership of the string to the string vector.

end VSS.Implementation.String_Vectors;

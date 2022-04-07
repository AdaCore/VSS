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

package body VSS.Strings.Internals is

   --------------------------
   -- Data_Access_Constant --
   --------------------------

   function Data_Access_Constant
     (Self : VSS.Strings.Virtual_String'Class)
      return not null VSS.Strings.Internals.String_Data_Constant_Access is
   begin
      return Self.Data'Unchecked_Access;
   end Data_Access_Constant;

   ----------------------------
   -- To_Magic_String_Access --
   ----------------------------

   function To_Magic_String_Access
     (Item : VSS.Implementation.Referrers.Virtual_String_Access)
      return VSS.Implementation.Referrers.Magic_String_Access is
   begin
      return VSS.Implementation.Referrers.Magic_String_Access (Item);
   end To_Magic_String_Access;

   -----------------------
   -- To_Virtual_String --
   -----------------------

   function To_Virtual_String
     (Item : in out VSS.Implementation.Strings.String_Data)
      return VSS.Strings.Virtual_String is
   begin
      return Result : VSS.Strings.Virtual_String do
         Result.Data := Item;

         VSS.Implementation.Strings.Reference (Result.Data);
      end return;
   end To_Virtual_String;

   ------------------------------
   -- To_Virtual_String_Access --
   ------------------------------

   function To_Virtual_String_Access
     (Item : VSS.Implementation.Referrers.Magic_String_Access)
      return VSS.Implementation.Referrers.Virtual_String_Access is
   begin
      return VSS.Implementation.Referrers.Virtual_String_Access (Item);
   end To_Virtual_String_Access;

end VSS.Strings.Internals;

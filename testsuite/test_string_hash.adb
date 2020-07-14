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

with VSS.Strings.Conversions;

procedure Test_String_Hash is

   use type VSS.Strings.Hash_Type;

   N : VSS.Strings.Virtual_String;
   pragma Warnings (Off, N);
   E : VSS.Strings.Virtual_String :=
     VSS.Strings.Conversions.To_Magic_String ("");
   V : VSS.Strings.Virtual_String :=
     VSS.Strings.Conversions.To_Magic_String ("foobar");

begin
   if N.Hash /= 16#CBF2_9CE4_8422_2325# then
      raise Program_Error;
   end if;

   if N.Hash /= E.Hash then
      raise Program_Error;
   end if;

   if V.Hash /= 16#6314_4B53_BA2E_7122# then
      raise Program_Error;
   end if;
end Test_String_Hash;

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

with VSS.Implementation.Environment_Utilities;

package body VSS.Standard_Paths is

   -----------------------
   -- Writable_Location --
   -----------------------

   function Writable_Location
     (Location : Standard_Location) return VSS.Strings.Virtual_String is
   begin
      case Location is
         when Home_Location =>
            declare
               HOME_Value : constant VSS.Strings.Virtual_String :=
                 VSS.Implementation.Environment_Utilities.Get_Env ("HOME");

            begin
               if HOME_Value.Is_Empty then
                  --  XXX Call NSHomeDirectory on Mac OS

                  return "/";

               else
                  return HOME_Value;
                  --  XXX Cleanup, normalization, and NFD conversion
                  --  (on Mac OS) need to be done
               end if;
            end;

         when Temp_Location =>
            declare
               TMPDIR_Value : constant VSS.Strings.Virtual_String :=
                 VSS.Implementation.Environment_Utilities.Get_Env ("TMPDIR");

            begin
               if TMPDIR_Value.Is_Empty then
                  --  XXX Call NSTemporaryDirectory on Mac OS

                  return "/tmp";

               else
                  return TMPDIR_Value;
                  --  XXX Cleanup, normalization, and NFD conversion
                  --  (on Mac OS) need to be done
               end if;
            end;
      end case;
   end Writable_Location;

end VSS.Standard_Paths;

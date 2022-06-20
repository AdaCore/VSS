--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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

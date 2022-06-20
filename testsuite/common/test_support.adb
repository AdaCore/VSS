--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body Test_Support is

   ------------
   -- Assert --
   ------------

   procedure Assert
     (Condition : Boolean;
      Message   : String := "";
      Location  : String := GNAT.Source_Info.Source_Location) is
   begin
      if not Condition then
         raise Test_Failed with "at "
                 & Location
                 & (if Message /= "" then " " & Message else "");
      end if;
   end Assert;

end Test_Support;

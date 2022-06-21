--
--  Copyright (C) 2021-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with GNAT.Source_Info;

package Test_Support is

   Test_Failed : exception;

   procedure Assert
     (Condition : Boolean;
      Message   : String := "";
      Location  : String := GNAT.Source_Info.Source_Location);

end Test_Support;

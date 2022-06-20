--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Strings;

package VSS.Standard_Paths is

   type Standard_Location is
     (Home_Location,
      Temp_Location);

   function Writable_Location
     (Location : Standard_Location) return VSS.Strings.Virtual_String;
   --  Return the directory where files of given kind should be written to,
   --  or an empty string if the location can't be determined.

end VSS.Standard_Paths;

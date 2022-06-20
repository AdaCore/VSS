--
--  Copyright (C) 2020, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Implementation.Strings;
with VSS.Implementation.UTF8_String_Handlers;

package VSS.Implementation.String_Configuration is

   pragma Preelaborate;

   UTF8_In_Place_Handler : aliased
     VSS.Implementation.UTF8_String_Handlers.UTF8_In_Place_String_Handler;

   Default_Handler  : not null
     VSS.Implementation.Strings.String_Handler_Access :=
       VSS.Implementation.UTF8_String_Handlers
         .Global_UTF8_String_Handler'Access;
   In_Place_Handler : not null
     VSS.Implementation.Strings.String_Handler_Access :=
       UTF8_In_Place_Handler'Access;

end VSS.Implementation.String_Configuration;

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

with VSS.Implementation.UTF8_String_Handlers;

private package VSS.Strings.Configuration is

   pragma Preelaborate;

   UTF8_String_Handler   : aliased
     VSS.Implementation.UTF8_String_Handlers.UTF8_String_Handler;
   UTF8_In_Place_Handler : aliased
     VSS.Implementation.UTF8_String_Handlers.UTF8_In_Place_String_Handler;

   Default_Handler  : not null
     VSS.Implementation.Strings.String_Handler_Access :=
       UTF8_String_Handler'Access;
   In_Place_Handler : not null
     VSS.Implementation.Strings.String_Handler_Access :=
       UTF8_In_Place_Handler'Access;

end VSS.Strings.Configuration;

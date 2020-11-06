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

with VSS.Implementation.String_Configuration;

package body VSS.Implementation.Strings is

   -------------
   -- Handler --
   -------------

   function Handler
     (Data : String_Data)
      return VSS.Implementation.Strings.String_Handler_Access is
   begin
      if Data.In_Place then
         return VSS.Implementation.String_Configuration.In_Place_Handler;

      else
         return Data.Handler;
      end if;
   end Handler;

   ---------------
   -- Reference --
   ---------------

   procedure Reference (Data : in out String_Data) is
      Handler : constant VSS.Implementation.Strings.String_Handler_Access :=
        VSS.Implementation.Strings.Handler (Data);

   begin
      if Handler /= null then
         Handler.Reference (Data);
      end if;
   end Reference;

   -----------------
   -- Unreference --
   -----------------

   procedure Unreference (Data : in out String_Data) is
      Handler : constant VSS.Implementation.Strings.String_Handler_Access :=
        VSS.Implementation.Strings.Handler (Data);

   begin
      if Handler /= null then
         Handler.Unreference (Data);
      end if;

      Data := (others => <>);
   end Unreference;

end VSS.Implementation.Strings;

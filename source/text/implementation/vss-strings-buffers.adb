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

with System;

with VSS.Strings.Configuration;

package body VSS.Strings.Buffers is

   ------------
   -- Append --
   ------------

   procedure Append
     (Self : in out Virtual_String_Buffer'Class;
      Item : VSS.Characters.Virtual_Character)
   is
      Handler : access
        VSS.Implementation.String_Buffer_Handlers
          .Abstract_String_Buffer_Handler'Class := Self.Handler;

   begin
      if Handler = null then
         Self.Data :=
           (In_Place => False,
            Handler  => VSS.Strings.Configuration.Default_Handler,
            Pointer  => System.Null_Address,
            others   => <>);

         Handler :=
           VSS.Implementation.String_Buffer_Handlers
             .Abstract_String_Buffer_Handler'Class
               (VSS.Strings.Configuration.Default_Handler.all)'Access;
      end if;

      Handler.Append (Self.Data, VSS.Characters.Virtual_Character'Pos (Item));
   end Append;

   -------------
   -- Handler --
   -------------

   function Handler
     (Self : Virtual_String_Buffer'Class)
      return access
        VSS.Implementation.String_Buffer_Handlers
       .Abstract_String_Buffer_Handler'Class
   is
      use type VSS.Implementation.Strings.String_Handler_Access;

   begin
      if Self.Data.In_Place then
         return
           VSS.Implementation.String_Buffer_Handlers
             .Abstract_String_Buffer_Handler'Class
               (VSS.Strings.Configuration.In_Place_Handler.all)'Access;

      elsif Self.Data.Handler /= null then
         return
           VSS.Implementation.String_Buffer_Handlers
             .Abstract_String_Buffer_Handler'Class
               (Self.Data.Handler.all)'Access;

      else
         return null;
      end if;
   end Handler;

end VSS.Strings.Buffers;

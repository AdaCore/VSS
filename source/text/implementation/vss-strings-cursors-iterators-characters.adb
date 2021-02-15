------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                    Copyright (C) 2020-2021, AdaCore                      --
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

with VSS.Implementation.String_Handlers;

package body VSS.Strings.Cursors.Iterators.Characters is

   use type VSS.Implementation.Strings.String_Handler_Access;

   -------------
   -- Element --
   -------------

   function Element
     (Self : Character_Iterator'Class) return VSS.Characters.Virtual_Character
   is
      Handler : constant VSS.Implementation.Strings.String_Handler_Access
        := (if Self.Owner = null then null
            else VSS.Implementation.Strings.Handler (Self.Owner.Data));

   begin
      if Handler /= null then
         return
           VSS.Characters.Virtual_Character'Val
             (Handler.Element (Self.Owner.Data, Self.Position));
      end if;

      return VSS.Characters.Virtual_Character'Val (16#00_0000#);
   end Element;

   -----------------
   -- Has_Element --
   -----------------

   overriding function Has_Element
     (Self : Character_Iterator) return Boolean
   is
      Handler : constant VSS.Implementation.Strings.String_Handler_Access
        := (if Self.Owner = null then null
            else VSS.Implementation.Strings.Handler (Self.Owner.Data));

   begin
      if Handler /= null then
         return Handler.Has_Character (Self.Owner.Data, Self.Position);
      end if;

      return False;
   end Has_Element;

   -------------
   -- Forward --
   -------------

   overriding function Forward
     (Self : in out Character_Iterator) return Boolean
   is
      Handler : constant  VSS.Implementation.Strings.String_Handler_Access
        := (if Self.Owner = null then null
            else VSS.Implementation.Strings.Handler (Self.Owner.Data));

   begin
      if Handler /= null then
         return Handler.Forward (Self.Owner.Data, Self.Position);
      end if;

      return False;
   end Forward;

end VSS.Strings.Cursors.Iterators.Characters;

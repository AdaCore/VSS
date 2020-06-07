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

with VSS.Strings.Configuration;

package body VSS.Strings.Iterators.Characters is

   -------------
   -- Element --
   -------------

   function Element
     (Self : Character_Iterator'Class) return VSS.Characters.Virtual_Character
   is
   begin
      if Self.Owner /= null then
         if Self.Owner.Data.In_Place then
            return
              VSS.Characters.Virtual_Character'Val
                (VSS.Strings.Configuration.In_Place_Handler.Element
                   (Self.Owner.Data, Self.Position));

         elsif Self.Owner.Data.Handler /= null then
            return
              VSS.Characters.Virtual_Character'Val
                (Self.Owner.Data.Handler.Element
                   (Self.Owner.Data, Self.Position));

         end if;
      end if;

      return VSS.Characters.Virtual_Character'Val (16#00_0000#);
   end Element;

   -------------
   -- Forward --
   -------------

   overriding function Forward
     (Self : in out Character_Iterator) return Boolean is
   begin
      if Self.Owner /= null then
         if Self.Owner.Data.In_Place then
            return
              VSS.Strings.Configuration.In_Place_Handler.Forward
                (Self.Owner.Data, Self.Position);

         elsif Self.Owner.Data.Handler /= null then
            return
              Self.Owner.Data.Handler.Forward (Self.Owner.Data, Self.Position);
         end if;
      end if;

      return False;
   end Forward;

end VSS.Strings.Iterators.Characters;

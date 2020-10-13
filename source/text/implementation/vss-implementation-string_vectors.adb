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

with Ada.Unchecked_Deallocation;

with VSS.Implementation.String_Handlers;

package body VSS.Implementation.String_Vectors is

   procedure Free is
     new Ada.Unchecked_Deallocation
       (VSS.Implementation.String_Vectors.String_Vector_Data,
        VSS.Implementation.String_Vectors.String_Vector_Data_Access);

   procedure Mutate
     (Self   : in out String_Vector_Data_Access;
      Length : Natural);
   --  Prepare object to be modified and reserve space for at least given
   --  number of items.

   ------------
   -- Append --
   ------------

   procedure Append
     (Self : in out String_Vector_Data_Access;
      Item : VSS.Implementation.Strings.String_Data) is
   begin
      Mutate (Self, (if Self = null then 1 else Self.Last + 1));

      Self.Last := Self.Last + 1;
      Self.Data (Self.Last) := Item;

      declare
         Handler : constant access
           VSS.Implementation.String_Handlers.Abstract_String_Handler'Class
             := VSS.Implementation.Strings.Handler (Self.Data (Self.Last));

      begin
         if Handler /= null then
            Handler.Reference (Self.Data (Self.Last));
         end if;
      end;
   end Append;

   -------------------------------
   -- Append_And_Move_Ownership --
   -------------------------------

   procedure Append_And_Move_Ownership
     (Self : in out String_Vector_Data_Access;
      Item : VSS.Implementation.Strings.String_Data) is
   begin
      Mutate (Self, (if Self = null then 1 else Self.Last + 1));

      Self.Last := Self.Last + 1;
      Self.Data (Self.Last) := Item;
   end Append_And_Move_Ownership;

   ------------
   -- Mutate --
   ------------

   procedure Mutate
     (Self   : in out String_Vector_Data_Access;
      Length : Natural)
   is
   begin
      if Self = null then
         Self := new String_Vector_Data (Length);

      elsif not System.Atomic_Counters.Is_One (Self.Counter)
        or else Self.Bulk < Length
      then
         declare
            Old : String_Vector_Data_Access := Self;

         begin
            Self := new String_Vector_Data (Length);
            Self.Last := Old.Last;
            Self.Data (1 .. Old.Last) := Old.Data (1 .. Old.Last);

            if not System.Atomic_Counters.Is_One (Old.Counter) then
               for Data of Self.Data (1 .. Self.Last) loop
                  declare
                     Handler : constant access
                       VSS.Implementation.String_Handlers
                         .Abstract_String_Handler'Class
                           := VSS.Implementation.Strings.Handler (Data);

                  begin
                     if Handler /= null then
                        Handler.Reference (Data);
                     end if;
                  end;
               end loop;

               Unreference (Old);

            else
               Free (Old);
            end if;
         end;
      end if;
   end Mutate;

   ---------------
   -- Reference --
   ---------------

   procedure Reference (Self : String_Vector_Data_Access) is
   begin
      if Self /= null then
         System.Atomic_Counters.Increment (Self.Counter);
      end if;
   end Reference;

   -----------------
   -- Unreference --
   -----------------

   procedure Unreference (Self : in out String_Vector_Data_Access) is
   begin
      if Self /= null
        and then System.Atomic_Counters.Decrement (Self.Counter)
      then
         for J in 1 .. Self.Last loop
            declare
               Handler : constant access
                 VSS.Implementation.String_Handlers
                   .Abstract_String_Handler'Class
                     := VSS.Implementation.Strings.Handler (Self.Data (J));

            begin
               if Handler /= null then
                  Handler.Unreference (Self.Data (J));
               end if;
            end;
         end loop;

         Free (Self);
      end if;
   end Unreference;

end VSS.Implementation.String_Vectors;

------------------------------------------------------------------------------
--                        M A G I C   R U N T I M E                         --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

with Ada.Unchecked_Conversion;
with System.Address_To_Access_Conversions;

package body VSS.Implementation.Node_References is

   use type System.Address;

   type uint128 is mod 2 ** 128;
   for uint128'Size use 128;

   procedure Sync_Add_And_Fetch
     (Ptr   : System.Address;
      Value : uint64);
   pragma Import (Intrinsic, Sync_Add_And_Fetch, "__sync_add_and_fetch_8");

   function Sync_Bool_Compare_And_Swap_128
     (Ptr      : System.Address;
      Expected : uint128;
      Desired  : uint128) return Boolean;
   pragma Import (Intrinsic,
                  Sync_Bool_Compare_And_Swap_128,
                  "__sync_bool_compare_and_swap_16");
   --  "__atomic_compare_exchange_16" is not supported by GCC on x86_64
   --  platform, thus this legacy intrinsic is used.

   function To_uint128 is
     new Ada.Unchecked_Conversion (Internal_Data, uint128);

   ------------
   -- Attach --
   ------------

   function Attach
     (Self    : in out Controller;
      Pointer : System.Address) return Boolean
   is
      Old_Data : Internal_Data;
      New_Data : Internal_Data;

   begin
      loop
         Old_Data := Self.Data;

         exit when Old_Data.Pointer /= System.Null_Address;

         New_Data := (Pointer, Old_Data.Counter);

         if Sync_Bool_Compare_And_Swap_128
           (Ptr      => Self.Data'Address,
            Expected => To_uint128 (Old_Data),
            Desired  => To_uint128 (New_Data))
         then
            return True;
         end if;
      end loop;

      return Old_Data.Pointer = Pointer;
   end Attach;

   ------------
   -- Detach --
   ------------

   function Detach (Self : in out Controller) return Boolean is
      Old_Data : Internal_Data;
      New_Data : Internal_Data;

   begin
      loop
         Old_Data := Self.Data;

         if Old_Data.Pointer = System.Null_Address then
            --  Node has been detached.

            return False;
         end if;

         New_Data.Pointer := System.Null_Address;
         New_Data.Counter := Old_Data.Counter;

         if Sync_Bool_Compare_And_Swap_128
           (Ptr      => Self.Data'Address,
            Expected => To_uint128 (Old_Data),
            Desired  => To_uint128 (New_Data))
         then
            return New_Data.Counter = 0;
         end if;
      end loop;
   end Detach;

   ------------------------
   -- External_Reference --
   ------------------------

   function External_Reference return Controller is
   begin
      return (Data => (System.Null_Address, 1));
   end External_Reference;

   -----------------------------------
   -- Generic_Controller_Operations --
   -----------------------------------

   package body Generic_Controller_Operations is

      package Conversions is
        new System.Address_To_Access_Conversions (Node);

      ------------
      -- Attach --
      ------------

      function Attach
        (Self   : in out Controller;
         Parent : not null Node_Access) return Boolean is
      begin
         return
           Attach
             (Self,
              Conversions.To_Address (Conversions.Object_Pointer (Parent)));
      end Attach;

      ------------------------
      -- Internal_Reference --
      ------------------------

      function Internal_Reference
        (Parent : not null Node_Access) return Controller is
      begin
         return
           Internal_Reference
             (Conversions.To_Address (Conversions.Object_Pointer (Parent)));
      end Internal_Reference;

   end Generic_Controller_Operations;

   ------------------------
   -- Internal_Reference --
   ------------------------

   function Internal_Reference
     (Parent : System.Address) return Controller is
   begin
      return (Data => (Parent, 0));
   end Internal_Reference;

   ------------------
   -- Is_Exclusive --
   ------------------

   function Is_Exclusive (Self : Controller) return Boolean is
   begin
      return Self.Data.Pointer = System.Null_Address and Self.Data.Counter = 1;
   end Is_Exclusive;

   ---------------
   -- Reference --
   ---------------

   procedure Reference (Self : in out Controller) is
   begin
      Sync_Add_And_Fetch (Self.Data.Counter'Address, 1);
   end Reference;

   -----------------
   -- Unreference --
   -----------------

   function Unreference (Self : in out Controller) return Boolean is
      Old_Data : Internal_Data;
      New_Data : Internal_Data;

   begin
      loop
         Old_Data := Self.Data;
         New_Data := (Old_Data.Pointer, Old_Data.Counter - 1);

         if Sync_Bool_Compare_And_Swap_128
              (Self.Data'Address, To_uint128 (Old_Data), To_uint128 (New_Data))
         then
            return
              New_Data.Pointer = System.Null_Address
                and then New_Data.Counter = 0;
         end if;
      end loop;
   end Unreference;

end VSS.Implementation.Node_References;

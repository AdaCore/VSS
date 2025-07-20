--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Unchecked_Deallocation;
with System.Address_To_Access_Conversions;
with System.Atomic_Counters;

package body VSS.Implementation.Storage_Managers.Heap
  with Preelaborate
is

   Growth_Factor            : constant := 32;
   --  The growth factor controls how much extra space is allocated when
   --  we have to increase the size of an allocated unbounded string. By
   --  allocating extra space, we avoid the need to reallocate on every
   --  append, particularly important when a string is built up by repeated
   --  append operations of small pieces. This is expressed as a factor so
   --  32 means add 1/32 of the length of the string as growth space.

   Minimal_Allocation_Block : constant := Standard'Maximum_Alignment;
   --  Allocation will be done by a multiple of Minimal_Allocation_Block.
   --  This causes no memory loss as most (all?) malloc implementations are
   --  obliged to align the returned memory on the maximum alignment as malloc
   --  does not know the target alignment.

   type UTF8_Shared_Segment
     (Capacity : VSS.Unicode.UTF8_Code_Unit_Count) is
   record
      Count : System.Atomic_Counters.Atomic_Counter;
      Data  : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
        (0 .. Capacity);
   end record;

   --  package Conversions is
   --    new System.Address_To_Access_Conversions (UTF8_Shared_Segment);
   --
   --  subtype UTF8_Shared_Segment_Access is Conversions.Object_Pointer;
   type UTF8_Shared_Segment_Access is access all UTF8_Shared_Segment;

   function Aligned_Capacity
     (Capacity : VSS.Unicode.UTF8_Code_Unit_Count)
      return VSS.Unicode.UTF8_Code_Unit_Count;
   --  Returns recommended capacity of the string data storage which is greater
   --  or equal to the specified requested capacity. Calculation take in sense
   --  alignment of the allocated memory segments to use memory effectively by
   --  sequential operations which extends size of the buffer.

   function Get_Shared
     (Self : Heap_Storage_Manager'Class) return UTF8_Shared_Segment_Access;

   procedure Set_Shared
     (Self   : in out Heap_Storage_Manager'Class;
      Shared : UTF8_Shared_Segment_Access);

   procedure Unreference (Shared : in out UTF8_Shared_Segment_Access);

   ----------------------
   -- Aligned_Capacity --
   ----------------------

   function Aligned_Capacity
     (Capacity : VSS.Unicode.UTF8_Code_Unit_Count)
      return VSS.Unicode.UTF8_Code_Unit_Count
   is
      subtype Empty_UTF8_Shared_Segment is UTF8_Shared_Segment (0);

      Static_Size : constant VSS.Unicode.UTF8_Code_Unit_Count :=
        Empty_UTF8_Shared_Segment'Max_Size_In_Storage_Elements;

   begin
      return
        ((Static_Size + Capacity) / Minimal_Allocation_Block + 1)
          * Minimal_Allocation_Block - Static_Size;
   end Aligned_Capacity;

   --------------
   -- Capacity --
   --------------

   overriding function Capacity
     (Self : in out Heap_Storage_Manager)
      return VSS.Unicode.UTF8_Code_Unit_Count is
   begin
      if Self.Pointer = System.Null_Address then
         return 0;

      else
         return Self.Get_Shared.Capacity;
      end if;
   end Capacity;

   ----------------
   -- Get_Shared --
   ----------------

   function Get_Shared
     (Self : Heap_Storage_Manager'Class) return UTF8_Shared_Segment_Access
   is
      package Conversions is
        new System.Address_To_Access_Conversions (UTF8_Shared_Segment);

   begin
      return
        UTF8_Shared_Segment_Access (Conversions.To_Pointer (Self.Pointer));
   end Get_Shared;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self            : in out Heap_Storage_Manager'Class;
      Storage_Address : out System.Address;
      Capacity        : VSS.Unicode.UTF8_Code_Unit_Count)
   is
      Shared : constant not null UTF8_Shared_Segment_Access :=
        new UTF8_Shared_Segment (Aligned_Capacity (Capacity));

   begin
      Self.Set_Shared (Shared);
      Storage_Address := Shared.Data (Shared.Data'First)'Address;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self            : in out Heap_Storage_Manager'Class;
      Storage_Address : out System.Address;
      Data            : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Capacity        : VSS.Unicode.UTF8_Code_Unit_Count)
   is
      Shared : constant not null UTF8_Shared_Segment_Access :=
        new UTF8_Shared_Segment (Aligned_Capacity (Capacity));

   begin
      Shared.Data (0 .. Data'Length - 1) := Data;
      Self.Set_Shared (Shared);
      Storage_Address := Shared.Data (Shared.Data'First)'Address;
   end Initialize;

   ------------
   -- Mutate --
   ------------

   overriding procedure Mutate
     (Self            : in out Heap_Storage_Manager;
      Storage_Address : in out System.Address;
      Capacity        : VSS.Unicode.UTF8_Code_Unit_Count)
   is
      Old_Shared : UTF8_Shared_Segment_Access := Self.Get_Shared;

   begin
      if not System.Atomic_Counters.Is_One (Old_Shared.Count)
        or Old_Shared.Capacity < Capacity
      then
         declare
            New_Capacity : constant VSS.Unicode.UTF8_Code_Unit_Count :=
              Capacity + Capacity / Growth_Factor;
            New_Shared   : constant not null UTF8_Shared_Segment_Access :=
              new UTF8_Shared_Segment (Aligned_Capacity (New_Capacity));

         begin
            New_Shared.Data (0 .. Old_Shared.Capacity) := Old_Shared.Data;
            Self.Set_Shared (New_Shared);
            Storage_Address := New_Shared.Data (New_Shared.Data'First)'Address;
            Unreference (Old_Shared);
         end;
      end if;
   end Mutate;

   ---------------
   -- Reference --
   ---------------

   overriding procedure Reference (Self : in out Heap_Storage_Manager) is
      pragma Suppress (Access_Check);

      Shared : constant UTF8_Shared_Segment_Access := Self.Get_Shared;

   begin
      if Shared /= null then
         System.Atomic_Counters.Increment (Shared.Count);
      end if;
   end Reference;

   ----------------
   -- Set_Shared --
   ----------------

   procedure Set_Shared
     (Self   : in out Heap_Storage_Manager'Class;
      Shared : UTF8_Shared_Segment_Access)
   is
      package Conversions is
        new System.Address_To_Access_Conversions (UTF8_Shared_Segment);

   begin
      Self.Pointer :=
        Conversions.To_Address (Conversions.Object_Pointer (Shared));
   end Set_Shared;

   -----------------
   -- Unreference --
   -----------------

   procedure Unreference (Shared : in out UTF8_Shared_Segment_Access) is
      pragma Suppress (Access_Check);

      procedure Free is
        new Ada.Unchecked_Deallocation
              (UTF8_Shared_Segment, UTF8_Shared_Segment_Access);

   begin
      if Shared /= null
        and then System.Atomic_Counters.Decrement (Shared.Count)
      then
         Free (Shared);
      end if;
   end Unreference;

   -----------------
   -- Unreference --
   -----------------

   overriding procedure Unreference (Self : in out Heap_Storage_Manager) is
      pragma Suppress (Access_Check);

      Shared : UTF8_Shared_Segment_Access := Self.Get_Shared;

   begin
      Self.Pointer := System.Null_Address;
      Unreference (Shared);
   end Unreference;

end VSS.Implementation.Storage_Managers.Heap;

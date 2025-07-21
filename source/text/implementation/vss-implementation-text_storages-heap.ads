--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Default storage manager, it use heap to allocate shared data segments.

with VSS.Implementation.UTF8_Encoding;
with VSS.Unicode;

package VSS.Implementation.Text_Storages.Heap
  with Preelaborate
is

   use type System.Address;
   use type VSS.Unicode.UTF8_Code_Unit_Offset;

   type Heap_Storage is new Abstract_Text_Storage with null record;

   procedure Initialize
     (Self            : in out Heap_Storage'Class;
      Storage_Address : out System.Address;
      Data            : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Capacity        : VSS.Unicode.UTF8_Code_Unit_Count)
     with Pre =>
       Self.Pointer = System.Null_Address
         and Data'Length <= Capacity;

   procedure Initialize
     (Self            : in out Heap_Storage'Class;
      Storage_Address : out System.Address;
      Capacity        : VSS.Unicode.UTF8_Code_Unit_Count)
     with Pre => Self.Pointer = System.Null_Address;

   overriding function Capacity
     (Self : in out Heap_Storage)
      return VSS.Unicode.UTF8_Code_Unit_Count;

   overriding procedure Reference (Self : in out Heap_Storage);

   overriding procedure Unreference (Self : in out Heap_Storage);

   overriding procedure Mutate
     (Self            : in out Heap_Storage;
      Storage_Address : in out System.Address;
      Capacity        : VSS.Unicode.UTF8_Code_Unit_Count);

end VSS.Implementation.Text_Storages.Heap;

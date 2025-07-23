--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Abstract API of the text data storage manager. It is responsible for
--  deallocation of shared memory segments.

pragma Ada_2022;

with System;

with VSS.Unicode;

package VSS.Implementation.Text_Storages
  with Preelaborate
is

   type Abstract_Text_Storage is tagged record
      Pointer : System.Address := System.Null_Address;
      --  Pointer to opaque data managed by the stroage manager. Its content
      --  depends from the particular implementation.
   end record with Preelaborable_Initialization;

   not overriding procedure Reference
     (Self : in out Abstract_Text_Storage) is null;
   --  Called when new instance of the text data is created.

   not overriding procedure Unreference
     (Self : in out Abstract_Text_Storage) is null;
   --  Called when instance of the text data is finalized.

   not overriding function Capacity
     (Self : in out Abstract_Text_Storage)
      return VSS.Unicode.UTF8_Code_Unit_Count is (0);

   not overriding procedure Mutate
     (Self            : in out Abstract_Text_Storage;
      Storage_Address : in out System.Address;
      Size            : VSS.Unicode.UTF8_Code_Unit_Count;
      Capacity        : VSS.Unicode.UTF8_Code_Unit_Count) is null;
   --  Mutate data:
   --    * make data modificable
   --    * reserve requested additional space
   --    * copy `Size` code units when reallocated

end VSS.Implementation.Text_Storages;

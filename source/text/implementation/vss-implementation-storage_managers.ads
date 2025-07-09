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

package VSS.Implementation.Storage_Managers
  with Preelaborate
is

   type Abstract_Storage_Manager is tagged record
      Pointer : System.Address := System.Null_Address;
   end record with Preelaborable_Initialization;

   not overriding procedure Reference
     (Self : in out Abstract_Storage_Manager) is null;

   not overriding procedure Unreference
     (Self : in out Abstract_Storage_Manager) is null;

   not overriding function Capacity
     (Self : in out Abstract_Storage_Manager)
      return VSS.Unicode.UTF8_Code_Unit_Count is (0);

   not overriding procedure Mutate
     (Self            : in out Abstract_Storage_Manager;
      Storage_Address : in out System.Address;
      Capacity        : VSS.Unicode.UTF8_Code_Unit_Count) is null;
   --  Mutate data:
   --    * make data modifiable
   --    * reserve requested additional space

end VSS.Implementation.Storage_Managers;

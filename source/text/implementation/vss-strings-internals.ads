--
--  Copyright (C) 2020-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  @private
--
--  This package is for internal use only.

with VSS.Implementation.Referrers;
with VSS.Implementation.UTF8_Strings;

package VSS.Strings.Internals is

   pragma Preelaborate;

   type String_Data_Constant_Access is
     access constant VSS.Implementation.UTF8_Strings.UTF8_String_Data;
   type String_Data_Variable_Access is
     access all VSS.Implementation.UTF8_Strings.UTF8_String_Data;
   --  This type intended to be used for "hack" code only to use internal
   --  low level string processing API to improve performance of critical
   --  part of the code. It was initially defined to avoid performance penalty
   --  in multitasking applications due to management of accessibility level
   --  value.

   function To_Virtual_String
     (Text : VSS.Implementation.UTF8_Strings.UTF8_String_Data)
      return VSS.Strings.Virtual_String;
   --  Convert string data into virtual string. Data is references.

   function Data_Access_Constant
     (Self : VSS.Strings.Virtual_String'Class)
      return not null VSS.Strings.Internals.String_Data_Constant_Access;
   function Data_Access_Variable
     (Self : in out VSS.Strings.Virtual_String'Class)
      return not null VSS.Strings.Internals.String_Data_Variable_Access;
   --  Return access to string data member of the Virtual_String.

   procedure Set_By_Move
     (Self : in out VSS.Strings.Virtual_String'Class;
      To   : in out VSS.Implementation.UTF8_Strings.UTF8_String_Data);
   --  Set given string to given data. Initial data of the Self is
   --  unreferenced, given data is copied and given value is reset.

   function To_Virtual_String_Access
     (Item : VSS.Implementation.Referrers.Magic_String_Access)
      return VSS.Implementation.Referrers.Virtual_String_Access with Inline;
   function To_Magic_String_Access
     (Item : VSS.Implementation.Referrers.Virtual_String_Access)
      return VSS.Implementation.Referrers.Magic_String_Access with Inline;
   --  Do type conversion. It is intended to be used inside the
   --  VSS.Implementation.Referrals package.

end VSS.Strings.Internals;

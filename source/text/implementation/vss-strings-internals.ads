--
--  Copyright (C) 2020-2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  @private
--
--  This package is for internal use only.

with VSS.Implementation.Referrers;
with VSS.Implementation.Strings;

package VSS.Strings.Internals is

   pragma Preelaborate;

   type String_Data_Constant_Access is
     access constant VSS.Implementation.Strings.String_Data;
   --  This type intended to be used for "hack" code only to use internal
   --  low level string processing API to improve performance of critical
   --  part of the code. It was initially defined to avoid performance penalty
   --  in multitasking applications due to management of accessibility level
   --  value.

   function To_Virtual_String
     (Item : in out VSS.Implementation.Strings.String_Data)
      return VSS.Strings.Virtual_String;
   --  Convert string data into virtual string. Data is references.

   function Data_Access_Constant
     (Self : VSS.Strings.Virtual_String'Class)
      return not null VSS.Strings.Internals.String_Data_Constant_Access;
   --  Return access to string data member of the Virtual_String.

   function To_Virtual_String_Access
     (Item : VSS.Implementation.Referrers.Magic_String_Access)
      return VSS.Implementation.Referrers.Virtual_String_Access with Inline;
   function To_Magic_String_Access
     (Item : VSS.Implementation.Referrers.Virtual_String_Access)
      return VSS.Implementation.Referrers.Magic_String_Access with Inline;
   --  Do type conversion. It is intended to be used inside the
   --  VSS.Implementation.Refrrals package.

end VSS.Strings.Internals;

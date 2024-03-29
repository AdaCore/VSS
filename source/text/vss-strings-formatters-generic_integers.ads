--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package provides formatter for integer types.
--
--  Integer_Formatter supports following formatting options:
--    [+-][0][1-9[0-9]*][#[1-9][0-9]+][_[1-9][0-9]+[_,. ]]
--
--    +  - reserve space for sign, put plus sign for positive values and minus
--         sign for negative values
--    -  - reserve space for sign, SPACE character for positive values and
--         minus sign for negative values
--
--  By default, space for sign is not reserved.
--
--    0  - fill leading zeros
--
--  By default, leading zeros is not filled.
--
--    1-9[0-9]* - number of digits (not include sign)
--
--  By default, shortest number of digits is used.
--
--    #[1-9][0-9]* - base
--
--  By default, base is 10.
--
--    _[1-9][0-9]+[^}]  - groups separation
--
--  Number of digits in group and character to be used as group separator.
--
--  By default, group separation is disabled; default group separator is
--  low line character.

generic
   type Integer_Type is range <>;

package VSS.Strings.Formatters.Generic_Integers is

   pragma Preelaborate;

   type Formatter is
     new VSS.Strings.Formatters.Abstract_Formatter with private;

   function Image (Item : Integer_Type) return Formatter;

   function Image
     (Name : VSS.Strings.Virtual_String;
      Item : Integer_Type) return Formatter;

private

   type Formatter is
     new VSS.Strings.Formatters.Abstract_Formatter with record
      Name  : VSS.Strings.Virtual_String;
      Value : Integer_Type;
   end record;

   overriding function Name
     (Self : Formatter) return VSS.Strings.Virtual_String;

   overriding function Format
     (Self   : Formatter;
      Format : VSS.Strings.Formatters.Format_Information)
      return VSS.Strings.Virtual_String;

end VSS.Strings.Formatters.Generic_Integers;

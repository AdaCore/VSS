--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package provides formatter for integer types.
--
--  Integer_Formatter supports following formatting options:
--    [0][1-9[0-9]*][#[1-9][0-9]+]
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

generic
   type Modular_Type is mod <>;

package VSS.Strings.Formatters.Generic_Modulars is

   pragma Preelaborate;

   type Formatter is
     new VSS.Strings.Formatters.Abstract_Formatter with private;

   function Image (Item : Modular_Type) return Formatter;

   function Image
     (Name : VSS.Strings.Virtual_String;
      Item : Modular_Type) return Formatter;

private

   type Formatter is
     new VSS.Strings.Formatters.Abstract_Formatter with record
      Name  : VSS.Strings.Virtual_String;
      Value : Modular_Type;
   end record;

   overriding function Name
     (Self : Formatter) return VSS.Strings.Virtual_String;

   overriding function Format
     (Self   : Formatter;
      Format : VSS.Strings.Formatters.Format_Information)
      return VSS.Strings.Virtual_String;

end VSS.Strings.Formatters.Generic_Modulars;

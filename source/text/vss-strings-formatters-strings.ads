--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package provides dummy formatter for strings.
--
--  String_Formatter doesn't support any formatting options.

package VSS.Strings.Formatters.Strings is

   pragma Preelaborate;

   type Formatter is
     new VSS.Strings.Formatters.Abstract_Formatter with private;

   function Image (Item : VSS.Strings.Virtual_String) return Formatter;

   function Image
     (Name : VSS.Strings.Virtual_String;
      Item : VSS.Strings.Virtual_String) return Formatter;

private

   type Formatter is
     new VSS.Strings.Formatters.Abstract_Formatter with record
      Name  : VSS.Strings.Virtual_String;
      Value : VSS.Strings.Virtual_String;
   end record;

   overriding function Name
     (Self : Formatter) return VSS.Strings.Virtual_String;

   overriding function Format
     (Self   : Formatter;
      Format : VSS.Strings.Formatters.Format_Information)
      return VSS.Strings.Virtual_String;

end VSS.Strings.Formatters.Strings;

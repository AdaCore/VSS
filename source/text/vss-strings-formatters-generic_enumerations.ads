--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package provides formatter for enumeration types.

generic
   type Enumeration_Type is (<>);

package VSS.Strings.Formatters.Generic_Enumerations is

   pragma Preelaborate;

   type Formatter is
     new VSS.Strings.Formatters.Abstract_Formatter with private;

   function Image (Item : Enumeration_Type) return Formatter;

   function Image
     (Name : VSS.Strings.Virtual_String;
      Item : Enumeration_Type) return Formatter;

private

   type Formatter is
     new VSS.Strings.Formatters.Abstract_Formatter with record
      Name  : VSS.Strings.Virtual_String;
      Value : Enumeration_Type;
   end record;

   overriding function Name
     (Self : Formatter) return VSS.Strings.Virtual_String;

   overriding function Format
     (Self   : Formatter;
      Format : VSS.Strings.Formatters.Format_Information)
      return VSS.Strings.Virtual_String;

end VSS.Strings.Formatters.Generic_Enumerations;

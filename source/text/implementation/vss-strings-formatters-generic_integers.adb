--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package body VSS.Strings.Formatters.Generic_Integers is

   ------------
   -- Format --
   ------------

   overriding function Format
     (Self   : Formatter;
      Format : VSS.Strings.Formatters.Format_Information)
      return VSS.Strings.Virtual_String is
   begin
      return Empty_Virtual_String;
   end Format;

   -----------
   -- Image --
   -----------

   function Image (Item : Integer_Type) return Formatter is
   begin
      return (Value => Item);
   end Image;

end VSS.Strings.Formatters.Generic_Integers;

--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

package body VSS.Strings.Formatters.Strings is

   ------------
   -- Format --
   ------------

   overriding function Format
     (Self   : Formatter;
      Format : VSS.Strings.Formatters.Format_Information)
      return VSS.Strings.Virtual_String is
   begin
      return Self.Value;
   end Format;

   -----------
   -- Image --
   -----------

   function Image (Item : VSS.Strings.Virtual_String) return Formatter is
   begin
      return (Name => <>, Value => Item);
   end Image;

   -----------
   -- Image --
   -----------

   function Image
     (Name : VSS.Strings.Virtual_String;
      Item : VSS.Strings.Virtual_String) return Formatter is
   begin
      return (Name => Name, Value => Item);
   end Image;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Self : Formatter) return VSS.Strings.Virtual_String is
   begin
      return Self.Name;
   end Name;

end VSS.Strings.Formatters.Strings;

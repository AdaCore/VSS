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
      return VSS.Strings.Virtual_String
   is
      Buffer : constant Wide_Wide_String :=
        Integer_Type'Wide_Wide_Image (Self.Value);

   begin
      if Buffer (Buffer'First) = ' ' then
         return
           VSS.Strings.To_Virtual_String
             (Buffer (Buffer'First + 1 .. Buffer'Last));

      else
         return VSS.Strings.To_Virtual_String (Buffer);
      end if;
   end Format;

   -----------
   -- Image --
   -----------

   function Image (Item : Integer_Type) return Formatter is
   begin
      return (Name => <>, Value => Item);
   end Image;

   -----------
   -- Image --
   -----------

   function Image
     (Name : VSS.Strings.Virtual_String;
      Item : Integer_Type) return Formatter is
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

end VSS.Strings.Formatters.Generic_Integers;

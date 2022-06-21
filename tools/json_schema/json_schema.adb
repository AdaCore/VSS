--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body JSON_Schema is

   --------------
   -- Is_False --
   --------------

   function Is_False (Self : Schema'Class) return Boolean is
   begin
      return Self.Negate /= null and then Self.Negate.Is_True;
   end Is_False;

   -------------
   -- Is_True --
   -------------

   function Is_True (Self : Schema'Class) return Boolean is
   begin
      return Self.Additional_Items = null
        and Self.Items.Is_Empty
        and Self.Additional_Properties = null
        and Self.Properties.Is_Empty
        and Self.Pattern_Properties.Is_Empty
        and Self.Property_Names = null
        and Self.Const.Is_Empty
        and Self.Enum.Is_Empty
        and Self.Kind.Is_Empty
        and Self.If_Schema = null
        and Self.All_Of.Is_Empty
        and Self.Any_Of.Is_Empty
        and Self.One_Of.Is_Empty
        and Self.Negate = null;
   end Is_True;

end JSON_Schema;
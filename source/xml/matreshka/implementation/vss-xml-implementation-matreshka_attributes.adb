--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body VSS.XML.Implementation.Matreshka_Attributes is

   ----------------
   -- Get_Length --
   ----------------

   overriding function Get_Length
     (Self : Matreshka_Attributes) return Natural is
   begin
      return Self.Attributes.Length;
   end Get_Length;

   --------------
   -- Get_Name --
   --------------

   overriding function Get_Name
     (Self  : Matreshka_Attributes;
      Index : Positive) return VSS.Strings.Virtual_String is
   begin
      return
        VSS.Strings.To_Virtual_String
          (Self.Attributes.Local_Name (Index).To_Wide_Wide_String);
   end Get_Name;

   -------------
   -- Get_URI --
   -------------

   overriding function Get_URI
     (Self  : Matreshka_Attributes;
      Index : Positive) return VSS.IRIs.IRI is
   begin
      return
        VSS.IRIs.To_IRI
          (VSS.Strings.To_Virtual_String
             (Self.Attributes.Namespace_URI (Index).To_Wide_Wide_String));
   end Get_URI;

   ---------------
   -- Get_Value --
   ---------------

   overriding function Get_Value
     (Self  : Matreshka_Attributes;
      Index : Positive) return VSS.Strings.Virtual_String is
   begin
      return
        VSS.Strings.To_Virtual_String
          (Self.Attributes.Value (Index).To_Wide_Wide_String);
   end Get_Value;

end VSS.XML.Implementation.Matreshka_Attributes;

--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Sax.Symbols;

with VSS.Strings.Conversions;

package body VSS.XML.Implementation.XmlAda_Attributes is

   ----------------
   -- Get_Length --
   ----------------

   overriding function Get_Length (Self : XmlAda_Attributes) return Natural is
   begin
      return Sax.Readers.Get_Length (Self.Attributes);
   end Get_Length;

   --------------
   -- Get_Name --
   --------------

   overriding function Get_Name
     (Self  : XmlAda_Attributes;
      Index : Positive) return VSS.Strings.Virtual_String is
   begin
      if Index <= Sax.Readers.Get_Length (Self.Attributes) then
         return
           VSS.Strings.Conversions.To_Virtual_String
             (Sax.Symbols.Get
                (Sax.Readers.Get_Name (Self.Attributes, Index).Local).all);

      else
         return VSS.Strings.Empty_Virtual_String;
      end if;
   end Get_Name;

   -------------
   -- Get_URI --
   -------------

   overriding function Get_URI
     (Self  : XmlAda_Attributes;
      Index : Positive) return VSS.IRIs.IRI is
   begin
      if Index <= Sax.Readers.Get_Length (Self.Attributes) then
         return
           VSS.IRIs.To_IRI
             (VSS.Strings.Conversions.To_Virtual_String
                (Sax.Symbols.Get
                   (Sax.Readers.Get_Name
                      (Self.Attributes, Index).NS).all));

      else
         return VSS.IRIs.Empty_IRI;
      end if;
   end Get_URI;

   ---------------
   -- Get_Value --
   ---------------

   overriding function Get_Value
     (Self  : XmlAda_Attributes;
      Index : Positive) return VSS.Strings.Virtual_String is
   begin
      if Index <= Sax.Readers.Get_Length (Self.Attributes) then
         return
           VSS.Strings.Conversions.To_Virtual_String
             (Sax.Symbols.Get
                (Sax.Readers.Get_Value (Self.Attributes, Index)).all);

      else
         return VSS.Strings.Empty_Virtual_String;
      end if;
   end Get_Value;

end VSS.XML.Implementation.XmlAda_Attributes;

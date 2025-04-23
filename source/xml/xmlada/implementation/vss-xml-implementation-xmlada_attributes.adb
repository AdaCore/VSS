--
--  Copyright (C) 2022-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Sax.Symbols;

with VSS.Strings.Conversions;
with VSS.XML.Namespaces;

package body VSS.XML.Implementation.XmlAda_Attributes is

   XMLNS_Prefix : constant VSS.Strings.Virtual_String := "xmlns";

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
      Index : Positive) return VSS.IRIs.IRI
   is
      use type VSS.Strings.Virtual_String;

   begin
      if Index <= Sax.Readers.Get_Length (Self.Attributes) then
         declare
            NS : constant VSS.Strings.Virtual_String :=
              VSS.Strings.Conversions.To_Virtual_String
                (Sax.Symbols.Get
                   (Sax.Readers.Get_Name
                      (Self.Attributes, Index).NS).all);

         begin
            if NS = XMLNS_Prefix then
               --  XmlAda doesn't report URI for 'xmlns:<bla>' attributes.

               return VSS.XML.Namespaces.XMLNS_Namespace;

            else
               return VSS.IRIs.To_IRI (NS);
            end if;
         end;

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

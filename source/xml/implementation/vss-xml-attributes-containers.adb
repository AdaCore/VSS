--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

package body VSS.XML.Attributes.Containers is

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Attributes'Class) is
   begin
      Self.Container.Clear;
   end Clear;

   ----------------
   -- Get_Length --
   ----------------

   overriding function Get_Length (Self : Attributes) return Natural is
   begin
      return Natural (Self.Container.Length);
   end Get_Length;

   --------------
   -- Get_Name --
   --------------

   overriding function Get_Name
     (Self  : Attributes;
      Index : Positive) return VSS.Strings.Virtual_String is
   begin
      return Self.Container (Index).Name;
   end Get_Name;

   -------------
   -- Get_URI --
   -------------

   overriding function Get_URI
     (Self  : Attributes;
      Index : Positive) return VSS.IRIs.IRI is
   begin
      return Self.Container (Index).URI;
   end Get_URI;

   ---------------
   -- Get_Value --
   ---------------

   overriding function Get_Value
     (Self  : Attributes;
      Index : Positive) return VSS.Strings.Virtual_String is
   begin
      return Self.Container (Index).Value;
   end Get_Value;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Self  : in out Attributes'Class;
      URI   : VSS.IRIs.IRI;
      Name  : VSS.Strings.Virtual_String;
      Value : VSS.Strings.Virtual_String)
   is
      use type VSS.IRIs.IRI;
      use type VSS.Strings.Virtual_String;

   begin
      for Item of Self.Container loop
         if Item.URI = URI and then Item.Name = Name then
            Item.Value := Value;

            return;
         end if;
      end loop;

      Self.Container.Append ((URI, Name, Value));
   end Insert;

end VSS.XML.Attributes.Containers;

--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Unchecked_Deallocation;

package body VSS.XML.Implementation.Template_Namespaces is

   use type VSS.XML.Templates.Proxies.Proxy_Access;

   ----------
   -- Bind --
   ----------

   procedure Bind
     (Self : in out Namespace'Class;
      Path : VSS.String_Vectors.Virtual_String_Vector;
      Item : not null VSS.XML.Templates.Proxies.Proxy_Access)
   is
      Name : constant VSS.Strings.Virtual_String := Path (1);

   begin
      if Path.Length = 1 then
         Self.Items.Insert (Name, Item);

      else
         declare
            Position : constant Name_Item_Maps.Cursor :=
              Self.Items.Find (Name);
            Child    : VSS.XML.Templates.Proxies.Proxy_Access :=
              (if Name_Item_Maps.Has_Element (Position)
               then Name_Item_Maps.Element (Position) else null);

         begin
            --  if not Name_Item_Maps.Has_Element (Position) then
            --     Self.Items.Insert (Name, new Namespace);
            --     Position := Self.Items (Name);
            --  end if;

            if Child = null then
               Child := new Namespace;
               Self.Items.Insert (Name, Child);
            end if;

            if Child.all
              in VSS.XML.Implementation.Template_Namespaces.Namespace'Class
            then
               VSS.XML.Implementation.Template_Namespaces.Namespace'Class
                 (Child.all).Bind (Path.Delete_First, Item);
            end if;
         end;
      end if;
   end Bind;

   ----------
   -- Bind --
   ----------

   procedure Bind
     (Self : in out Namespace'Class;
      Name : VSS.Strings.Virtual_String;
      Item : not null VSS.XML.Templates.Proxies.Proxy_Access) is
   begin
      Self.Items.Insert (Name, Item);
   end Bind;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Namespace) is
      procedure Free is
        new Ada.Unchecked_Deallocation
          (VSS.XML.Templates.Proxies.Abstract_Proxy'Class,
           VSS.XML.Templates.Proxies.Proxy_Access);

   begin
      for Item of Self.Items loop
         Free (Item);
      end loop;

      Self.Items.Clear;
   end Finalize;

   ---------------------
   -- Resolve_Content --
   ---------------------

   function Resolve_Content
     (Self : Namespace'Class;
      Path : VSS.String_Vectors.Virtual_String_Vector)
      return VSS.Strings.Virtual_String
   is
      Position : constant Name_Item_Maps.Cursor :=
        Self.Items.Find (Path (1));
      Subpath  : constant VSS.String_Vectors.Virtual_String_Vector :=
        Path.Delete_First;
      Item     : constant VSS.XML.Templates.Proxies.Proxy_Access :=
        (if Name_Item_Maps.Has_Element (Position)
         then Name_Item_Maps.Element (Position) else null);

   begin
      if Item /= null then
         if Item.all
              in VSS.XML.Templates.Proxies.Abstract_Content_Proxy'Class
         then
            return
              VSS.XML.Templates.Proxies.Abstract_Content_Proxy'Class
                (Item.all).Content (Subpath);

         elsif Item.all in Namespace'Class then
            return Namespace'Class (Item.all).Resolve_Content (Subpath);
         end if;

      elsif Self.Enclosing /= null then
         return Self.Enclosing.Resolve_Content (Path);
      end if;

      return VSS.Strings.Empty_Virtual_String;
   end Resolve_Content;

   ----------------------
   -- Resolve_Iterable --
   ----------------------

   function Resolve_Iterable
     (Self : Namespace'Class;
      Path : VSS.String_Vectors.Virtual_String_Vector)
      return Iterable_Iterator_Access
   is
      Position : constant Name_Item_Maps.Cursor :=
        Self.Items.Find (Path (1));
      Subpath  : constant VSS.String_Vectors.Virtual_String_Vector :=
        Path.Delete_First;
      Item     : constant VSS.XML.Templates.Proxies.Proxy_Access :=
        (if Name_Item_Maps.Has_Element (Position)
         then Name_Item_Maps.Element (Position) else null);

   begin
      if Item /= null then
         if Item.all
             in VSS.XML.Templates.Proxies.Abstract_Iterable_Proxy'Class
         then
            return
              new VSS.XML.Templates.Proxies.Abstract_Iterable_Iterator'Class'
                    (VSS.XML.Templates.Proxies.Abstract_Iterable_Proxy'Class
                       (Item.all).Iterator (Subpath));

         elsif Item.all in Namespace'Class then
            return Namespace'Class (Item.all).Resolve_Iterable (Subpath);
         end if;

      elsif Self.Enclosing /= null then
         return Self.Enclosing.Resolve_Iterable (Path);
      end if;

      return null;
   end Resolve_Iterable;

   -------------------
   -- Resolve_Value --
   -------------------

   function Resolve_Value
     (Self : Namespace'Class;
      Path : VSS.String_Vectors.Virtual_String_Vector)
      return VSS.XML.Templates.Values.Value
   is
      Position : constant Name_Item_Maps.Cursor :=
        Self.Items.Find (Path (1));
      Subpath  : constant VSS.String_Vectors.Virtual_String_Vector :=
        Path.Delete_First;
      Item     : constant VSS.XML.Templates.Proxies.Proxy_Access :=
        (if Name_Item_Maps.Has_Element (Position)
         then Name_Item_Maps.Element (Position) else null);

   begin
      if Item /= null then
         if Item.all
              in VSS.XML.Templates.Proxies.Abstract_Value_Proxy'Class
         then
            return
              VSS.XML.Templates.Proxies.Abstract_Value_Proxy'Class
                (Item.all).Value (Subpath);

         elsif Item.all in Namespace'Class then
            return Namespace'Class (Item.all).Resolve_Value (Subpath);
         end if;

      elsif Self.Enclosing /= null then
         return Self.Enclosing.Resolve_Value (Path);
      end if;

      return (Kind => VSS.XML.Templates.Values.Error);
   end Resolve_Value;

end VSS.XML.Implementation.Template_Namespaces;

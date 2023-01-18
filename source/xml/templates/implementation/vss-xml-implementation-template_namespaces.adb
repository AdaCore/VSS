--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Unchecked_Deallocation;

package body VSS.XML.Implementation.Template_Namespaces is

   use type VSS.XML.Templates.Proxies.Proxy_Access;

   function Resolve
     (Proxy : in out VSS.XML.Templates.Proxies.Abstract_Proxy'Class;
      Path  : VSS.String_Vectors.Virtual_String_Vector)
      return VSS.XML.Templates.Proxies.Abstract_Proxy'Class;

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

   -------------
   -- Resolve --
   -------------

   procedure Resolve
     (Self  : Namespace'Class;
      Path  : VSS.String_Vectors.Virtual_String_Vector;
      Proxy : out VSS.XML.Templates.Proxies.Proxy_Access;
      Owned : out Boolean)
   is
      procedure Free is
        new Ada.Unchecked_Deallocation
          (VSS.XML.Templates.Proxies.Abstract_Proxy'Class,
           VSS.XML.Templates.Proxies.Proxy_Access);

      Parent : VSS.XML.Templates.Proxies.Proxy_Access;
      Suffix : VSS.String_Vectors.Virtual_String_Vector;

   begin
      Owned := True;
      Self.Resolve (Path, Proxy, Suffix);

      loop
         exit when Proxy = null;
         exit when Suffix.Is_Empty;

         Parent := Proxy;

         if Proxy.all
              in VSS.XML.Templates.Proxies.Abstract_Composite_Proxy'Class
         then
            --  Composite proxy: lookup for next component.

            Proxy :=
              new VSS.XML.Templates.Proxies.Abstract_Proxy'Class'
                (VSS.XML.Templates.Proxies.Abstract_Composite_Proxy'Class
                   (Proxy.all).Component (Suffix (1)));

            Suffix.Delete_First;

         elsif Proxy.all
                 in VSS.XML.Templates.Proxies.Abstract_Iterable_Iterator'Class
         then
            --  "Dereference" iterator

            Proxy :=
              new VSS.XML.Templates.Proxies.Abstract_Proxy'Class'
                    (VSS.XML.Templates.Proxies.Abstract_Iterable_Iterator'Class
                       (Proxy.all).Element);

         elsif Proxy.all in VSS.XML.Templates.Proxies.Error_Proxy'Class then
            raise Program_Error;
      --     return
      --       VSS.XML.Templates.Proxies.Error_Proxy'
      --         (Message =>
      --          VSS.XML.Templates.Proxies.Error_Proxy'Class (Proxy).Message);
      --
         else
            raise Program_Error;
      --     return
      --       VSS.XML.Templates.Proxies.Error_Proxy'
      --         (Message => "proxy is not composite or iterator");
         end if;

         if not Owned then
            Free (Parent);
         end if;

         Owned := False;
      end loop;
   end Resolve;

   -------------
   -- Resolve --
   -------------

   procedure Resolve
     (Self      : Namespace'Class;
      Path      : VSS.String_Vectors.Virtual_String_Vector;
      Proxy     : out VSS.XML.Templates.Proxies.Proxy_Access;
      Remaining : out VSS.String_Vectors.Virtual_String_Vector)
   is
      Position : constant Name_Item_Maps.Cursor :=
        Self.Items.Find (Path (1));
      Suffix   : constant VSS.String_Vectors.Virtual_String_Vector :=
        Path.Delete_First;
      Item     : constant VSS.XML.Templates.Proxies.Proxy_Access :=
        (if Name_Item_Maps.Has_Element (Position)
         then Name_Item_Maps.Element (Position) else null);

   begin
      Proxy := null;
      Remaining.Clear;

      if Item /= null then
         if Item.all in Namespace'Class then
            Namespace'Class (Item.all).Resolve (Suffix, Proxy, Remaining);

         else
            Proxy     := Item;
            Remaining := Suffix;
         end if;

      elsif Self.Enclosing /= null then
         Self.Enclosing.Resolve (Path, Proxy, Remaining);
      end if;
   end Resolve;

   -------------
   -- Resolve --
   -------------

   function Resolve
     (Proxy : in out VSS.XML.Templates.Proxies.Abstract_Proxy'Class;
      Path  : VSS.String_Vectors.Virtual_String_Vector)
      return VSS.XML.Templates.Proxies.Abstract_Proxy'Class is
   begin
      if Proxy
        in VSS.XML.Templates.Proxies.Abstract_Composite_Proxy'Class
      then
         if Path.Length = 1 then
            return
              VSS.XML.Templates.Proxies.Abstract_Composite_Proxy'Class
                (Proxy).Component (Path (1));

         else
            declare
               Aux : VSS.XML.Templates.Proxies.Abstract_Proxy'Class :=
                 VSS.XML.Templates.Proxies.Abstract_Composite_Proxy'Class
                   (Proxy).Component (Path (1));

            begin
               return Resolve (Aux, Path.Delete_First);
            end;
         end if;

      elsif Proxy
        in VSS.XML.Templates.Proxies.Abstract_Iterable_Iterator'Class
      then
         --  "Dereference" iterator

         declare
            Element_Proxy : VSS.XML.Templates.Proxies.Abstract_Proxy'Class :=
              VSS.XML.Templates.Proxies.Abstract_Iterable_Iterator'Class
                (Proxy).Element;

         begin
            return Resolve (Element_Proxy, Path);
         end;

      elsif Proxy in VSS.XML.Templates.Proxies.Error_Proxy'Class then
         return
           VSS.XML.Templates.Proxies.Error_Proxy'
             (Message =>
                VSS.XML.Templates.Proxies.Error_Proxy'Class (Proxy).Message);

      else
         return
           VSS.XML.Templates.Proxies.Error_Proxy'
             (Message => "proxy is not composite or iterator");
      end if;
   end Resolve;

   ----------------------
   -- Resolve_Iterable --
   ----------------------

   function Resolve_Iterable
     (Self    : Namespace'Class;
      Path    : VSS.String_Vectors.Virtual_String_Vector;
      Error   : in out Error_Handler'Class;
      Success : in out Boolean) return Iterable_Iterator_Access
   is
      Binded : VSS.XML.Templates.Proxies.Proxy_Access;
      Suffix : VSS.String_Vectors.Virtual_String_Vector;

   begin
      Self.Resolve (Path, Binded, Suffix);

      if Suffix.Is_Empty then
         if Binded.all
           in VSS.XML.Templates.Proxies.Abstract_Iterable_Proxy'Class
         then
            return
              new VSS.XML.Templates.Proxies.Abstract_Iterable_Iterator'Class'
                (VSS.XML.Templates.Proxies.Abstract_Iterable_Proxy'Class
                   (Binded.all).Iterator);

         else
            raise Program_Error;
         end if;

      else
         declare
            Proxy : VSS.XML.Templates.Proxies.Abstract_Proxy'Class :=
              Resolve (Binded.all, Suffix);

         begin
            if Proxy
              in VSS.XML.Templates.Proxies.Abstract_Iterable_Proxy'Class
            then
               return
                 new
                 VSS.XML.Templates.Proxies.Abstract_Iterable_Iterator'Class'
                   (VSS.XML.Templates.Proxies.Abstract_Iterable_Proxy'Class
                      (Proxy).Iterator);

            elsif Proxy in VSS.XML.Templates.Proxies.Error_Proxy'Class then
               Error.Report_Error
                 (VSS.XML.Templates.Proxies.Error_Proxy'Class (Proxy).Message,
                  Success);

               return null;

            else
               raise Program_Error;
            end if;
         end;
      end if;
   end Resolve_Iterable;

end VSS.XML.Implementation.Template_Namespaces;

--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Containers.Hashed_Maps;
with Ada.Finalization;

with VSS.Strings.Hash;
with VSS.String_Vectors;
with VSS.XML.Templates.Proxies;

package VSS.XML.Implementation.Template_Namespaces is

   package Name_Item_Maps is
     new Ada.Containers.Hashed_Maps
       (VSS.Strings.Virtual_String,
        VSS.XML.Templates.Proxies.Proxy_Access,
        VSS.Strings.Hash,
        VSS.Strings."=",
        VSS.XML.Templates.Proxies."=");

   type Namespace is tagged;

   type Namespace_Access is access all Namespace'Class;

   type Namespace is
     limited new Ada.Finalization.Limited_Controlled
       and VSS.XML.Templates.Proxies.Abstract_Proxy with
   record
      Enclosing : Namespace_Access;
      Items     : Name_Item_Maps.Map;
   end record;

   procedure Resolve
     (Self  : Namespace'Class;
      Path  : VSS.String_Vectors.Virtual_String_Vector;
      Proxy : out VSS.XML.Templates.Proxies.Proxy_Access;
      Owned : out Boolean);
   --  Resolve given path to the proxy.
   --
   --  @param Self   Root namespace
   --  @param Path   Path to resolve
   --  @param Proxy  Proxy found at given path, if any; null overwise
   --  @param Owned
   --    Whether proxy is owned by the namespace. Returned proxy must be
   --    deallocated by the caller when proxy is not owned by the namespace.

   procedure Bind
     (Self : in out Namespace'Class;
      Path : VSS.String_Vectors.Virtual_String_Vector;
      Item : not null VSS.XML.Templates.Proxies.Proxy_Access)
     with Pre => not Path.Is_Empty;

   procedure Bind
     (Self : in out Namespace'Class;
      Name : VSS.Strings.Virtual_String;
      Item : not null VSS.XML.Templates.Proxies.Proxy_Access)
     with Pre => not Name.Is_Empty;

   overriding procedure Finalize (Self : in out Namespace);

end VSS.XML.Implementation.Template_Namespaces;

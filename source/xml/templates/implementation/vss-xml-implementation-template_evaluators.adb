--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Warnings (Off);
pragma Ada_2020;
pragma Ada_2022;
pragma Warnings (On);

--  with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;
--  with VSS.Strings.Conversions; use VSS.Strings.Conversions;

--  private with Ada.Containers.Vectors;
--
--  private with VSS.IRIs;
--  private with VSS.Strings;
--  private with VSS.XML.Attributes;
--  private with VSS.XML.Locators;
with VSS.XML.Templates;

package body VSS.XML.Implementation.Template_Evaluators is

   --------------
   -- Evaluate --
   --------------

   procedure Evaluate
     (Self    : in out Template_Evaluator'Class;
      Binded  :
        not null VSS.XML.Implementation.Template_Namespaces.Namespace_Access;
      Program :
        VSS.XML.Implementation.Template_Programs.Instruction_Vectors.Vector;
      Success : in out Boolean)
   is
      use type VSS.XML.Content_Handlers.SAX_Content_Handler_Access;
      use type VSS.XML.Implementation.Template_Programs.Address;
      use all type VSS.XML.Implementation.Template_Programs.Instruction_Kind;

      procedure Do_Content
        (Instruction : VSS.XML.Implementation.Template_Programs.Instruction);
      --    (Current : VSS.XML.Implementation.Template_Programs.Address);

      procedure Do_Repeat
        (Instruction : VSS.XML.Implementation.Template_Programs.Instruction);
        --  (Current : VSS.XML.Implementation.Template_Programs.Address);

      procedure Execute
        (Current : in out VSS.XML.Implementation.Template_Programs.Address);

      procedure Push_State;
      procedure Pop_State;

      ----------------
      -- Do_Content --
      ----------------

      procedure Do_Content
        (Instruction : VSS.XML.Implementation.Template_Programs.Instruction) is
      begin
         Push_State;
         --  if Self.Current.Element
         --    /= VSS.XML.Implementation.Template_Programs.Null_Address
         --  then
         --     raise Program_Error;
         --  end if;

         Self.Current.Content := Instruction.Content_Path;
      end Do_Content;

      ---------------
      -- Do_Repeat --
      ---------------

      procedure Do_Repeat
        --  (Current : VSS.XML.Implementation.Template_Programs.Address)
        (Instruction : VSS.XML.Implementation.Template_Programs.Instruction)
      is
         --  Inner    : VSS.XML.Implementation.Template_Programs.Address;
         Iterator :
           VSS.XML.Implementation.Template_Namespaces.Iterable_Iterator_Access;

      begin
         --  Iterator :=
         --    new VSS.XML.Templates.Proxies.Abstract_Iterable_Iterator'Class'
         --      (Self.Current.Namespace.Resolve_Iterable
         --         (Program (Current).Path));
         Iterator :=
         --    new VSS.XML.Templates.Proxies.Abstract_Iterable_Iterator'Class'
           Self.Current.Namespace.Resolve_Iterable
             (Instruction.Repeat_Path);
             --  (Program (Current).Repeat_Path);

         Push_State;

         Self.Current.Iterator := Iterator;
         Self.Current.Namespace.Bind
           (Instruction.Identifier,
           --  (Program (Current).Identifier,
            VSS.XML.Templates.Proxy_Access (Iterator));

         --  Put_Line
         --    ('''
         --  & To_Wide_Wide_String (Program (Current).Identifier) & ''');

         --  while Self.Current.Iterator.Next loop
         --     Inner := Current + 1;
         --
         --     while Inner <= Program.Last_Index loop
         --        case Program (Inner).Kind is
         --           when Start_Element =>
         --              if Self.Content /= null then
         --                 Self.Content.Start_Element
         --                   (Program (Inner).URI,
         --                    Program (Inner).Name,
         --                    Program (Inner).Attributes,
         --                    Success);
         --
         --                 exit when not Success;
         --              end if;
         --
         --           when End_Element =>
         --              if Self.Content /= null then
         --                 Self.Content.End_Element
         --                   (Program (Program (Inner).Start_Address).URI,
         --                    Program (Program (Inner).Start_Address).Name,
         --                    Success);
         --
         --                 exit when not Success;
         --              end if;
         --
         --           when Text =>
         --              if Self.Content /= null then
         --                 Self.Content.Characters
         --                   (Program (Inner).Text, Success);
         --
         --                 exit when not Success;
         --              end if;
         --
         --           when Repeat =>
         --              Do_Repeat (Inner);
         --
         --           when Done =>
         --              exit;
         --
         --           when others =>
         --              raise Program_Error with Program (Inner).Kind'Image;
         --        end case;
         --
         --        Inner := @ + 1;
         --     end loop;
         --  end loop;
         --
         --  Self.Current := Self.Stack.Last_Element;
         --  Self.Stack.Delete_Last;
         --
         --  Current := Inner;
      end Do_Repeat;

      -------------
      -- Execute --
      -------------

      procedure Execute
        (Current : in out VSS.XML.Implementation.Template_Programs.Address)
      is
         use type
           VSS.XML.Implementation.Template_Namespaces.Iterable_Iterator_Access;

         Instruction : VSS.XML.Implementation.Template_Programs.Instruction
           renames Program (Current);

      begin
         case Instruction.Kind is
            when Start_Element =>
               if Self.Current.Element =
                 VSS.XML.Implementation.Template_Programs.Null_Address
               then
                  Self.Current.Element := Current;

                  --  raise Program_Error;
               end if;

               if Self.Current.Element = Current
                 and then Self.Current.Iterator /= null
                 and then not Self.Current.Iterator.Next
               then
                  --  Rewind till end of the element.

                  loop
                     Current := @ + 1;

                     exit when Current > Program.Last_Index;

                     exit when Program (Current).Kind = End_Element
                       and then Program (Current).Start_Address
                       = Self.Current.Element;
                  end loop;

                  Pop_State;

               else
                  if Self.Content /= null then
                     Self.Content.Start_Element
                       (Instruction.URI,
                        Instruction.Name,
                        Instruction.Attributes,
                        Success);

                     --  exit when not Success;
                  end if;

                  if Self.Current.Element = Current
                    and then not Self.Current.Content.Is_Empty
                  then
                     if Self.Content /= null then
                        Self.Content.Characters
                          (Self.Current.Namespace.Resolve_Content
                             (Self.Current.Content),
                           Success);
                     end if;

                     --  Rewind till end of the element.

                     loop
                        Current := @ + 1;

                        exit when Current > Program.Last_Index;

                        if Program (Current).Kind = End_Element
                          and then Program (Current).Start_Address
                                     = Self.Current.Element
                        then
                           Current := @ - 1;

                           exit;
                        end if;
                     end loop;
                  end if;
                  --  if Self.Current.Element = Current then
                  --     if not Self.Current.Content.Is_Empty then
                  --        Self.Current.Ignore := Ignore_Children;
                  --     end if;
                  --  end if;
               end if;

            when End_Element =>
               if Self.Content /= null then
                  Self.Content.End_Element
                    (Program (Instruction.Start_Address).URI,
                     Program (Instruction.Start_Address).Name,
                     Success);

                  --  exit when not Success;
               end if;

               if Self.Current.Element = Instruction.Start_Address then
                  if Self.Current.Iterator /= null then
                     Current := Self.Current.Element - 1;

                  else
                     Pop_State;
                  end if;

                  --  raise Program_Error;
               end if;

               --  Current := @ + 1;

            when Text =>
               if Self.Content /= null then
                  Self.Content.Characters (Instruction.Text, Success);

                  --  exit when not Success;
               end if;

               --  Current := @ + 1;
            when Content =>
               Do_Content (Program (Current));

            when Repeat =>
               Do_Repeat (Program (Current));

            when Done =>
               --  exit;
               null;

            when others =>
               raise Program_Error with Instruction.Kind'Image;
         end case;

         Current := @ + 1;
      end Execute;

      ---------------
      -- Pop_State --
      ---------------

      procedure Pop_State is
      begin
         Self.Current := Self.Stack.Last_Element;
         Self.Stack.Delete_Last;
      end Pop_State;

      ----------------
      -- Push_State --
      ----------------

      procedure Push_State is
      begin
         if Self.Current.Element
           = VSS.XML.Implementation.Template_Programs.Null_Address
         then
            --  Start_Element event has not been processed, continue to
            --  build context for following Start_Element even.

            return;
         end if;

         Self.Stack.Append (Self.Current);
         Self.Current :=
           (Element   =>
              VSS.XML.Implementation.Template_Programs.Null_Address,
            Namespace =>
               new VSS.XML.Implementation.Template_Namespaces.Namespace'
              (Enclosing => Self.Current.Namespace, others => <>),
            Iterator  => null,
            Content   => VSS.String_Vectors.Empty_Virtual_String_Vector);
      end Push_State;

      Current : VSS.XML.Implementation.Template_Programs.Address :=
        Program.First_Index;

   begin
      Self.Current.Namespace :=
        new VSS.XML.Implementation.Template_Namespaces.Namespace'
              (Enclosing => Binded, others => <>);

      while Current <= Program.Last_Index loop
         --  case Program (Current).Kind is
         --     when Start_Element =>
         --        if Self.Content /= null then
         --           Self.Content.Start_Element
         --             (Program (Current).URI,
         --              Program (Current).Name,
         --              Program (Current).Attributes,
         --              Success);
         --
         --           exit when not Success;
         --        end if;
         --
         --     when End_Element =>
         --        if Self.Content /= null then
         --           Self.Content.End_Element
         --             (Program (Program (Current).Start_Address).URI,
         --              Program (Program (Current).Start_Address).Name,
         --              Success);
         --
         --           exit when not Success;
         --        end if;
         --
         --     when Text =>
         --        if Self.Content /= null then
         --        Self.Content.Characters (Program (Current).Text, Success);
         --
         --           exit when not Success;
         --        end if;
         --
         --     when Repeat =>
         --        Do_Repeat (Current);
         --
         --     --  when Done =>
         --     --     null;
         --
         --     when others =>
         --        raise Program_Error with Program (Current).Kind'Image;
         --  end case;
         --
         --  Current := @ + 1;
         Execute (Current);
      end loop;
   end Evaluate;

   --  ----------------------
   --  -- Resolve_Iterable --
   --  ----------------------
   --
   --  overriding function Resolve_Iterable
   --    (Self : Namespace;
   --     Path : VSS.String_Vectors.Virtual_String_Vector) return Cursor_Access
   --  is
   --     pragma Assert (not Path.Is_Empty);
   --
   --     Position : constant Named_Item_Maps.Cursor :=
   --       Self.Items.Find (Path (1));
   --     Subpath  : constant VSS.String_Vectors.Virtual_String_Vector :=
   --       Path.Delete_First;
   --
   --  begin
   --     return Result : Cursor_Access do
   --        if Named_Item_Maps.Has_Element (Position) then
   --           declare
   --              Item : constant not null Named_Item_Access :=
   --                Named_Item_Maps.Element (Position);
   --
   --           begin
   --              if Subpath.Is_Empty then
   --                 if Item.all in Iterable_Item'Class then
   --                    Result :=
   --                      new Iterable_Cursor'Class'
   --                            (Iterable_Item'Class (Item.all).Iterator);
   --                 end if;
   --
   --              elsif Item.all in Abstract_Namespace'Class then
   --                 Result :=
   --                   Abstract_Namespace'Class
   --                     (Item.all).Resolve_Iterable (Subpath);
   --              end if;
   --           end;
   --
   --        elsif Self.Enclosing /= null then
   --           Result := Self.Enclosing.Resolve_Iterable (Path);
   --        end if;
   --     end return;
   --  end Resolve_Iterable;

end VSS.XML.Implementation.Template_Evaluators;

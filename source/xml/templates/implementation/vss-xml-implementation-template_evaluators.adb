--
--  Copyright (C) 2022, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;

with Ada.Unchecked_Deallocation;

with VSS.IRIs;
with VSS.XML.Attributes.Containers;
with VSS.XML.Events;
with VSS.XML.Event_Vectors;
with VSS.XML.Implementation.Parse_Errors;
with VSS.XML.Templates.Proxies;
with VSS.XML.Templates.Values;

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

      procedure Do_Start_Element
        (Current : in out VSS.XML.Implementation.Template_Programs.Address);

      procedure Do_Condition
        (Instruction : VSS.XML.Implementation.Template_Programs.Instruction);

      procedure Do_Content
        (Instruction : VSS.XML.Implementation.Template_Programs.Instruction);

      procedure Do_Omit_Tag
        (Instruction : VSS.XML.Implementation.Template_Programs.Instruction);

      procedure Do_Repeat
        (Instruction : VSS.XML.Implementation.Template_Programs.Instruction);

      procedure Execute
        (Current : in out VSS.XML.Implementation.Template_Programs.Address);

      procedure Push_State;
      procedure Pop_State;

      ------------------
      -- Do_Condition --
      ------------------

      procedure Do_Condition
        (Instruction : VSS.XML.Implementation.Template_Programs.Instruction) is
      begin
         Push_State;

         Self.Current.Condition := Instruction.Condition_Path;
         Self.Current.Negate    := Instruction.Negate;
      end Do_Condition;

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

         Self.Current.Content      := Instruction.Content_Path;
         Self.Current.Text_Content := Instruction.Is_Text;
      end Do_Content;

      -----------------
      -- Do_Omit_Tag --
      -----------------

      procedure Do_Omit_Tag
        (Instruction : VSS.XML.Implementation.Template_Programs.Instruction) is
      begin
         Push_State;

         Self.Current.Omit_Tag := Instruction.Omit_Tag;
      end Do_Omit_Tag;

      ---------------
      -- Do_Repeat --
      ---------------

      procedure Do_Repeat
        (Instruction : VSS.XML.Implementation.Template_Programs.Instruction)
      is
         use type
           VSS.XML.Implementation.Template_Namespaces.Iterable_Iterator_Access;
         use type VSS.XML.Error_Handlers.SAX_Error_Handler_Access;

         Iterator :
           VSS.XML.Implementation.Template_Namespaces.Iterable_Iterator_Access;

      begin
         Iterator :=
           Self.Current.Namespace.Resolve_Iterable
             (Instruction.Repeat_Path, Self, Success);

         if Iterator = null then
            if Self.Error /= null then
               declare
                  Error : constant
                    VSS.XML.Implementation.Parse_Errors.Parse_Error_Location :=
                      (Public_Id => <>,
                       System_Id => Self.Current.System_Id,
                       Line      => Self.Current.Line,
                       Column    => Self.Current.Column,
                       Message   => "Unable to resolve path to iterable");

               begin
                  Self.Error.Error (Error, Success);
               end;
            end if;

         else
            Push_State;

            Self.Current.Iterator := Iterator;
            Self.Current.Namespace.Bind
              (Instruction.Identifier,
               VSS.XML.Templates.Proxies.Proxy_Access (Iterator));
         end if;
      end Do_Repeat;

      ----------------------
      -- Do_Start_Element --
      ----------------------

      procedure Do_Start_Element
        (Current : in out VSS.XML.Implementation.Template_Programs.Address)
      is
         use type
           VSS.XML.Implementation.Template_Namespaces.Iterable_Iterator_Access;

         procedure Skip_Element (Preserve_End_Element : Boolean);

         ------------------
         -- Skip_Element --
         ------------------

         procedure Skip_Element (Preserve_End_Element : Boolean) is
         begin
            loop
               Current := @ + 1;

               exit when Current > Program.Last_Index;

               if Program (Current).Kind = End_Element
                 and then Program (Current).Start_Address
                             = Self.Current.Element
               then
                  if Preserve_End_Element then
                     Current := @ - 1;
                  end if;

                  exit;
               end if;
            end loop;
         end Skip_Element;

         Element     :
           constant VSS.XML.Implementation.Template_Programs.Address :=
             Current;
         Instruction : VSS.XML.Implementation.Template_Programs.Instruction
           renames Program (Current);
         Attributes  : VSS.XML.Attributes.Containers.Attributes;

      begin
         if Self.Current.Element =
           VSS.XML.Implementation.Template_Programs.Null_Address
         then
            Self.Current.Element := Current;
         end if;

         if Self.Current.Element = Current
           and then not Self.Current.Condition.Is_Empty
         then
            declare
               Value : constant VSS.XML.Templates.Values.Value :=
                 Self.Current.Namespace.Resolve_Boolean_Value
                   (Self.Current.Condition);

            begin
               case Value.Kind is
                  when VSS.XML.Templates.Values.Boolean =>
                     if not (Value.Boolean_Value xor Self.Current.Negate) then
                        Skip_Element (False);

                        Pop_State;

                        return;
                     end if;

                  when VSS.XML.Templates.Values.Error =>
                     Self.Report_Error (Value.Message, Success);

                  when others =>
                     raise Program_Error;
               end case;
            end;
         end if;

         if Self.Current.Element = Current
           and then Self.Current.Iterator /= null
           and then not Self.Current.Iterator.Next
         then
            --  Rewind till end of the element.

            Skip_Element (False);

            Pop_State;

         else
            --  Process attributes

            loop
               Current := @ + 1;

               exit when Current > Program.Last_Index;
               exit when Program (Current).Kind /= Attribute;

               if Program (Current).Attribute_Path.Is_Empty then
                  Attributes.Insert
                    (Program (Current).Attribute_URI,
                     Program (Current).Attribute_Name,
                     Program (Current).Attribute_Value);

               else
                  declare
                     use type VSS.XML.Templates.Values.Value_Kind;

                     V : constant VSS.XML.Templates.Values.Value :=
                       Self.Current.Namespace.Resolve_Value
                         (Program (Current).Attribute_Path);

                  begin
                     if V.Kind = VSS.XML.Templates.Values.String then
                        Attributes.Insert
                          (Program (Current).Attribute_URI,
                           Program (Current).Attribute_Name,
                           V.String_Value);

                     elsif V.Kind = VSS.XML.Templates.Values.Error then
                        Self.Report_Error (V.Message, Success);

                        raise Program_Error;

                     else
                        raise Program_Error;
                     end if;
                  end;
               end if;
            end loop;

            Current := @ - 1;

            if Self.Current.Element /= Current
              or else not Self.Current.Omit_Tag
            then
               if Self.Content /= null then
                  Self.Content.Start_Element
                    (Instruction.URI,
                     Instruction.Name,
                     Attributes,
                     Success);
               end if;
            end if;

            if Self.Current.Element = Element
              and then not Self.Current.Content.Is_Empty
            then
               if Self.Content /= null then
                  if Self.Current.Text_Content then
                     Self.Content.Characters
                       (Self.Current.Namespace.Resolve_Text_Content
                          (Self.Current.Content, Self, Success),
                        Success);

                  else
                     declare
                        use type VSS.XML.Lexical_Handlers
                                   .SAX_Lexical_Handler_Access;

                        procedure Flush_Start_Element;

                        Content    : constant VSS.XML.Event_Vectors.Vector :=
                          Self.Current.Namespace.Resolve_Structure_Content
                            (Self.Current.Content, Self, Success);
                        URI        : VSS.IRIs.IRI;
                        Name       : VSS.Strings.Virtual_String;
                        Attributes : VSS.XML.Attributes.Containers.Attributes;

                        -------------------------
                        -- Flush_Start_Element --
                        -------------------------

                        procedure Flush_Start_Element is
                        begin
                           if not Name.Is_Empty then
                              Self.Content.Start_Element
                                (URI, Name, Attributes, Success);

                              --  URI.Clear;
                              Name.Clear;
                              Attributes.Clear;
                           end if;
                        end Flush_Start_Element;

                     begin
                        for Item of Content loop
                           case Item.Kind is
                              when VSS.XML.Events.None =>
                                 raise Program_Error;

                              when VSS.XML.Events.Start_Element =>
                                 Flush_Start_Element;

                                 URI  := Item.URI;
                                 Name := Item.Name;

                              when VSS.XML.Events.Attribute =>
                                 Attributes.Insert
                                   (Item.URI, Item.Name, Item.Value);

                              when VSS.XML.Events.End_Element =>
                                 Flush_Start_Element;

                                 if Self.Content /= null then
                                    Self.Content.End_Element
                                      (Item.URI, Item.Name, Success);
                                 end if;

                              when VSS.XML.Events.Comment =>
                                 Flush_Start_Element;

                                 if Self.Lexical /= null then
                                    Self.Lexical.Comment (Item.Text, Success);
                                 end if;

                              when VSS.XML.Events.Processing_Instruction =>
                                 Flush_Start_Element;

                                 if Self.Content /= null then
                                    Self.Content.Processing_Instruction
                                      (Item.Target, Item.Data, Success);
                                 end if;

                              when VSS.XML.Events.Text =>
                                 Flush_Start_Element;

                                 if Self.Content /= null then
                                    Self.Content.Characters
                                      (Item.Text, Success);
                                 end if;

                              when VSS.XML.Events.CDATA =>
                                 Flush_Start_Element;

                                 if Self.Lexical /= null then
                                    Self.Lexical.Start_CDATA (Success);
                                 end if;

                                 if Self.Content /= null then
                                    Self.Content.Characters
                                      (Item.Text, Success);
                                 end if;

                                 if Self.Lexical /= null then
                                    Self.Lexical.End_CDATA (Success);
                                 end if;
                           end case;
                        end loop;
                     end;
                  end if;
               end if;

               --  Rewind till end of the element.

               Skip_Element (True);
            end if;
         end if;
      end Do_Start_Element;

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
               Do_Start_Element (Current);

            when Attribute =>
               --  Attribute instructions are processed together with
               --  Start_Element instruction.

               raise Program_Error;

            when End_Element =>
               if Self.Current.Element /= Instruction.Start_Address
                 or else not Self.Current.Omit_Tag
               then
                  if Self.Content /= null then
                     Self.Content.End_Element
                       (Program (Instruction.Start_Address).URI,
                        Program (Instruction.Start_Address).Name,
                        Success);

                     --  exit when not Success;
                  end if;
               end if;

               if Self.Current.Element = Instruction.Start_Address then
                  if Self.Current.Iterator /= null then
                     Current := Self.Current.Element - 1;

                  else
                     Pop_State;
                  end if;

                  --  raise Program_Error;
               end if;

            when Text =>
               if Self.Content /= null then
                  Self.Content.Characters (Instruction.Text, Success);

                  --  exit when not Success;
               end if;

            when Location =>
               Push_State;

               Self.Current.System_Id := Instruction.System_Id;
               Self.Current.Line      := Instruction.Line;
               Self.Current.Column    := Instruction.Column;

            when Condition =>
               Do_Condition (Program (Current));

            when Content =>
               Do_Content (Program (Current));

            when Omit_Tag =>
               Do_Omit_Tag (Program (Current));

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
         procedure Free is
           new Ada.Unchecked_Deallocation
             (VSS.XML.Implementation.Template_Namespaces.Namespace'Class,
              VSS.XML.Implementation.Template_Namespaces.Namespace_Access);

      begin
         Free (Self.Current.Namespace);
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
           (Element      =>
              VSS.XML.Implementation.Template_Programs.Null_Address,
            Namespace    =>
               new VSS.XML.Implementation.Template_Namespaces.Namespace'
                     (Ada.Finalization.Limited_Controlled with
                        Enclosing => Self.Current.Namespace,
                        others    => <>),
            Condition    => VSS.String_Vectors.Empty_Virtual_String_Vector,
            Negate       => False,
            Iterator     => null,
            Content      => VSS.String_Vectors.Empty_Virtual_String_Vector,
            Text_Content => True,
            Omit_Tag     => False,
            System_Id    => <>,
            Line         => 0,
            Column       => 0);
      end Push_State;

      Current : VSS.XML.Implementation.Template_Programs.Address :=
        Program.First_Index;

   begin
      Self.Current.Namespace :=
        new VSS.XML.Implementation.Template_Namespaces.Namespace'
              (Ada.Finalization.Limited_Controlled with
                 Enclosing => Binded,
                 others    => <>);
      Self.Current.Element :=
        VSS.XML.Implementation.Template_Programs.Address'Last;

      while Current <= Program.Last_Index loop
         Execute (Current);
      end loop;
   end Evaluate;

   ------------------
   -- Report_Error --
   ------------------

   overriding procedure Report_Error
     (Self    : in out Template_Evaluator;
      Message : VSS.Strings.Virtual_String;
      Success : in out Boolean)
   is
      Error :
        constant VSS.XML.Implementation.Parse_Errors.Parse_Error_Location :=
        (Public_Id => <>,
         System_Id => Self.Current.System_Id,
         Line      => Self.Current.Line,
         Column    => Self.Current.Column,
         Message   => Message);

   begin
      Self.Error.Error (Error, Success);
   end Report_Error;

end VSS.XML.Implementation.Template_Evaluators;

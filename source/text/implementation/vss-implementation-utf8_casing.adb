--
--  Copyright (C) 2023-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with VSS.Implementation.Strings;
with VSS.Implementation.UCD_Casing;
with VSS.Implementation.UTF8_Strings.Mutable_Operations;

package body VSS.Implementation.UTF8_Casing is

   ------------------
   -- Convert_Case --
   ------------------

   procedure Convert_Case
     (Text    : VSS.Implementation.UTF8_Strings.UTF8_String_Data;
      Mapping : VSS.Implementation.UTF8_Casing.Case_Mapping;
      Result  : out VSS.Implementation.UTF8_Strings.UTF8_String_Data)
   is
      use type VSS.Unicode.UTF8_Code_Unit_Offset;

   begin
      VSS.Implementation.UTF8_Strings.Mutable_Operations.Initialize
        (Result, Text.Size);

      if Text.Size = 0 then
         return;
      end if;

      declare
         Storage : constant
           VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
             (0 .. Text.Size)
           with Import, Address => Text.Storage_Address;

      begin
         case Mapping is
            when Simple_Lowercase =>
               VSS.Implementation.UTF8_Casing.Convert_Case_Simple
                 (Storage,
                  Text.Size,
                  VSS.Implementation.UCD_Casing_UTF8.Simple_Lowercase_Index,
                  Result);

            when Simple_Titlecase =>
               raise Program_Error;

               --  Convert_Case_Simple
               --    (Source.Storage,
               --     Source.Size,
               --     VSS.Implementation.UCD_Casing_UTF8
               --       .Simple_Titlecase_Index,
               --     Result);

            when Simple_Uppercase =>
               VSS.Implementation.UTF8_Casing.Convert_Case_Simple
                 (Storage,
                  Text.Size,
                  VSS.Implementation.UCD_Casing_UTF8.Simple_Uppercase_Index,
                  Result);

            when Simple_Case_Folding =>
               VSS.Implementation.UTF8_Casing.Convert_Case_Simple
                 (Storage,
                  Text.Size,
                  VSS.Implementation.UCD_Casing_UTF8.Simple_Case_Folding_Index,
                  Result);

            when NFKC_Casefold =>
               VSS.Implementation.UTF8_Casing.Convert_Case_Simple
                 (Storage,
                  Text.Size,
                  VSS.Implementation.UCD_Casing_UTF8.NFKC_Casefold_Index,
                  Result);

            when Lowercase =>
               VSS.Implementation.UTF8_Casing.Convert_Case_Full
                 (Storage,
                  Text.Size,
                  VSS.Implementation.UCD_Casing_UTF8.Full_Lowercase_Index,
                  True,
                  Result);

            when Titlecase =>
               raise Program_Error;

               --  Convert_Case_Full
               --    (Source.Storage,
               --     Source.Size,
               --     VSS.Implementation.UCD_Casing_UTF8.Full_Titlecase_Index,
               --     False,
               --     Result);

            when Uppercase =>
               VSS.Implementation.UTF8_Casing.Convert_Case_Full
                 (Storage,
                  Text.Size,
                  VSS.Implementation.UCD_Casing_UTF8.Full_Uppercase_Index,
                  False,
                  Result);
         end case;
      end;
   end Convert_Case;

   -----------------------
   -- Convert_Case_Full --
   -----------------------

   procedure Convert_Case_Full
     (Source_Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Source_Size    : VSS.Unicode.UTF8_Code_Unit_Count;
      Mapping        :
        VSS.Implementation.UCD_Casing_UTF8.Mapping_Data_Offset_Array;
      To_Lower       : Boolean;
      Result_Data    : in out VSS.Implementation.UTF8_Strings.UTF8_String_Data)
   is
      use type VSS.Unicode.UTF8_Code_Unit_Offset;

      Code    : VSS.Unicode.Code_Point;
      Start   : VSS.Unicode.UTF8_Code_Unit_Offset;
      Offset  : VSS.Unicode.UTF8_Code_Unit_Offset := 0;
      Context : VSS.Implementation.UCD_Casing.Casing_Context;

   begin
      while Offset < Source_Size loop
         Start := Offset;
         VSS.Implementation.UTF8_Encoding.Unchecked_Decode_Forward
           (Source_Storage, Offset, Code);

         declare
            use type VSS.Unicode.Code_Point;

            Info : constant
              VSS.Implementation.UCD_Casing_UTF8.Contextual_Mapping_Information
                := Get_Contextual_Case_Mapping_Information (Mapping, Code);
            Skip : Boolean := False;

         begin
            if To_Lower and Context.Final_Sigma and Code = 16#03A3# then
               declare
                  Suffix_Offset : VSS.Unicode.UTF8_Code_Unit_Offset := Offset;
                  Match         : Boolean := False;
                  Suffix_Info   :
                    VSS.Implementation.UCD_Casing_UTF8
                      .Contextual_Mapping_Information;

               begin
                  while Suffix_Offset < Source_Size loop
                     VSS.Implementation.UTF8_Encoding.Unchecked_Decode_Forward
                       (Source_Storage, Suffix_Offset, Code);
                     Suffix_Info :=
                       Get_Contextual_Case_Mapping_Information (Mapping, Code);

                     if Suffix_Info.Case_Ignorable then
                        null;

                     else
                        Match := Suffix_Info.Cased;

                        exit;
                     end if;
                  end loop;

                  if not Match then
                     --  Replace by 03C2

                     VSS.Implementation.UTF8_Strings.Mutable_Operations
                       .Unchecked_Append
                          (Result_Data, [16#CF#, 16#82#], 0, 2, 1);
                     Skip := True;
                  end if;
               end;
            end if;

            VSS.Implementation.UCD_Casing.Apply (Context, Info.Context_Change);

            if not Skip then
               if Info.Changes then
                  VSS.Implementation.UTF8_Strings.Mutable_Operations
                    .Unchecked_Append
                      (Result_Data,
                       VSS.Implementation.UCD_Casing_UTF8.UTF8_Data_Table,
                       Info.Offset,
                       Info.Count,
                       Info.Length);

               else
                  VSS.Implementation.UTF8_Strings.Mutable_Operations
                    .Unchecked_Append
                      (Result_Data, Source_Storage, Start, Offset - Start, 1);
               end if;
            end if;
         end;
      end loop;
   end Convert_Case_Full;

   -------------------------
   -- Convert_Case_Simple --
   -------------------------

   procedure Convert_Case_Simple
     (Source_Storage : VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array;
      Source_Size    : VSS.Unicode.UTF8_Code_Unit_Count;
      Mapping        :
        VSS.Implementation.UCD_Casing_UTF8.Mapping_Data_Offset_Array;
      Result_Data    : in out
        VSS.Implementation.UTF8_Strings.UTF8_String_Data)
   is
      use type VSS.Unicode.UTF8_Code_Unit_Offset;

      Code   : VSS.Unicode.Code_Point;
      Start  : VSS.Unicode.UTF8_Code_Unit_Offset;
      Offset : VSS.Unicode.UTF8_Code_Unit_Offset := 0;

   begin
      while Offset < Source_Size loop
         Start := Offset;
         VSS.Implementation.UTF8_Encoding.Unchecked_Decode_Forward
           (Source_Storage, Offset, Code);

         declare
            Info : constant
              VSS.Implementation.UCD_Casing_UTF8.Simplified_Mapping_Information
                := Get_Simplified_Case_Mapping_Information (Mapping, Code);

         begin
            if Info.Changes then
               VSS.Implementation.UTF8_Strings.Mutable_Operations
                 .Unchecked_Append
                   (Result_Data,
                    VSS.Implementation.UCD_Casing_UTF8.UTF8_Data_Table,
                    Info.Offset,
                    Info.Count,
                    Info.Length);

            else
               VSS.Implementation.UTF8_Strings.Mutable_Operations
                 .Unchecked_Append
                   (Result_Data, Source_Storage, Start, Offset - Start, 1);
            end if;
         end;
      end loop;
   end Convert_Case_Simple;

   ----------------------
   -- Get_Case_Mapping --
   ----------------------

   procedure Get_Case_Mapping
     (Code    : VSS.Unicode.Code_Point;
      Mapping : VSS.Implementation.UTF8_Casing.Case_Mapping;
      Text    : out VSS.Implementation.UTF8_Strings.UTF8_String_Data)
   is
      use type VSS.Unicode.UTF8_Code_Unit_Offset;

      Changes : Boolean;
      Length  : VSS.Implementation.Strings.Character_Count;
      Offset  : VSS.Unicode.UTF8_Code_Unit_Offset;
      Size    : VSS.Unicode.UTF8_Code_Unit_Count;

   begin
      --  Amount of the data that might be stored in in-place object is know
      --  to be larger that largest case mapping, thus all checks for this case
      --  are omitted to don't have useless code.

      case Mapping is
         when Simple_Lowercase =>
            declare
               Info : constant
                 VSS.Implementation.UCD_Casing_UTF8
                   .Simplified_Mapping_Information :=
                     VSS.Implementation.UTF8_Casing.
                       Get_Simplified_Case_Mapping_Information
                         (VSS.Implementation.UCD_Casing_UTF8
                            .Simple_Lowercase_Index,
                          Code);

            begin
               Changes := Info.Changes;
               Length  := Info.Length;
               Offset  := Info.Offset;
               Size    := Info.Count;
            end;

         when Simple_Titlecase =>
            declare
               Info : constant
                 VSS.Implementation.UCD_Casing_UTF8
                   .Simplified_Mapping_Information :=
                     VSS.Implementation.UTF8_Casing
                       .Get_Simplified_Case_Mapping_Information
                         (VSS.Implementation.UCD_Casing_UTF8
                            .Simple_Titlecase_Index,
                          Code);

            begin
               Changes := Info.Changes;
               Length  := Info.Length;
               Offset  := Info.Offset;
               Size    := Info.Count;
            end;

         when Simple_Uppercase =>
            declare
               Info : constant
                 VSS.Implementation.UCD_Casing_UTF8
                   .Simplified_Mapping_Information :=
                     VSS.Implementation.UTF8_Casing
                       .Get_Simplified_Case_Mapping_Information
                         (VSS.Implementation.UCD_Casing_UTF8
                            .Simple_Uppercase_Index,
                          Code);

            begin
               Changes := Info.Changes;
               Length  := Info.Length;
               Offset  := Info.Offset;
               Size    := Info.Count;
            end;

         when Simple_Case_Folding =>
            declare
               Info : constant
                 VSS.Implementation.UCD_Casing_UTF8
                   .Simplified_Mapping_Information :=
                     VSS.Implementation.UTF8_Casing.
                       Get_Simplified_Case_Mapping_Information
                         (VSS.Implementation.UCD_Casing_UTF8
                            .Simple_Case_Folding_Index,
                          Code);

            begin
               Changes := Info.Changes;
               Length  := Info.Length;
               Offset  := Info.Offset;
               Size    := Info.Count;
            end;

         when NFKC_Casefold =>
            declare
               Info : constant
                 VSS.Implementation.UCD_Casing_UTF8
                   .Simplified_Mapping_Information :=
                     VSS.Implementation.UTF8_Casing
                       .Get_Simplified_Case_Mapping_Information
                       (VSS.Implementation.UCD_Casing_UTF8
                          .NFKC_Casefold_Index,
                        Code);

            begin
               Changes := Info.Changes;
               Length  := Info.Length;
               Offset  := Info.Offset;
               Size    := Info.Count;
            end;

         when Lowercase =>
            declare
               Info : constant
                 VSS.Implementation.UCD_Casing_UTF8
                   .Contextual_Mapping_Information :=
                     VSS.Implementation.UTF8_Casing
                       .Get_Contextual_Case_Mapping_Information
                         (VSS.Implementation.UCD_Casing_UTF8
                            .Full_Lowercase_Index,
                          Code);

            begin
               Changes := Info.Changes;
               Length  := Info.Length;
               Offset  := Info.Offset;
               Size    := Info.Count;
            end;

         when Titlecase =>
            declare
               Info : constant
                 VSS.Implementation.UCD_Casing_UTF8
                   .Contextual_Mapping_Information :=
                     VSS.Implementation.UTF8_Casing
                       .Get_Contextual_Case_Mapping_Information
                         (VSS.Implementation.UCD_Casing_UTF8
                            .Full_Titlecase_Index,
                          Code);

            begin
               Changes := Info.Changes;
               Length  := Info.Length;
               Offset  := Info.Offset;
               Size    := Info.Count;
            end;

         when Uppercase =>
            declare
               Info : constant
                 VSS.Implementation.UCD_Casing_UTF8
                   .Contextual_Mapping_Information :=
                     VSS.Implementation.UTF8_Casing
                       .Get_Contextual_Case_Mapping_Information
                         (VSS.Implementation.UCD_Casing_UTF8
                            .Full_Uppercase_Index,
                          Code);

            begin
               Changes := Info.Changes;
               Length  := Info.Length;
               Offset  := Info.Offset;
               Size    := Info.Count;
            end;
      end case;

      if Changes then
         VSS.Implementation.UTF8_Strings.Mutable_Operations.Append
           (Text,
            VSS.Implementation.UCD_Casing_UTF8.UTF8_Data_Table
              (Offset .. Offset + Size - 1),
            Length);

      else
         VSS.Implementation.UTF8_Strings.Mutable_Operations.Append
           (Text, Code);
      end if;
   end Get_Case_Mapping;

   ---------------------------------------------
   -- Get_Contextual_Case_Mapping_Information --
   ---------------------------------------------

   function Get_Contextual_Case_Mapping_Information
     (Mapping : VSS.Implementation.UCD_Casing_UTF8.Mapping_Data_Offset_Array;
      Code    : VSS.Unicode.Code_Point)
      return VSS.Implementation.UCD_Casing_UTF8.Contextual_Mapping_Information
   is
      use type VSS.Implementation.UCD_Casing_UTF8.Mapping_Data_Offset;
      use type VSS.Unicode.Code_Point;

      Group  : constant VSS.Implementation.UCD_Casing_UTF8.Mapping_Group :=
        VSS.Implementation.UCD_Casing_UTF8.Mapping_Group
          (Code / VSS.Implementation.UCD_Casing_UTF8.Mapping_Group_Size);
      Offset : constant
        VSS.Implementation.UCD_Casing_UTF8.Mapping_Data_Offset :=
          VSS.Implementation.UCD_Casing_UTF8.Mapping_Data_Offset
            (Code mod VSS.Implementation.UCD_Casing_UTF8.Mapping_Group_Size);

   begin
      return
        VSS.Implementation.UCD_Casing_UTF8.Contextual_Mapping_Data_Table
          (Mapping (Group) + Offset);
   end Get_Contextual_Case_Mapping_Information;

   ---------------------------------------------
   -- Get_Simplified_Case_Mapping_Information --
   ---------------------------------------------

   function Get_Simplified_Case_Mapping_Information
     (Mapping : VSS.Implementation.UCD_Casing_UTF8.Mapping_Data_Offset_Array;
      Code    : VSS.Unicode.Code_Point)
      return VSS.Implementation.UCD_Casing_UTF8.Simplified_Mapping_Information
   is
      use type VSS.Implementation.UCD_Casing_UTF8.Mapping_Data_Offset;
      use type VSS.Unicode.Code_Point;

      Group  : constant VSS.Implementation.UCD_Casing_UTF8.Mapping_Group :=
        VSS.Implementation.UCD_Casing_UTF8.Mapping_Group
          (Code / VSS.Implementation.UCD_Casing_UTF8.Mapping_Group_Size);
      Offset : constant
        VSS.Implementation.UCD_Casing_UTF8.Mapping_Data_Offset :=
          VSS.Implementation.UCD_Casing_UTF8.Mapping_Data_Offset
            (Code mod VSS.Implementation.UCD_Casing_UTF8.Mapping_Group_Size);

   begin
      return
        VSS.Implementation.UCD_Casing_UTF8.Simplified_Mapping_Data_Table
          (Mapping (Group) + Offset);
   end Get_Simplified_Case_Mapping_Information;

end VSS.Implementation.UTF8_Casing;

--
--  Copyright (C) 2023-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with VSS.Implementation.UCD_Casing;
with VSS.Implementation.Text_Handlers.UTF8.Variable.Dynamic;
with VSS.Implementation.Text_Handlers.UTF8.Variable.Static;

package body VSS.Implementation.UTF8_Casing is

   procedure Convert_Case
     (Self    :
        VSS.Implementation.Text_Handlers.UTF8.Variable.Dynamic
          .Dynamic_UTF8_Handler;
      Mapping : VSS.Implementation.UTF8_Casing.Case_Mapping;
      Result  : out VSS.Implementation.Strings.String_Data);

   procedure Convert_Case
     (Self    :
        VSS.Implementation.Text_Handlers.UTF8.Variable.Static
          .Static_UTF8_Handler;
      Mapping : VSS.Implementation.UTF8_Casing.Case_Mapping;
      Result  : out VSS.Implementation.Strings.String_Data);

   procedure Get_Case_Mapping
     (Code    : VSS.Unicode.Code_Point;
      Mapping : VSS.Implementation.UTF8_Casing.Case_Mapping;
      Text    : out
        VSS.Implementation.Text_Handlers.UTF8.Variable.Static
          .Static_UTF8_Handler);

   ------------------
   -- Convert_Case --
   ------------------

   procedure Convert_Case
     (Data    : VSS.Implementation.Strings.String_Data;
      Mapping : VSS.Implementation.UTF8_Casing.Case_Mapping;
      Result  : out VSS.Implementation.Strings.String_Data)
   is
      Handler : constant not null
        VSS.Implementation.Strings.Constant_Text_Handler_Access :=
          VSS.Implementation.Strings.Constant_Handler (Data);

   begin
      if Handler.Is_Empty then
         --  String is empty, nothing to do.

         Result := VSS.Implementation.Strings.Null_String_Data;

      elsif Handler.all
              in VSS.Implementation.Text_Handlers.UTF8.Variable.Dynamic
                   .Dynamic_UTF8_Handler
      then
         Convert_Case
           (VSS.Implementation.Text_Handlers.UTF8.Variable.Dynamic
              .Dynamic_UTF8_Handler (Handler.all),
            Mapping,
            Result);

      elsif Handler.all
              in VSS.Implementation.Text_Handlers.UTF8.Variable.Static
                   .Static_UTF8_Handler
      then
         Convert_Case
           (VSS.Implementation.Text_Handlers.UTF8.Variable.Static
              .Static_UTF8_Handler (Handler.all),
            Mapping,
            Result);

      else
         raise Program_Error;
      end if;
   end Convert_Case;

   ------------------
   -- Convert_Case --
   ------------------

   procedure Convert_Case
     (Self    :
        VSS.Implementation.Text_Handlers.UTF8.Variable.Dynamic
          .Dynamic_UTF8_Handler;
      Mapping : VSS.Implementation.UTF8_Casing.Case_Mapping;
      Result  : out VSS.Implementation.Strings.String_Data) is
   begin
      if Self.Is_Empty then
         --  Nothing to do for an empty string.

         Result := VSS.Implementation.Strings.Null_String_Data;

         return;
      end if;

      declare
         Handler : constant not null
           VSS.Implementation.Strings.Variable_Text_Handler_Access :=
             VSS.Implementation.Strings.Variable_Handler (Result);

      begin
         VSS.Implementation.Text_Handlers.UTF8.Variable.Unsafe_Initialize
           (Handler.all, Self.Pointer.Size);
      end;

      case Mapping is
         when Simple_Lowercase =>
            VSS.Implementation.UTF8_Casing.Convert_Case_Simple
              (Self.Pointer.Storage,
               Self.Pointer.Size,
               VSS.Implementation.UCD_Casing_UTF8.Simple_Lowercase_Index,
               Result);

         when Simple_Titlecase =>
            raise Program_Error;

            --  Convert_Case_Simple
            --    (Source.Storage,
            --     Source.Size,
            --     VSS.Implementation.UCD_Casing_UTF8.Simple_Titlecase_Index,
            --     Result);

         when Simple_Uppercase =>
            VSS.Implementation.UTF8_Casing.Convert_Case_Simple
              (Self.Pointer.Storage,
               Self.Pointer.Size,
               VSS.Implementation.UCD_Casing_UTF8.Simple_Uppercase_Index,
               Result);

         when Simple_Case_Folding =>
            VSS.Implementation.UTF8_Casing.Convert_Case_Simple
              (Self.Pointer.Storage,
               Self.Pointer.Size,
               VSS.Implementation.UCD_Casing_UTF8.Simple_Case_Folding_Index,
               Result);

         when NFKC_Casefold =>
            VSS.Implementation.UTF8_Casing.Convert_Case_Simple
              (Self.Pointer.Storage,
               Self.Pointer.Size,
               VSS.Implementation.UCD_Casing_UTF8.NFKC_Casefold_Index,
               Result);

         when Lowercase =>
            VSS.Implementation.UTF8_Casing.Convert_Case_Full
              (Self.Pointer.Storage,
               Self.Pointer.Size,
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
              (Self.Pointer.Storage,
               Self.Pointer.Size,
               VSS.Implementation.UCD_Casing_UTF8.Full_Uppercase_Index,
               False,
               Result);
      end case;
   end Convert_Case;

   ------------------
   -- Convert_Case --
   ------------------

   procedure Convert_Case
     (Self    :
        VSS.Implementation.Text_Handlers.UTF8.Variable.Static
          .Static_UTF8_Handler;
      Mapping : VSS.Implementation.UTF8_Casing.Case_Mapping;
      Result  : out VSS.Implementation.Strings.String_Data) is
   begin
      if Self.Is_Empty then
         --  Nothing to do for an empty string.

         Result := VSS.Implementation.Strings.Null_String_Data;

         return;
      end if;

      declare
         Handler : constant not null
           VSS.Implementation.Strings.Variable_Text_Handler_Access :=
             VSS.Implementation.Strings.Variable_Handler (Result);

      begin
         VSS.Implementation.Text_Handlers.UTF8.Variable.Unsafe_Initialize
           (Handler.all, Self.Size);
      end;

      case Mapping is
         when Simple_Lowercase =>
            VSS.Implementation.UTF8_Casing.Convert_Case_Simple
              (Self.Storage,
               Self.Size,
               VSS.Implementation.UCD_Casing_UTF8.Simple_Lowercase_Index,
               Result);

         when Simple_Titlecase =>
            raise Program_Error;

            --  Convert_Case_Simple
            --    (Source.Storage,
            --     Source.Size,
            --     VSS.Implementation.UCD_Casing_UTF8.Simple_Titlecase_Index,
            --     Result);

         when Simple_Uppercase =>
            VSS.Implementation.UTF8_Casing.Convert_Case_Simple
              (Self.Storage,
               Self.Size,
               VSS.Implementation.UCD_Casing_UTF8.Simple_Uppercase_Index,
               Result);

         when Simple_Case_Folding =>
            VSS.Implementation.UTF8_Casing.Convert_Case_Simple
              (Self.Storage,
               Self.Size,
               VSS.Implementation.UCD_Casing_UTF8.Simple_Case_Folding_Index,
               Result);

         when NFKC_Casefold =>
            VSS.Implementation.UTF8_Casing.Convert_Case_Simple
              (Self.Storage,
               Self.Size,
               VSS.Implementation.UCD_Casing_UTF8.NFKC_Casefold_Index,
               Result);

         when Lowercase =>
            VSS.Implementation.UTF8_Casing.Convert_Case_Full
              (Self.Storage,
               Self.Size,
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
              (Self.Storage,
               Self.Size,
               VSS.Implementation.UCD_Casing_UTF8.Full_Uppercase_Index,
               False,
               Result);
      end case;
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
      Result_Data    : in out VSS.Implementation.Strings.String_Data)
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

                     VSS.Implementation.Text_Handlers.UTF8.Variable
                       .Unchecked_Append
                          (Result_Data, [16#CF#, 16#82#], 0, 2, 1);
                     Skip := True;
                  end if;
               end;
            end if;

            VSS.Implementation.UCD_Casing.Apply (Context, Info.Context_Change);

            if not Skip then
               if Info.Changes then
                  VSS.Implementation.Text_Handlers.UTF8.Variable
                    .Unchecked_Append
                      (Result_Data,
                       VSS.Implementation.UCD_Casing_UTF8.UTF8_Data_Table,
                       Info.Offset,
                       Info.Count,
                       Info.Length);

               else
                  VSS.Implementation.Text_Handlers.UTF8.Variable
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
      Result_Data    : in out VSS.Implementation.Strings.String_Data)
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
               VSS.Implementation.Text_Handlers.UTF8.Variable.Unchecked_Append
                 (Result_Data,
                  VSS.Implementation.UCD_Casing_UTF8.UTF8_Data_Table,
                  Info.Offset,
                  Info.Count,
                  Info.Length);

            else
               VSS.Implementation.Text_Handlers.UTF8.Variable.Unchecked_Append
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
      Data    : out VSS.Implementation.Strings.String_Data) is
   begin
      declare
         Text : constant not null
           VSS.Implementation.Strings.Variable_Text_Handler_Access :=
             VSS.Implementation.Strings.Variable_Handler (Data);

      begin
         VSS.Implementation.Text_Handlers.UTF8.Variable.Unsafe_Initialize
           (Text.all, 0);

         Get_Case_Mapping
           (Code,
            Mapping,
            VSS.Implementation.Text_Handlers.UTF8.Variable.Static
              .Static_UTF8_Handler (Text.all));
      end;
   end Get_Case_Mapping;

   ----------------------
   -- Get_Case_Mapping --
   ----------------------

   procedure Get_Case_Mapping
     (Code    : VSS.Unicode.Code_Point;
      Mapping : VSS.Implementation.UTF8_Casing.Case_Mapping;
      Text    : out
        VSS.Implementation.Text_Handlers.UTF8.Variable.Static
          .Static_UTF8_Handler)
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
         Text.Storage (0 .. Size - 1) :=
           VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
             (VSS.Implementation.UCD_Casing_UTF8.UTF8_Data_Table
                (Offset .. Offset + Size - 1));
         Text.Length := Length;
         Text.Size   := Size;
         Text.Storage (Text.Size) := 16#00#;

      else
         declare
            L  : VSS.Unicode.Scalar_Value_UTF8_Code_Unit_Length;
            U1 : VSS.Unicode.UTF8_Code_Unit;
            U2 : VSS.Unicode.UTF8_Code_Unit;
            U3 : VSS.Unicode.UTF8_Code_Unit;
            U4 : VSS.Unicode.UTF8_Code_Unit;

         begin
            VSS.Implementation.UTF8_Encoding.Encode (Code, L, U1, U2, U3, U4);

            VSS.Implementation.UTF8_Encoding.Unchecked_Store
              (Text.Storage, 0, L, U1, U2, U3, U4);

            Text.Size   := L;
            Text.Length := 1;
            Text.Storage (Text.Size) := 16#00#;
         end;
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

--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

with VSS.Implementation.Null_String_Handlers;
with VSS.Implementation.String_Configuration;
with VSS.Implementation.UCD_Casing;
with VSS.Implementation.UTF8_String_Handlers;

package body VSS.Implementation.UTF8_Casing is

   procedure Convert_Case
     (Self    : VSS.Implementation.UTF8_String_Handlers.UTF8_String_Handler;
      Data    : VSS.Implementation.Strings.String_Data;
      Mapping : VSS.Implementation.UTF8_Casing.Case_Mapping;
      Result  : out VSS.Implementation.Strings.String_Data);

   procedure Convert_Case
     (Self    :
        VSS.Implementation.UTF8_String_Handlers.UTF8_In_Place_String_Handler;
      Data    : VSS.Implementation.Strings.String_Data;
      Mapping : VSS.Implementation.UTF8_Casing.Case_Mapping;
      Result  : out VSS.Implementation.Strings.String_Data);

   procedure Get_Case_Mapping
     (Self    :
        VSS.Implementation.UTF8_String_Handlers.UTF8_In_Place_String_Handler;
      Code    : VSS.Unicode.Code_Point;
      Mapping : VSS.Implementation.UTF8_Casing.Case_Mapping;
      Data    : out VSS.Implementation.Strings.String_Data);

   ------------------
   -- Convert_Case --
   ------------------

   procedure Convert_Case
     (Data    : VSS.Implementation.Strings.String_Data;
      Mapping : VSS.Implementation.UTF8_Casing.Case_Mapping;
      Result  : out VSS.Implementation.Strings.String_Data)
   is
      Handler :
        constant not null VSS.Implementation.Strings.String_Handler_Access :=
          VSS.Implementation.Strings.Handler (Data);

   begin
      if Handler.all
           in VSS.Implementation.Null_String_Handlers.Null_String_Handler
      then
         --  String is empty, nothing to do.

         Handler.Initialize (Result);

      elsif Handler.all
              in VSS.Implementation.UTF8_String_Handlers.UTF8_String_Handler
      then
         Convert_Case
           (VSS.Implementation.UTF8_String_Handlers.UTF8_String_Handler
              (Handler.all),
            Data,
            Mapping,
            Result);

      elsif Handler.all
              in VSS.Implementation.UTF8_String_Handlers
                   .UTF8_In_Place_String_Handler
      then
         Convert_Case
           (VSS.Implementation.UTF8_String_Handlers
              .UTF8_In_Place_String_Handler (Handler.all),
            Data,
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
     (Self    : VSS.Implementation.UTF8_String_Handlers.UTF8_String_Handler;
      Data    : VSS.Implementation.Strings.String_Data;
      Mapping : VSS.Implementation.UTF8_Casing.Case_Mapping;
      Result  : out VSS.Implementation.Strings.String_Data)
   is
      use type VSS.Unicode.UTF8_Code_Unit_Offset;

      Source : VSS.Implementation.UTF8_String_Handlers.UTF8_String_Data_Access
        with Import, Convention => Ada, Address => Data.Pointer'Address;

   begin
      Self.Initialize (Result);

      if Self.Is_Empty (Data) then
         --  Nothing to do for an empty string.

         return;
      end if;

      declare
         Target :
           VSS.Implementation.UTF8_String_Handlers.UTF8_String_Data_Access
             with Import, Convention => Ada, Address => Result.Pointer'Address;

      begin
         Target :=
           VSS.Implementation.UTF8_String_Handlers.Allocate
             (VSS.Unicode.UTF8_Code_Unit_Count (Data.Capacity) * 4,
              Source.Size);
      end;

      case Mapping is
         when Simple_Lowercase =>
            VSS.Implementation.UTF8_Casing.Convert_Case_Simple
              (Source.Storage,
               Source.Size,
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
              (Source.Storage,
               Source.Size,
               VSS.Implementation.UCD_Casing_UTF8.Simple_Uppercase_Index,
               Result);

         when NFKC_Casefold =>
            VSS.Implementation.UTF8_Casing.Convert_Case_Simple
              (Source.Storage,
               Source.Size,
               VSS.Implementation.UCD_Casing_UTF8.NFKC_Casefold_Index,
               Result);

         when Lowercase =>
            VSS.Implementation.UTF8_Casing.Convert_Case_Full
              (Source.Storage,
               Source.Size,
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
              (Source.Storage,
               Source.Size,
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
        VSS.Implementation.UTF8_String_Handlers.UTF8_In_Place_String_Handler;
      Data    : VSS.Implementation.Strings.String_Data;
      Mapping : VSS.Implementation.UTF8_Casing.Case_Mapping;
      Result  : out VSS.Implementation.Strings.String_Data)
   is
      Source : VSS.Implementation.UTF8_String_Handlers.UTF8_In_Place_Data
        with Import, Convention => Ada, Address => Data'Address;

   begin
      Self.Initialize (Result);

      case Mapping is
         when Simple_Lowercase =>
            VSS.Implementation.UTF8_Casing.Convert_Case_Simple
              (Source.Storage,
               Source.Size,
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
              (Source.Storage,
               Source.Size,
               VSS.Implementation.UCD_Casing_UTF8.Simple_Uppercase_Index,
               Result);

         when NFKC_Casefold =>
            VSS.Implementation.UTF8_Casing.Convert_Case_Simple
              (Source.Storage,
               Source.Size,
               VSS.Implementation.UCD_Casing_UTF8.NFKC_Casefold_Index,
               Result);

         when Lowercase =>
            VSS.Implementation.UTF8_Casing.Convert_Case_Full
              (Source.Storage,
               Source.Size,
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
              (Source.Storage,
               Source.Size,
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

                     VSS.Implementation.UTF8_String_Handlers.Unchecked_Append
                       (Result_Data, (16#CF#, 16#82#), 0, 2, 1);
                     Skip := True;
                  end if;
               end;
            end if;

            VSS.Implementation.UCD_Casing.Apply (Context, Info.Context_Change);

            if not Skip then
               if Info.Changes then
                  VSS.Implementation.UTF8_String_Handlers.Unchecked_Append
                    (Result_Data,
                     VSS.Implementation.UCD_Casing_UTF8.UTF8_Data_Table,
                     Info.Offset,
                     Info.Count,
                     Info.Length);

               else
                  VSS.Implementation.UTF8_String_Handlers.Unchecked_Append
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
               VSS.Implementation.UTF8_String_Handlers.Unchecked_Append
                 (Result_Data,
                  VSS.Implementation.UCD_Casing_UTF8.UTF8_Data_Table,
                  Info.Offset,
                  Info.Count,
                  Info.Length);

            else
               VSS.Implementation.UTF8_String_Handlers.Unchecked_Append
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
      Get_Case_Mapping
        (VSS.Implementation.UTF8_String_Handlers.UTF8_In_Place_String_Handler
           (VSS.Implementation.String_Configuration.In_Place_Handler.all),
         Code,
         Mapping,
         Data);
   end Get_Case_Mapping;

   ----------------------
   -- Get_Case_Mapping --
   ----------------------

   procedure Get_Case_Mapping
     (Self    :
        VSS.Implementation.UTF8_String_Handlers.UTF8_In_Place_String_Handler;
      Code    : VSS.Unicode.Code_Point;
      Mapping : VSS.Implementation.UTF8_Casing.Case_Mapping;
      Data    : out VSS.Implementation.Strings.String_Data)
   is
      use type VSS.Unicode.UTF8_Code_Unit_Offset;

      Changes : Boolean;
      Length  : VSS.Implementation.Strings.Character_Count;
      Offset  : VSS.Unicode.UTF8_Code_Unit_Offset;
      Size    : VSS.Unicode.UTF8_Code_Unit_Count;

      Target : VSS.Implementation.UTF8_String_Handlers.UTF8_In_Place_Data
        with Import, Convention => Ada, Address => Data'Address;

   begin
      Self.Initialize (Data);

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
         Target.Storage (0 .. Size - 1) :=
           VSS.Implementation.UTF8_Encoding.UTF8_Code_Unit_Array
             (VSS.Implementation.UCD_Casing_UTF8.UTF8_Data_Table
                (Offset .. Offset + Size - 1));
         Target.Length := Length;
         Target.Size   := Size;
         Target.Storage (Target.Size) := 16#00#;

      else
         declare
            L  : VSS.Implementation.UTF8_Encoding.UTF8_Sequence_Length;
            U1 : VSS.Unicode.UTF8_Code_Unit;
            U2 : VSS.Unicode.UTF8_Code_Unit;
            U3 : VSS.Unicode.UTF8_Code_Unit;
            U4 : VSS.Unicode.UTF8_Code_Unit;

         begin
            VSS.Implementation.UTF8_Encoding.Encode (Code, L, U1, U2, U3, U4);

            VSS.Implementation.UTF8_Encoding.Unchecked_Store
              (Target.Storage, 0, L, U1, U2, U3, U4);

            Target.Size   := L;
            Target.Length := 1;
            Target.Storage (Target.Size) := 16#00#;
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

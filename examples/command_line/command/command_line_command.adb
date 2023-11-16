
with VSS.Application;
with VSS.Command_Line.Parsers;
with VSS.Strings.Formatters.Strings;
with VSS.Strings.Templates;
with VSS.Text_Streams.Standards;

procedure Command_Line_Command is

   use type VSS.Strings.Virtual_String;

   Parser                : VSS.Command_Line.Parsers.Command_Line_Parser;
   Global_Help_Option    : constant VSS.Command_Line.Binary_Option :=
     (Short_Name  => "h",
      Long_Name   => "help",
      Description => "Display help information");
   Global_Verbose_Option : constant VSS.Command_Line.Binary_Option :=
     (Short_Name  => "v",
      Long_Name   => "verbose",
      Description => "Verbose output");
   Command_Option        : constant VSS.Command_Line.Positional_Option :=
     (Name        => "<command>",
      Description => "Command to be executed: info, list");

   List_Long_Option      : constant VSS.Command_Line.Binary_Option :=
     (Short_Name  => "l",
      Long_Name   => "long",
      Description => "Enable long output format");

   Info_Advanced_Option  : constant VSS.Command_Line.Binary_Option :=
     (Short_Name  => "a",
      Long_Name   => "advanced",
      Description => "Advanced output");

   Output                : VSS.Text_Streams.Output_Text_Stream'Class :=
     VSS.Text_Streams.Standards.Standard_Output;
   Success               : Boolean := True;

begin
   --  Add global options

   Parser.Add_Option (Global_Help_Option);
   Parser.Add_Option (Global_Verbose_Option);
   Parser.Add_Option (Command_Option);

   --  Parse command line to get command name.

   if not Parser.Parse (VSS.Application.Arguments) then
      VSS.Command_Line.Report_Error (Parser.Error_Message);
   end if;

   --  Add command's options when command is specified, report error for
   --  unknown command.

   if not Parser.Is_Specified (Command_Option) then
      if not Parser.Is_Specified (Global_Help_Option) then
         VSS.Command_Line.Report_Error ("no command specified");
      end if;

   elsif Parser.Value (Command_Option) = "info" then
      Parser.Add_Option (Info_Advanced_Option);

   elsif Parser.Value (Command_Option) = "list" then
      Parser.Add_Option (List_Long_Option);

   else
      VSS.Command_Line.Report_Error
        (VSS.Strings.Templates.To_Virtual_String_Template
           ("unknown command: '{}'").Format
             (VSS.Strings.Formatters.Strings.Image
                  (Parser.Value (Command_Option))));
   end if;

   --  Reparse command line

   if not Parser.Parse (VSS.Application.Arguments) then
      VSS.Command_Line.Report_Error (Parser.Error_Message);
   end if;

   --  Process help option

   if Parser.Is_Specified (Global_Help_Option) then
      VSS.Command_Line.Report_Error (Parser.Help_Text);
   end if;

   --  Report unknown options

   if not Parser.Unknown_Option_Arguments.Is_Empty then
      VSS.Command_Line.Report_Error
        (VSS.Strings.Templates.To_Virtual_String_Template
           ("unknown option: '{}'").Format
             (VSS.Strings.Formatters.Strings.Image
                  (Parser.Unknown_Option_Arguments.First_Element)));
   end if;

   --  Execute command

   if Parser.Value (Command_Option) = "info" then
      Output.Put_Line ("Some information", Success);

   elsif Parser.Value (Command_Option) = "list" then
      for Name of Parser.Positional_Arguments loop
         Output.Put_Line (Name, Success);
      end loop;
   end if;
end Command_Line_Command;

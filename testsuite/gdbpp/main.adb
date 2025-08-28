with Ada.Strings.Unbounded;
with VSS.Strings;

procedure Main is

   SS1 : String := "standard string";
   US1 : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String ("unbounded string");

   VSN : VSS.Strings.Virtual_String;
   VS1 : VSS.Strings.Virtual_String := "short text";
   VS2 : VSS.Strings.Virtual_String := "very long text that should not be stored in-place";
   VI1 : VSS.Strings.Virtual_String := "Привет! Это тоже длинная строка";

begin
   null;
end Main;

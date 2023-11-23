with Ada.Strings.Unbounded;

with GPR2.Project.Registry.Attribute;
with GPR2.Project.View;

package Test is

   function Default_Library_Standalone
     (View : GPR2.Project.View.Object) return GPR2.Value_Type
   is (if View.Is_Library and then View.Has_Any_Interfaces
       then "standard" else "no");

   DLSA : constant GPR2.Project.Registry.Attribute.Default_Value_Callback :=
            Default_Library_Standalone'Access;

   function Locate_Exec_On_Path (Exec_Name : String) return String;

   function Get_Exported_Registry_From_Tool
     (Exec_Name : String;
      Default   : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.Null_Unbounded_String) return
      Ada.Strings.Unbounded.Unbounded_String;

   procedure Import_Registry_From_Tool
     (Exec_Name : String;
      Default   : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.Null_Unbounded_String);

end Test;
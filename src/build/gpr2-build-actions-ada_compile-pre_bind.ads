--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Project.Registry.Attribute;

package GPR2.Build.Actions.Ada_Compile.Pre_Bind is
   package PRA renames GPR2.Project.Registry.Attribute;
   type Ada_Compile_Pre_Bind_Id (<>) is
    new Actions.Ada_Compile.Ada_Compile_Id with private;

   overriding function Image (Self : Ada_Compile_Pre_Bind_Id) return String;

   overriding function Db_Filename
     (Self : Ada_Compile_Pre_Bind_Id) return Simple_Name;

   type Object is new Actions.Ada_Compile.Object with private;

   Undefined : constant Object;

   overriding function UID (Self : Object) return Actions.Action_Id'Class;

   overriding function Is_Defined (Self : Object) return Boolean;

   overriding procedure Initialize
     (Self : in out Object; Src : GPR2.Build.Compilation_Unit.Object);

   overriding procedure Post_Command (Self : in out Object);

private

   type Ada_Compile_Pre_Bind_Id (Name_Len : Natural)
     is new Actions.Ada_Compile.Ada_Compile_Id (Name_Len) with null record;

   overriding function Image
     (Self : Ada_Compile_Pre_Bind_Id) return String is
        ("Compile Ada pre-bind : " & String (Self.Unit_Name) &
         " (" & String (Self.Ctxt.Path_Name.Simple_Name) & ")");

   overriding function Db_Filename
     (Self : Ada_Compile_Pre_Bind_Id) return Simple_Name is
        (Simple_Name ("compile_ada_pre_bind_" & To_Lower (Self.Unit_Name) &
         "_" & To_Lower (Self.Ctxt.Name) & ".json"));

   type Object is new Actions.Ada_Compile.Object with null record;

   Undefined : constant Object := (others => <>);

   overriding function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);
end GPR2.Build.Actions.Ada_Compile.Pre_Bind;

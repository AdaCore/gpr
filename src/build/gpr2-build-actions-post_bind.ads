--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Actions.Ada_Bind;
with GPR2.Build.Artifacts.Files;
with GPR2.Project.Registry.Attribute;

with GNATCOLL.OS.Process;

package GPR2.Build.Actions.Post_Bind is

   package PRA renames GPR2.Project.Registry.Attribute;

   type Post_Bind_Id (<>) is new Action_Id with private;

   type Object is new Actions.Object with private;
   --  Action responsible for building Ada sources generated by gnatbind

   Undefined : constant Object;

   function Is_Defined (Self : Object) return Boolean;

   function Create
     (Impl   : Artifacts.Files.Object;
      View   : GPR2.Project.View.Object;
      Binder : GPR2.Build.Actions.Ada_Bind.Object) return Object;

   overriding procedure On_Tree_Insertion
     (Self     : Object;
      Db       : in out GPR2.Build.Tree_Db.Object;
      Messages : in out GPR2.Log.Object);

   overriding procedure Compute_Command
     (Self : in out Object;
      Args : out GNATCOLL.OS.Process.Argument_List;
      Env  : out GNATCOLL.OS.Process.Environment_Dict);

   function Object_File (Self : Object) return Artifacts.Files.Object;

private

   type Post_Bind_Id (Name_Len : Natural) is new Action_Id with record
      Input : Filename_Type (1 .. Name_Len);
   end record;

   overriding function Image (Self : Post_Bind_Id) return String is
      ("[Compile Ada] " & String (Self.Input));

   overriding function Db_Filename (Self : Post_Bind_Id) return Simple_Name is
     ("ada_post_bind" & Self.Input & ".json");

   overriding function "<" (L, R : Post_Bind_Id) return Boolean is
     (L.Input < R.Input);

   type Object is new Actions.Object with record
      Input  : Artifacts.Files.Object;
      Output : Artifacts.Files.Object;
      Binder : Ada_Bind.Object;
      --  ??? Ideally we store Ada_Bind_Id here, but it's unconstrained so
      --  we store the object (so that post-bind object is unconstrained) but
      --  need to access it via Tree_Db.Actions (Binder.UID) to make sure the
      --  information is up-to-date
      View   : GPR2.Project.View.Object;
   end record;

   overriding function UID (Self : Object) return Action_Id'Class;

   overriding procedure Post_Command (Self   : in out Object;
                                      Status : Execution_Status);

   overriding function Working_Directory
     (Self : Object) return Path_Name.Object
   is (Self.View.Object_Directory);

   overriding function View (Self : Object) return GPR2.Project.View.Object is
      (Self.View);

   Undefined : constant Object := (others => <>);

   function Is_Defined (Self : Object) return Boolean is
     (Self /= Undefined);

   function Object_File (Self : Object) return Artifacts.Files.Object is
     (Self.Output);

end GPR2.Build.Actions.Post_Bind;

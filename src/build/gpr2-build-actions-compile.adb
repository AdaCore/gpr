--
--  Copyright (C) 2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.Build.Artifacts.Files;
with GPR2.Build.Tree_Db;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.View.Set;
with GPR2.Project.Registry.Attribute;

package body GPR2.Build.Actions.Compile is

   package PAI renames GPR2.Project.Attribute_Index;
   package PRA renames GPR2.Project.Registry.Attribute;

   function Lookup
     (V          : GPR2.Project.View.Object;
      BN         : Simple_Name;
      Must_Exist : Boolean) return GPR2.Path_Name.Object;
   --  Look for BN in V's hierarchy of object/lib directories

   ------------
   -- Create --
   ------------

   function Create (Src : GPR2.Build.Source.Object) return Object is
      Src_Name : constant Simple_Name :=
                   Src.Path_Name.Simple_Name;
      UID      : constant Compile_Id :=
                   (Name_Len => Src_Name'Length,
                    Lang     => Src.Language,
                    Ctxt     => Src.Owning_View,
                    Src_Name => Src_Name);
      BN       : constant Simple_Name := Src.Path_Name.Base_Filename;
      O_Suff   : constant Simple_Name :=
                   Simple_Name
                     (UID.Ctxt.Attribute
                        (PRA.Compiler.Object_File_Suffix,
                         PAI.Create (Src.Language)).Value.Text);
      Result   : Object :=
                   (Input_Len => UID.Name_Len,
                    UID       => UID,
                    others    => <>);
   begin
      Result.Obj_File := Lookup (UID.Ctxt, BN & O_Suff, True);

      if not Result.Obj_File.Is_Defined then
         Result.Obj_File := UID.Ctxt.Object_Directory.Compose (BN & O_Suff);
      end if;

      return Result;
   end Create;

   function Lookup
     (V          : GPR2.Project.View.Object;
      BN         : Simple_Name;
      Must_Exist : Boolean) return GPR2.Path_Name.Object
   is
      Todo      : GPR2.Project.View.Set.Object;
      Done      : GPR2.Project.View.Set.Object;
      Current   : GPR2.Project.View.Object := V;
      Candidate : GPR2.Path_Name.Object;
   begin
      loop
         Candidate := Current.Object_Directory.Compose (BN);
         exit when not Must_Exist or else Candidate.Exists;

         if Current.Is_Extending then
            Todo.Union (Current.Extended);
            Todo.Difference (Done);
         end if;

         if Todo.Is_Empty then
            return GPR2.Path_Name.Undefined;
         else
            Done.Include (Current);
            Current := Todo.First_Element;
            Todo.Delete_First;
         end if;
      end loop;

      return Candidate;
   end Lookup;

   -----------------------
   -- On_Tree_Insertion --
   -----------------------

   overriding procedure On_Tree_Insertion
     (Self     : Object;
      Db       : in out GPR2.Build.Tree_Db.Object;
      Messages : in out GPR2.Log.Object)
   is
   begin
      Db.Add_Output
        (Self.UID,
         Artifacts.Files.Create (Self.Obj_File),
         Messages);

      if Messages.Has_Error then
         return;
      end if;

      Db.Add_Input
        (Self.UID,
         Artifacts.Files.Create (Self.Input.Path_Name),
         True);
   end On_Tree_Insertion;

end GPR2.Build.Actions.Compile;

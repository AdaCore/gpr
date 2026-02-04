--
--  Copyright (C) 2020-2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with Ada.Containers.Ordered_Sets;

with GPR2.Build.Source.Sets;
with GPR2.C.JSON.Arrays;
with GPR2.C.JSON.Values;
with GPR2.C.JSON.Codecs.Contexts;
with GPR2.C.JSON.Codecs.Messages;
with GPR2.C.JSON.Codecs.Options;
with GPR2.C.JSON.Codecs.Sources;
with GPR2.C.Registry;
with GPR2.C.Reporter;
with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;

package body GPR2.C.Tree is

   function Get_Tree
     (Request : GPR2.C.JSON.Objects.JSON_Object)
      return GPR2.Project.Tree.Object;

   function Equal
     (Left  : GPR2.Build.Source.Object;
      Right : GPR2.Build.Source.Object) return Boolean;

   function Less
     (Left  : GPR2.Build.Source.Object;
      Right : GPR2.Build.Source.Object) return Boolean;

   package Sets is
     new Ada.Containers.Ordered_Sets
       (Element_Type => GPR2.Build.Source.Object,
        "<"          => Less,
        "="          => Equal);

   ------------------------
   -- Ada_Source_Closure --
   ------------------------

   procedure Ada_Source_Closure
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object)
   is
      use type GPR2.Project.View.Object;

      Tree              : constant GPR2.Project.Tree.Object :=
        Get_Tree (Request);
      Root_Project_Only : constant Boolean :=
        Request.Value ("root_project_only").To_Boolean;
      Externally_Built  : constant Boolean :=
        Request.Value ("externally_built").To_Boolean;
      Sources           : Sets.Set;
      Closure           : GPR2.C.JSON.Arrays.JSON_Array;

   begin
      for Root of Tree.Namespace_Root_Projects loop
         declare
            Visible_Sources : constant GPR2.Build.Source.Sets.Object :=
              Root.Visible_Sources;

         begin
            for Source of Visible_Sources loop
               if (Source.Owning_View = Tree.Root_Project
                   or else not Root_Project_Only
                   or else Externally_Built
                   or else not Source.Owning_View.Is_Externally_Built)
                 and then Source.Is_Visible
               then
                  if not Sources.Contains (Source) then
                     Sources.Include (Source);
                     Closure.Append
                       (GPR2.C.JSON.Codecs.Sources.Encode (Source));
                  end if;
               end if;
            end loop;
         end;
      end loop;

      Result.Insert ("ada_closure", Closure);
   end Ada_Source_Closure;

   -------------------------
   -- Artifacts_Directory --
   -------------------------

   procedure Artifacts_Directory
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object)
   is
      Tree : constant GPR2.Project.Tree.Object := Get_Tree (Request);

   begin
      Result.Insert
        ("artifacts_directory",
         (if Tree.Is_Defined
          then GPR2.C.JSON.Values.To_JSON_Value
                 (String (Tree.Artifacts_Dir.Value))
          else GPR2.C.JSON.Values.Null_Value));
   end Artifacts_Directory;

   -------------
   -- Context --
   -------------

   procedure Context
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object)
   is
      Tree : constant GPR2.Project.Tree.Object := Get_Tree (Request);

   begin
      Result.Insert
        ("context",
         (if Tree.Is_Defined
          then GPR2.C.JSON.Codecs.Contexts.Encode (Tree.Context).To_JSON_Value
          else GPR2.C.JSON.Values.Null_Value));
   end Context;

   ----------------
   -- Destructor --
   ----------------

   procedure Destructor
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object)
   is
      pragma Unreferenced (Result);

   begin
      GPR2.C.Registry.Tree.Unregister (Request.Value ("tree_id"));
   end Destructor;

   -----------
   -- Equal --
   -----------

   function Equal
     (Left  : GPR2.Build.Source.Object;
      Right : GPR2.Build.Source.Object) return Boolean
   is
      use type GPR2.Path_Name.Object;

   begin
      return Left.Path_Name = Right.Path_Name;
   end Equal;

   --------------
   -- Get_Tree --
   --------------

   function Get_Tree
     (Request : GPR2.C.JSON.Objects.JSON_Object)
      return GPR2.Project.Tree.Object is
   begin
      return GPR2.C.Registry.Tree.Lookup (Request.Value ("tree_id"));
   end Get_Tree;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object)
   is
      Tree  : constant GPR2.Project.Tree.Object := Get_Tree (Request);
      Views : GPR2.C.JSON.Arrays.JSON_Array;

   begin
      for J in Tree.Iterate loop
         Views.Append
           (GPR2.C.Registry.View.Register (GPR2.Project.Tree.Element (J)));
      end loop;

      Result.Insert ("iterate", Views);
   end Iterate;

   ----------
   -- Less --
   ----------

   function Less
     (Left  : GPR2.Build.Source.Object;
      Right : GPR2.Build.Source.Object) return Boolean
   is
      use type GPR2.Path_Name.Object;

   begin
      return Left.Path_Name < Right.Path_Name;
   end Less;

   ----------
   -- Load --
   ----------

   procedure Load
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object)
   is
      Tree     : GPR2.Project.Tree.Object;
      Reporter : GPR2.C.Reporter.Object;
      Loaded   : Boolean;

   begin
      Loaded :=
        Tree.Load
          (Options      =>
             GPR2.C.JSON.Codecs.Options.Decode (Request.Value ("options")),
           With_Runtime => Request.Value ("with_runtime").To_Boolean,
           Reporter     => Reporter);

      Result.Insert ("tree_id", GPR2.C.Registry.Tree.Register (Tree));
      Result.Insert ("is_loaded", GPR2.C.JSON.Values.To_JSON_Value (Loaded));
   end Load;

   ------------------
   -- Log_Messages --
   ------------------

   procedure Log_Messages
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object)
   is
      Tree     : constant GPR2.Project.Tree.Object := Get_Tree (Request);
      Messages : GPR2.C.JSON.Arrays.JSON_Array;

   begin
      if Tree.Is_Defined and then Tree.Has_Messages then
         for Message of Tree.Log_Messages.all loop
            Messages.Append (GPR2.C.JSON.Codecs.Messages.Encode (Message));
         end loop;
      end if;

      Result.Insert ("messages", Messages);
   end Log_Messages;

   ------------------
   -- Root_Project --
   ------------------

   procedure Root_Project
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object)
   is
      Tree : constant GPR2.Project.Tree.Object := Get_Tree (Request);

   begin
      Result.Insert
        ("view_id",
         (if Tree.Is_Defined
          then GPR2.C.Registry.View.Register (Tree.Root_Project)
          else GPR2.C.JSON.Values.Null_Value));
   end Root_Project;

   ---------------------
   -- Runtime_Project --
   ---------------------

   procedure Runtime_Project
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object)
   is
      Tree : constant GPR2.Project.Tree.Object := Get_Tree (Request);

   begin
      Result.Insert
        ("view_id",
         (if Tree.Is_Defined
          then GPR2.C.Registry.View.Register (Tree.Runtime_Project)
          else GPR2.C.JSON.Values.Null_Value));
   end Runtime_Project;

   -----------------
   -- Set_Context --
   -----------------

   procedure Set_Context
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object)
   is
      Tree    : GPR2.Project.Tree.Object := Get_Tree (Request);
      Context : constant GPR2.Context.Object :=
        GPR2.C.JSON.Codecs.Contexts.Decode (Request.Value ("context"));

   begin
      Result.Insert
        ("success",
         GPR2.C.JSON.Values.To_JSON_Value (Tree.Set_Context (Context)));
   end Set_Context;

   ------------
   -- Target --
   ------------

   procedure Target
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object)
   is
      Tree : constant GPR2.Project.Tree.Object := Get_Tree (Request);

   begin
      if Tree.Is_Defined then
         Result.Insert ("target", String (Tree.Target));

      else
         Result.Insert ("target", GPR2.C.JSON.Values.Null_Value);
      end if;
   end Target;

   --------------------
   -- Update_Sources --
   --------------------

   procedure Update_Sources
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object)
   is
      pragma Unreferenced (Result);

      Tree : constant GPR2.Project.Tree.Object := Get_Tree (Request);

   begin
      Tree.Update_Sources;
   end Update_Sources;

end GPR2.C.Tree;

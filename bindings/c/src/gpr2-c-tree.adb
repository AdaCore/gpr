--
--  Copyright (C) 2020-2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-Exception
--

with GPR2.C.JSON.Arrays;
with GPR2.C.JSON.Values;
with GPR2.C.JSON.Codecs.Contexts;
with GPR2.C.JSON.Codecs.Messages;
with GPR2.C.JSON.Codecs.Options;
with GPR2.C.Registry;
with GPR2.C.Reporter;
with GPR2.Context;
with GPR2.Project.Tree;

package body GPR2.C.Tree is

   function Get_Tree
     (Request : GPR2.C.JSON.Objects.JSON_Object)
      return GPR2.Project.Tree.Object;

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

   --------------
   -- Get_Tree --
   --------------

   function Get_Tree
     (Request : GPR2.C.JSON.Objects.JSON_Object)
      return GPR2.Project.Tree.Object is
   begin
      return GPR2.C.Registry.Tree.Lookup (Request.Value ("tree_id"));
   end Get_Tree;

   ----------
   -- Load --
   ----------

   procedure Load
     (Request : GPR2.C.JSON.Objects.JSON_Object;
      Result  : out GPR2.C.JSON.Objects.JSON_Object)
   is
      Tree     : GPR2.Project.Tree.Object;
      Reporter : GPR2.C.Reporter.Object;
      Dummy    : Boolean;

   begin
      Dummy :=
        Tree.Load
          (Options      =>
             GPR2.C.JSON.Codecs.Options.Decode (Request.Value ("options")),
           With_Runtime => Request.Value ("with_runtime").To_Boolean,
           Reporter     => Reporter);

      Result.Insert ("tree_id", GPR2.C.Registry.Tree.Register (Tree));
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
